%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck <gjd@cs.mu.oz.au>
%
%---------------------------------------------------------------------------%

:- module compile.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module hl_prog.
:- import_module ll_prog.

    % Compile a `hl_prog' into a `ll_prog'.
    %
:- pred compile(bool::in,hl_prog::in,list(hl_pragma)::in,ll_prog::out,
    io::di,io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- import_module float.
:- import_module int.
:- import_module map.
:- import_module time.

:- import_module cadmium_common.
:- import_module cadmium_error.
:- import_module hl_ac_analysis.
:- import_module hl_cc_analysis.
:- import_module hl_var_analysis.
:- import_module ll_compile.
:- import_module ll_optimise_peephole.
:- import_module ll_reg_analysis.
:- import_module ml_compile.
:- import_module ml_optimise_alias.
:- import_module ml_optimise_dead_var.
:- import_module ml_optimise_empty.
:- import_module ml_optimise_iterators.
:- import_module ml_optimise_lookups.
:- import_module ml_optimise_loop.
:- import_module ml_optimise_merge.
:- import_module ml_optimise_order.
:- import_module ml_optimise_switch.

%---------------------------------------------------------------------------%

compile(Debug,HLProg,Pragmas,LLProg,!IO) :-
    some [!MLProg,!LLProg] (
        Verbose = no,
        stage(Verbose,"variable analysis",hl_var_analysis,HLProg,VarInfo,!IO),
        stage(Verbose,"AC analysis",hl_ac_analysis(VarInfo),HLProg,ACInfo,!IO),
        stage(Verbose,"CC analysis",hl_cc_analysis(VarInfo),HLProg,CCInfo,!IO),
        throw_errors(!IO),
        stage(Verbose,"hl_prog to ml_prog",ml_compile_hl_prog(Debug,ACInfo),
            HLProg,!:MLProg,!IO),
        stage(Verbose,"lookup optimisation",ml_optimise_lookups,!MLProg,!IO),
        stage(Verbose,"iterator optimisation",ml_optimise_iterators,!MLProg,
            !IO),
        stage(Verbose,"loop optimisation",ml_optimise_loop,!MLProg,!IO),
        stage(Verbose,"merge optimisation",ml_optimise_merge,!MLProg,!IO),
        stage(Verbose,"ordering optimisation",ml_optimise_order,!MLProg,!IO),
        stage(Verbose,"alias optimisation",ml_optimise_alias,!MLProg,!IO),
        stage(Verbose,"dead-var optimisation",ml_optimise_dead_var,!MLProg,!IO),
        stage(Verbose,"empty optimisation",ml_optimise_empty,!MLProg,!IO),
        stage(Verbose,"switch optimisation",ml_optimise_switch,!MLProg,!IO),
        stage(Verbose,"ml_prog to ll_prog",
            ll_compile_ml_prog(Debug,VarInfo,ACInfo,CCInfo,Pragmas),!.MLProg,
            !:LLProg,!IO),
        stage(Verbose,"peephole optimisation",ll_optimise_peephole,!LLProg,!IO),
        stage(Verbose,"register allocation",ll_reg_analysis,!LLProg,!IO),
        % pprint_ll_prog(!.LLProg,!IO),
        throw_errors(!IO),
        LLProg = !.LLProg
    ).

%---------------------------------------------------------------------------%

:- pred stage(bool,string,pred(T,U,io,io),T,U,io,io).
:- mode stage(in,in,pred(in,out,di,uo) is det,in,out,di,uo) is det.

stage(Verbose,Name,Stage,In,Out,!IO) :-
    ( Verbose = yes,
        write_string("Cadmium: ",!IO),
        write_string(Name,!IO),
        write_string(": ",!IO),
        clock(Clock0,!IO),
        Stage(In,Out,!IO),
        clock(Clock1,!IO),
        Time = 1000.0*float(Clock1 - Clock0)/float(clocks_per_sec),
        write_float(Time,!IO),
        write_string("ms\n",!IO)
    ; Verbose = no,
        Stage(In,Out,!IO)
    ).

