%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% Order ml_instrs such that cheap/selective instructions are executed first.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_order.
:- interface.

:- import_module ml_prog.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred ml_optimise_order(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

:- pred ml_instr_cost(ml_instr::in,float::out) is det.

:- pred ml_instr_selectivity(ml_instr::in,float::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hl_prog.

:- import_module int.
:- import_module float.
:- import_module list.
:- import_module require.

%---------------------------------------------------------------------------%

ml_optimise_order(!MLProg,!IO) :-
    ml_prog_b_block_map(ml_b_block_optimise_order,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_order(ml_proc::in,ml_b_block::in,ml_b_block::out,
    ml_fixed::in,ml_fixed::out,io::di,io::uo) is det.

ml_b_block_optimise_order(MLProc,!BBlk,!Fixed,!IO) :-
    !.BBlk = ml_b_block(BId,MLItr,Instrs,SId),
    ml_instrs_optimise_order(MLProc,Instrs,NInstrs,!Fixed,!IO),
    !:BBlk = ml_b_block(BId,MLItr,NInstrs,SId).

%---------------------------------------------------------------------------%

:- pred ml_instrs_optimise_order(ml_proc::in,list(ml_instr)::in,
    list(ml_instr)::out,ml_fixed::in,ml_fixed::out,io::di,io::uo) is det.

ml_instrs_optimise_order(_,[],[],!Fixed,!IO).
ml_instrs_optimise_order(MLProc,!Instrs,!Fixed,!IO) :-
    !.Instrs = [HInstr|TInstrs],
    ml_score(MLProc,HInstr,TInstrs,!.Fixed,HScore),
    ml_instr_pick_best(MLProc,TInstrs,!.Instrs,!.Fixed,HScore,2,1,Idx,!IO),
    ml_get_instr(Idx,!Instrs,Instr),
    ml_instr_update_fixed(Instr,!Fixed),
    ml_instrs_optimise_order(MLProc,!Instrs,!Fixed,!IO),
    !:Instrs = [Instr|!.Instrs].

%---------------------------------------------------------------------------%

% Finds the instruction with the best score. The second argument functions as
% an iterator, the third contains all instructions.

:- pred ml_instr_pick_best(ml_proc::in,list(ml_instr)::in,list(ml_instr)::in,
    ml_fixed::in,float::in,int::in,int::in,int::out,io::di,io::uo) is det.

ml_instr_pick_best(_,[],_,_,_,_,!Idx,!IO).
ml_instr_pick_best(MLProc,[Instr|Instrs],AllInstrs,Fixed,Score,CIdx,!Idx,!IO) :-
    ( ml_instr_is_fixed(MLProc,Fixed,Instr),
      delete_first(AllInstrs,Instr,RemainingInstrs) ->
        ml_score(MLProc,Instr,RemainingInstrs,Fixed,NScore),
        ( NScore > Score ->
            ml_instr_pick_best(MLProc,Instrs,AllInstrs,Fixed,NScore,CIdx+1,
		CIdx,!:Idx,!IO)
        ;   ml_instr_pick_best(MLProc,Instrs,AllInstrs,Fixed,Score,CIdx+1,
		!Idx,!IO)
        )
    ;   ml_instr_pick_best(MLProc,Instrs,AllInstrs,Fixed,Score,CIdx+1,!Idx,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_get_instr(int::in,list(ml_instr)::in,list(ml_instr)::out,
    ml_instr::out) is det.

ml_get_instr(_,[],_,_) :-
    unexpected($file, $pred, "index out of range").
ml_get_instr(N,[Instr|Instrs],NInstrs,NInstr) :-
    ( N = 1 ->
        NInstr = Instr,
        NInstrs = Instrs
    ;   ml_get_instr(N-1,Instrs,NNInstrs,NInstr),
        NInstrs = [Instr|NNInstrs]
    ).

%---------------------------------------------------------------------------%

    % Has look-ahead of 0..1 instructions.  This is for the selection
    % of get_arg/3, since a set of get_args/3 all have the same score, but
    % may allow for the scheduling of better/worse subsequent instructions.
    %
:- pred ml_score(ml_proc::in,ml_instr::in,list(ml_instr)::in,ml_fixed::in,
    float::out) is det.

ml_score(MLProc,Instr,RemainingInstrs,Fixed,Score) :-
    ml_score_0(Instr,RemainingInstrs,Score0),
    ( ( Instr = get_arg(_,_,Out)
      ; Instr = copy(_,Out) ) ->
        ml_fix(Out,Fixed,NFixed),
        ml_score_best(MLProc,RemainingInstrs,RemainingInstrs,NFixed,0.0,Score1),
        Score = 0.9*Score1
    ;   Score = Score0
    ).

%---------------------------------------------------------------------------%

% Finds the instruction with the best score, different from get_arg/3 and 
% copy/2. Similar to ml_instr_pick_best/10

:- pred ml_score_best(ml_proc::in,list(ml_instr)::in,list(ml_instr)::in,
    ml_fixed::in,float::in,float::out) is det.

ml_score_best(_,[],_,_,!Score).
ml_score_best(MLProc,[Instr|Instrs],AllInstrs,Fixed,!Score) :-
    ( Instr \= get_arg(_,_,_),
      Instr \= copy(_,_),
      ml_instr_is_fixed(MLProc,Fixed,Instr),
      delete_first(AllInstrs,Instr,RemainingInstrs) ->
        ml_score_0(Instr,RemainingInstrs,Score0),
        ( Score0 > !.Score ->
            !:Score = Score0
        ;   true
        ),
        ml_score_best(MLProc,Instrs,AllInstrs,Fixed,!Score)
    ;   ml_score_best(MLProc,Instrs,AllInstrs,Fixed,!Score)
    ).

%---------------------------------------------------------------------------%

% new second argument, which is a list of all remaining instructions

:- pred ml_score_0(ml_instr::in,list(ml_instr)::in,float::out) is det.

ml_score_0(Instr,RemainingInstrs,Score) :-
    ml_instr_selectivity_ext(Instr,RemainingInstrs,Sel),
    ml_instr_cost(Instr,Cost),
    Score = Sel - 0.5*Sel*(1.0 - 1.0/Cost).

%---------------------------------------------------------------------------%

% Extended version of instruction selectivity for pattern guards.
% Pattern guards are compiled as pattern_guard(Var,Guard,MaybeConjColl) where
% Var is a fresh variable which will contain the result of normalising Guard.
% If the guard is not followed by an instruction that depends on Var, then the
% pattern guard cannot fail (and should be postponed as much as possible).
% The second argument is a list of all remaining instructions.

:- pred ml_instr_selectivity_ext(ml_instr::in,list(ml_instr)::in,float::out).

ml_instr_selectivity_ext(Instr,Instrs,Selectivity) :-
    (   Instr = pattern_guard(Var,_,_),
	\+ (
	    member(OtherInstr,Instrs),
	    ml_instr_depends_on(OtherInstr,Instrs,Var)
	)
    ->  Selectivity = 0.0
    ;   ml_instr_selectivity(Instr,Selectivity)
    ).

% ml_instr_depends_on(Instr,Instrs,Var):
%  Checks whether Instr depends on Var in that it may or may not fail depending
%  on the value of Var. This is so in the following cases:
%  - Instr is a guard or pattern guard involving Var;
%  - Instr checks the functor/type of Var (is_type/2, is_int/2, ...);
%  - Instr retrieves the annotations of Var;
%  - Instr is an equality test involving Var;
%  - Instr copies Var into another variable Var2, and some instruction in
%    Instrs depends on Var2;
%  Note that a get_arg/3 or get_ac_args/4 instruction is always accompanied by
%  a functor test. 

:- pred ml_instr_depends_on(ml_instr::in,list(ml_instr)::in,hl_var::in).

ml_instr_depends_on(guard(Guard,_),_,Var) :- 
    model_contains_var(Guard,Var).
ml_instr_depends_on(pattern_guard(_,Guard,_),_,Var) :- 
    model_contains_var(Guard,Var).
ml_instr_depends_on(is_type(Var,_),_,Var).
ml_instr_depends_on(is_int(Var,_),_,Var).
ml_instr_depends_on(is_float(Var,_),_,Var).
ml_instr_depends_on(is_string(Var,_),_,Var).
ml_instr_depends_on(is_named_var(Var,_),_,Var).
ml_instr_depends_on(is_functor(Var,_,_),_,Var).
ml_instr_depends_on(is_ac_functor(Var,_,_,_),_,Var).
ml_instr_depends_on(get_annots(Var,_),_,Var).
ml_instr_depends_on(eq(Var1,Var2),_,Var) :-
    ( Var1 = Var ; Var2 = Var ).
ml_instr_depends_on(copy(Var,Var2),Instrs,Var) :-
    member(OtherInstr,Instrs),
    ml_instr_depends_on(OtherInstr,Instrs,Var2).

% model_contains_var(Model,Var)
%  Succeeds if Model contains Var.

:- pred model_contains_var(hl_model::in,hl_var::in) is semidet.

model_contains_var(Model,Var) :-
    (   Model = var(Var,_)
    ;   Model = functor(_,Args,_),
	member(Arg,Args),
	model_contains_var(Arg,Var)
    ).


    % XXX: Base the selectivity of get_ac_args/get_cc_args based on the 
    %      lookup!
    %

ml_instr_selectivity(guard(_,_)             ,0.25).
ml_instr_selectivity(pattern_guard(_,_,_)   ,0.08).
ml_instr_selectivity(is_type(_,_)           ,0.50).
ml_instr_selectivity(is_int(_,_)            ,0.60).
ml_instr_selectivity(is_float(_,_)          ,0.60).
ml_instr_selectivity(is_string(_,_)         ,0.60).
ml_instr_selectivity(is_named_var(_,_)      ,0.60).
ml_instr_selectivity(is_functor(_,_,_)      ,0.60).
ml_instr_selectivity(is_ac_functor(_,_,_,_) ,0.60).
ml_instr_selectivity(get_arg(_,_,_)         ,0.00).
ml_instr_selectivity(get_annots(_,_)        ,0.00).
ml_instr_selectivity(eq(_,_)                ,0.30).
ml_instr_selectivity(copy(_,_)              ,0.00).
ml_instr_selectivity(is_diff(_,_)           ,0.02).
ml_instr_selectivity(get_ac_args(_,_,_,_)   ,0.30).
ml_instr_selectivity(get_cc_args(_,_,_,_)   ,0.25).
ml_instr_selectivity(get_collector(_,_,_,_) ,0.00).

%---------------------------------------------------------------------------%

ml_instr_cost(guard(_,_)                      ,30.0).
ml_instr_cost(pattern_guard(_,_,_)            ,30.0).
ml_instr_cost(is_type(_,_)                    ,1.1).
ml_instr_cost(is_int(_,_)                     ,1.0).
ml_instr_cost(is_float(_,_)                   ,1.3).
ml_instr_cost(is_string(_,_)                  ,2.5).
ml_instr_cost(is_named_var(_,_)               ,1.0).
ml_instr_cost(is_functor(_,_,_)               ,1.0).
ml_instr_cost(is_ac_functor(_,ml_at_least,_,_),1.1).
ml_instr_cost(is_ac_functor(_,ml_exactly,_,_) ,1.0).
ml_instr_cost(get_arg(_,_,_)                  ,1.2).
ml_instr_cost(get_annots(_,_)                 ,1.0).
ml_instr_cost(eq(_,_)                         ,10.0).
ml_instr_cost(copy(_,_)                       ,1.0).
ml_instr_cost(is_diff(_,_)                    ,1.1).
ml_instr_cost(get_ac_args(_,_,_,_)            ,20.0).
ml_instr_cost(get_cc_args(_,_,_,_)            ,22.0).
ml_instr_cost(get_collector(_,Ins,Outs,_)     ,Cost) :-
    Cost = 1.0 + float(length(Ins))*13.0 + float(length(Outs)).

%---------------------------------------------------------------------------%
:- end_module ml_optimise_order.
%---------------------------------------------------------------------------%
