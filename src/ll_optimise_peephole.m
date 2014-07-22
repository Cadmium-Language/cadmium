%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Peephole optimiser for ll_progs.
%
%---------------------------------------------------------------------------%

:- module ll_optimise_peephole.
:- interface.

:- import_module io.
:- import_module ll_prog.

:- pred ll_optimise_peephole(ll_prog(P)::in, ll_prog(P)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cadmium_common.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

ll_optimise_peephole(!LLProg,!IO) :-
    map_foldl(ll_proc_optimise_peephole,!LLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ll_proc_optimise_peephole(hl_symbol::in,ll_proc(P)::in,ll_proc(P)::out,
    io::di,io::uo) is det.

ll_proc_optimise_peephole(_Sym,!LLProc,!IO) :-
    !.LLProc = ll_proc(Label,Instrs,LookupInstrs,CreateDeps),
    ll_instrs_optimise_peephole(Instrs,NInstrs,!IO),
    !:LLProc = ll_proc(Label,NInstrs,LookupInstrs,CreateDeps).

%---------------------------------------------------------------------------%

:- pred ll_instrs_optimise_peephole(list(ll_instr(P))::in,
    list(ll_instr(P))::out,io::di,io::uo) is det.

ll_instrs_optimise_peephole([],[],!IO).
ll_instrs_optimise_peephole(Instrs@[Instr|Instrs0],NInstrs,!IO) :-
    ( Instrs = [get_arg(1,Reg)|Instrs1] ->
            % get_arg(1,X) == cset(X)
            %
        ll_instrs_optimise_peephole([cset(Reg)|Instrs1],NInstrs,!IO)

    ; Instrs = [call_sym(Sym),cpop(N),return|Instrs1] ->
            % Last Call Optimisation (LCO)
            % Note: the 'return' may still be needed because 'call_sym_lco'
            % may be equivalent to 'construct'.
            %
        ll_instrs_optimise_peephole([cpop(N),call_sym_lco(Sym),return|Instrs1],
            NInstrs,!IO)

    ; Instrs = [call_sym_lco(Sym)|Instrs1],
      is_ac(hl_symbol_name(Sym)) ->
            % Lazy AC return.
            %
        ll_instrs_optimise_peephole([call_ac_sym_lco(Sym)|Instrs1],NInstrs,!IO)

    ;
        ll_instrs_optimise_peephole(Instrs0,NInstrs0,!IO),
        NInstrs = [Instr|NInstrs0]
    ).

