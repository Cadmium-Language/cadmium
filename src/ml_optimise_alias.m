%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Optimise away alias vars.
%
% - This optimisation should only be applied after the order of instructions
%   has been fixed.
% - Favour eliminating introduced vars, since var_info/ac_info/etc., all use
%   original program vars.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_alias.
:- interface.

:- import_module io.
:- import_module ml_prog.

:- pred ml_optimise_alias(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.

:- import_module cadmium_common.
:- import_module hl_prog.

%---------------------------------------------------------------------------%

ml_optimise_alias(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_alias,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_alias(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_alias(_,!MLProc,!IO) :-
    ml_s_block_get_substitution(!.MLProc,ml_entry_s_block_id,init,Subs,!IO),
    ml_proc_get_src_var(!.MLProc,Src),
    hl_var_substitute(Subs,Src,NSrc),
    ml_proc_set_src_var(NSrc,!MLProc),
    ml_s_block_optimise_alias(Subs,ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_alias(ml_subs::in,ml_s_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_s_block_optimise_alias(Subs,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ml_proc_get_src_var(!.MLProc,Src),
    ( SBlk = ml_s_body_block(_,BodyInfo,Body),
        ml_body_info_substitute(Subs,BodyInfo,NBodyInfo),
        hl_model_substitute(Subs,Body,NBody),
        NSBlk = ml_s_body_block(SId,NBodyInfo,NBody),
        ml_s_block_insert(NSBlk,!MLProc)
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_optimise_alias(Src,Subs),BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_alias(hl_var::in,ml_subs::in,ml_b_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_optimise_alias(Src,Subs,BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
    ml_instrs_optimise_alias(Src,Subs,Instrs,NInstrs,!MLProc,!IO),
    ml_b_block_info_substitute(Subs,BBlkInfo,NBBlkInfo),
    NBBlk = ml_b_block(BId,NBBlkInfo,NInstrs,SId),
    ml_b_block_insert(NBBlk,!MLProc),
    ml_s_block_optimise_alias(Subs,SId,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_get_substitution(ml_proc::in,ml_s_block_id::in,
    ml_subs::in,ml_subs::out,io::di,io::uo) is det.

ml_s_block_get_substitution(MLProc,SId,!Subs,!IO) :-
    ml_s_block_lookup(MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_get_substitution(MLProc),BIds,!Subs,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_get_substitution(ml_proc::in,ml_b_block_id::in,
    ml_subs::in,ml_subs::out,io::di,io::uo) is det.

ml_b_block_get_substitution(MLProc,BId,!Subs,!IO) :-
    ml_b_block_lookup(MLProc,BId,BBlk),
    BBlk = ml_b_block(_,_,Instrs,SId),
    ml_instrs_get_substitution(MLProc,Instrs,!Subs,!IO),
    ml_s_block_get_substitution(MLProc,SId,!Subs,!IO).

%---------------------------------------------------------------------------%

:- pred ml_instrs_get_substitution(ml_proc::in,list(ml_instr)::in,ml_subs::in,
    ml_subs::out,io::di,io::uo) is det.

ml_instrs_get_substitution(_,[],!Subs,!IO).
ml_instrs_get_substitution(MLProc,[Instr|Instrs],!Subs,!IO) :-
    ( Instr = copy(Var1,Var2),
        hl_var_unify(Var1,Var2,!Subs)
    ; Instr = is_functor(Var1,_,Var2),
        hl_var_unify(Var2,Var1,!Subs)
    ; Instr = is_ac_functor(Var1,_,_,Var2),
        hl_var_unify(Var2,Var1,!Subs)
    ; ( Instr = get_ac_args(_,LId,_,_)
      ; Instr = get_cc_args(_,LId,_,_) ),
        ml_l_block_lookup(MLProc,LId,LBlk),
        LBlk = ml_l_block(_,_,_,LInstrs),
        ml_instrs_get_substitution(MLProc,LInstrs,!Subs,!IO)
    ; ( Instr = guard(_,_)
      ; Instr = pattern_guard(_,_,_)
      ; Instr = is_type(_,_)
      ; Instr = is_int(_,_)
      ; Instr = is_float(_,_)
      ; Instr = is_string(_,_)
      ; Instr = is_named_var(_,_)
      ; Instr = get_arg(_,_,_)
      ; Instr = get_annots(_,_)
      ; Instr = eq(_,_)
      ; Instr = is_diff(_,_)
      ; Instr = get_collector(_,_,_,_) ),
        true
    ),
    ml_instrs_get_substitution(MLProc,Instrs,!Subs,!IO).

%---------------------------------------------------------------------------%

:- pred ml_instrs_optimise_alias(hl_var::in,ml_subs::in,list(ml_instr)::in,
    list(ml_instr)::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_instrs_optimise_alias(_,_,[],[],!MLProc,!IO).
ml_instrs_optimise_alias(Src,Subs,[Instr|Instrs],NInstrs,!MLProc,!IO) :-
    ml_instr_substitute(Subs,Instr,NInstr,!MLProc),
    ml_instrs_optimise_alias(Src,Subs,Instrs,NInstrs0,!MLProc,!IO),
    ( NInstr = copy(Var,Var),
      Var \= Src ->
        NInstrs = NInstrs0
    ;   NInstrs = [NInstr|NInstrs0]
    ).

%---------------------------------------------------------------------------%
:- end_module ml_optimise_alias.
%---------------------------------------------------------------------------%
