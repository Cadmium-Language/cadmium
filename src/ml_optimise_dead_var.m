%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Remove instructions that generate variables that are not used for anything.
%
% Assumes ml_optimise_alias has been applied to imput ml_prog.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_dead_var.
:- interface.

:- import_module ml_prog.

:- import_module io.

:- pred ml_optimise_dead_var(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module maybe.

:- import_module cadmium_common.
:- import_module hl_prog.

%---------------------------------------------------------------------------%

:- type usage
    ---> used
    ;    used_if(list(hl_var)).

:- type usage_info == map(hl_var,usage).

%---------------------------------------------------------------------------%

ml_optimise_dead_var(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_dead_var,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_dead_var(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_dead_var(_Sym,!MLProc,!IO) :-
    ml_s_block_usage_analysis(!.MLProc,ml_entry_s_block_id,init,UsageInfo),
    ml_s_block_optimise_dead_var(UsageInfo,ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_dead_var(usage_info::in,ml_s_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_s_block_optimise_dead_var(UsageInfo,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_optimise_dead_var(UsageInfo),BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_dead_var(usage_info::in,ml_b_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_optimise_dead_var(UsageInfo,BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,MLItr,Instrs,SId),
    filter(ml_instr_optimise_dead_var(UsageInfo),Instrs,NInstrs),
    NBBlk = ml_b_block(BId,MLItr,NInstrs,SId),
    ml_b_block_insert(NBBlk,!MLProc),
    ml_s_block_optimise_dead_var(UsageInfo,SId,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_instr_optimise_dead_var(usage_info::in,ml_instr::in) is semidet.

ml_instr_optimise_dead_var(UsageInfo,Instr) :-
        % For now we will assume get_ac/cc_args is always used.
        %
    ( Instr = get_arg(_,_,Out),
        hl_var_is_used(UsageInfo,Out)
    ; Instr = get_annots(_,Out),
        hl_var_is_used(UsageInfo,Out)
    ; Instr = copy(_,Out),
        hl_var_is_used(UsageInfo,Out)
    ; Instr = get_collector(_,_,Outs,Out),
        ( hl_var_is_used(UsageInfo,Out)
        ; cadmium_common.any_true(hl_var_is_used(UsageInfo),Outs) )
    ; ( Instr = guard(_,_)
      ; Instr = pattern_guard(_,_,_)
      ; Instr = is_type(_,_)
      ; Instr = is_int(_,_)
      ; Instr = is_float(_,_)
      ; Instr = is_string(_,_)
      ; Instr = is_named_var(_,_)
      ; Instr = is_functor(_,_,_)
      ; Instr = is_ac_functor(_,_,_,_)
      ; Instr = eq(_,_)
      ; Instr = is_diff(_,_)
      ; Instr = get_ac_args(_,_,_,_)
      ; Instr = get_cc_args(_,_,_,_) ),
        true
    ).

%---------------------------------------------------------------------------%

:- pred ml_s_block_usage_analysis(ml_proc::in,ml_s_block_id::in,usage_info::in,
    usage_info::out) is det.

ml_s_block_usage_analysis(MLProc,SId,!UsageInfo) :-
    ml_s_block_lookup(MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,BodyInfo,Body),
        BodyInfo = ml_body_info(MaybeVar,_),
        fold_maybe(hl_var_is_used,MaybeVar,!UsageInfo),
        hl_model_usage_analysis(Body,!UsageInfo)
    ; SBlk = ml_s_block(_,_,BIds),
        foldl(ml_b_block_usage_analysis(MLProc),BIds,!UsageInfo)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_usage_analysis(ml_proc::in,ml_b_block_id::in,usage_info::in,
    usage_info::out) is det.

ml_b_block_usage_analysis(MLProc,BId,!UsageInfo) :-
    ml_b_block_lookup(MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
    BBlkInfo = ml_b_block_info(MLItr,_,_,_),
    ( MLItr = ml_none,
        true
    ; MLItr = ml_ac_itr(_,_,_,Arg),
        hl_var_is_used(Arg,!UsageInfo)
        % ml_l_block_lookup(MLProc,LId,LBlk),
        % LBlk = ml_l_block(_,_,_,LookupInstrs),
        % foldl(ml_instr_usage_analysis,LookupInstrs,!UsageInfo)
    ),
    foldl(ml_instr_usage_analysis(MLProc),Instrs,!UsageInfo),
    ml_s_block_usage_analysis(MLProc,SId,!UsageInfo).

%---------------------------------------------------------------------------%

:- pred ml_instr_usage_analysis(ml_proc::in,ml_instr::in,usage_info::in,
    usage_info::out) is det.

ml_instr_usage_analysis(_,guard(Guard,MaybeIn),!UsageInfo) :-
    fold_maybe(hl_var_is_used,MaybeIn,!UsageInfo),
    hl_model_usage_analysis(Guard,!UsageInfo).
ml_instr_usage_analysis(_,pattern_guard(_,Guard,MaybeIn),!UsageInfo) :-
    fold_maybe(hl_var_is_used,MaybeIn,!UsageInfo),
    hl_model_usage_analysis(Guard,!UsageInfo).
ml_instr_usage_analysis(_,is_type(In,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,is_int(In,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,is_float(In,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,is_string(In,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,is_named_var(In,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,is_functor(In,_,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,is_ac_functor(In,_,_,_),!UsageInfo) :-
    hl_var_is_used(In,!UsageInfo).
ml_instr_usage_analysis(_,get_arg(In,_,Out),!UsageInfo) :-
    hl_var_maybe_used(Out,In,!UsageInfo).
ml_instr_usage_analysis(_,get_annots(In,Out),!UsageInfo) :-
    hl_var_maybe_used(Out,In,!UsageInfo).
ml_instr_usage_analysis(_,eq(In1,In2),!UsageInfo) :-
    hl_var_is_used(In1,!UsageInfo),
    hl_var_is_used(In2,!UsageInfo).
ml_instr_usage_analysis(_,copy(In,Out),!UsageInfo) :-
    hl_var_maybe_used(Out,In,!UsageInfo).
ml_instr_usage_analysis(_,is_diff(In1,In2),!UsageInfo) :-
    hl_var_is_used(In1,!UsageInfo),
    hl_var_is_used(In2,!UsageInfo).
ml_instr_usage_analysis(MLProc,get_ac_args(In,LId,_,MaybeOut),!UsageInfo) :-
    ml_l_block_lookup(MLProc,LId,LBlk),
    LBlk = ml_l_block(_,_,_,LookupInstrs),
    foldl(ml_instr_usage_analysis(MLProc),LookupInstrs,!UsageInfo),
    ( MaybeOut = yes(Out),
        hl_var_maybe_used(Out,In,!UsageInfo)
    ; MaybeOut = no,
        true
    ).
ml_instr_usage_analysis(MLProc,get_cc_args(MaybeIn,LId,_,MaybeOut),
        !UsageInfo) :-
    ml_l_block_lookup(MLProc,LId,LBlk),
    LBlk = ml_l_block(_,_,_,LookupInstrs),
    foldl(ml_instr_usage_analysis(MLProc),LookupInstrs,!UsageInfo),
    ( MaybeOut = yes(Out),
        fold_maybe(hl_var_maybe_used(Out),MaybeIn,!UsageInfo)
    ; MaybeOut = no,
        true
    ).
ml_instr_usage_analysis(_,get_collector(In,Ins,Outs,Out),!UsageInfo) :-
    Outs1 = [Out|Outs],
    hl_var_maybe_used_any(Outs1,In,!UsageInfo),
    foldl(hl_var_maybe_used_any(Outs1),Ins,!UsageInfo).

%---------------------------------------------------------------------------%

:- pred hl_model_usage_analysis(hl_model::in,usage_info::in,usage_info::out)
    is det.

hl_model_usage_analysis(Model,!UsageInfo) :-
    ( Model = var(Var,_),
        hl_var_is_used(Var,!UsageInfo)
    ; Model = functor(_,Args,_),
        foldl(hl_model_usage_analysis,Args,!UsageInfo)
    ; ( Model = int(_,_)
      ; Model = float(_,_)
      ; Model = string(_,_)
      ; Model = named_var(_,_) ),
        true
    ),
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        hl_model_usage_analysis(Annots,!UsageInfo)
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred hl_var_is_used(hl_var::in,usage_info::in,usage_info::out) is det.

hl_var_is_used(Var,!UsageInfo) :-
    map.set(Var,used,!UsageInfo).

%---------------------------------------------------------------------------%

:- pred hl_var_maybe_used_any(list(hl_var)::in,hl_var::in,usage_info::in,
    usage_info::out) is det.

hl_var_maybe_used_any(Deps,Var,!UsageInfo) :-
    ( search(!.UsageInfo,Var,Used) ->
        ( Used = used,
            true
        ; Used = used_if(Deps0),
            append(Deps,Deps0,Deps1),
            map.set(Var,used_if(Deps1),!UsageInfo)
        )
    ;   map.det_insert(Var,used_if(Deps),!UsageInfo)
    ).

%---------------------------------------------------------------------------%

:- pred hl_var_maybe_used(hl_var::in,hl_var::in,usage_info::in,usage_info::out)
    is det.

hl_var_maybe_used(Dep,Var,!UsageInfo) :-
    ( search(!.UsageInfo,Var,Used) ->
        ( Used = used,
            true
        ; Used = used_if(Deps),
            map.set(Var,used_if([Dep|Deps]),!UsageInfo)
        )
    ;   map.det_insert(Var,used_if([Dep]),!UsageInfo)
    ).

%---------------------------------------------------------------------------%

:- pred hl_var_is_used(usage_info::in,hl_var::in) is semidet.

hl_var_is_used(UsageInfo,Var) :-
    search(UsageInfo,Var,Used),
    ( Used = used,
        true
    ; Used = used_if(Deps),
        cadmium_common.any_true(hl_var_is_used(UsageInfo),Deps)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_optimise_dead_var.
%---------------------------------------------------------------------------%
