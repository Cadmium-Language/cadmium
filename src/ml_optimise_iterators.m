%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Remove iterators if possible.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_iterators.
:- interface.

:- import_module ml_prog.

:- import_module io.

:- pred ml_optimise_iterators(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cadmium_common.
:- import_module hl_prog.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

ml_optimise_iterators(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_iterators,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_iterators(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_iterators(_Sym,!MLProc,!IO) :-
    ml_s_block_optimise_iterators(ml_entry_s_block_id,!MLProc,!IO),
    true.
    % ml_s_block_cleanup_b_blocks(ml_entry_s_block_id,_,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_iterators(ml_s_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_s_block_optimise_iterators(SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_optimise_iterators,BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_iterators(ml_b_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_b_block_optimise_iterators(BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    some [!Instrs] (
        BBlk = ml_b_block(_,BBlkInfo,!:Instrs,SId),
        map_foldl2(ml_instr_optimise_iterators(SId),!Instrs,!MLProc,!IO),
        NBBlk = ml_b_block(BId,BBlkInfo,!.Instrs,SId),
        ml_b_block_insert(NBBlk,!MLProc)
    ),
    ml_s_block_optimise_iterators(SId,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_instr_optimise_iterators(ml_s_block_id::in,ml_instr::in,
    ml_instr::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_instr_optimise_iterators(SId,!Instr,!MLProc,!IO) :-
    ( !.Instr = get_ac_args(Src,Lookup,Arg,yes(Itr)) ->
        ml_s_block_maybe_remove_iterator(Itr,SId,Removed,!MLProc,!IO),
        ( Removed = yes,
            !:Instr = get_ac_args(Src,Lookup,Arg,no)
        ; Removed = no,
            true
        )
    ; !.Instr = get_cc_args(MaybeColl,Lookup,Arg,yes(Itr)) ->
        ml_s_block_maybe_remove_iterator(Itr,SId,Removed,!MLProc,!IO),
        ( Removed = yes,
            !:Instr = get_cc_args(MaybeColl,Lookup,Arg,no)
        ; Removed = no,
            true
        )
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred ml_s_block_maybe_remove_iterator(hl_var::in,ml_s_block_id::in,
    bool::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_s_block_maybe_remove_iterator(Itr,SId,Removed,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_block(_,_,[BId]) ->
        ml_b_block_maybe_remove_iterator(Itr,BId,Removed,!MLProc,!IO)
    ;   Removed = no   
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_maybe_remove_iterator(hl_var::in,ml_b_block_id::in,
    bool::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_maybe_remove_iterator(Itr,BId,Removed,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
    BBlkInfo = ml_b_block_info(MLItr,MaybePrev,RId,Cxt),
    ( MLItr = ml_ac_itr(_,LId,Itr,Arg) ->
        ml_l_block_lookup(!.MLProc,LId,LBlk),
        LBlk = ml_l_block(_,_,OutVars0,_),
        ml_fix(Arg,OutVars0,OutVars),
        ( ml_b_block_are_vars_not_matched(OutVars,!.MLProc,BId) ->
            NBBlkInfo = ml_b_block_info(ml_none,MaybePrev,RId,Cxt),
            NBBlk = ml_b_block(BId,NBBlkInfo,Instrs,SId),
            ml_b_block_insert(NBBlk,!MLProc),
            Removed = yes
        ;   Removed = no
        )
    ;   ml_s_block_maybe_remove_iterator(Itr,SId,Removed,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

    % Search through the rest of the matching.  If we discover a semidet 
    % instruction that (in)directly uses a variable bound by the lookup, then
    % the lookup requires an iterator.  Otherwise, it means the rest of the 
    % matching is independent of the output of a lookup, and thus no
    % iterator is required and we commit to the first choice.
    %
:- pred ml_b_block_are_vars_not_matched(ml_fixed::in,ml_proc::in,
    ml_b_block_id::in) is semidet.

ml_b_block_are_vars_not_matched(Vars,MLProc,BId) :-
    ml_b_block_lookup(MLProc,BId,BBlk),
    BBlk = ml_b_block(_,_,Instrs,SId),
    foldl(ml_instr_are_vars_not_matched(MLProc),Instrs,Vars,NVars),
    ml_s_block_are_vars_not_matched(NVars,MLProc,SId).

%---------------------------------------------------------------------------%

:- pred ml_s_block_are_vars_not_matched(ml_fixed::in,ml_proc::in,
    ml_s_block_id::in) is semidet.

ml_s_block_are_vars_not_matched(Vars,MLProc,SId) :-
    ml_s_block_lookup(MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,[BId]),
        ml_b_block_are_vars_not_matched(Vars,MLProc,BId)
    ).

%---------------------------------------------------------------------------%

:- pred ml_instr_are_vars_not_matched(ml_proc::in,ml_instr::in,ml_fixed::in,
    ml_fixed::out) is semidet.

ml_instr_are_vars_not_matched(MLProc,Instr,!Fixed) :-
    ( ( Instr = get_arg(In,_,Out)
      ; Instr = get_annots(In,Out)
      ; Instr = copy(In,Out) ),
        ( ml_is_fixed(!.Fixed,In) ->
            ml_fix(Out,!Fixed)
        ;   true
        )
    ; Instr = get_collector(In,Ins,Outs,Out),
        ( ( ml_is_fixed(!.Fixed,In)
          ; cadmium_common.any_true(ml_is_fixed(!.Fixed),Ins) ) ->
            ml_fix(Out,!Fixed),
            foldl(ml_fix,Outs,!Fixed)
        ;   true
        )
    ; ( Instr = guard(Guard,MaybeIn)
      ; Instr = pattern_guard(_,Guard,MaybeIn) ),
        ( MaybeIn = yes(In),
            not ml_is_fixed(!.Fixed,In)
        ; MaybeIn = no,
            true
        ),
        not hl_model_uses_any_var(!.Fixed,Guard)
    ; ( Instr = is_type(In,_)
      ; Instr = is_int(In,_)
      ; Instr = is_float(In,_)
      ; Instr = is_string(In,_)
      ; Instr = is_named_var(In,_)
      ; Instr = is_functor(In,_,_)
      ; Instr = is_ac_functor(In,_,_,_) ),
        not ml_is_fixed(!.Fixed,In)
    ; ( Instr = eq(In1,In2)
      ; Instr = is_diff(In1,In2) ),
        not ml_is_fixed(!.Fixed,In1),
        not ml_is_fixed(!.Fixed,In2)
    ; Instr = get_ac_args(In,LId,_,_),
        not ml_is_fixed(!.Fixed,In),
        ml_l_block_lookup(MLProc,LId,LBlk),
        LBlk = ml_l_block(_,Ins,_,_),
        not cadmium_common.any_true(ml_is_fixed(!.Fixed),to_sorted_list(Ins))
    ; Instr = get_cc_args(MaybeIn,LId,_,_),
        ( MaybeIn = yes(In),
            not ml_is_fixed(!.Fixed,In)
        ; MaybeIn = no,
            true
        ),
        ml_l_block_lookup(MLProc,LId,LBlk),
        LBlk = ml_l_block(_,Ins,_,_),
        not cadmium_common.any_true(ml_is_fixed(!.Fixed),to_sorted_list(Ins))
    ).

%---------------------------------------------------------------------------%

:- pred hl_model_uses_any_var(ml_fixed::in,hl_model::in) is semidet.

hl_model_uses_any_var(Fixed,Model) :-
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
      hl_model_uses_any_var(Fixed,Annots) ->
        true
    ;   ( Model = var(Var,_),
            ml_is_fixed(Fixed,Var)
        ; Model = functor(_,Args,_),
            cadmium_common.any_true(hl_model_uses_any_var(Fixed),Args)
        )
    ).

%---------------------------------------------------------------------------%

:- pred ml_s_block_cleanup_b_blocks(ml_s_block_id::in,list(ml_b_block)::out,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_s_block_cleanup_b_blocks(SId,Removed,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        Removed = []
    ; SBlk = ml_s_block(_,_,BIds),
        ( BIds = [BId] ->
            ml_b_block_cleanup_b_blocks(BId,SId,Removed,!MLProc,!IO)
        ;   Removed = []
        )
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_cleanup_b_blocks(ml_b_block_id::in,ml_s_block_id::in,
    list(ml_b_block)::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_cleanup_b_blocks(BId,SId0,Removed,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
    BBlkInfo = ml_b_block_info(MLItr,_,_,_),
    ml_s_block_cleanup_b_blocks(SId,Removed0,!MLProc,!IO),
    ( SId0 \= ml_entry_s_block_id,
      MLItr = ml_none ->
        ml_b_block_delete(BId,!MLProc),
        Removed = [BBlk|Removed0]
    ; Removed0 = [_|_] ->
        ml_b_blocks_append(Removed0,[],Instrs0,NSId),
        append(Instrs,Instrs0,NInstrs),
        NBBlk = ml_b_block(BId,BBlkInfo,NInstrs,NSId),
        ml_b_block_insert(NBBlk,!MLProc),
        Removed = []
    ;   Removed = []
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_blocks_append(list(ml_b_block)::in,list(ml_instr)::in,
    list(ml_instr)::out,ml_s_block_id::out) is det.

ml_b_blocks_append([],_,_,_) :-
    unexpected($file, $pred, "empty list").
ml_b_blocks_append([BBlk|Removed],!Instrs,SId) :-
    BBlk = ml_b_block(_,_,Instrs,SId0),
    ( Removed = [],
        SId = SId0
    ; Removed = [_|_],
        ml_b_blocks_append(Removed,!Instrs,SId),
        append(Instrs,!Instrs)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_optimise_iterators.
%---------------------------------------------------------------------------%
