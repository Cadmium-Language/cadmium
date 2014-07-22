%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% Remove empty b_blocks if possible.  Empty blocks are created by some 
% optimisations that remove or move instructions.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_empty.
:- interface.

:- import_module ml_prog.

:- import_module io.

:- pred ml_optimise_empty(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cadmium_common.

:- import_module bool.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

ml_optimise_empty(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_empty,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_empty(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_empty(_Sym,!MLProc,!IO) :-
    ml_s_block_optimise_empty(ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_empty(ml_s_block_id::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_s_block_optimise_empty(SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        ml_b_blocks_optimise_empty(BIds,NBIds,no,Changed,!MLProc,!IO),
        ( Changed = yes,
            NSBlk = ml_s_block(SId,[],NBIds),
            ml_s_block_insert(NSBlk,!MLProc),
            foldl2(ml_b_block_optimise_empty,NBIds,!MLProc,!IO)
        ; Changed = no,
            foldl2(ml_b_block_optimise_empty,BIds,!MLProc,!IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_empty(ml_b_block_id::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_b_block_optimise_empty(BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,_,_,SId),
    ml_s_block_optimise_empty(SId,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_b_blocks_optimise_empty(list(ml_b_block_id)::in,
    list(ml_b_block_id)::out,bool::in,bool::out,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_b_blocks_optimise_empty([],[],!Changed,!MLProc,!IO).
ml_b_blocks_optimise_empty([BId|BIds],NBIds,!Changed,!MLProc,!IO) :-
    ( can_eliminate_b_block(!.MLProc,BId,SId,BBlkBIds) ->
        ml_s_block_delete(SId,!MLProc),
        ml_b_block_delete(BId,!MLProc),
        append(BBlkBIds,BIds,NextBIds),
        !:Changed = yes,
        ml_b_blocks_optimise_empty(NextBIds,NBIds,!Changed,!MLProc,!IO)
    ;   ml_b_blocks_optimise_empty(BIds,NBIds0,!Changed,!MLProc,!IO),
        NBIds = [BId|NBIds0]
    ).

%---------------------------------------------------------------------------%

:- pred can_eliminate_b_block(ml_proc::in,ml_b_block_id::in,ml_s_block_id::out,
    list(ml_b_block_id)::out) is semidet.

can_eliminate_b_block(MLProc,BId,SId,BIds) :-
    ml_b_block_lookup(MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,[],SId),
    BBlkInfo = ml_b_block_info(MLItr,_,_,_),
    MLItr = ml_none,
    ml_s_block_lookup(MLProc,SId,SBlk),
    SBlk = ml_s_block(_,_,BIds).

%---------------------------------------------------------------------------%
:- end_module ml_optimise_empty.
%---------------------------------------------------------------------------%
