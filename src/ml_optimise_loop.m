%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Loop invariant optimisation.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_loop.
:- interface.

:- import_module io.

:- import_module ml_prog.

    % Schedule ml_instrs out of loops (for AC/CC matching) if possible.
    %
:- pred ml_optimise_loop(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.

:- import_module cadmium_common.

%---------------------------------------------------------------------------%

ml_optimise_loop(!MLProg,!IO) :-
    map_foldl(ml_optimise_loop_ml_proc,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_optimise_loop_ml_proc(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_optimise_loop_ml_proc(_Sym,!MLProc,!IO) :-
    ml_proc_get_src_var(!.MLProc,Src),
    ml_fix(Src,init,Deps),
    ml_optimise_loop_ml_s_block(Deps,ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_optimise_loop_ml_s_block(ml_fixed::in,ml_s_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_optimise_loop_ml_s_block(Deps,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_optimise_loop_ml_b_block(Deps),BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_optimise_loop_ml_b_block(ml_fixed::in,ml_b_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_optimise_loop_ml_b_block(Deps,BId,!MLProc,!IO) :-
    some [!Deps,!Instrs] (
        !:Deps = Deps,
        ml_b_block_lookup(!.MLProc,BId,BBlk),
        BBlk = ml_b_block(_,BBlkInfo,!:Instrs,NextSId),
        BBlkInfo = ml_b_block_info(MLItr,_,_,_),
        ( MLItr = ml_none,
            true
        ; MLItr = ml_ac_itr(_,LId,_,Arg),
            ml_fix(Arg,!Deps),
            ml_l_block_lookup(!.MLProc,LId,LBlk),
            LBlk = ml_l_block(_,_,Outs,_),
            union(Outs,!Deps)
        ),
        map_foldl(ml_instr_optimise_loop_init,!Instrs,!Deps),
        ml_optimise_invariant_move_ml_s_block(NextSId,!Deps,[],DInstrs,!MLProc,
            !IO),
        append(!.Instrs,DInstrs,!:Instrs),
        NBBlk = ml_b_block(BId,BBlkInfo,!.Instrs,NextSId),
        ml_b_block_insert(NBBlk,!MLProc),
        ml_optimise_loop_ml_s_block(!.Deps,NextSId,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_instr_optimise_loop_init(ml_instr::in,ml_instr::out,ml_fixed::in,
    ml_fixed::out) is det.

ml_instr_optimise_loop_init(!Instr,!Fixed) :-
        % Because some eq/2 instructions may be scheduled early as copy/2
        % instructions, all subsequent copy/2 operations (where the 2nd arg. is
        % now fixed) must be translated to eq/2 instructions.
        %
    ( !.Instr = copy(Var1,Var2),
      ml_is_fixed(!.Fixed,Var2) ->
        !:Instr = eq(Var1,Var2)
    ;   ml_instr_update_fixed(!.Instr,!Fixed)
    ).

%---------------------------------------------------------------------------%

:- pred ml_optimise_invariant_move_ml_s_block(ml_s_block_id::in,
    ml_fixed::in,ml_fixed::out,list(ml_instr)::in,list(ml_instr)::out,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_optimise_invariant_move_ml_s_block(SId,!Deps,!DInstrs,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl4(ml_optimise_invariant_move_ml_b_block,BIds,!Deps,!DInstrs,
            !MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_optimise_invariant_move_ml_b_block(ml_b_block_id::in,ml_fixed::in,
    ml_fixed::out,list(ml_instr)::in,list(ml_instr)::out,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_optimise_invariant_move_ml_b_block(BId,!Deps,!DInstrs,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,MLItr,Instrs,NextSId),
    ml_optimise_invariant_move_ml_instrs(!.MLProc,Instrs,NInstrs,!Deps,
        !DInstrs,!IO),
    NBBlk = ml_b_block(BId,MLItr,NInstrs,NextSId),
    ml_b_block_insert(NBBlk,!MLProc),
    ml_optimise_invariant_move_ml_s_block(NextSId,!Deps,[],NextDInstrs,!MLProc,
        !IO),
    append(!.DInstrs,NextDInstrs,!:DInstrs).

%---------------------------------------------------------------------------%

:- pred ml_optimise_invariant_move_ml_instrs(ml_proc::in,list(ml_instr)::in,
    list(ml_instr)::out,ml_fixed::in,ml_fixed::out,list(ml_instr)::in,
    list(ml_instr)::out,io::di,io::uo) is det.

ml_optimise_invariant_move_ml_instrs(_,[],[],!Deps,!DInstrs,!IO).
ml_optimise_invariant_move_ml_instrs(MLProc,[Instr|Instrs],NInstrs,!Deps,
        !DInstrs,!IO) :-
    ( Instr = eq(Var1,Var2) ->
        ( ml_is_fixed(!.Deps,Var1) ->
            ( ml_is_fixed(!.Deps,Var2) ->
                NInstr = Instr,
                Fixed = yes
            ;       % The temporary variable is fixed but the program variable
                    % is unfixed.  We can schedule this eq/2 early as a copy/2
                    % instruction.
                    %
                NInstr = copy(Var1,Var2),
                Fixed = yes
            )
        ;   NInstr = Instr,
            Fixed = no
        ),
        ( Fixed = yes,
            ml_instr_update_fixed(NInstr,!Deps)
        ; Fixed = no,
            true
        )
    ; ml_instr_is_fixed(MLProc,!.Deps,Instr) ->
        NInstr = Instr,
        Fixed = yes,
        ml_instr_update_fixed(Instr,!Deps)
    ;   NInstr = Instr,
        Fixed = no
    ),
    ml_optimise_invariant_move_ml_instrs(MLProc,Instrs,NInstrs0,!Deps,!DInstrs,
        !IO),
    ( Fixed = yes,
        !:DInstrs = [NInstr|!.DInstrs],
        NInstrs = NInstrs0
    ; Fixed = no,
        NInstrs = [NInstr|NInstrs0]
    ).

