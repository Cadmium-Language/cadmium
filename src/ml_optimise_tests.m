%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% Optimise away test instructions that will always succeed.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_tests.
:- interface.

:- import_module ml_prog.

:- import_module io.

:- pred ml_optimise_tests(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module svmap.

:- import_module cadmium_common.
:- import_module hl_prog.

:- type test_info == map(hl_var,test).

:- type test
    ---> is_functor(hl_symbol)
    ;    is_type(hl_model_type).

%---------------------------------------------------------------------------%

ml_optimise_tests(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_tests,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_tests(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_tests(Sym,!MLProc,!IO) :-
    ml_proc_get_src_var(!.MLProc,Src),
    ( not is_ac(hl_symbol_name(Sym)) ->
        det_insert(Src,is_functor(Sym),init,TestInfo)
    ;   TestInfo = init
    ),
    ml_s_block_optimise_tests(TestInfo,ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_tests(test_info::in,ml_s_block_id::in,ml_proc::in,  
    ml_proc::out,io::di,io::uo) is det.

ml_s_block_optimise_tests(TestInfo,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_optimise_tests(TestInfo),BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_tests(test_info::in,ml_b_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_optimise_tests(TestInfo,BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
    BBlkInfo = ml_b_block_info(MLItr,_,_,_),
    some [!TestInfo] (
        !:TestInfo = TestInfo,
        ( MLItr = ml_ac_itr(ml_cc(Sym),_,Arg),
            det_insert(Arg,is_functor(Sym),!TestInfo)
        ; MLItr = ml_ac_itr(ml_ac(Sym),_,Arg),
            det_insert(Arg,is_functor(Sym),!TestInfo)
        ; MLItr = ml_ac_itr(ml_ac_gen,_,_),
            true
        ; MLItr = ml_none,
            true
        ),
        ml_instrs_optimise_tests(Instrs,NInstrs,!TestInfo,!IO),
        NBBlk = ml_b_block(BId,BBlkInfo,NInstrs,SId),
        ml_b_block_insert(NBBlk,!MLProc),
        ml_s_block_optimise_tests(!.TestInfo,SId,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_instrs_optimise_tests(list(ml_instr)::in,list(ml_instr)::out,
    test_info::in,test_info::out,io::di,io::uo) is det.

ml_instrs_optimise_tests([],[],!TestInfo,!IO).
ml_instrs_optimise_tests(!Instrs,!TestInfo,!IO) :-
    !.Instrs = [Instr|!:Instrs],
    ml_instr_optimise_tests(Instr,KeepInstr,!TestInfo,!IO),
    ( KeepInstr = no,
        ml_instrs_optimise_tests(!Instrs,!TestInfo,!IO)
    ; KeepInstr = yes,
         ml_instrs_optimise_tests(!Instrs,!TestInfo,!IO),
         !:Instrs = [Instr|!.Instrs]
    ).

%---------------------------------------------------------------------------%

:- pred ml_instr_optimise_tests(ml_instr::in,bool::out,test_info::in,
    test_info::out,io::di,io::uo) is det.

ml_instr_optimise_tests(Instr,KeepInstr,!TestInfo,!IO) :-
    ( ml_instr_to_test(Instr,Var,Test) ->
        ( search(!.TestInfo,Var,Test) ->
            KeepInstr = no
        ;   KeepInstr = yes,
            insert_test(Var,Test,!TestInfo)
        )
    ;   KeepInstr = yes
    ).

%---------------------------------------------------------------------------%

:- pred ml_instr_to_test(ml_instr::in,hl_var::out,test::out) is semidet.

ml_instr_to_test(is_functor(Var,Sym,_),Var,is_functor(Sym)).
ml_instr_to_test(is_type(Var,Type),Var,is_type(Type)).

%---------------------------------------------------------------------------%

:- pred insert_test(hl_var::in,test::in,test_info::in,test_info::out) is det.

insert_test(Var,Test,!TestInfo) :-
    ( contains(!.TestInfo,Var) ->
        true
    ;   det_insert(Var,Test,!TestInfo)
    ).

