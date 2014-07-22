%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Merge similar matchings into a single matching if possible.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_merge.
:- interface.

:- import_module io.

:- import_module ml_prog.

:- pred ml_optimise_merge(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module float.

:- import_module cadmium_common.
:- import_module hl_prog.

:- import_module ml_optimise_order. % for instruction costs/selectivities

%---------------------------------------------------------------------------%

ml_optimise_merge(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_merge,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_merge(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_merge(_Sym,!MLProc,!IO) :-
    ml_proc_get_src_var(!.MLProc,Src),
    ml_fix(Src,init,Fixed),
    ml_s_block_optimise_merge(Fixed,ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_merge(ml_fixed::in,ml_s_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_s_block_optimise_merge(Fixed,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        ml_s_block_delete(SId,!MLProc),
        ml_b_blocks_optimise_merge(Fixed,BIds,NBIds,!MLProc,!IO),
        NSBlk = ml_s_block(SId,[],NBIds),
        ml_s_block_insert(NSBlk,!MLProc),
        ml_b_block_optimise_merge_children(Fixed,NBIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_merge_children(ml_fixed::in,
    list(ml_b_block_id)::in,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_optimise_merge_children(_,[],!MLProc,!IO).
ml_b_block_optimise_merge_children(Fixed,[BId|BIds],!MLProc,!IO) :-
    some [!Fixed] (
        !:Fixed = Fixed,
        ml_b_block_lookup(!.MLProc,BId,BBlk),
        BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
        BBlkInfo = ml_b_block_info(MLItr,_,_,_),
        ml_s_block_lookup(!.MLProc,SId,SBlk),
        ( SBlk \= ml_s_body_block(_,_,_) ->
            ( MLItr = ml_none,
                true
            ; MLItr = ml_ac_itr(_,LId,_,Arg),
                ml_fix(Arg,!Fixed),
                ml_l_block_lookup(!.MLProc,LId,LBlk),
                LBlk = ml_l_block(_,_,Outs,_),
                union(Outs,!Fixed)
            ),
            foldl(ml_instr_update_fixed,Instrs,!Fixed),
            ml_s_block_optimise_merge(!.Fixed,SId,!MLProc,!IO)
        ;   true
        )
    ),
    ml_b_block_optimise_merge_children(Fixed,BIds,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_b_blocks_optimise_merge(ml_fixed::in,list(ml_b_block_id)::in,
    list(ml_b_block_id)::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_blocks_optimise_merge(_,[],[],!MLProc,!IO).
ml_b_blocks_optimise_merge(_,BIds@[_],BIds,!MLProc,!IO).
ml_b_blocks_optimise_merge(Fixed,[BIdA,BIdB|BIds],NBIds,!MLProc,!IO) :-
    ml_b_blocks_merge(Fixed,BIdA,BIdB,MaybeBIdM,!MLProc,!IO),
    ( MaybeBIdM = yes(BIdM),
        ml_b_blocks_optimise_merge(Fixed,[BIdM|BIds],NBIds,!MLProc,!IO)
    ; MaybeBIdM = no,
        ml_b_blocks_optimise_merge(Fixed,[BIdB|BIds],NNBIds,!MLProc,!IO),
        NBIds = [BIdA|NNBIds]
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_blocks_merge(ml_fixed::in,ml_b_block_id::in,ml_b_block_id::in,
    maybe(ml_b_block_id)::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_blocks_merge(Fixed,BIdA,BIdB,MaybeBIdM,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BIdA,BBlkA),
    ml_b_block_lookup(!.MLProc,BIdB,BBlkB),
    BBlkA = ml_b_block(_,BBlkInfoA,InstrsA,PassA),
    BBlkB = ml_b_block(_,BBlkInfoB,InstrsB,PassB),
    BBlkInfoA = ml_b_block_info(MLItrA,_,RIdA,CxtA),
    BBlkInfoB = ml_b_block_info(MLItrB,_,_,_),
    ( MLItrA = ml_none,
      MLItrA = MLItrB ->
        ml_instrs_merge(!.MLProc,InstrsA,NInstrsA,InstrsB,NInstrsB0,[],
            InstrsM0,Fixed,_,init,Subs),
        ( InstrsM0 = [],
            MaybeBIdM = no
        ; InstrsM0 = [_|_],
            map_foldl(ml_instr_substitute(Subs),NInstrsB0,NInstrsB,!MLProc),
            ml_s_block_apply_subs(Subs,PassB,!MLProc),
            reverse(InstrsM0,InstrsM),
            ml_b_block_delete(BIdA,!MLProc),
            ml_b_block_delete(BIdB,!MLProc),
            ml_new_b_block_id(BIdM,!IO),
            BBlkInfoM = ml_b_block_info(ml_none,no,RIdA,CxtA),
            ( NInstrsA = [],
                ( NInstrsB = [],
                    ml_s_s_block_merge(PassA,PassB,!MLProc,!IO),
                    BBlkM = ml_b_block(BIdM,BBlkInfoM,InstrsM,PassA),
                    ml_b_block_insert(BBlkM,!MLProc)
                ; NInstrsB = [_|_],
                    NBBlkB = ml_b_block(BIdB,BBlkInfoB,NInstrsB,PassB),
                    ml_b_block_insert(NBBlkB,!MLProc),
                    ml_s_b_block_merge(PassA,BIdB,!MLProc),
                    BBlkM = ml_b_block(BIdM,BBlkInfoM,InstrsM,PassA)
                )
            ; NInstrsA = [_|_],
                ( NInstrsB = [],
                    NBBlkA = ml_b_block(BIdA,BBlkInfoA,NInstrsA,PassA),
                    ml_b_block_insert(NBBlkA,!MLProc),
                    ml_b_s_block_merge(BIdA,PassB,!MLProc,!IO),
                    BBlkM = ml_b_block(BIdM,BBlkInfoM,InstrsM,PassB)
                ; NInstrsB = [_|_],
                    NBBlkA = ml_b_block(BIdA,BBlkInfoA,NInstrsA,PassA),
                    NBBlkB = ml_b_block(BIdB,BBlkInfoB,NInstrsB,PassB),
                    ml_b_block_insert(NBBlkA,!MLProc),
                    ml_b_block_insert(NBBlkB,!MLProc),
                    ml_new_s_block_id(SIdC,!IO),
                    SBlkC = ml_s_block(SIdC,[],[BIdA,BIdB]),
                    ml_s_block_insert(SBlkC,!MLProc),
                    BBlkM = ml_b_block(BIdM,BBlkInfoM,InstrsM,SIdC)
                )
            ),
            ml_b_block_insert(BBlkM,!MLProc),
            MaybeBIdM = yes(BIdM)
        )
    ;   MaybeBIdM = no
    ).

%---------------------------------------------------------------------------%

:- pred ml_s_block_apply_subs(ml_subs::in,ml_s_block_id::in,ml_proc::in,
    ml_proc::out) is det.

ml_s_block_apply_subs(Subs,SId,!MLProc) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,BodyInfo,Body),
        ml_body_info_substitute(Subs,BodyInfo,NBodyInfo),
        hl_model_substitute(Subs,Body,NBody),
        NSBlk = ml_s_body_block(SId,NBodyInfo,NBody),
        ml_s_block_insert(NSBlk,!MLProc)
    ; SBlk = ml_s_block(_,_,BIds),
        foldl(ml_b_block_apply_subs(Subs),BIds,!MLProc)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_apply_subs(ml_subs::in,ml_b_block_id::in,ml_proc::in,
    ml_proc::out) is det.

ml_b_block_apply_subs(Subs,BId,!MLProc) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,SId),
    ml_b_block_info_substitute(Subs,BBlkInfo,NBBlkInfo),
    map_foldl(ml_instr_substitute(Subs),Instrs,NInstrs,!MLProc),
    NBBlk = ml_b_block(BId,NBBlkInfo,NInstrs,SId),
    ml_b_block_insert(NBBlk,!MLProc),
    ml_s_block_apply_subs(Subs,SId,!MLProc).

%---------------------------------------------------------------------------%

:- pred ml_b_s_block_merge(ml_b_block_id::in,ml_s_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_b_s_block_merge(BId,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,MaybeVar,Body),
        ml_new_s_block_id(NSId,!IO),
        NSBBlk = ml_s_body_block(NSId,MaybeVar,Body),
        ml_s_block_insert(NSBBlk,!MLProc),
        ml_s_block_wrap(NSId,EBId,!MLProc,!IO),
        NBIds = [BId,EBId],
        NSBlk = ml_s_block(SId,[],NBIds)
    ; SBlk = ml_s_block(_,_,BIds),
        NBIds = [BId|BIds],
        NSBlk = ml_s_block(SId,[],NBIds)
    ),
    ml_s_block_insert(NSBlk,!MLProc).

%---------------------------------------------------------------------------%

:- pred ml_s_b_block_merge(ml_s_block_id::in,ml_b_block_id::in,ml_proc::in,
    ml_proc::out) is det.

ml_s_b_block_merge(SId,BId,!MLProc) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
            % Since the body cannot "fail", the BId will never be called.
            % TODO: This case should be a warning -- since it implies a rule
            % can never fire.
            %
        ml_b_block_delete(BId,!MLProc)
    ; SBlk = ml_s_block(_,_,BIds),
        append(BIds,[BId],NBIds),
        NSBlk = ml_s_block(SId,[],NBIds),
        ml_s_block_insert(NSBlk,!MLProc)
    ).

%---------------------------------------------------------------------------%

:- pred ml_s_s_block_merge(ml_s_block_id::in,ml_s_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_s_s_block_merge(SIdA,SIdB,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SIdA,SBlkA),
    ml_s_block_lookup(!.MLProc,SIdB,SBlkB),
    ( SBlkA = ml_s_body_block(_,_,_),
      SBlkB = ml_s_body_block(_,_,_),
            % As above, SBlkB cannot be reached.
            %
        ml_s_block_delete(SIdB,!MLProc)
    ; SBlkA = ml_s_body_block(_,_,_),
      SBlkB = ml_s_block(_,_,_),
            % As above, SBlkB cannot be reached.
            %
        ml_s_block_delete(SIdB,!MLProc)
    ; SBlkA = ml_s_block(_,_,BIdsA),
      SBlkB = ml_s_body_block(_,_,_),
        ml_s_block_wrap(SIdB,EBId,!MLProc,!IO),
        append(BIdsA,[EBId],NBIdsA),
        NSBlkA = ml_s_block(SIdA,[],NBIdsA),
        ml_s_block_insert(NSBlkA,!MLProc)
    ; SBlkA = ml_s_block(_,_,BIdsA),
      SBlkB = ml_s_block(_,_,BIdsB),
        append(BIdsA,BIdsB,BIdsC),
        SBlkC = ml_s_block(SIdA,[],BIdsC),
        ml_s_block_delete(SIdB,!MLProc),
        ml_s_block_insert(SBlkC,!MLProc)
    ).

%---------------------------------------------------------------------------%

:- pred ml_s_block_wrap(ml_s_block_id::in,ml_b_block_id::out,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_s_block_wrap(SId,BId,!MLProc,!IO) :-
    ml_new_b_block_id(BId,!IO),
    BBlkInfo = ml_b_block_info(ml_none,no,-1,none),
    BBlk = ml_b_block(BId,BBlkInfo,[],SId),
    ml_b_block_insert(BBlk,!MLProc).

%---------------------------------------------------------------------------%

% Merge instructions of two B-blocks B1 and B2 that are consecutive 
% alternatives of an S-block. Potential benefit: not having to redo common 
% tests. Potential downside: may require reordering instructions, which may
% cause early evaluation of more expensive tests.
%
% We will use a simple cost model to check whether it is worthwhile.
% Let C(I) be the cost of evaluation instruction I and let S(I) be the
% selectivity of instruction I, and similar for lists of instructions.
% Let Is1, Is2 be the instruction lists of B1, B2 before merging, let M
% be the instruction to be merged, and Is1', Is2' the instruction lists of
% B1, B2 after removing M from Is1, Is2.
% The cost before merging is
% C(Is1) + (1 - S(Is1)) * C(Is2)
% where we note that B2 is only tried if B1 fails.
% The cost after merging then is
% C(M) + S(M) * (C(Is1') + (1 - S(Is1')) * C(Is2'))
%
% The cost model assumes (a.o.) that the S block after a B block never fails,
% so it may underestimate the probability that the second B block is tried.

:- pred ml_instrs_merge(ml_proc::in,list(ml_instr)::in,list(ml_instr)::out,
    list(ml_instr)::in,list(ml_instr)::out,list(ml_instr)::in,
    list(ml_instr)::out,ml_fixed::in,ml_fixed::out,ml_subs::in,ml_subs::out) 
    is det.

ml_instrs_merge(MLProc,!InstrsA,!InstrsB,!InstrsM,!FixedA,!Subs) :-
    ( InstrsA0 = !.InstrsA,
      InstrsB0 = !.InstrsB,
      ml_instrs_merge_search(MLProc,!InstrsA,!InstrsB,InstrM,!FixedA,!Subs),
      ml_instrs_cost_and_selectivity(InstrsA0,A0Cost,A0Selectivity),
      ml_instrs_cost_and_selectivity(InstrsB0,B0Cost,_),
      Cost0 = A0Cost + (1.0 - A0Selectivity) * B0Cost, 
      !:InstrsM = [InstrM|!.InstrsM],
      InstrsA1 = !.InstrsA,
      InstrsB1 = !.InstrsB,
      ml_instr_cost(InstrM,MCost),
      ml_instr_selectivity(InstrM,MSelectivityInv), % 0 = always succeeds, 1 = always fail
      ml_instrs_cost_and_selectivity(InstrsA1,A1Cost,A1Selectivity),
      ml_instrs_cost_and_selectivity(InstrsB1,B1Cost,_),
      Cost1 = MCost + (1.0 - MSelectivityInv) * (A1Cost + (1.0 - A1Selectivity) * B1Cost),
      Cost1 =< Cost0 -> % only merge if it makes things cheaper
	ml_instrs_merge(MLProc,!InstrsA,!InstrsB,!InstrsM,!FixedA,!Subs)
    ;   true
    ).


%---------------------------------------------------------------------------%

% Here a selectivity of 0 means always fails, and 1 means always succeeds

:- pred ml_instrs_cost_and_selectivity(list(ml_instr)::in,float::out,float::out) is det.

ml_instrs_cost_and_selectivity([H|T],Cost,Selectivity) :-
	ml_instr_cost(H,HCost),
	ml_instr_selectivity(H,HSelectivityInv), % 0 = always succeeds, 1 = always fails
	ml_instrs_cost_and_selectivity(T,TCost,TSelectivity),
	Cost = HCost + (1.0 - HSelectivityInv) * TCost,
	Selectivity = (1.0 - HSelectivityInv) * TSelectivity.
	
ml_instrs_cost_and_selectivity([],0.0,1.0).


:- pred ml_instrs_merge_search(ml_proc::in,list(ml_instr)::in,
    list(ml_instr)::out,list(ml_instr)::in,list(ml_instr)::out,ml_instr::out,
    ml_fixed::in,ml_fixed::out,ml_subs::in,ml_subs::out) is semidet.

ml_instrs_merge_search(MLProc,[InstrA|InstrsA],NInstrsA,!InstrsB,InstrM,
        !FixedA,!Subs) :-
    ( ml_instr_is_fixed(MLProc,!.FixedA,InstrA),
      ml_instr_unify_search(InstrA,!InstrsB,!Subs) ->
        NInstrsA = InstrsA,
        InstrM = InstrA,
        ml_instr_update_fixed(InstrM,!FixedA)
    ;   ml_instrs_merge_search(MLProc,InstrsA,NInstrsA0,!InstrsB,InstrM,
            !FixedA,!Subs),
        NInstrsA = [InstrA|NInstrsA0]
    ).

%---------------------------------------------------------------------------%

:- pred ml_instr_unify_search(ml_instr::in,list(ml_instr)::in,
    list(ml_instr)::out,ml_subs::in,ml_subs::out) is semidet.

ml_instr_unify_search(InstrA,[InstrB|InstrsB],NInstrsB,!Subs) :-
    ( ml_instr_unify(InstrA,InstrB,!Subs) ->
        NInstrsB = InstrsB
    ;   ml_instr_unify_search(InstrA,InstrsB,NInstrsB0,!Subs),
        NInstrsB = [InstrB|NInstrsB0]
    ).

%---------------------------------------------------------------------------%

:- pred ml_instr_unify(ml_instr::in,ml_instr::in,ml_subs::in,ml_subs::out)
    is semidet.

ml_instr_unify(guard(Guard1,MaybeIn1),guard(Guard2,MaybeIn2),!Subs) :-
    hl_var_maybe_eqeq(!.Subs,MaybeIn1,MaybeIn2),
    hl_model_eqeq(!.Subs,Guard1,Guard2).
ml_instr_unify(pattern_guard(Out1,Guard1,MaybeIn1),
        pattern_guard(Out2,Guard2,MaybeIn2),!Subs) :-
    hl_var_maybe_eqeq(!.Subs,MaybeIn1,MaybeIn2),
    hl_model_eqeq(!.Subs,Guard1,Guard2),
    hl_var_unify(Out2,Out1,!Subs).
ml_instr_unify(is_type(In1,Type),is_type(In2,Type),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2).
ml_instr_unify(is_int(In1,Int),is_int(In2,Int),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2).
ml_instr_unify(is_float(In1,Flt),is_float(In2,Flt),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2).
ml_instr_unify(is_string(In1,Str),is_string(In2,Str),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2).
ml_instr_unify(is_named_var(In1,Name),is_named_var(In2,Name),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2).
ml_instr_unify(is_functor(In1,Sym,Out1),is_functor(In2,Sym,Out2),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2),
    hl_var_unify(Out2,Out1,!Subs).
ml_instr_unify(is_ac_functor(In1,Aty,Sym,Out1),is_ac_functor(In2,Aty,Sym,Out2),
        !Subs) :-
    hl_var_eqeq(!.Subs,In1,In2),
    hl_var_unify(Out2,Out1,!Subs).
ml_instr_unify(get_arg(In1,Idx,Out1),get_arg(In2,Idx,Out2),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2),
    hl_var_unify(Out2,Out1,!Subs).
ml_instr_unify(get_annots(In1,Out1),get_annots(In2,Out2),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2),
    hl_var_unify(Out2,Out1,!Subs).
ml_instr_unify(eq(In1,In2),eq(In3,In4),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In3),
    hl_var_eqeq(!.Subs,In2,In4).
ml_instr_unify(copy(In1,Out1),copy(In2,Out2),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2),
    hl_var_unify(Out2,Out1,!Subs).
ml_instr_unify(get_ac_args(In1,Lookup,In2,MaybeOut1),
        get_ac_args(In3,Lookup,In4,MaybeOut2),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In3),
    hl_var_eqeq(!.Subs,In2,In4),
    hl_var_maybe_unify(MaybeOut2,MaybeOut1,!Subs).
ml_instr_unify(get_cc_args(MaybeIn1,Lookup,In2,MaybeOut1),
        get_cc_args(MaybeIn3,Lookup,In4,MaybeOut2),!Subs) :-
    hl_var_maybe_eqeq(!.Subs,MaybeIn1,MaybeIn3),
    hl_var_eqeq(!.Subs,In2,In4),
    hl_var_maybe_unify(MaybeOut2,MaybeOut1,!Subs).
ml_instr_unify(get_collector(In1,Ins1,Outs1,Out1),
        get_collector(In2,Ins2,Outs2,Out2),!Subs) :-
    hl_var_eqeq(!.Subs,In1,In2),
    hl_var_set_eqeq(!.Subs,Ins1,Ins2),
    length(Outs1,Len),
    length(Outs2,Len),
    hl_var_unify(Out2,Out1,!Subs),
    foldl_corresponding(hl_var_unify,Outs1,Outs2,!Subs).

%---------------------------------------------------------------------------%

:- pred hl_var_maybe_unify(maybe(hl_var)::in,maybe(hl_var)::in,ml_subs::in,
    ml_subs::out) is semidet.

hl_var_maybe_unify(no,no,!Subs).
hl_var_maybe_unify(yes(Var1),yes(Var2),!Subs) :-
    hl_var_unify(Var1,Var2,!Subs).

%---------------------------------------------------------------------------%

:- pred hl_var_maybe_eqeq(ml_subs::in,maybe(hl_var)::in,maybe(hl_var)::in) 
    is semidet.

hl_var_maybe_eqeq(_,no,no).
hl_var_maybe_eqeq(Subs,yes(Var1),yes(Var2)) :-
    hl_var_eqeq(Subs,Var1,Var2).

%---------------------------------------------------------------------------%

:- pred hl_model_eqeq(ml_subs::in,hl_model::in,hl_model::in) is semidet.

hl_model_eqeq(Subs,Model1,Model2) :-
    ( Model1 = int(Int,_),
        Model2 = int(Int,_)
    ; Model1 = float(Flt,_),
        Model2 = float(Flt,_)
    ; Model1 = string(Str,_),
        Model2 = string(Str,_)
    ; Model1 = named_var(Name,_),
        Model2 = named_var(Name,_)
    ; Model1 = var(Var1,_),
        Model2 = var(Var2,_),
        hl_var_eqeq(Subs,Var1,Var2)
    ; Model1 = functor(Sym,Args1,_),
        Model2 = functor(Sym,Args2,_),
        hl_model_eqeq_args(Subs,Args1,Args2)
    ),
    hl_annots_eqeq(Subs,Model1,Model2).

%---------------------------------------------------------------------------%

:- pred hl_model_eqeq_args(ml_subs::in,list(hl_model)::in,list(hl_model)::in)
    is semidet.

hl_model_eqeq_args(_,[],[]).
hl_model_eqeq_args(Subs,[Arg1|Args1],[Arg2|Args2]) :-
    hl_model_eqeq(Subs,Arg1,Arg2),
    hl_model_eqeq_args(Subs,Args1,Args2).

%---------------------------------------------------------------------------%

:- pred hl_annots_eqeq(ml_subs::in,hl_model::in,hl_model::in) is semidet.

hl_annots_eqeq(Subs,Model1,Model2) :-
    MaybeAnnots1 = get_annotations(Model1),
    ( MaybeAnnots1 = yes(Annots1),
        yes(Annots2) = get_annotations(Model2),
        hl_model_eqeq(Subs,Annots1,Annots2)
    ; MaybeAnnots1 = no,
        no = get_annotations(Model2)
    ).

%---------------------------------------------------------------------------%

:- pred hl_var_set_eqeq(ml_subs::in,list(hl_var)::in,list(hl_var)::in) 
    is semidet.

hl_var_set_eqeq(Subs,Vars1,Vars2) :-
    length(Vars1,Len),
    length(Vars2,Len),
    hl_var_set_eqeq_2(Subs,Vars1,Vars2).

%---------------------------------------------------------------------------%

:- pred hl_var_set_eqeq_2(ml_subs::in,list(hl_var)::in,list(hl_var)::in)
    is semidet.

hl_var_set_eqeq_2(_,[],[]).
hl_var_set_eqeq_2(Subs,[Var1|Vars1],Vars2) :-
    hl_var_set_delete(Subs,Var1,Vars2,NVars2),
    hl_var_set_eqeq_2(Subs,Vars1,NVars2).

%---------------------------------------------------------------------------%

:- pred hl_var_set_delete(ml_subs::in,hl_var::in,list(hl_var)::in,
    list(hl_var)::out) is semidet.

hl_var_set_delete(Subs,Var1,[Var2|Vars2],NVars2) :-
    ( hl_var_eqeq(Subs,Var1,Var2) ->
        NVars2 = Vars2
    ;   hl_var_set_delete(Subs,Var1,Vars2,NNVars2),
        NVars2 = [Var2|NNVars2]
    ).

