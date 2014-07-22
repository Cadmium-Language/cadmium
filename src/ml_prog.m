%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% Medium Level program representation.
%
% This representation is very messy; however, it is a lot more useful than the 
% adhoc representations it replaced.  Basically, matching code is broken down
% into a graph of basic/sequence/lookup blocks of abstract instructions, etc.
% The graph structure is there editable (and some of the optimisations do edit
% it).
%
% NOTE: the functor names of various types should be updated to something more
% readable.
%
%---------------------------------------------------------------------------%

:- module ml_prog.
:- interface.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- import_module cadmium_common.
:- import_module hl_prog.

:- type ml_b_block_id ---> ml_b_block_id(int).
:- type ml_s_block_id ---> ml_s_block_id(int).
:- type ml_l_block_id ---> ml_l_block_id(int).

:- type ml_b_blocks == map(ml_b_block_id,ml_b_block).
:- type ml_s_blocks == map(ml_s_block_id,ml_s_block).
:- type ml_l_blocks == map(ml_l_block_id,ml_l_block).

:- type ml_prog == map(hl_symbol,ml_proc).

:- type ml_proc
    ---> ml_proc(hl_var,ml_b_blocks,ml_s_blocks,ml_l_blocks,cxt,cxt).

:- type ml_itr_kind
    ---> ml_cc
    ;    ml_ac.

:- type ml_itr
    ---> ml_ac_itr(ml_itr_kind,ml_l_block_id,hl_var,hl_var)
    ;    ml_none.

:- type ml_b_block_info
    ---> ml_b_block_info(ml_itr,maybe({rule_id,cxt}),rule_id,cxt).

:- type ml_body_info
    ---> ml_body_info(maybe(hl_var),map(string,hl_var)).

:- type ml_switch
    ---> ml_switch(int,int,hl_var,list(hl_symbol)).

:- type ml_b_block
        % b_block == basic block.  A sequence of matching instructions that
        % can either succeed (maybe many times) or fail.
        %
        % On failure, control is passed back to the calling s_block.  On 
        % success control is passed to the subsequent s_block.
        %
    ---> ml_b_block(ml_b_block_id,ml_b_block_info,list(ml_instr),
            ml_s_block_id).

:- type ml_s_block
        % ml_s_block == sequence block:
        %   - Tries the basic blocks in sequence.
        %   - If basic block <n> fails, try basic block <n+1>
        %   - Fails if all basic blocks fail.
        %
    ---> ml_s_block(ml_s_block_id,list(ml_switch),list(ml_b_block_id))

        % The body of a rule is also represented as a sequence block.
        %
    ;    ml_s_body_block(ml_s_block_id,ml_body_info,hl_model).

:- type ml_l_block
        % ml_l_block == lookup block.  Similar to a basic block, but contains
        % only instructions that are used for AC/CC lookups.  As such, the 
        % sequence of ml_instrs is subject to the following constraints:
        %
        % - Only is_type, is_int, is_float, is_string, is_named_var, is_functor,
        %   is_ac_functor, get_arg, and eq instructions are allowed.
        % - eq cannot be used to compare 2 hl_vars bound inside the current
        %   lookup.
        % - The ordering of the instructions must be lexicographic (i.e. 
        %   generated from a left-to-right traversal of the pattern).  
        %   Reordering optimisations are invalid for lookups.
        %
    ---> ml_l_block(ml_l_block_id,set(hl_var),set(hl_var),list(ml_instr)).

:- type ml_ac_aty
        % Arity has to exactly match the given arity.
        %
    ---> ml_exactly
        
        % Arity must be >= the given arity.
        %
    ;    ml_at_least.

    % Assumption: for eq/2 and copy/2, the second argument is always a program
    % variable.
    %
:- type ml_instr
    ---> guard(hl_model,maybe(hl_var))
    ;    pattern_guard(hl_var,hl_model,maybe(hl_var))
    ;    is_type(hl_var,hl_model_type)
    ;    is_int(hl_var,int)
    ;    is_float(hl_var,float)
    ;    is_string(hl_var,string)
    ;    is_named_var(hl_var,string)
    ;    is_functor(hl_var,hl_symbol,hl_var)
    ;    is_ac_functor(hl_var,ml_ac_aty,hl_symbol,hl_var)
    ;    get_arg(hl_var,int,hl_var)
    ;    get_annots(hl_var,hl_var)
    ;    eq(hl_var,hl_var)
    ;    copy(hl_var,hl_var)
    ;    is_diff(hl_var,hl_var)
    ;    get_ac_args(hl_var,ml_l_block_id,hl_var,maybe(hl_var))
    ;    get_cc_args(maybe(hl_var),ml_l_block_id,hl_var,maybe(hl_var))
    ;    get_collector(hl_var,list(hl_var),list(hl_var),hl_var).

:- type ml_subs == map(hl_var,hl_var).
:- type ml_fixed == set(hl_var).

    % Create an empty ml_proc.
    %
:- pred ml_proc_init(ml_proc::out,ml_fixed::out,io::di,io::uo) is det.

    % Get the source variable of a ml_proc.
    %
:- pred ml_proc_get_src_var(ml_proc::in,hl_var::out) is det.

    % Set the source variable of a ml_proc.
    %
:- pred ml_proc_set_src_var(hl_var::in,ml_proc::in,ml_proc::out) is det.

    % Get the context information of a ml_proc.
    %
:- pred ml_proc_get_cxt(ml_proc::in,cxt::out,cxt::out) is det.

    % Set the context information of a ml_proc.
    %
:- pred ml_proc_set_cxt(cxt::in,cxt::in,ml_proc::in,ml_proc::out) is det.

    % Get all lookups from a ml_proc.
    %
:- pred ml_proc_get_ml_l_blocks(ml_proc::in,ml_l_blocks::out) is det.

    % Create a new ml_b_block_id.
    %
:- pred ml_new_b_block_id(ml_b_block_id::out,io::di,io::uo) is det.

    % Create a new ml_s_block_id.
    %
:- pred ml_new_s_block_id(ml_s_block_id::out,io::di,io::uo) is det.

    % Create a new ml_l_block_id.
    %
:- pred ml_new_l_block_id(ml_l_block_id::out,io::di,io::uo) is det.

    % The ID of the entry ml_s_block.
    %
:- func ml_entry_s_block_id = ml_s_block_id.

    % Insert a ml_b_block into a ml_proc
    %
:- pred ml_b_block_insert(ml_b_block::in,ml_proc::in,ml_proc::out) is det.

    % Insert a ml_s_block into a ml_proc
    %
:- pred ml_s_block_insert(ml_s_block::in,ml_proc::in,ml_proc::out) is det.

    % Insert a ml_l_block into a ml_proc
    %
:- pred ml_l_block_insert(ml_l_block::in,ml_proc::in,ml_proc::out) is det.

    % Delete a ml_b_block from a ml_proc
    %
:- pred ml_b_block_delete(ml_b_block_id::in,ml_proc::in,ml_proc::out) is det.

    % Delete a ml_s_block from a ml_proc
    %
:- pred ml_s_block_delete(ml_s_block_id::in,ml_proc::in,ml_proc::out) is det.

    % Lookup a ml_b_block.
    %
:- pred ml_b_block_lookup(ml_proc::in,ml_b_block_id::in,ml_b_block::out) is det.

    % Lookup a ml_s_block.
    %
:- pred ml_s_block_lookup(ml_proc::in,ml_s_block_id::in,ml_s_block::out) is det.

    % Lookup a ml_l_block.
    %
:- pred ml_l_block_lookup(ml_proc::in,ml_l_block_id::in,ml_l_block::out) is det.

    % Map ml_l_blocks.
    %
:- pred ml_l_block_map_foldl(
    pred(ml_l_block,ml_l_block,T,T)::pred(in,out,di,uo) is det,
    ml_proc::in,ml_proc::out,T::di,T::uo) is det.

    % Test if the given variable is fixed.
    %
:- pred ml_is_fixed(ml_fixed::in,hl_var::in) is semidet.

    % Fix a variable.
    %
:- pred ml_fix(hl_var::in,ml_fixed::in,ml_fixed::out) is det.

    % Test if the inputs to the given instruction are fixed.
    %
:- pred ml_instr_is_fixed(ml_proc::in,ml_fixed::in,ml_instr::in) is semidet.

    % Fix all variables in the given hl_model.
    %
:- pred hl_model_fix(hl_model::in,ml_fixed::in,ml_fixed::out) is det.

    % Test if the given hl_model is fixed.
    %
:- pred hl_model_is_fixed(ml_fixed::in,hl_model::in) is semidet.

    % Unify two hl_vars.
    %
:- pred hl_var_unify(hl_var::in,hl_var::in,ml_subs::in,ml_subs::out) is det.

    % Test if two hl_vars have been unified.
    %
:- pred hl_var_eqeq(ml_subs::in,hl_var::in,hl_var::in) is semidet.

    % Apply a substitution to a ml_instr.
    %
:- pred ml_instr_substitute(ml_subs::in,ml_instr::in,ml_instr::out,ml_proc::in,
    ml_proc::out) is det.

    % Apply a substitution to a ml_b_block_info.
    %
:- pred ml_b_block_info_substitute(ml_subs::in,ml_b_block_info::in,
    ml_b_block_info::out) is det.

    % Apply a substitution to a ml_body_info.
    %
:- pred ml_body_info_substitute(ml_subs::in,ml_body_info::in,ml_body_info::out)
    is det.

    % Apply a substitution to a hl_model.
    %
:- pred hl_model_substitute(ml_subs::in,hl_model::in,hl_model::out) is det.

    % Apply a substitution to a hl_var.
    %
:- pred hl_var_substitute(ml_subs::in,hl_var::in,hl_var::out) is det.

    % Fix the outputs of the given instruction.
    %
:- pred ml_instr_update_fixed(ml_instr::in,ml_fixed::in,ml_fixed::out) is det.

    % Create a new variable and record it as fixed.
    %
:- pred ml_new_var(hl_var::out,ml_fixed::in,ml_fixed::out,io::di,io::uo)
    is det.

    % Print an ml_prog.
    %
:- pred ml_prog_write(ml_prog::in,io::di,io::uo) is det.

    % Apply a map over all ml_b_blocks.
    %
:- pred ml_prog_b_block_map(
    pred(ml_proc,ml_b_block,ml_b_block,ml_fixed,ml_fixed,io,io),ml_prog,
    ml_prog,io,io).
:- mode ml_prog_b_block_map(pred(in,in,out,in,out,di,uo) is det,in,out,di,uo) 
    is det.

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

ml_proc_init(MLProc,Fixed,!IO) :-
    ml_new_var(Src,init,Fixed,!IO),
    MLProc = ml_proc(Src,init,init,init,none,none).

%---------------------------------------------------------------------------%

ml_proc_get_src_var(MLProc,Src) :-
    MLProc = ml_proc(Src,_,_,_,_,_).

%---------------------------------------------------------------------------%

ml_proc_set_src_var(Src,!MLProc) :-
    !.MLProc = ml_proc(_,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    !:MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_proc_get_cxt(MLProc,FstCxt,LstCxt) :-
    MLProc = ml_proc(_,_,_,_,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_proc_set_cxt(FstCxt,LstCxt,!MLProc) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,_,_),
    !:MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_proc_get_ml_l_blocks(MLProc,LBlks) :-
    MLProc = ml_proc(_,_,_,LBlks,_,_).

%---------------------------------------------------------------------------%

:- mutable(id_counter,int,1,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

ml_new_b_block_id(BId,!IO) :-
    get_id_counter(Id,!IO),
    BId = ml_b_block_id(Id),
    set_id_counter(Id+1,!IO).

%---------------------------------------------------------------------------%

ml_new_s_block_id(SId,!IO) :-
    get_id_counter(Id,!IO),
    SId = ml_s_block_id(Id),
    set_id_counter(Id+1,!IO).

%---------------------------------------------------------------------------%

ml_new_l_block_id(LId,!IO) :-
    get_id_counter(Id,!IO),
    LId = ml_l_block_id(Id),
    set_id_counter(Id+1,!IO).

%---------------------------------------------------------------------------%

ml_entry_s_block_id = ml_s_block_id(0).

%---------------------------------------------------------------------------%

ml_b_block_insert(BBlk,!MLProc) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    BBlk = ml_b_block(BId,_,_,_),
    map.set(BId,BBlk,BBlks,NBBlks),
    !:MLProc = ml_proc(Src,NBBlks,SBlks,LBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_s_block_insert(SBlk,!MLProc) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    ( SBlk = ml_s_block(SId0,_,_),
        SId = SId0
    ; SBlk = ml_s_body_block(SId0,_,_),
        SId = SId0
    ),
    map.set(SId,SBlk,SBlks,NSBlks),
    !:MLProc = ml_proc(Src,BBlks,NSBlks,LBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_l_block_insert(LBlk,!MLProc) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    LBlk = ml_l_block(LId,_,_,_),
    map.set(LId,LBlk,LBlks,NLBlks),
    !:MLProc = ml_proc(Src,BBlks,SBlks,NLBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_b_block_delete(BId,!MLProc) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    map.delete(BId,BBlks,NBBlks),
    !:MLProc = ml_proc(Src,NBBlks,SBlks,LBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_s_block_delete(SId,!MLProc) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    map.delete(SId,SBlks,NSBlks),
    !:MLProc = ml_proc(Src,BBlks,NSBlks,LBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

ml_b_block_lookup(MLProc,BId,BBlk) :-
    MLProc = ml_proc(_,BBlks,_,_,_,_),
    lookup(BBlks,BId,BBlk).

%---------------------------------------------------------------------------%

ml_s_block_lookup(MLProc,SId,SBlk) :-
    MLProc = ml_proc(_,_,SBlks,_,_,_),
    lookup(SBlks,SId,SBlk).

%---------------------------------------------------------------------------%

ml_l_block_lookup(MLProc,LId,LBlk) :-
    MLProc = ml_proc(_,_,_,LBlks,_,_),
    lookup(LBlks,LId,LBlk).

%---------------------------------------------------------------------------%

ml_l_block_map_foldl(Pred,!MLProc,!T) :-
    !.MLProc = ml_proc(Src,BBlks,SBlks,LBlks,FstCxt,LstCxt),
    map_foldl(ml_l_block_process(Pred),LBlks,NLBlks,!T),
    !:MLProc = ml_proc(Src,BBlks,SBlks,NLBlks,FstCxt,LstCxt).

%---------------------------------------------------------------------------%

:- pred ml_l_block_process(
    pred(ml_l_block,ml_l_block,T,T)::pred(in,out,di,uo) is det,
    ml_l_block_id::in,ml_l_block::in,ml_l_block::out,T::di,T::uo) is det.

ml_l_block_process(Pred,_,!LBlk,!T) :-
    Pred(!LBlk,!T).

%---------------------------------------------------------------------------%

ml_is_fixed(MLFixed,Var) :-
    member(Var,MLFixed).

%---------------------------------------------------------------------------%

ml_fix(Var,!MLFixed) :-
    set.insert(Var,!MLFixed).

%---------------------------------------------------------------------------%

ml_instr_is_fixed(_,Fixed,guard(Guard,MaybeVar)) :-
    ml_maybe_is_fixed(Fixed,MaybeVar),
    hl_model_is_fixed(Fixed,Guard).
ml_instr_is_fixed(_,Fixed,pattern_guard(_,Guard,MaybeVar)) :-
    ml_maybe_is_fixed(Fixed,MaybeVar),
    hl_model_is_fixed(Fixed,Guard).
ml_instr_is_fixed(_,Fixed,is_type(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_int(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_float(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_string(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_named_var(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_functor(Var,_,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_ac_functor(Var,_,_,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,get_arg(Var,_,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,get_annots(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,eq(Var1,Var2)) :-
    ml_is_fixed(Fixed,Var1),
    ml_is_fixed(Fixed,Var2).
ml_instr_is_fixed(_,Fixed,copy(Var,_)) :-
    ml_is_fixed(Fixed,Var).
ml_instr_is_fixed(_,Fixed,is_diff(Var1,Var2)) :-
    ml_is_fixed(Fixed,Var1),
    ml_is_fixed(Fixed,Var2).
ml_instr_is_fixed(MLProc,Fixed,get_ac_args(Var,LId,_,_)) :-
    ml_is_fixed(Fixed,Var),
    ml_l_block_lookup(MLProc,LId,LBlk),
    LBlk = ml_l_block(_,Vars,_,_),
    all_true(ml_is_fixed(Fixed),to_sorted_list(Vars)).
ml_instr_is_fixed(MLProc,Fixed,get_cc_args(MaybeVar,LId,_,_)) :-
    ml_maybe_is_fixed(Fixed,MaybeVar),
    ml_l_block_lookup(MLProc,LId,LBlk),
    LBlk = ml_l_block(_,Vars,_,_),
    all_true(ml_is_fixed(Fixed),to_sorted_list(Vars)).
ml_instr_is_fixed(_,Fixed,get_collector(Var,Vars,_,_)) :-
    ml_is_fixed(Fixed,Var),
    all_true(ml_is_fixed(Fixed),Vars).

%---------------------------------------------------------------------------%

:- pred ml_maybe_is_fixed(ml_fixed::in,maybe(hl_var)::in) is semidet.

ml_maybe_is_fixed(_,no).
ml_maybe_is_fixed(Fixed,yes(Var)) :-
    ml_is_fixed(Fixed,Var).

%---------------------------------------------------------------------------%

hl_model_fix(Model,!Fixed) :-
    ( Model = var(Var,_),
        ml_fix(Var,!Fixed)
    ; Model = functor(_,Args,_),
        foldl(hl_model_fix,Args,!Fixed)
    ; ( Model = int(_,_)
      ; Model = float(_,_)
      ; Model = string(_,_)
      ; Model = named_var(_,_) ),
        true
    ),
    MaybeAt = get_at_var(Model),
    ( MaybeAt = yes(AtVar),
        ml_fix(AtVar,!Fixed)
    ; MaybeAt = no,
        true
    ),
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        hl_model_fix(Annots,!Fixed)
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

hl_model_is_fixed(Fixed,Model) :-
    ( Model = var(Var,_),
        ml_is_fixed(Fixed,Var)
    ; Model = functor(_,Args,_),
        all_true(hl_model_is_fixed(Fixed),Args)
    ; ( Model = int(_,_)
      ; Model = float(_,_)
      ; Model = string(_,_)
      ; Model = named_var(_,_) ),
        true
    ),
    hl_annots_is_fixed(Fixed,Model).

%---------------------------------------------------------------------------%

:- pred hl_annots_is_fixed(ml_fixed::in,hl_model::in) is semidet.

hl_annots_is_fixed(Fixed,Model) :-
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        hl_model_is_fixed(Fixed,Annots)
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

ml_instr_update_fixed(guard(_,_),!Fixed).
ml_instr_update_fixed(pattern_guard(Out,_,_),!Fixed) :-
    set.insert(Out,!Fixed).
ml_instr_update_fixed(is_type(_,_),!Fixed).
ml_instr_update_fixed(is_int(_,_),!Fixed).
ml_instr_update_fixed(is_float(_,_),!Fixed).
ml_instr_update_fixed(is_string(_,_),!Fixed).
ml_instr_update_fixed(is_named_var(_,_),!Fixed).
ml_instr_update_fixed(is_functor(_,_,Out),!Fixed) :-
    set.insert(Out,!Fixed).
ml_instr_update_fixed(is_ac_functor(_,_,_,Out),!Fixed) :-
    set.insert(Out,!Fixed).
ml_instr_update_fixed(get_arg(_,_,Out),!Fixed) :-
    set.insert(Out,!Fixed).
ml_instr_update_fixed(get_annots(_,Out),!Fixed) :-
    set.insert(Out,!Fixed).
ml_instr_update_fixed(eq(_,_),!Fixed).
ml_instr_update_fixed(copy(_,Out),!Fixed) :-
    set.insert(Out,!Fixed).
ml_instr_update_fixed(is_diff(_,_),!Fixed).
ml_instr_update_fixed(get_ac_args(_,_,_,MaybeOut),!Fixed) :-
    fold_maybe(set.insert,MaybeOut,!Fixed).
ml_instr_update_fixed(get_cc_args(_,_,_,MaybeOut),!Fixed) :-
    fold_maybe(set.insert,MaybeOut,!Fixed).
ml_instr_update_fixed(get_collector(_,_,Outs,Out),!Fixed) :-
    list.foldl(set.insert,Outs,!Fixed),
    set.insert(Out,!Fixed).

%---------------------------------------------------------------------------%

hl_var_unify(Var1,Var2,!Subs) :-
    hl_var_find_root(!.Subs,Var1,Root1),
    hl_var_find_root(!.Subs,Var2,Root2),
    ( Root1 = Root2 ->
        true
    ;   map.det_insert(Root1,Root2,!Subs)
    ).

%---------------------------------------------------------------------------%

:- pred hl_var_find_root(ml_subs::in,hl_var::in,hl_var::out) is det.

hl_var_find_root(Subs,Var,Root) :-
    ( search(Subs,Var,NVar) ->
        hl_var_find_root(Subs,NVar,Root)
    ;   Root = Var
    ).

%---------------------------------------------------------------------------%

hl_var_eqeq(Subs,Var1,Var2) :-
    hl_var_find_root(Subs,Var1,Root),
    hl_var_find_root(Subs,Var2,Root).

%---------------------------------------------------------------------------%

ml_instr_substitute(Subs,guard(Guard,MaybeVar),guard(NGuard,NMaybeVar),
        !MLProc) :-
    map_maybe(hl_var_substitute(Subs),MaybeVar,NMaybeVar),
    hl_model_substitute(Subs,Guard,NGuard).
ml_instr_substitute(Subs,pattern_guard(Var,Guard,MaybeVar),
        pattern_guard(NVar,NGuard,NMaybeVar),!MLProc) :-
    map_maybe(hl_var_substitute(Subs),MaybeVar,NMaybeVar),
    hl_var_substitute(Subs,Var,NVar),
    hl_model_substitute(Subs,Guard,NGuard).
ml_instr_substitute(Subs,is_type(Var,Type),is_type(NVar,Type),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar).
ml_instr_substitute(Subs,is_int(Var,Int),is_int(NVar,Int),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar).
ml_instr_substitute(Subs,is_float(Var,Flt),is_float(NVar,Flt),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar).
ml_instr_substitute(Subs,is_string(Var,Str),is_string(NVar,Str),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar).
ml_instr_substitute(Subs,is_named_var(Var,Name),is_named_var(NVar,Name),
        !MLProc) :-
    hl_var_substitute(Subs,Var,NVar).
ml_instr_substitute(Subs,is_functor(Var,Sym,Out),is_functor(NVar,Sym,NOut),
        !MLProc) :-
    hl_var_substitute(Subs,Var,NVar),
    hl_var_substitute(Subs,Out,NOut).
ml_instr_substitute(Subs,is_ac_functor(Var,AtyCons,Sym,Out),
        is_ac_functor(NVar,AtyCons,Sym,NOut),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar),
    hl_var_substitute(Subs,Out,NOut).
ml_instr_substitute(Subs,get_arg(Var,Idx,Out),get_arg(NVar,Idx,NOut),
        !MLProc) :-
    hl_var_substitute(Subs,Var,NVar),
    hl_var_substitute(Subs,Out,NOut).
ml_instr_substitute(Subs,get_annots(Var,Out),get_annots(NVar,NOut),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar),
    hl_var_substitute(Subs,Out,NOut).
ml_instr_substitute(Subs,eq(Var1,Var2),eq(NVar1,NVar2),!MLProc) :-
    hl_var_substitute(Subs,Var1,NVar1),
    hl_var_substitute(Subs,Var2,NVar2).
ml_instr_substitute(Subs,copy(Var,Out),copy(NVar,NOut),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar),
    hl_var_substitute(Subs,Out,NOut).
ml_instr_substitute(Subs,is_diff(Var1,Var2),is_diff(NVar1,NVar2),!MLProc) :-
    hl_var_substitute(Subs,Var1,NVar1),
    hl_var_substitute(Subs,Var2,NVar2).
ml_instr_substitute(Subs,get_ac_args(Var1,Lookup,Var2,MaybeOut),
        get_ac_args(NVar1,Lookup,NVar2,NMaybeOut),!MLProc) :-
    hl_var_substitute(Subs,Var1,NVar1),
    hl_var_substitute(Subs,Var2,NVar2),
    map_maybe(hl_var_substitute(Subs),MaybeOut,NMaybeOut),
    ml_lookup_substitute(Subs,Lookup,!MLProc).
ml_instr_substitute(Subs,get_cc_args(MaybeVar,Lookup,Var,MaybeOut),
        get_cc_args(NMaybeVar,Lookup,NVar,NMaybeOut),!MLProc) :-
    map_maybe(hl_var_substitute(Subs),MaybeVar,NMaybeVar),
    hl_var_substitute(Subs,Var,NVar),
    map_maybe(hl_var_substitute(Subs),MaybeOut,NMaybeOut),
    ml_lookup_substitute(Subs,Lookup,!MLProc).
ml_instr_substitute(Subs,get_collector(Var,Vars,Outs,Out),
        get_collector(NVar,NVars,NOuts,NOut),!MLProc) :-
    hl_var_substitute(Subs,Var,NVar),
    map(hl_var_substitute(Subs),Vars,NVars),
    map(hl_var_substitute(Subs),Outs,NOuts),
    hl_var_substitute(Subs,Out,NOut).

%---------------------------------------------------------------------------%

:- pred ml_lookup_substitute(ml_subs::in,ml_l_block_id::in,ml_proc::in,
    ml_proc::out) is det.

ml_lookup_substitute(Subs,LId,!MLProc) :-
    ml_l_block_lookup(!.MLProc,LId,LBlk),
    LBlk = ml_l_block(_,Ins,Outs,Instrs),
    map(hl_var_substitute(Subs),Ins,NIns),
    map(hl_var_substitute(Subs),Outs,NOuts),
    map_foldl(ml_instr_substitute(Subs),Instrs,NInstrs,!MLProc),
    NLBlk = ml_l_block(LId,NIns,NOuts,NInstrs),
    ml_l_block_insert(NLBlk,!MLProc).

%---------------------------------------------------------------------------%

ml_b_block_info_substitute(Subs,!BBlkInfo) :-
    !.BBlkInfo = ml_b_block_info(MLItr,MaybePrev,RId,Cxt),
    ( MLItr = ml_none,
        NMLItr = MLItr
    ; MLItr = ml_ac_itr(Kind,LId,Itr,Arg),
        hl_var_substitute(Subs,Itr,NItr),
        hl_var_substitute(Subs,Arg,NArg),
        NMLItr = ml_ac_itr(Kind,LId,NItr,NArg)
    ),
    !:BBlkInfo = ml_b_block_info(NMLItr,MaybePrev,RId,Cxt).

%---------------------------------------------------------------------------%

ml_body_info_substitute(Subs,!BodyInfo) :-
    !.BodyInfo = ml_body_info(MaybeAnnots,VarNames),
    map_maybe(hl_var_substitute(Subs),MaybeAnnots,NMaybeAnnots),
    map_values(hl_named_var_substitute(Subs),VarNames,NVarNames),
    !:BodyInfo = ml_body_info(NMaybeAnnots,NVarNames).

%---------------------------------------------------------------------------%

:- pred hl_named_var_substitute(ml_subs::in,string::in,hl_var::in,hl_var::out)
    is det.

hl_named_var_substitute(Subs,_,!Var) :-
    hl_var_substitute(Subs,!Var).

%---------------------------------------------------------------------------%

hl_model_substitute(Subs,!Model) :-
    ( !.Model = var(Var,Attr),
        hl_var_substitute(Subs,Var,NVar),
        !:Model = var(NVar,Attr)
    ; !.Model = functor(Sym,Args,Attr),
        map(hl_model_substitute(Subs),Args,NArgs),
        !:Model = functor(Sym,NArgs,Attr)
    ; ( !.Model = int(_,_)
      ; !.Model = float(_,_)
      ; !.Model = string(_,_)
      ; !.Model = named_var(_,_) ),
        true
    ),
    MaybeAnnots = get_annotations(!.Model),
    ( MaybeAnnots = yes(Annots),
        hl_model_substitute(Subs,Annots,NAnnots),
        !:Model = set_annotations(!.Model,NAnnots)
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

hl_var_substitute(Subs,Var,NVar) :-
    hl_var_find_root(Subs,Var,NVar).

%---------------------------------------------------------------------------%

ml_new_var(Var,!MLFixed,!IO) :-
    new_hl_var(Var,!IO),
    set.insert(Var,!MLFixed).

%---------------------------------------------------------------------------%

ml_prog_b_block_map(Map,!MLProg,!IO) :-
    map_foldl(ml_proc_b_block_map(Map),!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_b_block_map(
    pred(ml_proc,ml_b_block,ml_b_block,ml_fixed,ml_fixed,io,io),hl_symbol,
    ml_proc,ml_proc,io,io).
:- mode ml_proc_b_block_map(pred(in,in,out,in,out,di,uo) is det,in,in,out,
    di,uo) is det.

ml_proc_b_block_map(Map,_Sym,!MLProc,!IO) :-
    ml_proc_get_src_var(!.MLProc,Src),
    ml_fix(Src,init,Fixed),
    ml_s_block_b_block_map(Map,ml_entry_s_block_id,Fixed,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_b_block_map(
    pred(ml_proc,ml_b_block,ml_b_block,ml_fixed,ml_fixed,io,io),
    ml_s_block_id,ml_fixed,ml_proc,ml_proc,io,io).
:- mode ml_s_block_b_block_map(pred(in,in,out,in,out,di,uo) is det,in,in,in,
    out,di,uo) is det.

ml_s_block_b_block_map(Map,SId,Fixed,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_map(Map,!.MLProc,Fixed),BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_map(
    pred(ml_proc,ml_b_block,ml_b_block,ml_fixed,ml_fixed,io,io),
    ml_proc,ml_fixed,ml_b_block_id,ml_proc,ml_proc,io,io).
:- mode ml_b_block_map(pred(in,in,out,in,out,di,uo) is det,in,in,in,in,out,
    di,uo) is det.

ml_b_block_map(Map,MLProc,Fixed,BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    Map(MLProc,BBlk,NBBlk,Fixed,NFixed,!IO),
    ml_b_block_insert(NBBlk,!MLProc),
    BBlk = ml_b_block(_,_,_,NextSId),
    ml_s_block_b_block_map(Map,NextSId,NFixed,!MLProc,!IO).

%---------------------------------------------------------------------------%

ml_prog_write(MLProg,!IO) :-
    foldl(ml_proc_write,MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_write(hl_symbol::in,ml_proc::in,io::di,io::uo) is det.

ml_proc_write(Sym,MLProc,!IO) :-
    format("%s/%d:\n",[s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO),
    ml_s_block_write(MLProc,ml_entry_s_block_id,!IO),
    ml_proc_get_ml_l_blocks(MLProc,LBlks),
    foldl(ml_l_block_write,LBlks,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_write(ml_proc::in,ml_s_block_id::in,io::di,io::uo) is det.

ml_s_block_write(MLProc,SId,!IO) :-
    SId = ml_s_block_id(Id),
    format("\n  S_block#%d:\n",[i(Id)],!IO),
    ml_s_block_lookup(MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,Body),
        write_string("    <<BODY>> ",!IO),
        write(Body,!IO),
        nl(!IO)
    ; SBlk = ml_s_block(_,_,BIds),
        write_string("    [\n",!IO),
        foldl(ml_b_block_id_write,BIds,!IO),
        write_string("    ]\n",!IO),
        foldl(ml_b_block_write(MLProc),BIds,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_id_write(ml_b_block_id::in,io::di,io::uo) is det.

ml_b_block_id_write(ml_b_block_id(Id),!IO) :-
    format("      B_block#%d\n",[i(Id)],!IO).

%---------------------------------------------------------------------------%

:- pred ml_b_block_write(ml_proc::in,ml_b_block_id::in,io::di,io::uo) is det.

ml_b_block_write(MLProc,BId,!IO) :-
    BId = ml_b_block_id(Id),
    format("\n  B_block#%d:\n",[i(Id)],!IO),
    ml_b_block_lookup(MLProc,BId,BBlk),
    BBlk = ml_b_block(_,BBlkInfo,Instrs,NextSId),
    BBlkInfo = ml_b_block_info(MLItr,_,_,_),
    ( MLItr = ml_none ->
        write_string("    SEMIDET\n",!IO)
    ;   write_string("    NONDET ",!IO),
        write(MLItr,!IO),
        nl(!IO)
    ),
    write_string("    [\n",!IO),
    foldl(ml_instr_write,Instrs,!IO),
    write_string("    ]\n",!IO),
    NextSId = ml_s_block_id(NId),
    format("    NEXT(S_block#%d)\n",[i(NId)],!IO),
    ml_s_block_write(MLProc,NextSId,!IO).

%---------------------------------------------------------------------------%

:- pred ml_l_block_write(ml_l_block_id::in,ml_l_block::in,io::di,io::uo) 
    is det.

ml_l_block_write(LId,LBlk,!IO) :-
    LId = ml_l_block_id(Id),
    format("\n  L_block#%d:\n",[i(Id)],!IO),
    write_string("    [\n",!IO),
    LBlk = ml_l_block(_,_,_,Instrs),
    foldl(ml_instr_write,Instrs,!IO),
    write_string("    ]\n",!IO).

%---------------------------------------------------------------------------%

:- pred ml_instr_write(ml_instr::in,io::di,io::uo) is det.

ml_instr_write(Instr,!IO) :-
    write_string("      ",!IO),
    write(Instr,!IO),
    nl(!IO).

%---------------------------------------------------------------------------%
:- end_module ml_prog.
%---------------------------------------------------------------------------%
