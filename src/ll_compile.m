%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Compile a ml_prog into a ll_prog_0.
%
%---------------------------------------------------------------------------%

:- module ll_compile.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module hl_ac_analysis.
:- import_module hl_cc_analysis.
:- import_module hl_var_analysis.
:- import_module hl_prog.
:- import_module ll_prog.
:- import_module ml_prog.

%---------------------------------------------------------------------------%

:- pred ll_compile_ml_prog(bool::in,var_info::in,ac_info::in,cc_info::in,
    list(hl_pragma)::in,ml_prog::in,ll_prog_0::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module cadmium_common.
:- import_module cadmium_debug.

:- type lookup_info == map(ml_l_block_id,label).

:- type info
    ---> info(ml_proc,hl_symbol,hl_var,ac_info,cc_info,var_info,lookup_info,
         bool).

:- type coll_info == set(hl_var).

:- type switch_instr
    ---> no_switch(label,label,ml_b_block_id)
    ;    switch_case(label,ml_b_block_id)
    ;    switch_head(label,hl_var,maybe(label),list(hl_symbol),list(label))
    ;    switch_tail
    ;    end(label).

%---------------------------------------------------------------------------%

:- func ll_var(hl_var)   = ll_var.
:- func ll_var_1(hl_var) = ll_var.
:- func ll_var_2(hl_var) = ll_var.
:- func ll_var_3(hl_var) = ll_var.
:- func ll_global_var(hl_var) = ll_var.
:- func ll_global_var_1(hl_var) = ll_var.
:- func ll_global_var_2(hl_var) = ll_var.
:- func ll_global_var_3(hl_var) = ll_var.
:- func ll_global_var_4(hl_var) = ll_var.
:- func ll_global_var_5(hl_var) = ll_var.

ll_var(Var)   = ll_var(0,Var).
ll_var_1(Var) = ll_var(1,Var).
ll_var_2(Var) = ll_var(2,Var).
ll_var_3(Var) = ll_var(3,Var).
ll_global_var(Var)   = ll_global(1,Var).
ll_global_var_1(Var) = ll_global(2,Var).
ll_global_var_2(Var) = ll_global(3,Var).
ll_global_var_3(Var) = ll_global(4,Var).
ll_global_var_4(Var) = ll_global(5,Var).
ll_global_var_5(Var) = ll_global(6,Var).

%---------------------------------------------------------------------------%

ll_compile_ml_prog(Debug,VarInfo,ACInfo,CCInfo,Pragmas,MLProg,LLProg,!IO) :-
    some [!Label,!LLProg] (
        !:Label = label_min,
        !:LLProg = init,
        foldl3(ll_compile_pragma,Pragmas,!Label,!LLProg,!IO),
        foldl3(ll_compile_ml_proc(Debug,VarInfo,ACInfo,CCInfo),MLProg,!Label,
            !LLProg,!IO),
        !.Label = _,
        LLProg = !.LLProg
    ).

%---------------------------------------------------------------------------%

:- pred ll_compile_pragma(hl_pragma::in,label::in,label::out,ll_prog_0::in,
    ll_prog_0::out,io::di,io::uo) is det.

ll_compile_pragma(pragma_foreign(Sym,CName,Flag),!Label,!LLProg,!IO) :-
    ( Flag = normalised,
        Instrs0 = [return]
    ; Flag = unnormalised,
        Instrs0 = [interpret,return]
    ; Flag = almost_normalised,
        Instrs0 = [call_ho(0),return]
    ),
    Instrs = [label(!.Label),call_foreign(hl_symbol_arity(Sym),CName)|Instrs0],
    !:Label = 1 + !.Label,
    LLProc = ll_proc(!.Label,Instrs,init,[]),
    !:Label = 1 + !.Label,
    map.det_insert(Sym,LLProc,!LLProg).
ll_compile_pragma(pragma_doc(_,_,_,_),!Label,!LLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ll_compile_ml_proc(bool::in,var_info::in,ac_info::in,cc_info::in,
    hl_symbol::in,ml_proc::in,label::in,label::out,ll_prog_0::in,ll_prog_0::out,
    io::di,io::uo) is det.

ll_compile_ml_proc(Debug,VarInfo,ACInfo,CCInfo,Sym,MLProc,!Label,!LLProg,!IO) :-
    some [!Instrs] (
            % First we compile all lookups.
            %
        ll_compile_lookups(MLProc,LookupInfo,!Label,init,LookupInstrs,!IO),

            % Next we compile the procedure proper.
            %
        Dummy = -1,
        !:Instrs = [cpush(Dummy)],
        ml_proc_get_cxt(MLProc,FstCxt,LstCxt),
        ( Debug = yes,
            DebugInfo0 = debug_info(Sym,call(Sym),FstCxt,init),
            !:Instrs = [debug(DebugInfo0)|!.Instrs]
        ; Debug = no,
            true
        ),
        ml_proc_get_src_var(MLProc,Src),
        Info = info(MLProc,Sym,Src,ACInfo,CCInfo,VarInfo,LookupInfo,Debug),
        EndLabel = !.Label,
        !:Label = !.Label + 1,
        !:Instrs = [create_cp(EndLabel)|!.Instrs],
        ll_compile_ml_proc_s_block(Info,1,ml_entry_s_block_id,!Instrs,!Label,
            !IO),
        !:Instrs = [label(EndLabel)|!.Instrs],
        !:Instrs = [cpop(Dummy)|!.Instrs],
        ( Debug = yes,
            DebugInfo1 = debug_info(Sym,return(Sym,no),LstCxt,init),
            !:Instrs = [debug(DebugInfo1)|!.Instrs]
        ; Debug = no,
            true
        ),
        reverse(!Instrs),
        lookup_creates(CCInfo,Sym,CreateDeps),
        LLProc = ll_proc(!.Label,!.Instrs,LookupInstrs,CreateDeps),
        !:Label = !.Label + 1
    ),
    map.det_insert(Sym,LLProc,!LLProg).

%---------------------------------------------------------------------------%

:- pred ll_compile_ml_proc_s_block(info::in,int::in,ml_s_block_id::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,label::in,
    label::out,io::di,io::uo) is det.

ll_compile_ml_proc_s_block(Info,NumCPs,SId,!Instrs,!Label,!IO) :-
    lookup_s_block(Info,SId,SBlk),
    ( SBlk = ml_s_body_block(_,BodyInfo,Body),
        BodyInfo = ml_body_info(MaybeAnnots,VarNames),
        lookup_stack_frame_size(Info,StkSize),
        ( MaybeAnnots = yes(AnnotVar),
            !:Instrs = [set_annots(ll_var(AnnotVar))|!.Instrs]
        ; MaybeAnnots = no,
            true
        ),
        lookup_symbol(Info,Sym),
        ( is_debug(Info) ->
                % XXX: It appears the rule_id can be fake.
                %
            DummyRId = -1,
            map_values(named_hl_var_to_ll_var,VarNames,LLVarNames),
            DebugInfo0 = debug_info(Sym,pass(DummyRId),get_cxt(Body),
                LLVarNames),
            !:Instrs = [debug(DebugInfo0)|!.Instrs]
        ;   true
        ),
        !:Instrs = [commit(NumCPs,StkSize)|!.Instrs],
        lookup_need_redo(Info,get_pos(Body),NeedRedo),
        ( NeedRedo = yes,
            !:Instrs = [wakeup_on_redo|!.Instrs]
        ; NeedRedo = no,
            true
        ),
        IsTL = yes,
        ACCxt = no,
        ll_compile_body(Info,IsTL,ACCxt,Body,init,_,!Instrs,!IO),
        Dummy = -1,
        ( is_debug(Info) ->
            DebugInfo1 = debug_info(Sym,return(Sym,yes),get_cxt(Body),init),
            !:Instrs = [debug(DebugInfo1)|!.Instrs]
        ;   true
        ),
        !:Instrs = [cpop(Dummy)|!.Instrs],
        !:Instrs = [return|!.Instrs]
    ; SBlk = ml_s_block(_,Switches,BIds),
        ll_compile_switches(Switches,1,BIds,SInstrs,_,!Label),
        ll_compile_switch_instrs(Info,NumCPs,SInstrs,!Instrs,!Label,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred named_hl_var_to_ll_var(string::in,hl_var::in,ll_var::out) is det.

named_hl_var_to_ll_var(_,Var,LLVar) :-
    LLVar = ll_var(Var).

%---------------------------------------------------------------------------%

:- pred ll_compile_switches(list(ml_switch)::in,int::in,
    list(ml_b_block_id)::in,list(switch_instr)::out,label::out,label::in,
    label::out) is det.

ll_compile_switches(_,_,[],[end(ThisLabel)],ThisLabel,!Label) :-
    ThisLabel = !.Label,
    !:Label = !.Label + 1.
ll_compile_switches(Switches,N,BIds@[BId|BIds0],SIds,ThisLabel,!Label) :-
    ThisLabel = !.Label,
    !:Label = !.Label + 1,
    ( Switches = [Switch|Switches0],
        ( Switch = ml_switch(N,_,Var,Syms) ->
            ll_compile_switches_switch(Switches0,N,Syms,BIds,SIds0,Labels,
                MaybeNextLabel,!Label),
            SIds = [switch_head(ThisLabel,Var,MaybeNextLabel,Syms,Labels)|SIds0]
        ;   ll_compile_switches(Switches,N+1,BIds0,SIds0,NextLabel,!Label),
            SIds = [no_switch(ThisLabel,NextLabel,BId)|SIds0]
        )
    ; Switches = [],
        ll_compile_switches(Switches,N+1,BIds0,SIds0,NextLabel,!Label),
        SIds = [no_switch(ThisLabel,NextLabel,BId)|SIds0]
    ).

%---------------------------------------------------------------------------%

    % Compiles switches + b_block_ids into a sequence of switch instructions
    % which are designed to make ll_instr generation easier.
    %
:- pred ll_compile_switches_switch(list(ml_switch)::in,int::in,
    list(hl_symbol)::in,list(ml_b_block_id)::in,list(switch_instr)::out,
    list(label)::out,maybe(label)::out,label::in,label::out) is det.

ll_compile_switches_switch(Switches,N,[],BIds,SIds,[],MaybeNextLabel,!Label) :-
    ll_compile_switches(Switches,N,BIds,SIds0,NextLabel,!Label),
    ( BIds = [] ->
        MaybeNextLabel = no,
        SIds = SIds0
    ;   MaybeNextLabel = yes(NextLabel),
        SIds = [switch_tail|SIds0]
    ).
ll_compile_switches_switch(Switches,N,[_|Syms],BIds,SIds,Labels,MaybeNextLabel,
        !Label) :-
    ThisLabel = !.Label,
    !:Label = !.Label + 1,
    BId = det_head(BIds),
    ll_compile_switches_switch(Switches,N+1,Syms,det_tail(BIds),SIds0,Labels0,
        MaybeNextLabel,!Label),
    Labels = [ThisLabel|Labels0],
    SIds = [switch_case(ThisLabel,BId)|SIds0].

%---------------------------------------------------------------------------%

:- pred ll_compile_switch_instrs(info::in,int::in,list(switch_instr)::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,label::in,
    label::out,io::di,io::uo) is det.

ll_compile_switch_instrs(_,_,[],!Instrs,!Label,!IO).
ll_compile_switch_instrs(Info,NumCPs,[SInstr|SInstrs],!Instrs,!Label,!IO) :-
    ( SInstr = switch_head(ThisLabel,Var,MaybeNextLabel,Syms,Labels),
        !:Instrs = [label(ThisLabel)|!.Instrs],
        ( MaybeNextLabel = yes(NextLabel),
            !:Instrs = [create_cp(NextLabel)|!.Instrs],
            NNumCPs = NumCPs + 1
        ; MaybeNextLabel = no,
            NNumCPs = NumCPs
        ),
        Table = ll_switch_table(Syms,Labels),
        !:Instrs = [switch(ll_var(Var),Table,length(Syms))|!.Instrs]
    ; SInstr = switch_tail,
        NNumCPs = NumCPs - 1
    ; SInstr = end(EndLabel),
        !:Instrs = [label(EndLabel)|!.Instrs],
        NNumCPs = NumCPs
    ; ( SInstr = no_switch(ThisLabel,_,BId)
      ; SInstr = switch_case(ThisLabel,BId) ),
        
        NNumCPs = NumCPs,
        !:Instrs = [label(ThisLabel)|!.Instrs],
        lookup_b_block(Info,BId,BBlk),
        BBlk = ml_b_block(_,BBlkInfo,MLInstrs,NextSId),
        BBlkInfo = ml_b_block_info(MLItr,MaybePrev,_RId,_Cxt),
        ( is_debug(Info),
          MaybePrev = yes({PrevRId,PrevCxt}) ->
            lookup_symbol(Info,CallSym),
            DebugInfo = debug_info(CallSym,fail(PrevRId),PrevCxt,init),
            !:Instrs = [debug(DebugInfo)|!.Instrs]
        ;   true
        ),
        some [!NumCPs] (
            !:NumCPs = NumCPs,
            ( SInstrs \= [end(_)],
              SInstr = no_switch(_,NextLabel,_) ->
                    % If there exists a next b_block, jump to it if this one
                    % fails.
                    %
                !:NumCPs = !.NumCPs + 1,
                !:Instrs = [create_cp(NextLabel)|!.Instrs]
            ;   true
            ),
            ( MLItr = ml_ac_itr(ItrKind,Lookup,Itr0,Arg0),
                lookup_lookup_label(Info,Lookup,LookupLabel),
                Node = ll_global_var_1(Itr0),
                Pos  = ll_global_var_2(Itr0),
                Itr = ll_global_var_3(Itr0),
                Arg = ll_var(Arg0),
                Id = ll_var_1(Arg0),
                LoopLabel = !.Label,
                !:Label = !.Label + 1,
                ( ItrKind = ml_ac,
                    !:Instrs = [init_itr(Node,Pos,Itr)|!.Instrs],
                    !:Instrs = [label(LoopLabel)|!.Instrs],
                    !:Instrs = [get_next(Itr,LoopLabel,LookupLabel,Arg,Id)|
                        !.Instrs]
                ; ItrKind = ml_cc,
                    ItrNo = ll_global_var_4(Itr0),
                    LclItrNo = ll_global_var_5(Itr0),
                    !:Instrs = [init_itr(Node,Pos,Itr)|!.Instrs],
                    !:Instrs = [ccopy(ItrNo,LclItrNo)|!.Instrs],
                    !:Instrs = [label(LoopLabel)|!.Instrs],
                    !:Instrs = [get_next_cc(LclItrNo,Itr,LoopLabel,LookupLabel,
                        Arg,Id)|!.Instrs]
                ),
                !:NumCPs = !.NumCPs + 1
            ; MLItr = ml_none,
                true
            ),
            foldl3(ll_compile_ml_instr(Info),MLInstrs,!Instrs,!Label,!IO),
            ll_compile_ml_proc_s_block(Info,!.NumCPs,NextSId,!Instrs,!Label,
                !IO)
        )
    ),
    ll_compile_switch_instrs(Info,NNumCPs,SInstrs,!Instrs,!Label,!IO).

%---------------------------------------------------------------------------%

:- pred ll_compile_ml_instr(info::in,ml_instr::in,list(ll_instr(ll_var))::in,
    list(ll_instr(ll_var))::out,label::in,label::out,io::di,io::uo) is det.

ll_compile_ml_instr(Info,guard(Guard,MaybeVar),!Instrs,!Label,!IO) :-
    ll_new_var(Ch,!IO),
    ( MaybeVar = yes(Var),
        !:Instrs = [ccpush(ll_var(Var))|!.Instrs]
    ; MaybeVar = no,
        true
    ),
    !:Instrs = [pre_guard(Ch)|!.Instrs],
    IsTL = no,
    ACCxt = no,
    ll_compile_body(Info,IsTL,ACCxt,Guard,init,_,!Instrs,!IO),
    !:Instrs = [post_guard(Ch)|!.Instrs],
    ( MaybeVar = yes(_),
        !:Instrs = [ccpop|!.Instrs]
    ; MaybeVar = no,
        true
    ).
ll_compile_ml_instr(Info,pattern_guard(Res,Guard,MaybeVar),!Instrs,!Label,
        !IO) :-
    ll_new_var(Ch,!IO),
    ( MaybeVar = yes(Var),
        !:Instrs = [ccpush(ll_var(Var))|!.Instrs]
    ; MaybeVar = no,
        true
    ),
    !:Instrs = [pre_guard(Ch)|!.Instrs],
    IsTL = no,
    ACCxt = no,
    ll_compile_body(Info,IsTL,ACCxt,Guard,init,_,!Instrs,!IO),
    !:Instrs = [post_match_guard(ll_var(Res),Ch)|!.Instrs],
    ( MaybeVar = yes(_),
        !:Instrs = [ccpop|!.Instrs]
    ; MaybeVar = no,
        true
    ).
ll_compile_ml_instr(Info,is_type(Var,Type),!Instrs,!Label,!IO) :-
    ( Type = var,
      is_stack_src(Info,Var) ->
        true
    ;   !:Instrs = [is_type(ll_var(Var),Type)|!.Instrs]
    ).
ll_compile_ml_instr(_,is_int(Var,Int),!Instrs,!Label,!IO) :-
    !:Instrs = [is_int(ll_var(Var),Int)|!.Instrs].
ll_compile_ml_instr(_,is_float(Var,Flt),!Instrs,!Label,!IO) :-
    !:Instrs = [is_float(ll_var(Var),Flt)|!.Instrs].
ll_compile_ml_instr(_,is_string(Var,Str),!Instrs,!Label,!IO) :-
    !:Instrs = [is_string(ll_var(Var),Str)|!.Instrs].
ll_compile_ml_instr(_,is_named_var(Var,Name),!Instrs,!Label,!IO) :-
    !:Instrs = [is_named_var(ll_var(Var),Name)|!.Instrs].
ll_compile_ml_instr(Info,is_functor(Var,Sym,_),!Instrs,!Label,!IO) :-
        % If the variable represents the stack, then this is a NOP, since the
        % is_functor must always hold.
        %
     ( not is_stack_src(Info,Var) ->
        !:Instrs = [is_functor(ll_var(Var),Sym)|!.Instrs]
     ;  true
     ).
ll_compile_ml_instr(Info,is_ac_functor(Var,AtyCons,Sym,_),!Instrs,
        !Label,!IO) :-
    ( is_stack_src(Info,Var) ->
        ( AtyCons = ml_exactly,
            !:Instrs = [is_ac_functor(Sym)|!.Instrs]
        ; AtyCons = ml_at_least,
            !:Instrs = [is_geq_ac_functor(Sym)|!.Instrs]
        )
    ;   ( AtyCons = ml_exactly,
            !:Instrs = [is_ac_functor(ll_var(Var),Sym)|!.Instrs]
        ; AtyCons = ml_at_least,
            !:Instrs = [is_geq_ac_functor(ll_var(Var),Sym)|!.Instrs]
        )
    ).
ll_compile_ml_instr(Info,get_arg(Var,Idx,Arg),!Instrs,!Label,!IO) :-
    ( is_stack_src(Info,Var) ->
        lookup_stack_frame_size(Info,Size),
        !:Instrs = [get_arg(index_to_stack_index(Size,Idx),ll_var(Arg))|
            !.Instrs]
    ;   !:Instrs = [get_arg(ll_var(Var),Idx,ll_var(Arg))|!.Instrs]
    ).
ll_compile_ml_instr(Info,get_annots(Var,Annots),!Instrs,!Label,!IO) :-
    ( is_stack_src(Info,Var) ->
        !:Instrs = [get_annots(ll_var(Annots))|!.Instrs]
    ;   !:Instrs = [get_annots(ll_var(Var),ll_var(Annots))|!.Instrs]
    ).
ll_compile_ml_instr(_,eq(Var1,Var2),!Instrs,!Label,!IO) :-
    !:Instrs = [ceqeq(ll_var(Var1),ll_var(Var2))|!.Instrs].
ll_compile_ml_instr(Info,copy(Var1,Var2),!Instrs,!Label,!IO) :-
    ( is_stack_src(Info,Var1) ->
        !:Instrs = [cset(ll_var(Var2))|!.Instrs]
    ;   !:Instrs = [ccopy(ll_var(Var1),ll_var(Var2))|!.Instrs]
    ).
ll_compile_ml_instr(_,is_diff(Var1,Var2),!Instrs,!Label,!IO) :-
    Id1 = ll_var_1(Var1),
    Id2 = ll_var_1(Var2),
    !:Instrs = [is_diff(Id1,Id2)|!.Instrs].
ll_compile_ml_instr(Info,get_ac_args(Var,Lookup,Arg0,MaybeItr0),!Instrs,!Label,
        !IO) :-
    lookup_lookup_label(Info,Lookup,LookupLabel),
    ( MaybeItr0 = yes(Itr0),
        Node = ll_global_var_1(Itr0),
        Pos  = ll_global_var_2(Itr0),
        ( is_stack_src(Info,Var) ->
            !:Instrs = [lookup_(LookupLabel,ll_var(Arg0),Node,Pos)|!.Instrs]
        ;   !:Instrs = [lookup_(ll_var(Var),LookupLabel,ll_var(Arg0),Node,Pos)|
                !.Instrs]
        )
    ; MaybeItr0 = no,
        ( is_stack_src(Info,Var) ->
            !:Instrs = [lookup_first(LookupLabel,ll_var(Arg0))|!.Instrs]
        ;   !:Instrs = [lookup_first(ll_var(Var),LookupLabel,ll_var(Arg0))|
                !.Instrs]
        )
    ).
ll_compile_ml_instr(Info,get_cc_args(MaybeVar,Lookup,Arg0,MaybeItr0),!Instrs,
        !Label,!IO) :-
    lookup_lookup_label(Info,Lookup,LookupLabel),
    ( MaybeItr0 = yes(Itr0),
        Node  = ll_global_var_1(Itr0),
        Pos   = ll_global_var_2(Itr0),
        ItrNo = ll_global_var_4(Itr0),
        ( MaybeVar = yes(CollVar),
            !:Instrs = [lookup_cc(ll_var(CollVar),LookupLabel,ItrNo,
                ll_var(Arg0),Node,Pos)|!.Instrs]
        ; MaybeVar = no,
            !:Instrs = [lookup_cc(LookupLabel,ItrNo,ll_var(Arg0),Node,Pos)|
                !.Instrs]
        )
    ; MaybeItr0 = no,
        ( MaybeVar = yes(CollVar),
            !:Instrs = [lookup_first_cc(ll_var(CollVar),LookupLabel,
                ll_var(Arg0))|!.Instrs]
        ; MaybeVar = no,
            !:Instrs = [lookup_first_cc(LookupLabel,ll_var(Arg0))|!.Instrs]
        )
    ).
ll_compile_ml_instr(Info,get_collector(Var,Args,DetStrps,Coll),!Instrs,!Label,
        !IO) :-
    LLColl = ll_var(Coll),
    ( is_stack_src(Info,Var) ->
        !:Instrs = [cset(LLColl)|!.Instrs]
    ;   !:Instrs = [ccopy(ll_var(Var),LLColl)|!.Instrs]
    ),
    ll_compile_collector_deletes(Args,LLColl,!Instrs,!IO),
    ll_compile_collector_splits(DetStrps,LLColl,!Instrs,!IO),
    !:Instrs = [flatten(LLColl)|!.Instrs].

%---------------------------------------------------------------------------%

:- pred ll_compile_lookups(ml_proc::in,lookup_info::out,label::in,label::out,
    ll_lookup_instrs(ll_var)::in,ll_lookup_instrs(ll_var)::out,io::di,io::uo)
    is det.

ll_compile_lookups(MLProc,LookupInfo,!Label,!LookupInstrs,!IO) :-
    ml_proc_get_ml_l_blocks(MLProc,LBlks),
    foldl4(ll_compile_lookup,LBlks,!Label,init,LookupInfo,!LookupInstrs,!IO).

%---------------------------------------------------------------------------%

:- pred ll_compile_lookup(ml_l_block_id::in,ml_l_block::in,
    label::in,label::out,lookup_info::in,lookup_info::out,
    ll_lookup_instrs(ll_var)::in,ll_lookup_instrs(ll_var)::out,io::di,io::uo)
    is det.

ll_compile_lookup(LId,LBlk,!Label,!LookupInfo,!LookupInstrs,!IO) :-
    some [!Instrs] (
        LookupLabel = !.Label,
        !:Label = !.Label + 1,
        map.det_insert(LId,LookupLabel,!LookupInfo),
        !:Instrs = [],
        LBlk = ml_l_block(_,_,_,MLInstrs),
        foldl2(ll_compile_lookup_ml_instr,MLInstrs,!Instrs,!IO),
        reverse(!Instrs),
        map.det_insert(LookupLabel,!.Instrs,!LookupInstrs)
    ).

%---------------------------------------------------------------------------%

:- pred ll_compile_lookup_ml_instr(ml_instr::in,list(ll_instr(ll_var))::in,
    list(ll_instr(ll_var))::out,io::di,io::uo) is det.

ll_compile_lookup_ml_instr(is_type(Var,Type),!Instrs,!IO) :-
    !:Instrs = [lookup_is_type(ll_var(Var),Type)|!.Instrs].
ll_compile_lookup_ml_instr(is_int(Var,Int),!Instrs,!IO) :-
    !:Instrs = [lookup_is_int(ll_var(Var),Int)|!.Instrs].
ll_compile_lookup_ml_instr(is_float(Var,Flt),!Instrs,!IO) :-
    !:Instrs = [lookup_is_float(ll_var(Var),Flt)|!.Instrs].
ll_compile_lookup_ml_instr(is_string(Var,Str),!Instrs,!IO) :-
    !:Instrs = [lookup_is_string(ll_var(Var),Str)|!.Instrs].
ll_compile_lookup_ml_instr(is_named_var(Var,Str),!Instrs,!IO) :-
    !:Instrs = [lookup_is_named_var(ll_var(Var),Str)|!.Instrs].
ll_compile_lookup_ml_instr(is_functor(Var,Sym,_),!Instrs,!IO) :-
    !:Instrs = [lookup_is_functor(ll_var(Var),Sym)|!.Instrs].
ll_compile_lookup_ml_instr(is_ac_functor(Var,AtyCons,Sym,_),!Instrs,!IO) :-
    ( AtyCons = ml_exactly,
        !:Instrs = [lookup_is_ac_functor(ll_var(Var),Sym)|!.Instrs]
    ; AtyCons = ml_at_least,
        !:Instrs = [lookup_is_geq_ac_functor(ll_var(Var),Sym)|!.Instrs]
    ).
ll_compile_lookup_ml_instr(get_arg(Var,Idx,Arg),!Instrs,!IO) :-
    !:Instrs = [get_arg(ll_var(Var),Idx,ll_var(Arg))|!.Instrs].
ll_compile_lookup_ml_instr(eq(Var1,Var2),!Instrs,!IO) :-
    !:Instrs = [lookup_compare(ll_var(Var1),ll_var(Var2))|!.Instrs].
ll_compile_lookup_ml_instr(Instr,_,_,!IO) :-
    ( Instr = guard(_,_)
    ; Instr = pattern_guard(_,_,_)
    ; Instr = get_annots(_,_)
    ; Instr = copy(_,_)
    ; Instr = is_diff(_,_)
    ; Instr = get_ac_args(_,_,_,_)
    ; Instr = get_cc_args(_,_,_,_)
    ; Instr = get_collector(_,_,_,_) ),
    error($pred ++ ": unexpected lookup instruction").

%---------------------------------------------------------------------------%

:- pred ll_compile_collector_deletes(list(hl_var)::in,ll_var::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,io::di,io::uo)
    is det.

ll_compile_collector_deletes([],_,!Instrs,!IO).
ll_compile_collector_deletes([Arg|Args],Coll,!Instrs,!IO) :-
    !:Instrs = [delete(Coll,ll_var(Arg))|!.Instrs],
    ll_compile_collector_deletes(Args,Coll,!Instrs,!IO).

%---------------------------------------------------------------------------%

:- pred ll_compile_collector_splits(list(hl_var)::in,ll_var::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,io::di,io::uo)
    is det.

ll_compile_collector_splits([],_,!Instrs,!IO).
ll_compile_collector_splits([Arg|Args],Coll,!Instrs,!IO) :-
    !:Instrs = [split(Coll,ll_var(Arg))|!.Instrs],
    ll_compile_collector_splits(Args,Coll,!Instrs,!IO).

%---------------------------------------------------------------------------%

:- pred ll_compile_body(info::in,bool::in,maybe(hl_symbol)::in,hl_model::in,
    coll_info::in,coll_info::out,list(ll_instr(ll_var))::in,
    list(ll_instr(ll_var))::out,io::di,io::uo) is det.

ll_compile_body(Info,IsTL,ACCxt,Model,!CollInfo,!Instrs,!IO) :-
    ( Model = int(Int,_),
        ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,!IO),
        !:Instrs = [push_int(Int)|!.Instrs]
    ; Model = float(Flt,_),
        ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,!IO),
        !:Instrs = [push_float(Flt)|!.Instrs]
    ; Model = string(Str,_),
        ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,!IO),
        !:Instrs = [push_string(Str)|!.Instrs]
    ; Model = named_var(Name,_),
        ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,!IO),
        !:Instrs = [push_named_var(Name)|!.Instrs]
    ; Model = var(Var,_),
        ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,!IO),
        lookup_var_occs(Info,Var,Occs),
        Pos = get_pos(Model),
        ( Occs = [body(Pos)|Occs1] ->
                % This is the first occurrence of the variable, and we are in
                % the rule body.  Therefore generate a new run-time variable.
                %
            !:Instrs = [push_new_var|!.Instrs],
            ( Occs1 = [_|_],
                    % Variable is used later in the body, so save it here.
                    %
                !:Instrs = [cset(ll_var(Var))|!.Instrs]
            ; Occs1 = [],
                true
            ),
            Sym = hl_symbol(var_name,0),
            ll_compile_call(Sym,ACCxt,!Instrs,!IO)
        ; 
                % Variable is not the first occurrence.
                %
            lookup_ac_collector(Info,Var,Names),
            ( set.is_singleton(Names, Name) ->
                    % Note: if Syms is not a singleton set, then Var is a 
                    % collector for multiple AC operators.  In that case, Var 
                    % cannot be an AC term, so must already be in normal form.
                    %
                ( member(Var,!.CollInfo) ->
                    !:Instrs = [cget(ll_var_1(Var))|!.Instrs],
                    CallAnnots = yes
                ; ACCxt = yes(Sym),
                  Name = hl_symbol_name(Sym) ->
                    !:Instrs = [cget(ll_var(Var))|!.Instrs],
                    CallAnnots = yes
                ;   !:Instrs = [cget(ll_var(Var))|!.Instrs],
                    CSym = hl_symbol(Name,0),
                    ll_compile_call(CSym,ACCxt,!Instrs,!IO),
                    !:Instrs = [cset(ll_var_1(Var))|!.Instrs],
                    set.insert(Var,!CollInfo),
                    CallAnnots = no
                )
            ;   !:Instrs = [cget(ll_var(Var))|!.Instrs],
                CallAnnots = yes
            ),

                % If the annotations have changed, and if the variable has not
                % (already) been normalised, renormalise the variable but with
                % its new annotations.
                %
            ( CallAnnots = yes,
                MaybeAnnots = get_annotations(Model),
                ( MaybeAnnots = yes(_),
                    !:Instrs = [call_annots|!.Instrs]
                ; MaybeAnnots = no,
                    true
                )
            ; CallAnnots = no,
                true
            )
        )
    ; Model = functor(Sym,Args,_),
        Name = hl_symbol_name(Sym),
        Aty = hl_symbol_arity(Sym),
        ( Name = call_name ->
                % Higher order call(C,A1,...,AN)
                %
            foldl3(ll_compile_body(Info,no,ACCxt),Args,!CollInfo,!Instrs,!IO),
            N = Aty - 1,
            !:Instrs = [call_ho(N)|!.Instrs]
        ; is_d_only(Name),
          Args = [_,_] ->
                % CC-cons '$cc'(X,Y)
                %   (or other D-only operator).
                %   Normalisation of such operators is complicated, so we just
                %   leave it for the interpreter.
                %
            ll_compile_body_construct(Info,Model,!Instrs,!IO),
            !:Instrs = [interpret|!.Instrs]
        ; Name = top_level_name,
          Args = [Arg,TLArg] ->
                % A $top_level B
                %
            ll_compile_body(Info,no,no,Arg,!CollInfo,!Instrs,!IO),
            ll_compile_body(Info,no,no,TLArg,!CollInfo,!Instrs,!IO),
            !:Instrs = [top_level|!.Instrs]
        ; is_acd(Name) ->
                % Conjunction /\ and other ACD functors.
                %
            ll_new_var(Events,!IO),
            Sym0 = hl_symbol(Name,0),
            ( find_acd_collector(Info,Name,Args,CollVar,NArgs) ->
                foldl2(ll_compile_body_construct(Info),NArgs,!Instrs,!IO),
                ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,
                    !Instrs,!IO),
                !:Instrs = [construct_acd(Events,Sym0,Aty-1,ll_var(CollVar))
                    |!.Instrs]
            ;   foldl2(ll_compile_body_construct(Info),Args,!Instrs,!IO),
                ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,
                    !Instrs,!IO),
                !:Instrs = [construct_acd(Events,Sym0,Aty)|!.Instrs]
            ),
            !:Instrs = [interpret(Events)|!.Instrs]
        ; is_ac(Name) ->
                % AC functors not ACD.
                %
            Sym0 = hl_symbol(Name,0),
            foldl3(ll_compile_body(Info,no,yes(Sym0)),Args,!CollInfo,!Instrs,
                !IO),
            ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,
                !IO),
            !:Instrs = [construct_ac(Sym0,Aty)|!.Instrs],
            ll_compile_call(Sym0,ACCxt,!Instrs,!IO)
        ;       % Vanilla functors.
                %
            foldl3(ll_compile_body(Info,no,no),Args,!CollInfo,!Instrs,!IO),
            ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,
                !IO),
            ll_compile_call(Sym,ACCxt,!Instrs,!IO)
        )
    ),
    ll_compile_annots_post_construct(Model,!Instrs,!IO).

%---------------------------------------------------------------------------%

:- pred find_acd_collector(info::in,string::in,list(hl_model)::in,hl_var::out,
    list(hl_model)::out) is semidet.

find_acd_collector(Info,Name,[Arg|Args],ConjColl,NArgs) :-
        % Prefer the right-most conjunction collector.
        %
    ( find_acd_collector(Info,Name,Args,ConjColl0,NArgs0) ->
        ConjColl = ConjColl0,
        NArgs = [Arg|NArgs0]
    ; Arg = var(Var,_),
      no = get_annotations(Arg),
      lookup_ac_collector(Info,Var,Names),
      set.is_singleton(Names, NName),
      NName = Name ->
        ConjColl = Var,
        NArgs = Args
    ;   fail
    ).

%---------------------------------------------------------------------------%

:- pred ll_compile_annots_pre_construct(info::in,bool::in,hl_model::in,
    coll_info::in,coll_info::out,list(ll_instr(ll_var))::in,
    list(ll_instr(ll_var))::out,io::di,io::uo) is det.

ll_compile_annots_pre_construct(Info,IsTL,Model,!CollInfo,!Instrs,!IO) :-
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
            % Make the annotations.
            %
        maybe_restore_annots(Info,IsTL,Model,!Instrs),
        ll_compile_body(Info,no,no,Annots,!CollInfo,!Instrs,!IO),

        !:Instrs = [setup_annots|!.Instrs]
    ; MaybeAnnots = no,
        maybe_restore_annots(Info,IsTL,Model,!Instrs)
    ).

%---------------------------------------------------------------------------%

:- pred ll_compile_annots_post_construct(hl_model::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,io::di,io::uo) 
    is det.

ll_compile_annots_post_construct(Model,!Instrs,!IO) :-
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(_),
        !:Instrs = [clear_annots|!.Instrs]
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_restore_annots(info::in,bool::in,hl_model::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out) is det.

maybe_restore_annots(Info,IsTL,Model,!Instrs) :-
    ( IsTL = yes,
        ( Model = var(Var,_),
          lookup_var_occs(Info,Var,Occs),
          Occs \= [body(_)|_] ->
                % Do not generate a restore_annots instruction if the body 
                % is just head-var.  In this case, the var-default-rule
                % overrides the top-level-default-rule.
                %
            true
        ;       % Restore the annotations from the head, so that they are
                % attached correctly to the body as per the defaulting rules.
                %
            !:Instrs = [restore_annots|!.Instrs]
        )
    ; IsTL = no,
        true
    ).

%---------------------------------------------------------------------------%

    % Like ll_compile_body but constructs the terms only.  This is needed for
    % arguments to conjunction, which are not allowed to be executed until
    % the entire conjunction is made.  This is to handle CC correctly.
    %
:- pred ll_compile_body_construct(info::in,hl_model::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,io::di,io::uo)
    is det.

ll_compile_body_construct(Info,Model,!Instrs,!IO) :-
    ( Model = int(Int,_),
        !:Instrs = [push_int(Int)|!.Instrs]
    ; Model = float(Flt,_),
        !:Instrs = [push_float(Flt)|!.Instrs]
    ; Model = string(Str,_),
        !:Instrs = [push_string(Str)|!.Instrs]
    ; Model = named_var(Name,_),
        !:Instrs = [push_named_var(Name)|!.Instrs]
    ; Model = var(Var,_),
        lookup_var_occs(Info,Var,Occs),
        Pos = get_pos(Model),
        ( Occs = [body(Pos)|Occs1] ->
            !:Instrs = [push_new_var|!.Instrs],
            ( Occs1 = [_|_],
                !:Instrs = [cset(ll_var(Var))|!.Instrs]
            ; Occs1 = [],
                true
            )
        ;   !:Instrs = [cget(ll_var(Var))|!.Instrs]
        )
    ; Model = functor(Sym,Args,_),
        foldl2(ll_compile_body_construct(Info),Args,!Instrs,!IO),
        Name = hl_symbol_name(Sym),
        Aty = hl_symbol_arity(Sym),
        ( is_ac(Name) ->
            Sym0 = hl_symbol(Name,0),
            !:Instrs = [construct_ac(Sym0,Aty)|!.Instrs]
        ;   !:Instrs = [construct(Sym)|!.Instrs]
        )
    ),
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        ll_compile_body_construct(Info,Annots,!Instrs,!IO),
        !:Instrs = [attach_annots|!.Instrs]
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred ll_compile_call(hl_symbol::in,maybe(hl_symbol)::in,
    list(ll_instr(ll_var))::in,list(ll_instr(ll_var))::out,io::di,io::uo) 
    is det.

ll_compile_call(Sym,no,!Instrs,!IO) :-
    !:Instrs = [call_sym(Sym)|!.Instrs].
ll_compile_call(Sym,yes(ACSym),!Instrs,!IO) :-
    !:Instrs = [call_ac_arg_sym(ACSym,Sym)|!.Instrs].

%---------------------------------------------------------------------------%

:- pred ll_new_var(ll_var::out,io::di,io::uo) is det.

ll_new_var(LLVar,!IO) :-
    new_hl_var(HLVar,!IO),
    LLVar = ll_var(HLVar).

%---------------------------------------------------------------------------%

:- func index_to_stack_index(int,int) = int.

index_to_stack_index(Aty,Idx0) = Idx1 :-
    Idx1 = Aty - Idx0 + 1.

%---------------------------------------------------------------------------%

:- pred lookup_s_block(info::in,ml_s_block_id::in,ml_s_block::out) is det.

lookup_s_block(info(MLProc,_,_,_,_,_,_,_),SId,SBlk) :-
    ml_s_block_lookup(MLProc,SId,SBlk).

%---------------------------------------------------------------------------%

:- pred lookup_b_block(info::in,ml_b_block_id::in,ml_b_block::out) is det.

lookup_b_block(info(MLProc,_,_,_,_,_,_,_),BId,BBlk) :-
    ml_b_block_lookup(MLProc,BId,BBlk).

%---------------------------------------------------------------------------%

:- pred lookup_stack_frame_size(info::in,int::out) is det.

lookup_stack_frame_size(info(_,Sym,_,_,_,_,_,_),Size) :-
    Name = hl_symbol_name(Sym),
    ( ( Name = var_name
      ; is_ac(Name) ) ->
        Size = 1
    ;   Size = hl_symbol_arity(Sym)
    ).

%---------------------------------------------------------------------------%

:- pred lookup_symbol(info::in,hl_symbol::out) is det.

lookup_symbol(info(_,Sym,_,_,_,_,_,_),Sym).

%---------------------------------------------------------------------------%

:- pred lookup_ac_collector(info::in,hl_var::in,set(string)::out) is det.

lookup_ac_collector(info(_,_,_,ACInfo,_,_,_,_),Var,Syms) :-
    lookup_ac_collector(ACInfo,Var,Syms).

%---------------------------------------------------------------------------%

:- pred lookup_need_redo(info::in,pos::in,bool::out) is det.

lookup_need_redo(info(_,_,_,_,CCInfo,_,_,_),Pos,NeedRedo) :-
    lookup_need_redo(CCInfo,Pos,NeedRedo).

%---------------------------------------------------------------------------%

:- pred lookup_var_occs(info::in,hl_var::in,list(pos_info)::out) is det.

lookup_var_occs(info(_,_,_,_,_,VarInfo,_,_),Var,Occs) :-
    lookup_var_occs(VarInfo,Var,Occs).

%---------------------------------------------------------------------------%

:- pred lookup_lookup_label(info::in,ml_l_block_id::in,label::out) is det.

lookup_lookup_label(info(_,_,_,_,_,_,LookupInfo,_),LId,Label) :-
    lookup(LookupInfo,LId,Label).

%---------------------------------------------------------------------------%

:- pred is_stack_src(info::in,hl_var::in) is semidet.

is_stack_src(info(_,_,Src,_,_,_,_,_),Src).

%---------------------------------------------------------------------------%

:- pred is_debug(info::in) is semidet.

is_debug(info(_,_,_,_,_,_,_,yes)).

