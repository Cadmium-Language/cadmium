%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Compile a hl_prog into a ml_prog.
%
%---------------------------------------------------------------------------%

:- module ml_compile.
:- interface.

:- import_module bool.
:- import_module io.

:- import_module hl_ac_analysis.
:- import_module hl_prog.
:- import_module ml_prog.

    % Compile a hl_prog into an un-optimised ml_prog.
    %
:- pred ml_compile_hl_prog(bool::in,ac_info::in,hl_prog::in,ml_prog::out,
    io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cadmium_common.
:- import_module cadmium_error.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

ml_compile_hl_prog(Debug,ACInfo,HLProg,MLProg,!IO) :-
    foldl2(ml_compile_hl_rules(Debug,ACInfo),HLProg,init,MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_compile_hl_rules(bool::in,ac_info::in,hl_symbol::in,
    list(hl_rule)::in,ml_prog::in,ml_prog::out,io::di,io::uo) is det.

ml_compile_hl_rules(Debug,ACInfo,Sym,Rules,!MLProg,!IO) :-
    some [!MLProc,!Fixed] (
        ml_proc_init(!:MLProc,!:Fixed,!IO),
        ( Debug = yes,
            FstRule = det_head(Rules),
            LstRule = det_last(Rules),
            get_rule_cxt(FstRule,FstCxt),
            get_rule_cxt(LstRule,LstCxt),
            ml_proc_set_cxt(FstCxt,LstCxt,!MLProc)
        ; Debug = no,
            true
        ),
        ml_proc_get_src_var(!.MLProc,Src),
        ml_compile_hl_rules_2(Rules,BIds,Src,Sym,Debug,no,!.Fixed,ACInfo,
            !MLProc,!IO),
        SBlk = ml_s_block(ml_entry_s_block_id,[],BIds),
        ml_s_block_insert(SBlk,!MLProc),
        map.det_insert(Sym,!.MLProc,!MLProg)
    ).

%---------------------------------------------------------------------------%

:- pred get_rule_cxt(hl_rule::in,cxt::out) is det.

get_rule_cxt(hl_rule(_,_,Head,_,_,_),Cxt) :-
    Cxt = get_cxt(Head).

%---------------------------------------------------------------------------%

:- pred ml_compile_hl_rules_2(list(hl_rule)::in,list(ml_b_block_id)::out,
    hl_var::in,hl_symbol::in,bool::in,maybe({rule_id,cxt})::in,ml_fixed::in,
    ac_info::in,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_compile_hl_rules_2([],[],_,_,_,_,_,_,!MLProc,!IO).
ml_compile_hl_rules_2([Rule|Rules],[BId|BIds],Src,Sym,Debug,MaybePrev,Fixed,
        ACInfo,!MLProc,!IO) :-
    Rule = hl_rule(RId,MaybeCC,Head,Guards,Body,VarNameInfo),
    ml_compile_hl_rules_2(Rules,BIds,Src,Sym,Debug,yes({RId,get_cxt(Head)}),
        Fixed,ACInfo,!MLProc,!IO),
    ml_new_s_block_id(BodyId,!IO),
    
        % - If the head uses an implicit collector, then add it to the body 
        %   here.
        % - Get the top-level ACD collector if it exists.
        %
    Name = hl_symbol_name(Sym),
    ( is_ac(Name),
      lookup_ac_match(ACInfo,get_pos(Head),ACMatch),
      ACMatch = ac_match(_,_,Coll),
      Coll = implicit(CVar) ->
        Attr = hl_attr_init,
        HLCVar = var(CVar,Attr),
        NBody = hl_ac_merge(Name,HLCVar,Body),
        ( is_acd(Name) ->
            MaybeConjColl = yes(CVar)
        ;   MaybeConjColl = no
        )
    ;   NBody = Body,
        MaybeConjColl = no
    ),

        % Record the top-level annotation collector if it exists.
        %
    MaybeAnnots = get_annotations(Head),
    ( MaybeAnnots = yes(Annots),
        lookup_ac_match(ACInfo,get_pos(Annots),AnnotMatch),
        AnnotMatch = ac_match(_,_,AnnotColl),
        ( AnnotColl = none,
            MaybeAnnotColl = no
        ; ( AnnotColl = implicit(AnnotVar)
          ; AnnotColl = explicit(AnnotVar,_) ),
            MaybeAnnotColl = yes(AnnotVar)
        )
    ; MaybeAnnots = no,
        MaybeAnnotColl = no
    ),
    
    ( Debug = yes,
        foldl(insert_name,VarNameInfo,init,VarNames)
    ; Debug = no,
        VarNames = init
    ),
    BodyInfo = ml_body_info(MaybeAnnotColl,VarNames),
    BodyBlock = ml_s_body_block(BodyId,BodyInfo,NBody),
    ml_s_block_insert(BodyBlock,!MLProc),
    some [!BId,!Fixed,!BBlkInfo,!Instrs] (
        !:Instrs = [],
        !:Fixed = Fixed,
        !:BBlkInfo = ml_b_block_info(ml_none,MaybePrev,RId,get_cxt(Body)),
        ml_new_b_block_id(!:BId,!IO),
        BId = !.BId,
        ml_compile_hl_model_match(ACInfo,Guards,Src,Head,!BBlkInfo,!BId,!Fixed,
            !Instrs,!MLProc,!IO),
        ( MaybeCC = yes(_),
            ml_compile_hl_model_cc_match(ACInfo,Guards,MaybeConjColl,
                get_pos(Body),!BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO)
        ; MaybeCC = no,
            true
        ),
        ml_compile_hl_guards(Guards,get_cxt(Head),Guards,ACInfo,MaybeConjColl,
            !BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO),
        !.Fixed = _,
        reverse(!Instrs),
        BBlk = ml_b_block(!.BId,!.BBlkInfo,!.Instrs,BodyId),
        ml_b_block_insert(BBlk,!MLProc)
    ).

%---------------------------------------------------------------------------%

    % Compiles a list of hl_guard into a list of guard ml_instr.  Note that
    % we must schedule guards in (a) correct order, since pattern guards have 
    % outputs.
    %
:- pred ml_compile_hl_guards(list(hl_guard)::in,cxt::in,list(hl_guard)::in,
    ac_info::in,maybe(hl_var)::in,ml_b_block_info::in,ml_b_block_info::out,
    ml_b_block_id::in,ml_b_block_id::out,ml_fixed::in,ml_fixed::out,
    list(ml_instr)::in,list(ml_instr)::out,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_compile_hl_guards([],_,_,_,_,!BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO).
ml_compile_hl_guards(Guards@[_|_],Cxt,Gds,ACInfo,MaybeConjColl,!BBlkInfo,!BId,
        !Fixed,!Instrs,!MLProc,!IO) :-
    ml_schedule_guard(Cxt,!.Fixed,Guards,NGuards,MaybeGuard,!IO),
    ( MaybeGuard = yes(Guard),
        ( Guard = user_guard(Goal),
            !:Instrs = [guard(Goal,MaybeConjColl)|!.Instrs]
        ; Guard = match_guard(Head,Goal),
            ml_new_var(Src,!Fixed,!IO),
            !:Instrs = [pattern_guard(Src,Goal,MaybeConjColl)|!.Instrs],
            ml_compile_hl_model_match(ACInfo,Gds,Src,Head,!BBlkInfo,!BId,
                !Fixed,!Instrs,!MLProc,!IO)
        ; Guard = is_type(_,_,_),
                % Type guards are already scheduled.
                %
            true
        ; Guard = ac_stripper(_,_),
            true
        ),
        ml_compile_hl_guards(NGuards,Cxt,Gds,ACInfo,MaybeConjColl,!BBlkInfo,
            !BId,!Fixed,!Instrs,!MLProc,!IO)
    ; MaybeGuard = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred ml_schedule_guard(cxt::in,ml_fixed::in,list(hl_guard)::in,
    list(hl_guard)::out,maybe(hl_guard)::out,io::di,io::uo) is det.

ml_schedule_guard(Cxt,_,[],[],MaybeSGuard,!IO) :-
    Message = message("guard contains variables not bound in rule head",[],
        Cxt),
    compiler_error(Message,!IO),
    MaybeSGuard = no.
ml_schedule_guard(Cxt,Fixed,[Guard|Guards],NGuards,MaybeSGuard,!IO) :-
    ( ml_can_schedule_guard(Fixed,Guard) ->
        MaybeSGuard = yes(Guard),
        NGuards = Guards
    ;   ml_schedule_guard(Cxt,Fixed,Guards,NGuards0,MaybeSGuard,!IO),
        NGuards = [Guard|NGuards0]
    ).

%---------------------------------------------------------------------------%

:- pred ml_can_schedule_guard(ml_fixed::in,hl_guard::in) is semidet.

ml_can_schedule_guard(Fixed,Guard) :-
    ( Guard = user_guard(Goal),
        hl_model_is_fixed(Fixed,Goal)
    ; Guard = match_guard(_,Goal),
        hl_model_is_fixed(Fixed,Goal)
    ; Guard = is_type(_,Var,_),
        ml_is_fixed(Fixed,Var)
    ; Guard = ac_stripper(_,_),
        true
    ).

%---------------------------------------------------------------------------%

:- pred ml_compile_hl_model_cc_match(ac_info::in,list(hl_guard)::in,
    maybe(hl_var)::in,pos::in,ml_b_block_info::in,ml_b_block_info::out,
    ml_b_block_id::in,ml_b_block_id::out,ml_fixed::in,ml_fixed::out,
    list(ml_instr)::in,list(ml_instr)::out,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_compile_hl_model_cc_match(ACInfo,Gds,MaybeConjColl,Pos,!BBlkInfo,!BId,!Fixed,
        !Instrs,!MLProc,!IO) :-
    MaybeSrc = no,
    lookup_ac_match(ACInfo,Pos,CCMatch),
    CCMatch = ac_match(CCStrps,_,_),
    ml_compile_hl_model_match_ac_args(MaybeSrc,MaybeConjColl,Gds,CCStrps,
        ACInfo,[],_,[],_,!BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_compile_hl_model_match(ac_info::in,list(hl_guard)::in,hl_var::in,
    hl_model::in,ml_b_block_info::in,ml_b_block_info::out,ml_b_block_id::in,
    ml_b_block_id::out,ml_fixed::in,ml_fixed::out,list(ml_instr)::in,
    list(ml_instr)::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_compile_hl_model_match(ACInfo,Gds,Src,Model,!BBlkInfo,!BId,!Fixed,!Instrs,
        !MLProc,!IO) :-
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        ml_new_var(ASrc,!Fixed,!IO),
        !:Instrs = [get_annots(Src,ASrc)|!.Instrs],
        ml_compile_hl_model_match(ACInfo,Gds,ASrc,Annots,!BBlkInfo,!BId,!Fixed,
            !Instrs,!MLProc,!IO)
    ; MaybeAnnots = no,
        true
    ),
    MaybeAtVar = get_at_var(Model),
    ( MaybeAtVar = yes(AtVar),
        ( ml_is_fixed(!.Fixed,AtVar) ->
            !:Instrs = [eq(Src,AtVar)|!.Instrs]
        ;   !:Instrs = [copy(Src,AtVar)|!.Instrs],
            ml_fix(AtVar,!Fixed),
            ( has_type_guard(Gds,AtVar,AtType) ->
                !:Instrs = [is_type(Src,AtType)|!.Instrs]
            ;   true
            )
        )
    ; MaybeAtVar = no,
        true
    ),
    ( Model = int(Int,_),
        !:Instrs = [is_int(Src,Int)|!.Instrs]
    ; Model = float(Flt,_),
        !:Instrs = [is_float(Src,Flt)|!.Instrs]
    ; Model = string(Str,_),
        !:Instrs = [is_string(Src,Str)|!.Instrs]
    ; Model = named_var(Name,_),
        !:Instrs = [is_named_var(Src,Name)|!.Instrs]
    ; Model = var(Var,_),
        ( ml_is_fixed(!.Fixed,Var) ->
            !:Instrs = [eq(Src,Var)|!.Instrs]
        ;   !:Instrs = [copy(Src,Var)|!.Instrs], 
            ml_fix(Var,!Fixed),
            ( has_type_guard(Gds,Var,Type) ->
                !:Instrs = [is_type(Src,Type)|!.Instrs]
            ;   true
            )
        )
    ; Model = functor(Sym,Args,_),
        Name = hl_symbol_name(Sym),
        ml_new_var(NSrc,!Fixed,!IO),
        ( is_ac(Name) ->
            Pos = get_pos(Model),
            lookup_ac_match(ACInfo,Pos,ACMatch),
            ACMatch = ac_match(Strps,DetStrps,Coll),
            ( Coll = none,
                AtyCons = ml_exactly
            ; Coll = implicit(_),
                AtyCons = ml_at_least
            ; Coll = explicit(_,_),
                AtyCons = ml_at_least
            ),
                % Annotation operator `::' is ACI, thus the collector can be
                % empty.
                %
            ( AtyCons = ml_at_least,
              Name = annotate_name ->
                Sym1 = hl_symbol(annotate_name,hl_symbol_arity(Sym)-1),
                !:Instrs = [is_ac_functor(Src,AtyCons,Sym1,NSrc)|!.Instrs]
            ;   !:Instrs = [is_ac_functor(Src,AtyCons,Sym,NSrc)|!.Instrs]
            ),
            MaybeNSrc = yes(NSrc),
            ml_compile_hl_model_match_ac_args(MaybeNSrc,no,Gds,Strps,ACInfo,[],
                ASrcs,[],_,!BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO),
            ( Coll = none,
                ( DetStrps = [_|_],
                        % TODO make CSrc optional.
                        %
                    ml_new_var(CSrc,!Fixed,!IO),
                    !:Instrs = [get_collector(NSrc,ASrcs,DetStrps,CSrc)|
                        !.Instrs]
                ; DetStrps = [],
                    true
                )
            ; ( Coll = implicit(CVar)
              ; Coll = explicit(CVar,_) ),
                ml_new_var(CSrc,!Fixed,!IO),
                !:Instrs = [get_collector(NSrc,ASrcs,DetStrps,CSrc)|!.Instrs],
                ( ml_is_fixed(!.Fixed,CVar) ->
                    !:Instrs = [eq(CSrc,CVar)|!.Instrs]
                ;   !:Instrs = [copy(CSrc,CVar)|!.Instrs],
                    ml_fix(CVar,!Fixed),
                    ( has_type_guard(Gds,CVar,Type) ->
                        !:Instrs = [is_type(CSrc,Type)|!.Instrs]
                    ;   true
                    )
                )
            )
        ;   !:Instrs = [is_functor(Src,Sym,NSrc)|!.Instrs],
            ml_compile_hl_model_match_args(NSrc,Gds,Args,1,ACInfo,!BBlkInfo,
                !BId,!Fixed,!Instrs,!MLProc,!IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred ml_compile_hl_model_match_args(hl_var::in,list(hl_guard)::in,
    list(hl_model)::in,int::in,ac_info::in,ml_b_block_info::in,
    ml_b_block_info::out,ml_b_block_id::in,ml_b_block_id::out,ml_fixed::in,
    ml_fixed::out,list(ml_instr)::in,list(ml_instr)::out,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_compile_hl_model_match_args(_,_,[],_,_,!BBlkInfo,!BId,!Fixed,!Instrs,
        !MLProc,!IO).
ml_compile_hl_model_match_args(Src,Gds,[Arg|Args],N,ACInfo,!BBlkInfo,!BId,
        !Fixed,!Instrs,!MLProc,!IO) :-
    ml_new_var(ASrc,!Fixed,!IO),
    !:Instrs = [get_arg(Src,N,ASrc)|!.Instrs],
    ml_compile_hl_model_match(ACInfo,Gds,ASrc,Arg,!BBlkInfo,!BId,!Fixed,!Instrs,
        !MLProc,!IO),
    ml_compile_hl_model_match_args(Src,Gds,Args,N+1,ACInfo,!BBlkInfo,!BId,
        !Fixed,!Instrs,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_compile_hl_model_match_ac_args(maybe(hl_var)::in,maybe(hl_var)::in,
    list(hl_guard)::in,list(hl_model)::in,ac_info::in,list(hl_var)::in,
    list(hl_var)::out,list(maybe(hl_symbol))::in,list(maybe(hl_symbol))::out,
    ml_b_block_info::in,ml_b_block_info::out,ml_b_block_id::in,
    ml_b_block_id::out,ml_fixed::in,ml_fixed::out,list(ml_instr)::in,
    list(ml_instr)::out,ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_compile_hl_model_match_ac_args(_,_,_,[],_,!ASrcs,!MaybeSyms,!BBlkInfo,!BId,
        !Fixed,!Instrs,!MLProc,!IO).
ml_compile_hl_model_match_ac_args(MaybeSrc,MaybeConjColl,Gds,[Arg|Strps],
        ACInfo,!ASrcs,!MaybeSyms,!BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO) :-
        
        % If we have a Src, it means we are compiling an AC matching.  
        % Otherwise we are compiling a CC matching.
        %
    ml_new_l_block_id(LId,!IO),
    ml_new_var(ASrc,!Fixed,!IO),
    ml_new_var(Itr,!Fixed,!IO),
    ( MaybeSrc = yes(Src),
        !:Instrs = [get_ac_args(Src,LId,ASrc,yes(Itr))|!.Instrs]
    ; MaybeSrc = no,
        !:Instrs = [get_cc_args(MaybeConjColl,LId,ASrc,yes(Itr))|!.Instrs]
    ),
    reverse(!Instrs),
    ml_new_s_block_id(SId,!IO),
    BBlk = ml_b_block(!.BId,!.BBlkInfo,!.Instrs,SId),
    !:Instrs = [],
    ml_b_block_insert(BBlk,!MLProc),
    ml_new_b_block_id(!:BId,!IO),
    SBlk = ml_s_block(SId,[],[!.BId]),
    ml_s_block_insert(SBlk,!MLProc),

    ( get_sym(Gds,Arg,Sym) ->
        Name = hl_symbol_name(Sym),
        ( is_ac(Name) ->
            MaybeSym = yes(hl_symbol(Name,0))
        ;   MaybeSym = yes(Sym)
        )
    ;   MaybeSym = no
    ),

        % Generate the code for the remaining matching.
        %
    ml_compile_difference_tests(ASrc,MaybeSym,!.ASrcs,!.MaybeSyms,!Instrs,!IO),
    !:ASrcs = [ASrc|!.ASrcs],
    !:MaybeSyms = [MaybeSym|!.MaybeSyms],
    ( MaybeSrc = yes(_),
        ItrKind = ml_ac
    ; MaybeSrc = no,
        ItrKind = ml_cc
    ),
    !.BBlkInfo = ml_b_block_info(_,_,RId,Cxt),
    MLItr = ml_ac_itr(ItrKind,LId,Itr,ASrc),
    !:BBlkInfo = ml_b_block_info(MLItr,no,RId,Cxt),
    ml_compile_hl_model_match(ACInfo,Gds,ASrc,Arg,!BBlkInfo,!BId,!Fixed,!Instrs,
        !MLProc,!IO),
    ml_compile_hl_model_match_ac_args(MaybeSrc,MaybeConjColl,Gds,Strps,ACInfo,
        !ASrcs,!MaybeSyms,!BBlkInfo,!BId,!Fixed,!Instrs,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_compile_difference_tests(hl_var::in,maybe(hl_symbol)::in,
    list(hl_var)::in,list(maybe(hl_symbol))::in,list(ml_instr)::in,
    list(ml_instr)::out,io::di,io::uo) is det.

ml_compile_difference_tests(_,_,[],_,!Instrs,!IO).
ml_compile_difference_tests(Id0,MaybeSym0,[Id1|Ids],MaybeSyms,!Instrs,!IO) :-
    MaybeSym1 = det_head(MaybeSyms),
        % Difference test is only valid if the AC arguments are unifiable.
        % However, our test for unifiablity is restricted to the top-level
        % functor.
        %
    ( is_compatible_iterator(MaybeSym0,MaybeSym1) ->
        !:Instrs = [is_diff(Id0,Id1)|!.Instrs]
    ;   true
    ),
    ml_compile_difference_tests(Id0,MaybeSym0,Ids,det_tail(MaybeSyms),!Instrs,
        !IO).

%---------------------------------------------------------------------------%

:- pred is_compatible_iterator(maybe(hl_symbol)::in,maybe(hl_symbol)::in)
    is semidet.

is_compatible_iterator(MaybeSym0,MaybeSym1) :-
    ( ( MaybeSym0 = no
      ; MaybeSym1 = no ) ->
        true
    ;   MaybeSym0 = MaybeSym1
    ).

%---------------------------------------------------------------------------%

:- pred has_type_guard(list(hl_guard)::in,hl_var::in,hl_model_type::out) 
    is semidet.

has_type_guard([Gd|Gds],Var,Type) :-
    ( Gd = is_type(Type0,Var,_) ->
        Type = Type0
    ;   has_type_guard(Gds,Var,Type)
    ).

%---------------------------------------------------------------------------%

:- pred insert_name(hl_var::in,string::in,map(string,hl_var)::in,
    map(string,hl_var)::out) is det.

insert_name(Var,Name,!Map) :-
    map.det_insert(Name,Var,!Map).

%---------------------------------------------------------------------------%
:- end_module ml_compile.
%---------------------------------------------------------------------------%
