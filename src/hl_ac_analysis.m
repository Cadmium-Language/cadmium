%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Decides what the compiler should generate in order to handle AC operators.
%
%---------------------------------------------------------------------------%

:- module hl_ac_analysis.

:- interface.

:- import_module io.
:- import_module list.
:- import_module set.

:- import_module cadmium_common.
:- import_module hl_prog.
:- import_module hl_var_analysis.

:- type ac_collector
    ---> none
    ;    implicit(hl_var)
    ;    explicit(hl_var,pos).

    % ac_match(Strippers,DetStippers,Collector)
    %
:- type ac_match
    ---> ac_match(list(hl_model),list(hl_var),ac_collector).
    
    % Abstract data structure describes how AC operators are to be compiled.
    %
:- type ac_info.

    % Generate the ac_info for a hl_prog.
    %
:- pred hl_ac_analysis(var_info::in,hl_prog::in,ac_info::out,io::di,io::uo) 
    is det.

    % Return the ac_match for an AC operator in the head of a rule.
    %
:- pred lookup_ac_match(ac_info::in,pos::in,ac_match::out) is det.

    % Return a set of AC symbols that the given variable is a collector for.
    %
:- pred lookup_ac_collector(ac_info::in,hl_var::in,set(hl_name)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module cadmium_error.

%---------------------------------------------------------------------------%

:- type ac_info 
    ---> ac_info(ac_match_info,ac_coll_info).

:- type ac_coll_info == map(hl_var,set(hl_name)).

:- type ac_match_info == map(pos,ac_match).

:- type ac_sort
    ---> normal
    ;    top_level
    ;    annotation.

%---------------------------------------------------------------------------%

lookup_ac_match(ACInfo,Pos,ACMatch) :-
    ACInfo = ac_info(ACMatchInfo,_),    
    lookup(ACMatchInfo,Pos,ACMatch).

%---------------------------------------------------------------------------%

lookup_ac_collector(ACInfo,Var,Names) :-
    ACInfo = ac_info(_,CollInfo),
    ( search(CollInfo,Var,Names0) ->
        Names = Names0
    ;   Names = init
    ).

%---------------------------------------------------------------------------%

hl_ac_analysis(VarInfo,HLProg,ACInfo,!IO) :-
    ACInfo0 = ac_info(init,init),
    hl_prog_foldl2(hl_ac_analysis_on_rule(VarInfo),HLProg,ACInfo0,ACInfo,!IO).

%---------------------------------------------------------------------------%

:- pred hl_ac_analysis_on_rule(var_info::in,hl_rule::in,ac_info::in,
    ac_info::out,io::di,io::uo) is det.

hl_ac_analysis_on_rule(VarInfo,Rule,!ACInfo,!IO) :-
    Rule = hl_rule(_,YNCC,Hd,Gds,Body,VarNameInfo),
    IsTL = yes,
    generate_ac_match(VarInfo,VarNameInfo,Gds,IsTL,Hd,!ACInfo,!IO),
    ( YNCC = no,
        true
    ; YNCC = yes(CC),
        ( CC = functor(Sym,CCArgs0,_),
          hl_symbol_name(Sym) = conj_name ->
            CCArgs = CCArgs0
        ;   CCArgs = [CC]
        ),
        foldl2(generate_ac_match(VarInfo,VarNameInfo,Gds,no),CCArgs,
            !ACInfo,!IO),
        CCMatch = ac_match(CCArgs,[],none),
        insert_ac_match(get_pos(Body),CCMatch,!ACInfo)
    ),
    foldl2(generate_ac_match_on_guard(VarInfo,VarNameInfo,Gds),Gds,!ACInfo,
        !IO).

%---------------------------------------------------------------------------%

:- pred insert_ac_match(pos::in,ac_match::in,ac_info::in,ac_info::out) is det.

insert_ac_match(Pos,ACMatch,!ACInfo) :-
    !.ACInfo = ac_info(ACMatchInfo,CollInfo),
    map.det_insert(Pos,ACMatch,ACMatchInfo,NACMatchInfo),
    !:ACInfo = ac_info(NACMatchInfo,CollInfo).

%---------------------------------------------------------------------------%

:- pred generate_ac_match(var_info::in,var_name_info::in,list(hl_guard)::in,
    bool::in,hl_model::in,ac_info::in,ac_info::out,io::di,io::uo) is det.

generate_ac_match(VarInfo,VarNameInfo,Gds,IsTL,Model,!ACInfo,!IO) :-
    ( Model = functor(Sym,Args,_) ->
        Pos = get_pos(Model),
        Cxt = get_cxt(Model),
        ( is_ac(hl_symbol_name(Sym)) ->
            ( Args = [_] ->
                singleton_ac_error(Sym,Cxt,!IO),
                MaybeACMatch = no
            ;   ( IsTL = yes ->
                    Sort = top_level
                ;   Sort = normal
                ),
                maybe_generate_ac_match(VarInfo,VarNameInfo,Gds,Sort,Sym,
                    Args,Cxt,MaybeACMatch,!ACInfo,!IO)
            ),
            ( MaybeACMatch = yes(ACMatch),
                insert_ac_match(Pos,ACMatch,!ACInfo)
            ; MaybeACMatch = no,
                true
            )
        ;   true
        ),
        foldl2(generate_ac_match(VarInfo,VarNameInfo,Gds,no),Args,!ACInfo,!IO)
    ;   true
    ),
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        generate_ac_match_on_annotations(VarInfo,VarNameInfo,Gds,Annots,
            !ACInfo,!IO)
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred generate_ac_match_on_guard(var_info::in,var_name_info::in,
    list(hl_guard)::in,hl_guard::in,ac_info::in,ac_info::out,io::di,io::uo)
    is det.

generate_ac_match_on_guard(VarInfo,VarNameInfo,Gds,Guard,!ACInfo,!IO) :-
    ( Guard = match_guard(Model,_) ->
        IsTL = no,
        generate_ac_match(VarInfo,VarNameInfo,Gds,IsTL,Model,!ACInfo,!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred generate_ac_match_on_annotations(var_info::in,var_name_info::in,
    list(hl_guard)::in,hl_model::in,ac_info::in,ac_info::out,io::di,io::uo) 
    is det.

generate_ac_match_on_annotations(VarInfo,VarNameInfo,Gds,Annots,!ACInfo,!IO) :-
    Cxt = get_cxt(Annots),
    Sort = annotation,
    ( Annots = functor(Sym,Args,_),
      hl_symbol_name(Sym) = annotate_name ->
        maybe_generate_ac_match(VarInfo,VarNameInfo,Gds,Sort,Sym,Args,Cxt,
            MaybeACMatch,!ACInfo,!IO)
    ;   Sym = hl_symbol(annotate_name,1),
        Args = [Annots],
        maybe_generate_ac_match(VarInfo,VarNameInfo,Gds,Sort,Sym,Args,Cxt,
            MaybeACMatch,!ACInfo,!IO)
    ),
    ( MaybeACMatch = yes(ACMatch),
        insert_ac_match(get_pos(Annots),ACMatch,!ACInfo)
    ; MaybeACMatch = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_generate_ac_match(var_info::in,var_name_info::in,
    list(hl_guard)::in,ac_sort::in,hl_symbol::in,list(hl_model)::in,
    cxt::in,maybe(ac_match)::out,ac_info::in,ac_info::out,io::di,io::uo)
    is det.

maybe_generate_ac_match(VarInfo,VarNameInfo,Gds,Sort,Sym,Args1,Cxt,
        MaybeACMatch,!ACInfo,!IO) :-
    partition_strips_colls(VarInfo,Gds,Args1,Strps,DetStrps0,Colls,!IO),
    map(hl_model_to_hl_var,DetStrps0,DetStrps),
    ( Colls = [] ->
        ( ( Sort = top_level
          ; Sort = annotation ),
            new_hl_var(CVar,!IO),
            insert_ac_collector(Sym,CVar,!ACInfo),
            MaybeACMatch = yes(ac_match(Strps,DetStrps,implicit(CVar)))
        ; Sort = normal,
            MaybeACMatch = yes(ac_match(Strps,DetStrps,none))
        )
    ; Colls = [C],
      C = var(CVar,_) ->
        CPos = get_pos(C),
        insert_ac_collector(Sym,CVar,!ACInfo),
        ( Sort = top_level ->
                % The top-level collector must be unconstrained, otherwise we 
                % need two collectors.
                %
            ( is_unconstrained_var(VarInfo,CVar,CPos) ->
                MaybeACMatch = yes(ac_match(Strps,DetStrps,
                    explicit(CVar,CPos)))
            ;   implicit_collector_error(Sym,CVar,VarNameInfo,Cxt,!IO),
                MaybeACMatch = no
            )
        ;   MaybeACMatch = yes(ac_match(Strps,DetStrps,explicit(CVar,CPos)))
        )
    ;   more_than_one_collector_error(Sym,Colls,VarNameInfo,Cxt,!IO),
        MaybeACMatch = no
    ).

%---------------------------------------------------------------------------%

:- pred hl_model_to_hl_var(hl_model::in,hl_var::out) is det.

hl_model_to_hl_var(Model,Var) :-
    ( if Model = var(Var0,_)
    then Var = Var0
    else unexpected($file, $pred, "expected variable")
    ).

%---------------------------------------------------------------------------%

:- pred insert_ac_collector(hl_symbol::in,hl_var::in,ac_info::in,ac_info::out) 
    is det.

insert_ac_collector(Sym,Coll,!ACInfo) :-
    !.ACInfo = ac_info(ACMatchInfo,CollInfo),
    ( search(CollInfo,Coll,Syms0) ->
        set.insert(hl_symbol_name(Sym),Syms0,Syms1)
    ;   Syms1 = make_singleton_set(hl_symbol_name(Sym))
    ),
    map.set(Coll,Syms1,CollInfo,NCollInfo),
    !:ACInfo = ac_info(ACMatchInfo,NCollInfo).

%---------------------------------------------------------------------------%

:- pred partition_strips_colls(var_info::in,list(hl_guard)::in,
    list(hl_model)::in,list(hl_model)::out,list(hl_model)::out,
    list(hl_model)::out,io::di,io::uo) is det.

partition_strips_colls(VarInfo,Gds,Args,Strps,DetStrps,Colls,!IO) :-
    partition_strips_colls_2(VarInfo,Gds,Args,Strps,DetStrps0,Colls0,!IO),
    ( Colls0 = [],
      pick_collector(Gds,DetStrps0,DetStrp,DetStrps1) ->
        Colls = [DetStrp],
        DetStrps = DetStrps1
    ;   DetStrps = DetStrps0,
        Colls = Colls0
    ).

%---------------------------------------------------------------------------%

:- pred pick_collector(list(hl_guard)::in,list(hl_model)::in,hl_model::out,
    list(hl_model)::out) is semidet.

pick_collector(Gds,[DetStrp|DetStrps],Coll,NDetStrps) :-
        % If is_stripper/1 holds, then DetStrp is forced to be a stripper
        % (e.g. via a term/1 guard).
        %
    ( is_stripper(Gds,DetStrp) ->
        pick_collector(Gds,DetStrps,Coll,NDetStrps0),
        NDetStrps = [DetStrp|NDetStrps0]
    ;   Coll = DetStrp,
        NDetStrps = DetStrps
    ).

%---------------------------------------------------------------------------%

:- pred partition_strips_colls_2(var_info::in,list(hl_guard)::in,
    list(hl_model)::in,list(hl_model)::out,list(hl_model)::out,
    list(hl_model)::out,io::di,io::uo) is det.

partition_strips_colls_2(_,_,[],[],[],[],!IO).
partition_strips_colls_2(VarInfo,Gds,[Arg|Args],Strps,DetStrps,Colls,!IO) :-
    partition_strips_colls_2(VarInfo,Gds,Args,Strps0,DetStrps0,Colls0,!IO),
    ( Arg = var(Var,_),
      Pos = get_pos(Arg),
      no = get_annotations(Arg),
      is_unconstrained_var(VarInfo,Var,Pos) ->
        Strps = Strps0,
        DetStrps = [Arg|DetStrps0],
        Colls = Colls0
    ; is_stripper(Gds,Arg) ->
        Strps = [Arg|Strps0],
        DetStrps = DetStrps0,
        Colls = Colls0
    ; Arg = var(_,_) ->
        Strps = Strps0,
        DetStrps = DetStrps0,
        Colls = [Arg|Colls0]
    ;   sorry($file, $pred, "non-variable collectors")
    ).

%---------------------------------------------------------------------------%

:- pred is_unconstrained_var(var_info::in,hl_var::in,pos::out) is semidet.

is_unconstrained_var(VarInfo,CVar,CPos) :-
    lookup_var_occs(VarInfo,CVar,COccs),
    COccs = [head(CPos)|COccs1],
    ( COccs1 = []
    ; COccs1 = [body(_)|_]
    ).

%---------------------------------------------------------------------------%

    % Tests whether a model qualifies as a stripper, otherwise it is a 
    % collector.
    %
:- pred is_stripper(list(hl_guard)::in,hl_model::in) is semidet.

is_stripper(_,int(_,_)).
is_stripper(_,float(_,_)).
is_stripper(_,string(_,_)).
is_stripper(_,functor(Sym,_,_)) :-
    hl_symbol_name(Sym) \= call_name.
is_stripper(Gds,var(MVar,Attr)) :-
    ( contains(hl_attr_get_pragmas(Attr),pragma_ac_stripper) ->
        true
    ;   not all_false(is_stripper_guard(MVar),Gds)
    ).

%---------------------------------------------------------------------------%

:- pred is_stripper_guard(hl_var::in,hl_guard::in) is semidet.

is_stripper_guard(Var,ac_stripper(Var,_)).
is_stripper_guard(Var,is_type(Type,Var,_)) :-
    ( Type = int
    ; Type = float
    ; Type = string
    ; Type = var
    ).

%---------------------------------------------------------------------------%

    % This really isn't an error, rather a limitation of the implementation.
    % However it is useful to treat it as an error so (hopefully) the user
    % can address the issue at its source.
    %
:- pred more_than_one_collector_error(hl_symbol::in,list(hl_model)::in,
    var_name_info::in,cxt::in,io::di,io::uo) is det.

more_than_one_collector_error(Sym,Colls,Names,Cxt,!IO) :-
    Name = hl_symbol_name(Sym),
    hl_vars_to_string(Names,Colls,CollsStr),
    Error = message(
    "more than one possible collector %s for AC operator (%s)",
        [s(CollsStr),s(Name)],Cxt),
    compiler_error(Error,!IO).

%---------------------------------------------------------------------------%

:- pred hl_vars_to_string(var_name_info::in,list(hl_model)::in,string::out) 
    is det.

hl_vars_to_string(_,[],"").
hl_vars_to_string(Names,[HVar|HVars],String) :-
    hl_vars_to_string(Names,HVars,String0),
    ( HVar = var(MVar,_) ->
        ( search(Names,MVar,Name0) ->
            Name = Name0
        ;   Name = "_"
        ),
        ( HVars = [] ->
            String = Name
        ;   String = Name ++ "," ++ String0
        )
    ;   unexpected($file, $pred, "expected variable")
    ).

%---------------------------------------------------------------------------%

:- pred implicit_collector_error(hl_symbol::in,hl_var::in,var_name_info::in,
    cxt::in,io::di,io::uo) is det.

implicit_collector_error(Sym,Var,Names,Cxt,!IO) :-
    lookup(Names,Var,CName),
    Name = hl_symbol_name(Sym),
    Error = message(
        "collector %s conflicts with the implicit collector for top-level" ++
        " AC operator (%s)",
        [s(CName),s(Name)],Cxt),
    compiler_error(Error,!IO).

%---------------------------------------------------------------------------%

    % Again not really an error, rather a limitation.  Terms of the form
    % '+'(X) where '+' is AC are considered (by the implementation) to be 
    % equivalent to X, thus it makes no sense to have such terms in a rule 
    % head.
:- pred singleton_ac_error(hl_symbol::in,cxt::in,io::di,io::uo) is det.

singleton_ac_error(Sym,Cxt,!IO) :-
    Name = hl_symbol_name(Sym),
    Error = message(
        "singleton AC expressions of the form '%s'(_) are not allowed",
        [s(Name)],Cxt),
    compiler_error(Error,!IO).

%---------------------------------------------------------------------------%
:- end_module hl_ac_analysis.
%---------------------------------------------------------------------------%
