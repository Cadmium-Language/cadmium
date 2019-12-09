%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Finds all occurrences of variables in the program.  A trivial analysis.
%
%---------------------------------------------------------------------------%

:- module hl_var_analysis.

:- interface.

:- import_module io.
:- import_module list.

:- import_module hl_prog.

:- type guard_kind
    ---> test
    ;    pattern.

:- type pos_info
    ---> cc(pos)
    ;    head(pos)
    ;    guard(pos,guard_kind)
    ;    body(pos).

:- type var_info.

    % Generates the var_info for the given hl_prog.
    %
:- pred hl_var_analysis(hl_prog::in,var_info::out,io::di,io::uo) is det.

    % Lookup the body occurrences for a given variable.
    %
:- pred lookup_var_occs(var_info::in,hl_var::in,list(pos_info)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module map.
:- import_module string.

:- import_module cadmium_common.
:- import_module cadmium_error.

%---------------------------------------------------------------------------%

:- type pos_kind
    ---> cc
    ;    head
    ;    guard
    ;    pattern_guard
    ;    body.

:- type var_info == map(hl_var,list(pos_info)).

%---------------------------------------------------------------------------%

lookup_var_occs(VarInfo,Var,Occs) :-
    ( search(VarInfo,Var,Occs0) ->
        Occs = Occs0
    ;   Occs = []
    ).

%---------------------------------------------------------------------------%

hl_var_analysis(HLProg,VarInfo,!IO) :-
    hl_prog_foldl2(hl_var_analysis_on_rule,HLProg,init,VarInfo0,!IO),
    map_values(do_reverse,VarInfo0,VarInfo).

%---------------------------------------------------------------------------%

:- pred hl_var_analysis_on_rule(hl_rule::in,var_info::in,var_info::out,
    io::di,io::uo) is det.

hl_var_analysis_on_rule(Rule,!VarInfo,!IO) :-
    Rule = hl_rule(_,MaybeCC,Hd,Gds,Bd,VarNameInfo),
    hl_var_analysis_on_rule_2(MaybeCC,Hd,Gds,Bd,init,LclVarInfo,!IO),
    Cxt = get_cxt(Hd),
    foldl(check_for_singleton_var(Cxt,VarNameInfo,Gds),LclVarInfo,!IO),
        % Note: do not use map.merge!
        % On some examples map.merge is significantly slower than the 
        % alternative here.  In map.m it seems map.merge uses to_assoc_list...
        %
    foldl(map.det_insert,LclVarInfo,!VarInfo).

%---------------------------------------------------------------------------%

:- pred hl_var_analysis_on_rule_2(maybe(hl_model)::in,hl_model::in,
    list(hl_guard)::in,hl_model::in,var_info::in,var_info::out,io::di,io::uo)
    is det.

hl_var_analysis_on_rule_2(MaybeCC,Hd,Gds,Bd,!VarInfo,!IO) :-
    generate_var_info(head,Hd,!VarInfo),
    ( MaybeCC = no,
        true
    ; MaybeCC = yes(CC),
        generate_var_info(cc,CC,!VarInfo)
    ),
    foldl(generate_var_info_on_guard,Gds,!VarInfo),
    generate_var_info(body,Bd,!VarInfo).

%---------------------------------------------------------------------------%

:- pred check_for_singleton_var(cxt::in,var_name_info::in,list(hl_guard)::in,
    hl_var::in,list(pos_info)::in,io::di,io::uo) is det.

check_for_singleton_var(Cxt,VarNameInfo,Gds,MVar,Poss,!IO) :-
    ( Poss = [_],
      search(VarNameInfo,MVar,Name) ->
        ( index(Name,0,'_') ->
            true
        ; not all_false(guard_contains_var(MVar),Gds) ->
            true
        ;   Message = message("variable `%s' is singleton in this rule",
                [s(Name)],Cxt),
            compiler_warning(Message,!IO)
        )
    ;   true
    ).

%---------------------------------------------------------------------------%

    % The guard `term(X)' does not register an occ in the var_info, however
    % it should suppress a singleton variable warning for X.
    %
:- pred guard_contains_var(hl_var::in,hl_guard::in) is semidet.

guard_contains_var(Var,ac_stripper(Var,_)).

%---------------------------------------------------------------------------%

:- pred do_reverse(T::in,list(U)::in,list(U)::out) is det.

do_reverse(_,Xs,Ys) :-
    reverse(Xs,Ys).

%---------------------------------------------------------------------------%

:- pred generate_var_info(pos_kind::in,hl_model::in,var_info::in,
    var_info::out) is det.

generate_var_info(Kind,Model,!VarInfo) :-
    ( Model = var(Var,_),
        Pos = get_pos(Model),
        generate_var_info_on_var(Kind,Pos,Var,!VarInfo)
    ; Model = functor(_,Args,_),
        foldl(generate_var_info(Kind),Args,!VarInfo)
    ; ( Model = int(_,_)
      ; Model = float(_,_)
      ; Model = string(_,_)
      ; Model = named_var(_,_) ),
        true
    ),
    generate_var_info_on_annotations(Kind,Model,!VarInfo),
    generate_var_info_on_at_var(Kind,Model,!VarInfo).

%---------------------------------------------------------------------------%

:- pred generate_var_info_on_annotations(pos_kind::in,hl_model::in,
    var_info::in,var_info::out) is det.

generate_var_info_on_annotations(Kind,Model,!VarInfo) :-
    Annots = get_annotations(Model),
    fold_maybe(generate_var_info(Kind),Annots,!VarInfo).

%---------------------------------------------------------------------------%

:- pred generate_var_info_on_at_var(pos_kind::in,hl_model::in,
    var_info::in,var_info::out) is det.

generate_var_info_on_at_var(Kind,Model,!VarInfo) :-
    AtVar = get_at_var(Model),
    Pos = get_pos(Model),
    fold_maybe(generate_var_info_on_var(Kind,Pos),AtVar,!VarInfo).

%---------------------------------------------------------------------------%

:- pred generate_var_info_on_guard(hl_guard::in,var_info::in,var_info::out) 
    is det.

generate_var_info_on_guard(user_guard(Gd),!VarInfo) :-
    generate_var_info(guard,Gd,!VarInfo).
generate_var_info_on_guard(match_guard(Hd,Gd),!VarInfo) :-
    generate_var_info(head,Hd,!VarInfo),
    generate_var_info(pattern_guard,Gd,!VarInfo).
generate_var_info_on_guard(is_type(_,Var,Attr),!VarInfo) :-
    Pos = hl_attr_get_pos(Attr),
    generate_var_info_on_var(guard,Pos,Var,!VarInfo).
generate_var_info_on_guard(ac_stripper(_,_),!VarInfo).

%---------------------------------------------------------------------------%

:- pred generate_var_info_on_var(pos_kind::in,pos::in,hl_var::in,var_info::in,
    var_info::out) is det.

generate_var_info_on_var(Kind,Pos,Var,!VarInfo) :-
    ( Kind = cc,
        PosInfo = cc(Pos)
    ; Kind = head,
        PosInfo = head(Pos)
    ; Kind = guard,
        PosInfo = guard(Pos,test)
    ; Kind = pattern_guard,
        PosInfo = guard(Pos,pattern)
    ; Kind = body,
        PosInfo = body(Pos)
    ),
    ( search(!.VarInfo,Var,Occs0) ->
        Occs1 = [PosInfo|Occs0],
        map.set(Var,Occs1,!VarInfo)
    ;   map.set(Var,[PosInfo],!VarInfo)
    ).

