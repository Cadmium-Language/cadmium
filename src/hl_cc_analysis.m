%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Decides events/wakeup conditions.
%
%---------------------------------------------------------------------------%

:- module hl_cc_analysis.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module cadmium_common.
:- import_module hl_prog.
:- import_module hl_var_analysis.

:- type cc_info.

    %
    %
:- pred hl_cc_analysis(var_info::in,hl_prog::in,cc_info::out,io::di,io::uo) 
    is det.

    %
    %
:- pred lookup_creates(cc_info::in,hl_symbol::in,list(hl_symbol)::out) is det.

    %
    %
:- pred lookup_need_redo(cc_info::in,pos::in,bool::out) is det.

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- import_module map.
:- import_module maybe.
:- import_module set.

:- type cc_info
    ---> cc_info(create_info,redo_info).

:- type create_info == map(hl_symbol,set(hl_symbol)).
:- type redo_info == map(pos,bool).

%---------------------------------------------------------------------------%

lookup_creates(CCInfo,Sym,SymLs) :-
    CCInfo = cc_info(CreateInfo,_),
    ( search(CreateInfo,Sym,SymSet) ->
        to_sorted_list(SymSet,SymLs)
    ;   SymLs = []
    ).

%---------------------------------------------------------------------------%

lookup_need_redo(CCInfo,Pos,NeedRedo) :-
    CCInfo = cc_info(_,RedoInfo),
    ( search(RedoInfo,Pos,NeedRedo0) ->
        NeedRedo = NeedRedo0
    ;   NeedRedo = no
    ).

%---------------------------------------------------------------------------%

hl_cc_analysis(VarInfo,HLProg,CCInfo,!IO) :-
    CCInfo0 = cc_info(init,init),
    foldl2(hl_cc_analysis_on_rules(VarInfo),HLProg,CCInfo0,CCInfo,!IO).

%---------------------------------------------------------------------------%

:- pred hl_cc_analysis_on_rules(var_info::in,hl_symbol::in,list(hl_rule)::in,
    cc_info::in,cc_info::out,io::di,io::uo) is det.

hl_cc_analysis_on_rules(VarInfo,Sym,Rules,!CCInfo,!IO) :-
    foldl(generate_creates_on_rule,Rules,init,Creates),
    insert_creates(Sym,Creates,!CCInfo),
    foldl(generate_need_redo_on_rule(VarInfo),Rules,!CCInfo).

%---------------------------------------------------------------------------%

:- pred generate_need_redo_on_rule(var_info::in,hl_rule::in,cc_info::in,
    cc_info::out) is det.

generate_need_redo_on_rule(VarInfo,Rule,!CCInfo) :-
    Rule = hl_rule(_,MaybeCC,_,_,Body,_),
    ( MaybeCC = yes(CC),
        generate_need_redo_on_cc(VarInfo,get_pos(Body),CC,!CCInfo)
    ; MaybeCC = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred generate_need_redo_on_cc(var_info::in,pos::in,hl_model::in,cc_info::in,
    cc_info::out) is det.

generate_need_redo_on_cc(VarInfo,Pos,Model,!CCInfo) :-
    ( Model = var(Var,_),
        lookup_var_occs(VarInfo,Var,Occs),
        ( do_not_need_wakeup_on_redo_on_occs(Occs) ->
            true
        ;   insert_need_redo(Pos,yes,!CCInfo)
        )
    ; Model = functor(_,Args,_),
        foldl(generate_need_redo_on_cc(VarInfo,Pos),Args,!CCInfo)
    ; ( Model = int(_,_)
      ; Model = float(_,_)
      ; Model = string(_,_)
      ; Model = named_var(_,_) ),
        true
    ),
    MaybeAnnots = get_annotations(Model),
    ( MaybeAnnots = yes(Annots),
        generate_need_redo_on_cc(VarInfo,Pos,Annots,!CCInfo)
    ; MaybeAnnots = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred do_not_need_wakeup_on_redo_on_occs(list(pos_info)::in) is semidet.

do_not_need_wakeup_on_redo_on_occs([]).
do_not_need_wakeup_on_redo_on_occs([Occ|Occs]) :-
    ( Occ = cc(_),
        do_not_need_wakeup_on_redo_on_occs(Occs)
    ; Occ = head(_:pos),
        true
    ; Occ = guard(_,Kind),
        ( Kind = test,
            do_not_need_wakeup_on_redo_on_occs(Occs)
        ; Kind = pattern,
            fail
        )
    ; Occ = body(_),
        fail
    ).

%---------------------------------------------------------------------------%

:- pred generate_creates_on_rule(hl_rule::in,set(hl_symbol)::in,
    set(hl_symbol)::out) is det.

generate_creates_on_rule(Rule,!Creates) :-
    Rule = hl_rule(_,MaybeCC,_,Gds,_,_),
    ( MaybeCC = yes(CC),
        generate_creates_on_cc(Gds,CC,!Creates)
    ; MaybeCC = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred generate_creates_on_cc(list(hl_guard)::in,hl_model::in,
    set(hl_symbol)::in,set(hl_symbol)::out) is det.

generate_creates_on_cc(Gds,CC,!Wakeups) :-
    ( CC = functor(Sym,Args,_),
      hl_symbol_name(Sym) = conj_name ->
        list.foldl(generate_creates_on_cc(Gds),Args,!Wakeups)
    ;   get_sym_det(Gds,CC,Sym),
        set.insert(Sym,!Wakeups)
    ).

%---------------------------------------------------------------------------%

:- pred insert_creates(hl_symbol::in,set(hl_symbol)::in,cc_info::in,
    cc_info::out) is det.

insert_creates(Sym,Creates,!CCInfo) :-
    !.CCInfo = cc_info(CreateInfo,RedoInfo),
    map.det_insert(Sym,Creates,CreateInfo,NCreateInfo),
    !:CCInfo = cc_info(NCreateInfo,RedoInfo).

%---------------------------------------------------------------------------%

:- pred insert_need_redo(pos::in,bool::in,cc_info::in,cc_info::out) is det.

insert_need_redo(Pos,NeedRedo,!CCInfo) :-
    !.CCInfo = cc_info(CreateInfo,RedoInfo),
    map.set(Pos,NeedRedo,RedoInfo,NRedoInfo),
    !:CCInfo = cc_info(CreateInfo,NRedoInfo).

