%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck <gjd@cs.mu.oz.au>
%
% Defines a high level representation of the (currently loaded) rules.
%
%---------------------------------------------------------------------------%

:- module hl_prog.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module term.
:- import_module varset.

:- import_module cadmium_common.

:- type pos == int.

:- type hl_pragma
    ---> pragma_foreign(hl_symbol,string,pragma_foreign_flag)
    ;    pragma_doc(hl_symbol,list(string),string,cxt).

:- type pragma_foreign_flag
    ---> normalised
    ;    unnormalised
    ;    almost_normalised.

:- type hl_inline_pragma
    ---> pragma_ac_stripper.

:- type hl_attr 
    ---> hl_attr(pos,cxt,maybe(hl_model),maybe(hl_var),set(hl_inline_pragma)).

:- type hl_var.

:- type hl_model
    ---> int(int,hl_attr)
    ;    float(float,hl_attr)
    ;    string(string,hl_attr)
    ;    var(hl_var,hl_attr)
    ;    named_var(string,hl_attr)
    ;    functor(hl_symbol,list(hl_model),hl_attr).

:- type rule_id == int.

:- type hl_guard 
    ---> user_guard(hl_model)
    ;    match_guard(hl_model,hl_model)
    ;    is_type(hl_model_type,hl_var,hl_attr)
    ;    ac_stripper(hl_var,hl_attr).

    % High-level representation of a rule:
    %   hl_rule(RuleID,MaybeCC,Head,Guards,Body,VarNames)
    %
:- type hl_rule
    ---> hl_rule(rule_id,maybe(hl_model),hl_model,list(hl_guard),hl_model,
            var_name_info).

:- type hl_prog == map(hl_symbol,list(hl_rule)).

:- type var_hl_var_map == map(var,hl_var).

:- type var_name_info == map(hl_var,string).

    % Create a new (unique) hl_var.
    %
:- impure func new_hl_var = hl_var.

    % Pure version of the function above.
    %
:- pred new_hl_var(hl_var::out,io::di,io::uo) is det.

    % Convert a list of `hl_rule' into a `hl_prog'.
    %
:- pred hl_rules_to_hl_prog(list(hl_rule)::in,hl_prog::in,hl_prog::out,
    io::di,io::uo) is det.

    % Convert a `hl_model' to another `hl_model' w.r.t. the new positions.
    % 
:- pred hl_model_update_pos(hl_model::in,hl_model::out,pos::in,pos::out) is det.

    % Construct a high-level model.  Note: uses `dummy_pos'.
    %
:- func hl_model_construct(string,list(hl_model),pos,cxt) = hl_model.

    % Convert a Mercury library `term' into a `hl_model'.
    %
:- pred term_to_hl_model(bool::in,varset::in,term::in,hl_model::out,
    var_hl_var_map::in,var_hl_var_map::out,io::di,io::uo) is det.

    % Convert a Mercury library `term' into a `hl_pragma'.
    %
:- pred term_to_hl_pragma(term::in,varset::in,hl_pragma::out,io::di,io::uo) 
    is det.

    % Convert a `hl_model' into a list of `hl_guard'.
    %
:- pred hl_model2hl_guard(var_name_info::in,hl_model::in,list(hl_guard)::out)
    is det.

    % Search for the subterm at position P, fail if there is no such subterm.
    %
:- pred search(hl_model::in,pos::in,hl_model::out) is semidet.

    % Like search, but abort if there is no such subterm.
    %
:- pred lookup(hl_model::in,pos::in,hl_model::out) is det.

    % Tests if the given term contains the given position.
    %
:- pred contains(hl_model::in,pos::in) is semidet.

    % Tests if the given terms are unifiable.  May return false positives.
    %
:- pred is_unifiable(hl_model::in,hl_model::in) is semidet.

    % Get the top-most position in a hl_model.
    %
:- func get_pos(hl_model) = pos.

    % Get the top-most context in a hl_model.
    %
:- func get_cxt(hl_model) = cxt.

    % Get the top-most annotations of a hl_model.
    %
:- func get_annotations(hl_model) = maybe(hl_model).

    % Set the top-most annotations of a hl_model.
    %
:- func set_annotations(hl_model,hl_model) = hl_model.

    % Get the at-variable of a hl_model.
    %
:- func get_at_var(hl_model) = maybe(hl_var).

    % Get the top-most symbol of a hl_model.
    %
:- func get_sym(hl_model) = hl_symbol.

    % Get the top-most symbol of a hl_model.  Also handles constrained
    % variables.  Fails if the model is an unconstrainted variable or a
    % higher-order term (thus the symbol is not known).
    %
:- pred get_sym(list(hl_guard)::in,hl_model::in,hl_symbol::out) is semidet.

    % Like get_sym/3 but throws an exception rather than failing.
    %
:- pred get_sym_det(list(hl_guard)::in,hl_model::in,hl_symbol::out) is det.

    % Annotate a hl_model.
    %
:- func hl_annotate(hl_model,hl_model) = hl_model.

    % Merge two hl_model into an AC hl_model.
    %
:- func hl_ac_merge(string,hl_model,hl_model) = hl_model.

    % Create a new (empty) hl_attr.
    %
:- func hl_attr_init = hl_attr.

    % Get the pos from a hl_attr.
    %
:- func hl_attr_get_pos(hl_attr) = pos.

    % Get the cxt from a hl_attr.
    %
:- func hl_attr_get_cxt(hl_attr) = cxt.

    % Get the annotations from a hl_attr.
    %
:- func hl_attr_get_annotations(hl_attr) = maybe(hl_model).

    % Get the @-var from a hl_attr.
    %
:- func hl_attr_get_at_var(hl_attr) = maybe(hl_var).

    % Get the set of pragmas from a hl_attr.
    %
:- func hl_attr_get_pragmas(hl_attr) = set(hl_inline_pragma).

    % A pos that (should) never appear in a hl_model.
    %
:- func dummy_pos = pos.

    % Folds an accumulator over a hl_prog.
    %
:- pred hl_prog_foldl(pred(hl_rule,T,T),hl_prog,T,T).
:- mode hl_prog_foldl(pred(in,in,out) is det,in,in,out) is det.

    % Folds two accumulatora over a hl_prog.
    %
:- pred hl_prog_foldl2(pred(hl_rule,T,T,U,U),hl_prog,T,T,U,U).
:- mode hl_prog_foldl2(pred(in,in,out,di,uo) is det,in,in,out,di,uo) is det.

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- import_module int.
:- import_module require.
:- import_module string.

:- import_module cadmium_error.

:- type hl_var 
    ---> hl_var(int).

%---------------------------------------------------------------------------%

    % The next available hl_var.
    %
:- mutable(next_hl_var,int,0,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

new_hl_var = HLVar :-
    semipure get_next_hl_var(C),
    HLVar = hl_var(C),
    impure set_next_hl_var(C+1).

%---------------------------------------------------------------------------%

new_hl_var(HLVar,!IO) :-
    get_next_hl_var(C,!IO),
    HLVar = hl_var(C),
    set_next_hl_var(C+1,!IO).

%---------------------------------------------------------------------------%

hl_rules_to_hl_prog(Rules,!Prog,!IO) :-
    foldr3(hl_rule_to_hl_prog,Rules,1,_,!Prog,!IO).

%---------------------------------------------------------------------------%

:- pred hl_rule_to_hl_prog(hl_rule::in,pos::in,pos::out,
    hl_prog::in,hl_prog::out,io::di,io::uo) is det.

hl_rule_to_hl_prog(Rule0,!Pos,!Prog,!IO) :-
    Rule0 = hl_rule(RId,MaybeCC0,Hd0,Gds0,Bd0,VarNames),
    map_fold_maybe(hl_model_update_pos,MaybeCC0,MaybeCC,!Pos),
    hl_model_update_pos(Hd0,Hd,!Pos),
    map_foldl(hl_guard_update_pos,Gds0,Gds,!Pos),
    hl_model_update_pos(Bd0,Bd,!Pos),
    Rule = hl_rule(RId,MaybeCC,Hd,Gds,Bd,VarNames),
    hl_get_head_symbol(Hd,Gds,YNSym,!IO),
    ( YNSym = yes(Sym),
        check_for_bad_procedure_error(Sym,get_cxt(Hd),!IO),
        ( search(!.Prog,Sym,Rs0) ->
            Rs1 = [Rule|Rs0]
        ;   Rs1 = [Rule]
        ),
        map.set(Sym,Rs1,!Prog)
    ; YNSym = no,
        true
    ).

%---------------------------------------------------------------------------%

:- pred hl_get_head_symbol(hl_model::in,list(hl_guard)::in,
    maybe(hl_symbol)::out,io::di,io::uo) is det.

hl_get_head_symbol(Hd,Gd,YNSym,!IO) :-
    ( Hd = functor(Sym0,_,_) ->
        Name = hl_symbol_name(Sym0),
        ( is_ac(Name) ->
            Sym1 = hl_symbol(Name,0),
            YNSym = yes(Sym1)
        ;   YNSym = yes(Sym0)
        )
    ; Hd = var(Var,_),
      not all_false(constrained_to_be_var(Var),Gd) ->
        Sym = hl_symbol(var_name,0),
        YNSym = yes(Sym)
    ;   compiler_error(hl_model_to_head_error(Hd),!IO),
        YNSym = no
    ).

%---------------------------------------------------------------------------%

:- func hl_model_to_head_error(hl_model) = message.

hl_model_to_head_error(int(Int,Attr)) = message(
    "rule head cannot be an integer, found %d",[i(Int)],hl_attr_get_cxt(Attr)).
hl_model_to_head_error(float(Flt,Attr)) = message(
    "rule head cannot be a float, found %f",[f(Flt)],hl_attr_get_cxt(Attr)).
hl_model_to_head_error(string(Str,Attr)) = message(
    "rule head cannot be a string, found ""%s""",[s(Str)],
        hl_attr_get_cxt(Attr)).
hl_model_to_head_error(var(_,Attr)) = message(
    "rule head cannot be a variable in absence of is_var/1 guard",[],
        hl_attr_get_cxt(Attr)).
hl_model_to_head_error(named_var(Name,Attr)) = message(
    "rule head cannot be a named variable, found %s",[s(Name)],
        hl_attr_get_cxt(Attr)).
hl_model_to_head_error(functor(_,_,_)) = _ :-
    unexpected($file, $pred, "unexpected functor").

%---------------------------------------------------------------------------%

    % Some builtins use the following functors and assume they are not 
    % reduced.  We enforce that they are not reduced here.  The functors are:
    % []/0, [|]/2, true/0, false/0, '$cc'/2.
    %
:- pred check_for_bad_procedure_error(hl_symbol::in,cxt::in,io::di,io::uo) 
    is det.

check_for_bad_procedure_error(Sym,Cxt,!IO) :-
    Name = hl_symbol_name(Sym),
    Aty = hl_symbol_arity(Sym),
    ( Name = nil_name,
      Aty = 0 ->
        throw_bad_procedure_error(Sym,Cxt,!IO)
    ; Name = cons_name,
      Aty = 2 ->
        throw_bad_procedure_error(Sym,Cxt,!IO)
    ; Name = true_name,
      Aty = 0 ->
        throw_bad_procedure_error(Sym,Cxt,!IO)
    ; Name = false_name,
      Aty = 0 ->
        throw_bad_procedure_error(Sym,Cxt,!IO)
    ; Name = cc_name,
      Aty = 0 ->
        throw_bad_procedure_error(Sym,Cxt,!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred throw_bad_procedure_error(hl_symbol::in,cxt::in,io::di,io::uo) is det.

throw_bad_procedure_error(Sym,Cxt,!IO) :-
    Error = message(
        "rule head cannot have reserved top-level functor %s/%d",
        [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],Cxt),
    compiler_error(Error,!IO).

%---------------------------------------------------------------------------%

:- pred constrained_to_be_var(hl_var::in,hl_guard::in) is semidet.

constrained_to_be_var(Var,is_type(var,Var,_)).

%---------------------------------------------------------------------------%

hl_model_update_pos(!Model,!Pos) :-
    some [!Attr] (
        !:Attr = get_hl_attr(!.Model),
        MaybeAnnots = hl_attr_get_annotations(!.Attr),
        ( MaybeAnnots = yes(Annots),
            hl_model_update_pos(Annots,NAnnots,!Pos),
            !:Attr = hl_attr_set_annotations(!.Attr,NAnnots)
        ; MaybeAnnots = no,
            true
        ),
        !:Attr = hl_attr_set_pos(!.Attr,!.Pos),
        !:Pos = !.Pos + 1,
        ( !.Model = functor(Sym,Args,_) ->
            map_foldl(hl_model_update_pos,Args,NArgs,!Pos),
            !:Model = functor(Sym,NArgs,!.Attr)
        ;   !:Model = set_hl_attr(!.Model,!.Attr)
        )
    ).

%---------------------------------------------------------------------------%

:- pred hl_guard_update_pos(hl_guard::in,hl_guard::out,pos::in,pos::out)
    is det.

hl_guard_update_pos(user_guard(Model0),Guard,!Pos) :-
    hl_model_update_pos(Model0,Model1,!Pos),
    Guard = user_guard(Model1).
hl_guard_update_pos(match_guard(Model0,Model1),Guard,!Pos) :-
    hl_model_update_pos(Model0,Model2,!Pos),
    hl_model_update_pos(Model1,Model3,!Pos),
    Guard = match_guard(Model2,Model3).
hl_guard_update_pos(is_type(Type,Var,Attr0),Guard,!Pos) :-
    Attr0 = hl_attr(_,Cxt,Annots,AtVar,Pragmas),
    Attr1 = hl_attr(!.Pos,Cxt,Annots,AtVar,Pragmas),
    Guard = is_type(Type,Var,Attr1),
    !:Pos = !.Pos + 1.
hl_guard_update_pos(ac_stripper(Var,Attr0),Guard,!Pos) :-
    Attr0 = hl_attr(_,Cxt,Annots,AtVar,Pragmas),
    Attr1 = hl_attr(!.Pos,Cxt,Annots,AtVar,Pragmas),
    Guard = ac_stripper(Var,Attr1),
    !:Pos = !.Pos + 1.

%---------------------------------------------------------------------------%

hl_model_construct(Fct,Args,Pos,Cxt) = HLModel :-
    length(Args,Aty),
    Sym = hl_symbol(Fct,Aty),
    ( is_ac(Fct) ->
        foldl(flatten_ac_arg(Sym),Args,[],NArgs),
        length(NArgs,NAty),
        NSym = hl_symbol(Fct,NAty)
    ;   NArgs = Args,
        NSym = Sym
    ),
    Attr = hl_attr(Pos,Cxt,no,no,init),
    HLModel = functor(NSym,NArgs,Attr).

%---------------------------------------------------------------------------%

:- pred flatten_ac_arg(hl_symbol::in,hl_model::in,list(hl_model)::in,
    list(hl_model)::out) is det.

flatten_ac_arg(_,Cnst,Args,[Cnst|Args]) :-
    ( Cnst = int(_,_)
    ; Cnst = float(_,_)
    ; Cnst = string(_,_)
    ; Cnst = named_var(_,_)
    ; Cnst = var(_,_)
    ).
flatten_ac_arg(Sym,Model,!Args) :-
    Model = functor(FSym,FArgs,_),
    ( hl_symbol_name(Sym) = hl_symbol_name(FSym) ->
        append(FArgs,!Args)
    ;   !:Args = [Model|!.Args]
    ).

%---------------------------------------------------------------------------%

:- type t2m_state
    ---> t2m_state(bool,bool,bool).

:- pred is_top_level(t2m_state::in) is semidet.
:- pred is_matching(t2m_state::in) is semidet.
:- pred is_annotatable(t2m_state::in) is semidet.

is_top_level(t2m_state(yes,_,_)).
is_matching(t2m_state(_,yes,_)).
is_annotatable(t2m_state(_,_,yes)).

:- func set_top_level(bool,t2m_state) = t2m_state.
:- func set_matching(bool,t2m_state) = t2m_state.
:- func set_annotatable(bool,t2m_state) = t2m_state.

set_top_level(IsTopLevel,t2m_state(_,B2,B3)) = 
    t2m_state(IsTopLevel,B2,B3).
set_matching(IsMatching,t2m_state(B1,_,B3)) =
    t2m_state(B1,IsMatching,B3).
set_annotatable(IsAnnotatable,t2m_state(B1,B2,_)) =
    t2m_state(B1,B2,IsAnnotatable).

%---------------------------------------------------------------------------%

term_to_hl_pragma(Term,VarSet,Pragma,!IO) :-
    ( Term = functor(atom(Atom),Args,Cxt0) ->
        Cxt = context_to_cxt(Cxt0),
        ( Atom = "foreign",
          Args = [SymTerm,CNameTerm,FlagTerm] ->
            ( SymTerm = functor(atom("/"),[NameTerm,AtyTerm],_),
              NameTerm = functor(atom(Name),[],_),
              term_to_int(AtyTerm, Aty) ->
                ( CNameTerm = functor(string(CName),[],_) ->
                    ( FlagTerm = functor(atom(FlagName),[],_),
                      string_to_pragma_foreign_flag(FlagName,Flag) ->
                        Sym = hl_symbol(Name,Aty),
                        Pragma = pragma_foreign(Sym,CName,Flag)
                    ;   Message = message("expected a flag " ++
                        "(normalised, unnormalised, or almost_normalised) " ++
                        "as third argument to pragma foreign",[],Cxt),
                        throw_compiler_error(Message,!IO)
                    )
                ;   Message = message("expected a string (C function name) " ++
                        "as the second argument to pragma foreign",[],Cxt),
                    throw_compiler_error(Message,!IO)
                )
            ;   Message = message("expected a name/arity pair as the first " ++
                        "argument to pragma foreign",[],Cxt),
                throw_compiler_error(Message,!IO)
            )
        ; Atom = "doc",
          Args = [HeadTerm,TextTerm] ->
            ( HeadTerm = functor(atom(Name),ArgTerms,_),
              map(get_variable_name(VarSet),ArgTerms,Params) ->
                ( TextTerm = functor(string(Text),[],_) ->
                    length(Params,Aty),
                    Sym = hl_symbol(Name,Aty),
                    Pragma = pragma_doc(Sym,Params,Text,Cxt)
                ;   Message = message("expected text string as the second " ++
                        "argument to pragma doc",[],Cxt),
                    throw_compiler_error(Message,!IO)
                )
            ;   Message = message("expected term of form f(Var1,...,VarN) " ++
                    "as first argument to pragra doc",[],Cxt),
                throw_compiler_error(Message,!IO)
            )
        ;   Message = message("undefined pragma %s/%d",
                [s(Atom),i(length(Args))],Cxt),
            throw_compiler_error(Message,!IO)
        )
    ;   ( Term = variable(_,Cxt0),
            Cxt = context_to_cxt(Cxt0)
        ; Term = functor(_,_,Cxt0),
            Cxt = context_to_cxt(Cxt0)
        ),
        Message = message("expected a term after `pragma' keyword",[],Cxt),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred get_variable_name(varset::in,term::in,string::out) is semidet.

get_variable_name(VarSet,Term,Name) :-
    Term = variable(Var,_),
    search_name(VarSet,Var,Name).

%---------------------------------------------------------------------------%

:- pred string_to_pragma_foreign_flag(string::in,pragma_foreign_flag::out)
    is semidet.

string_to_pragma_foreign_flag("normalised",normalised).
string_to_pragma_foreign_flag("normalized",normalised).
string_to_pragma_foreign_flag("unnormalised",unnormalised).
string_to_pragma_foreign_flag("unnormalized",unnormalised).
string_to_pragma_foreign_flag("almost_normalised",almost_normalised).
string_to_pragma_foreign_flag("almost_normalized",almost_normalised).

%---------------------------------------------------------------------------%

term_to_hl_model(IsMatch,VarSet,Term,Model,!VarMap,!IO) :-
    IsTopLevel = yes,
    IsAnnotatable = yes,
    T2MState = t2m_state(IsTopLevel,IsMatch,IsAnnotatable),
    term_to_hl_model_2(T2MState,VarSet,Term,Model,!VarMap,!IO).

%---------------------------------------------------------------------------%

:- pred term_to_hl_model_2(t2m_state::in,varset::in,term::in,hl_model::out,
    var_hl_var_map::in,var_hl_var_map::out,io::di,io::uo) is det.

term_to_hl_model_2(_,_,variable(Var,Cxt0),Model,!VarMap,!IO) :-
    Cxt = context_to_cxt(Cxt0),
    ( search(!.VarMap,Var,MVar0) ->
        MVar = MVar0
    ;   new_hl_var(MVar,!IO),
        map.set(Var,MVar,!VarMap)
    ),
    Attr = hl_attr(unset_pos,Cxt,no,no,init),
    Model = var(MVar,Attr).
term_to_hl_model_2(T2MState,VarSet,Functor,Model,!VarMap,!IO) :-
    Functor = functor(Cnst,Args,Cxt0),
    Cxt = context_to_cxt(Cxt0),
    Attr = hl_attr(unset_pos,Cxt,no,no,init),
    NT2MState = set_top_level(no,T2MState),
    ( Cnst = integer(_, _, _, _),
        ( if term_to_int(Functor, Int) then
             Model = int(Int,Attr)
        else
              unexpected($file, $pred, "unsupported integer type")
        )
    ; Cnst = float(Flt),
        Model = float(Flt,Attr)
    ; Cnst = string(Str),
        Model = string(Str,Attr)
    ; Cnst = implementation_defined(_),
        unexpected($file, $pred, "implementation_defined/1 constant")
    ; Cnst = atom(Name0),
        ( Name0 = "" ->
            Name = "call"
        ;   Name = Name0
        ),
        ( Name = "call",
          is_matching(T2MState) ->
            higher_order_matching_error(Cxt,!IO)
        ;   true
        ),
        ( Name = annotate_name,
          Args = [Arg1,Arg2] ->
            ( is_annotatable(NT2MState) ->
                NNT2MState = set_annotatable(no,NT2MState),
                term_to_flattened_ac_arg(Cnst,NNT2MState,VarSet,Arg2,[],
                    HLArgs,!VarMap,!IO),
                length(HLArgs,RealAty),
                Sym = hl_symbol(Name,RealAty),
                Annots = functor(Sym,HLArgs,Attr),
                term_to_hl_model_2(NT2MState,VarSet,Arg1,Model0,!VarMap,!IO),
                Model = hl_annotate(Model0,Annots)
            ;   annotated_annotation_error(Cxt,!IO),
                term_to_hl_model_2(NT2MState,VarSet,Arg2,Model,!VarMap,!IO)
            )
        ; Name = "$::" ->
            NNT2MState = set_annotatable(no,NT2MState),
            foldl3(term_to_flattened_ac_arg(Cnst,NNT2MState,VarSet),Args,[],
                HLArgs,!VarMap,!IO),
            length(HLArgs,RealAty),
            Sym = hl_symbol(annotate_name,RealAty),
            Model = functor(Sym,HLArgs,Attr)
        ; Name = ":=",
          Args = [Arg1,Arg2] ->
            NNT2MState = set_matching(yes,NT2MState),
            term_to_hl_model_2(NNT2MState,VarSet,Arg1,HLArg1,!VarMap,!IO),
            term_to_hl_model_2(NT2MState,VarSet,Arg2,HLArg2,!VarMap,!IO),
            Sym = hl_symbol(Name,2),
            Model = functor(Sym,[HLArg1,HLArg2],Attr)
        ; is_ac(Name) ->
            foldl3(term_to_flattened_ac_arg(Cnst,NT2MState,VarSet),Args,[],
                HLArgs,!VarMap,!IO),
            length(HLArgs,RealAty),
            Sym = hl_symbol(Name,RealAty),
            Model = functor(Sym,HLArgs,Attr)
        ; Name = "^",
          Args = [Arg],
          Arg = variable(Var,_),
          search_name(VarSet,Var,VarName) ->
            Model = named_var(VarName,Attr)
        ; Name = "@",
          Args = [Arg1,Arg2] ->
            ( not is_matching(T2MState) ->
                non_match_at_var_error(Cxt,!IO),
                term_to_hl_model_2(NT2MState,VarSet,Arg2,Model,!VarMap,!IO)
            ; is_top_level(T2MState) ->
                top_level_at_var_error(Cxt,!IO),
                term_to_hl_model_2(NT2MState,VarSet,Arg2,Model,!VarMap,!IO)
            ; Arg1 = variable(Var,_) ->
                ( search(!.VarMap,Var,MVar0) ->
                    MVar = MVar0
                ;   new_hl_var(MVar,!IO),
                    map.set(Var,MVar,!VarMap)
                ),
                term_to_hl_model_2(NT2MState,VarSet,Arg2,Model0,!VarMap,!IO),
                ( get_at_var(Model0) = yes(_) ->
                    double_at_var_error(Cxt,!IO)
                ;   true
                ),
                Model = set_at_var(Model0,MVar)
            ;   lhs_of_at_var_not_variable_error(Cxt,!IO),
                term_to_hl_model_2(NT2MState,VarSet,Arg2,Model,!VarMap,!IO)
            )
        ;   ( ( Name = "<=>"
              ; Name = "\\"
              ; Name = "|" ) ->
                Message = message("invalid functor name '%s' conflicts " ++
                    "with rule syntax",[s(Name)],Cxt),
                compiler_error(Message,!IO)
            ;   true
            ),
            map_foldl2(term_to_hl_model_2(NT2MState,VarSet),Args,HLArgs,
                !VarMap,!IO),
            Aty = length(Args),
            Sym = hl_symbol(Name,Aty),
            Model = functor(Sym,HLArgs,Attr)
        )
    ).

%---------------------------------------------------------------------------%

hl_annotate(Model0,Annots) = Model1 :-
    MaybeAnnots0 = get_annotations(Model0),
    ( MaybeAnnots0 = yes(Annots0),
        Annots1 = hl_ac_merge(annotate_name,Annots0,Annots)
    ; MaybeAnnots0 = no,
        Annots1 = Annots
    ),
    Model1 = set_annotations(Model0,Annots1).

%---------------------------------------------------------------------------%

hl_ac_merge(Name,Model0,Model1) = Model2 :-
    ( Model0 = functor(Sym0,Args0,_),
      Name = hl_symbol_name(Sym0) ->
        ( Model1 = functor(Sym1,Args1,_),
          Name = hl_symbol_name(Sym1) ->
            append(Args0,Args1,Args2),
            Aty2 = hl_symbol_arity(Sym0) + hl_symbol_arity(Sym1)
        ;   Args2 = [Model1|Args0],
            Aty2 = hl_symbol_arity(Sym0) + 1
        )
    ; Model1 = functor(Sym1,Args1,_),
      Name = hl_symbol_name(Sym1) ->
        Args2 = [Model0|Args1],
        Aty2 = hl_symbol_arity(Sym1) + 1
    ;   Args2 = [Model0,Model1],
        Aty2 = 2
    ),
    Sym2 = hl_symbol(Name,Aty2),
    Model2 = functor(Sym2,Args2,get_hl_attr(Model0)).

%---------------------------------------------------------------------------%

:- pred annotated_annotation_error(cxt::in,io::di,io::uo) is det.

annotated_annotation_error(Cxt,!IO) :-
    Message = message("annotations must not be annotated",[],Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred double_at_var_error(cxt::in,io::di,io::uo) is det.

double_at_var_error(Cxt,!IO) :-
    Message = message("only one @-match is allowed per term",[],Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred lhs_of_at_var_not_variable_error(cxt::in,io::di,io::uo) is det.

lhs_of_at_var_not_variable_error(Cxt,!IO) :-
    Message = message("left-hand-side of @-match must be a variable",[],Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred rhs_of_at_var_not_a_variable_error(cxt::in,io::di,io::uo) is det.

rhs_of_at_var_not_a_variable_error(Cxt,!IO) :-
    Message = message("right-hand-side of @-match must not be a variable",[],
        Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred top_level_at_var_error(cxt::in,io::di,io::uo) is det.

top_level_at_var_error(Cxt,!IO) :-
    Message = message("@-match cannot be top-level",[],Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred non_match_at_var_error(cxt::in,io::di,io::uo) is det.

non_match_at_var_error(Cxt,!IO) :-
    Message = message("@-match can only appear in matchings",[],Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred higher_order_matching_error(cxt::in,io::di,io::uo) is det.

higher_order_matching_error(Cxt,!IO) :-
    Message = message("higher-order terms cannot appear in matchings",[],Cxt),
    compiler_error(Message,!IO).

%---------------------------------------------------------------------------%

:- pred term_to_flattened_ac_arg(const::in,t2m_state::in,varset::in,
    term::in,list(hl_model)::in,list(hl_model)::out,var_hl_var_map::in,
    var_hl_var_map::out,io::di,io::uo) is det.

term_to_flattened_ac_arg(ACOp,T2MState,VarSet,Arg,!ACArgs,!VarMap,!IO) :-
    ( Arg = functor(ACOp,Args0,_) ->
        foldl3(term_to_flattened_ac_arg(ACOp,T2MState,VarSet),Args0,!ACArgs,
            !VarMap,!IO)
    ;   term_to_hl_model_2(T2MState,VarSet,Arg,HLArg,!VarMap,!IO),
        !:ACArgs = [HLArg|!.ACArgs]
    ).

%---------------------------------------------------------------------------%

hl_model2hl_guard(VNI,HLM,Gds) :-
    hl_model2hl_guard(VNI,HLM,[],Gds).

%---------------------------------------------------------------------------%

:- pred hl_model2hl_guard(var_name_info::in,hl_model::in,list(hl_guard)::in,
    list(hl_guard)::out) is det.

:- pragma promise_pure(hl_model2hl_guard/4).
hl_model2hl_guard(VarNames,Gd,!Gds) :-
    ( Gd = functor(Sym,Args,_) ->
        ( hl_symbol_name(Sym) = conj_name ->
            foldl(hl_model2hl_guard(VarNames),Args,!Gds)
        ; Args = [Arg] ->
            ( hl_symbol_arity(Sym) = 1,
              hl_symbol_name(Sym) = term_name ->
                ( Arg = var(Var,Attr) ->
                    !:Gds = [ac_stripper(Var,Attr)|!.Gds]
                ;   Message = message(
                        "argument for guard %s/1 must be a variable",
                        [s(term_name)],get_cxt(Gd)),
                    impure compiler_error(Message)
                )
            ; Arg = var(Var,Attr) ->
                ( hl_symbol_name(Sym) = is_int_name ->
                    !:Gds = [is_type(int,Var,Attr)|!.Gds]
                ; hl_symbol_name(Sym) = is_float_name ->
                    !:Gds = [is_type(float,Var,Attr)|!.Gds]
                ; hl_symbol_name(Sym) = is_string_name ->
                    !:Gds = [is_type(string,Var,Attr)|!.Gds]
                ; hl_symbol_name(Sym) = is_var_name ->
                    !:Gds = [is_type(var,Var,Attr)|!.Gds]
                ; hl_symbol_name(Sym) = is_functor_name ->
                    !:Gds = [is_type(functor,Var,Attr)|!.Gds]
                ;   !:Gds = [user_guard(Gd)|!.Gds]
                )
            ;   !:Gds = [user_guard(Gd)|!.Gds]
            )
        ; Args = [Arg1,Arg2],
          hl_symbol_name(Sym) = ":=" ->
            !:Gds = [match_guard(Arg1,Arg2)|!.Gds]
        ;   !:Gds = [user_guard(Gd)|!.Gds]
        )
    ;   Error = hl_model2guard_error(VarNames,Gd),
        impure compiler_error(Error)
    ).

%---------------------------------------------------------------------------%

:- func hl_model2guard_error(var_name_info,hl_model) = message.

hl_model2guard_error(_,int(Int,Attr)) = message(
    "rule guard cannot be an integer, found %d",[i(Int)],
        hl_attr_get_cxt(Attr)).
hl_model2guard_error(_,float(Flt,Attr)) = message(
    "rule guard cannot be a float, found %f",[f(Flt)],
        hl_attr_get_cxt(Attr)).
hl_model2guard_error(_,string(Str,Attr)) = message(
    "rule guard cannot be a string, found ""%s""",[s(Str)],
        hl_attr_get_cxt(Attr)).
hl_model2guard_error(VarNames,var(Var,Attr)) = Message :-
    ( search(VarNames,Var,Name0) ->
        Name = Name0
    ;   Name = "_"
    ),
    Message = message(
        "rule guard cannot be a variable, found %s",[s(Name)],
        hl_attr_get_cxt(Attr)).
hl_model2guard_error(_,named_var(Name,Attr)) = message(
    "rule guard cannot be a named variable, found %s",[s(Name)],
        hl_attr_get_cxt(Attr)).
hl_model2guard_error(_,functor(_,_,_)) = _ :-
    unexpected($file, $pred, "unexpected functor").

%---------------------------------------------------------------------------%

search(HLM,Pos,Res) :-
    Pos >= 0,
    search_2(HLM,Pos,Res).

%---------------------------------------------------------------------------%

:- pred search_2(hl_model::in,pos::in,hl_model::out) is semidet.

search_2(Var,Pos,Var) :-
    Var = var(_,Attr),
    Pos = hl_attr_get_pos(Attr).
search_2(Str,Pos,Str) :-
    Str = string(_,Attr),
    Pos = hl_attr_get_pos(Attr).
search_2(Int,Pos,Int) :-
    Int = int(_,Attr),
    Pos = hl_attr_get_pos(Attr).
search_2(Flt,Pos,Flt) :-
    Flt = float(_,Attr),
    Pos = hl_attr_get_pos(Attr).
search_2(NVar,Pos,NVar) :-
    NVar = named_var(_,Attr),
    Pos = hl_attr_get_pos(Attr).
search_2(Fct,Pos,Res) :-
    Fct = functor(_,Args,Attr),
    Pos0 = hl_attr_get_pos(Attr),
    ( Pos = Pos0 ->
        Res = Fct
    ;   Args = [Arg0|Args0],
        search_args(Arg0,Args0,Pos,Res)
    ).

%---------------------------------------------------------------------------%

:- pred search_args(hl_model::in,list(hl_model)::in,pos::in,hl_model::out)
     is semidet.

search_args(Arg0,[],Pos,Res) :-
    search_2(Arg0,Pos,Res).
search_args(Arg0,[Arg1|Args],Pos,Res) :-
    Pos0 = get_pos(Arg0),
    Pos1 = get_pos(Arg1),
    ( Pos >= Pos0,
      Pos < Pos1 ->
        search_2(Arg0,Pos,Res)
    ;   search_args(Arg1,Args,Pos,Res)
    ).
    
%---------------------------------------------------------------------------%

lookup(HLM,Pos,Res) :-
    ( search(HLM,Pos,Res0) ->
        Res = Res0
    ;   error("lookup: no such position " ++ int_to_string(Pos) ++ 
            " in term")
    ).

%---------------------------------------------------------------------------%

contains(HLM,Pos) :-
    search(HLM,Pos,_).

%---------------------------------------------------------------------------%

is_unifiable(int(Int,_),Model) :-
    ( Model = int(Int,_) ->
        true
    ;   Model = var(_,_)
    ).
is_unifiable(float(Flt,_),Model) :-
    ( Model = float(Flt,_) ->
        true
    ;   Model = var(_,_)
    ).
is_unifiable(string(Str,_),Model) :-
    ( Model = string(Str,_) ->
        true
    ;   Model = var(_,_)
    ).
is_unifiable(var(_,_),_).
is_unifiable(named_var(Name,_),Model) :-
    ( Model = named_var(Name,_) ->
        true
    ;   Model = var(_,_)
    ).
is_unifiable(functor(Sym1,Args1,_),Model) :-
    ( Model = functor(Sym2,Args2,_) ->
        ( Sym1 = Sym2 ->
            is_unifiable_args(Args1,Args2)
        ;   Name1 = hl_symbol_name(Sym1),
            Name2 = hl_symbol_name(Sym2),
            Name1 = Name2,
            is_ac(Name1)
        )
    ;   Model = var(_,_)
    ).

%---------------------------------------------------------------------------%

:- pred is_unifiable_args(list(hl_model)::in,list(hl_model)::in) is semidet.

is_unifiable_args([],_).
is_unifiable_args([Arg1|Args1],Args0) :-
    Arg2 = det_head(Args0),
    is_unifiable(Arg1,Arg2),
    Args2 = det_tail(Args0),
    is_unifiable_args(Args1,Args2).

%---------------------------------------------------------------------------%

:- func get_hl_attr(hl_model) = hl_attr.

get_hl_attr(int(_,Attr))       = Attr.
get_hl_attr(float(_,Attr))     = Attr.
get_hl_attr(string(_,Attr))    = Attr.
get_hl_attr(var(_,Attr))       = Attr.
get_hl_attr(named_var(_,Attr)) = Attr.
get_hl_attr(functor(_,_,Attr)) = Attr.

%---------------------------------------------------------------------------%

:- func set_hl_attr(hl_model,hl_attr) = hl_model.

set_hl_attr(int(Int,_),Attr)          = int(Int,Attr).
set_hl_attr(float(Flt,_),Attr)        = float(Flt,Attr).
set_hl_attr(string(Str,_),Attr)       = string(Str,Attr).
set_hl_attr(var(Var,_),Attr)          = var(Var,Attr).
set_hl_attr(named_var(Var,_),Attr)    = named_var(Var,Attr).
set_hl_attr(functor(Sym,Args,_),Attr) = functor(Sym,Args,Attr).

%---------------------------------------------------------------------------%

get_pos(Model) = hl_attr_get_pos(get_hl_attr(Model)).

%---------------------------------------------------------------------------%

get_cxt(Model) = hl_attr_get_cxt(get_hl_attr(Model)).

%---------------------------------------------------------------------------%

get_annotations(Model) = hl_attr_get_annotations(get_hl_attr(Model)).

%---------------------------------------------------------------------------%

get_at_var(Model) = hl_attr_get_at_var(get_hl_attr(Model)).

%---------------------------------------------------------------------------%

get_sym(int(_,_))         = hl_symbol(int_name,0).
get_sym(float(_,_))       = hl_symbol(float_name,0).
get_sym(string(_,_))      = hl_symbol(string_name,0).
get_sym(var(_,_))         = hl_symbol(var_name,0).
get_sym(named_var(_,_))   = hl_symbol(var_name,0).
get_sym(functor(Sym,_,_)) = Sym.

%---------------------------------------------------------------------------%

get_sym(_,int(_,_),Sym) :-
    Sym = hl_symbol(int_name,0).
get_sym(_,float(_,_),Sym) :-
    Sym = hl_symbol(float_name,0).
get_sym(_,string(_,_),Sym) :-
    Sym = hl_symbol(string_name,0).
get_sym(Gds,var(Var,_),Sym) :-
    get_sym_from_guards(Gds,Var,Sym).
get_sym(_,named_var(_,_),Sym) :-
    Sym = hl_symbol(var_name,0).
get_sym(_,functor(Sym,_,_),Sym) :-
    hl_symbol_name(Sym) \= call_name.

%---------------------------------------------------------------------------%

get_sym_det(Gds,Model,Sym) :-
    ( get_sym(Gds,Model,Sym0) ->
        Sym = Sym0
    ;   error("get_sym_det: unable to determine symbol")
    ).

%---------------------------------------------------------------------------%

:- pred get_sym_from_guards(list(hl_guard)::in,hl_var::in,hl_symbol::out)
    is semidet.

get_sym_from_guards([Gd|Gds],Var,Sym) :-
    ( Gd = is_type(Type,Var,_) ->
        ( Type = int,
            Sym = hl_symbol(int_name,0)
        ; Type = float,
            Sym = hl_symbol(float_name,0)
        ; Type = string,
            Sym = hl_symbol(string_name,0)
        ; Type = var,
            Sym = hl_symbol(var_name,0)
        )
    ;   get_sym_from_guards(Gds,Var,Sym)
    ).

%---------------------------------------------------------------------------%

set_annotations(Model,Annots) = 
    set_hl_attr(Model,hl_attr_set_annotations(get_hl_attr(Model),Annots)).

%---------------------------------------------------------------------------%

:- func set_at_var(hl_model,hl_var) = hl_model.

set_at_var(Model,AtVar) =
    set_hl_attr(Model,hl_attr_set_at_var(get_hl_attr(Model),AtVar)).

%---------------------------------------------------------------------------%

:- func set_pragma(hl_model,hl_inline_pragma) = hl_model.

set_pragma(Model,Pragma) = NModel :-
    Pragmas = hl_attr_get_pragmas(get_hl_attr(Model)),
    set.insert(Pragma,Pragmas,NPragmas),
    NModel = 
        set_hl_attr(Model,hl_attr_set_pragmas(get_hl_attr(Model),NPragmas)).

%---------------------------------------------------------------------------%

dummy_pos = (-1).

%---------------------------------------------------------------------------%

hl_prog_foldl(Pred,HLP,!Acc) :-
    foldl(hl_prog_foldl_on_rules(Pred),HLP,!Acc).

%---------------------------------------------------------------------------%

:- pred hl_prog_foldl_on_rules(pred(hl_rule,T,T),hl_symbol,list(hl_rule),T,T).
:- mode hl_prog_foldl_on_rules(pred(in,in,out) is det,in,in,in,out) is det.

hl_prog_foldl_on_rules(Pred,_,Rules,!Acc) :-
    foldl(Pred,Rules,!Acc).

%---------------------------------------------------------------------------%

hl_prog_foldl2(Pred,HLP,!Acc1,!Acc2) :-
    foldl2(hl_prog_foldl_on_rules_2(Pred),HLP,!Acc1,!Acc2).

%---------------------------------------------------------------------------%

:- pred hl_prog_foldl_on_rules_2(pred(hl_rule,T,T,U,U),hl_symbol,list(hl_rule),
    T,T,U,U).
:- mode hl_prog_foldl_on_rules_2(pred(in,in,out,di,uo) is det,in,in,in,out,
    di,uo) is det.

hl_prog_foldl_on_rules_2(Pred,_,Rules,!Acc1,!Acc2) :-
    foldl2(Pred,Rules,!Acc1,!Acc2).

%---------------------------------------------------------------------------%

hl_attr_get_pos(hl_attr(Pos,_,_,_,_)) = Pos.

%---------------------------------------------------------------------------%

hl_attr_get_cxt(hl_attr(_,Cxt,_,_,_)) = Cxt.

%---------------------------------------------------------------------------%

hl_attr_get_annotations(hl_attr(_,_,Annots,_,_)) = Annots.

%---------------------------------------------------------------------------%

hl_attr_get_at_var(hl_attr(_,_,_,AtVar,_)) = AtVar.

%---------------------------------------------------------------------------%

hl_attr_get_pragmas(hl_attr(_,_,_,_,Pragmas)) = Pragmas.

%---------------------------------------------------------------------------%

:- func hl_attr_set_pos(hl_attr,pos) = hl_attr.

hl_attr_set_pos(hl_attr(_,Cxt,Annots,AtVar,Pragmas),Pos) =
    hl_attr(Pos,Cxt,Annots,AtVar,Pragmas).

%---------------------------------------------------------------------------%

:- func hl_attr_set_annotations(hl_attr,hl_model) = hl_attr.

hl_attr_set_annotations(hl_attr(Pos,Cxt,_,AtVar,Pragmas),Annots) =
    hl_attr(Pos,Cxt,yes(Annots),AtVar,Pragmas).

%---------------------------------------------------------------------------%

:- func hl_attr_set_at_var(hl_attr,hl_var) = hl_attr.

hl_attr_set_at_var(hl_attr(Pos,Cxt,Annots,_,Pragmas),AtVar) = 
    hl_attr(Pos,Cxt,Annots,yes(AtVar),Pragmas).

%---------------------------------------------------------------------------%

:- func hl_attr_set_pragmas(hl_attr,set(hl_inline_pragma)) = hl_attr.

hl_attr_set_pragmas(hl_attr(Pos,Cxt,Annots,AtVar,_),Pragmas) =
    hl_attr(Pos,Cxt,Annots,AtVar,Pragmas).

%---------------------------------------------------------------------------%

hl_attr_init = hl_attr(unset_pos,none,no,no,init).

%---------------------------------------------------------------------------%

:- func unset_pos = pos.

unset_pos = -1.

