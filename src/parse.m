%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% A parser for Cadmium rules written CHR-style.  Just uses the Mercury parser
% and interprets the resulting terms.
%
%---------------------------------------------------------------------------%

:- module parse.
:- interface.

:- import_module io.
:- import_module list.
:- import_module ops.
:- import_module set.

:- import_module hl_prog.

:- type cadmium_op_table ---> cadmium_op_table.

:- instance op_table(cadmium_op_table).

    % Parse an entire .acd file from stdin.
    %
:- pred parse_acd_file(list(hl_rule)::out,set(string)::out,
    list(hl_pragma)::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

:- import_module cadmium_common.
:- import_module cadmium_error.

%---------------------------------------------------------------------------%

:- type cadmium_item
    ---> rule(maybe(term),term,maybe(term),term,varset)
    ;    import(string)
    ;    pragma(term,varset)
    ;    eof
    ;    error(string,context).

%---------------------------------------------------------------------------%

parse_acd_file(Rules,Imports,Pragmas,!IO) :-
    parse_acd_file(0,_,[],Rules,init,Imports,[],Pragmas,!IO).

%---------------------------------------------------------------------------%

:- pred parse_acd_file(rule_id::in,rule_id::out,
    list(hl_rule)::in,list(hl_rule)::out,set(string)::in,set(string)::out,
    list(hl_pragma)::in,list(hl_pragma)::out,io::di,io::uo) is det.

parse_acd_file(!RId,!Rules,!Imports,!Pragmas,!IO) :-
    read_cadmium_item(Item,!IO),
    ( Item = parse.eof,
        true
    ; Item = parse.error(Msg,Cxt),
        Cxt = context(File,Line),
        Error = message(Msg,[],cxt(File,Line)),
        compiler_error(Error,!IO)
    ; Item = import(Module),
        set.insert(Module,!Imports),
        parse_acd_file(!RId,!Rules,!Imports,!Pragmas,!IO)
    ; Item = pragma(Pragma0,VarSet),
        term_to_hl_pragma(Pragma0,VarSet,Pragma,!IO),
        !:Pragmas = [Pragma|!.Pragmas],
        parse_acd_file(!RId,!Rules,!Imports,!Pragmas,!IO)
    ; Item = rule(MaybeCC,Hd,MaybeGd,Bd,VarSet),
        some [!VM,!State,!VarSet,!SVars] (
            !:VM = init,
            !:State = 0,
            !:VarSet = VarSet,
            !:SVars = map.init,
            IsMatch = yes,
            ( MaybeCC = no,
                HLMaybeCC = no
            ; MaybeCC = yes(CC),
                state_var_expand(CC,NCC,!State,!VarSet,!SVars,!IO),
                term_to_hl_model(IsMatch,!.VarSet,NCC,HLCC,!VM,!IO),
                HLMaybeCC = yes(HLCC)
            ),
            state_var_expand(Hd,NHd,!State,!VarSet,!SVars,!IO),
            term_to_hl_model(IsMatch,!.VarSet,NHd,HLHd,!VM,!IO),
            IsNotMatch = no,
            ( MaybeGd = no,
                state_var_expand(Bd,NBd,!State,!VarSet,!SVars,!IO),
                term_to_hl_model(IsNotMatch,!.VarSet,NBd,HLBd,!VM,!IO),
                map.foldl(generate_hl_var_name_info(!.VarSet),!.VM,init,VNI),
                HLGds = []
            ; MaybeGd = yes(Gd),
                state_var_expand(Gd,NGd,!State,!VarSet,!SVars,!IO),
                term_to_hl_model(IsNotMatch,!.VarSet,NGd,HLGds0,!VM,!IO),
                state_var_expand(Bd,NBd,!State,!VarSet,!SVars,!IO),
                term_to_hl_model(IsNotMatch,!.VarSet,NBd,HLBd,!VM,!IO),
                map.foldl(generate_hl_var_name_info(!.VarSet),!.VM,init,VNI),
                hl_model2hl_guard(VNI,HLGds0,HLGds)
            ),
            !.State = _,
            !.SVars = _
        ),
        HLRule = hl_rule(!.RId,HLMaybeCC,HLHd,HLGds,HLBd,VNI),
        !:RId = !.RId + 1,
        parse_acd_file(!RId,!Rules,!Imports,!Pragmas,!IO),
        !:Rules = [HLRule|!.Rules]
    ).

%---------------------------------------------------------------------------%

:- pred generate_hl_var_name_info(varset::in,var::in,hl_var::in,
    var_name_info::in,var_name_info::out) is det.

generate_hl_var_name_info(VS,Var,MVar,!VNI) :-
    ( search_name(VS,Var,Name) ->
        map.det_insert(MVar,Name,!VNI)
    ;   true
    ).

%---------------------------------------------------------------------------%

    % Read a single Cadmium item, e.g. a rule or declaration, from the input
    % stream.
    %
:- pred read_cadmium_item(cadmium_item::out,io::di,io::uo) is det.

read_cadmium_item(Item,!IO) :-
    read_term_with_op_table(cadmium_op_table,Res,!IO),
    ( Res = eof,
        Item = eof
    ; Res = error(Msg,Line),
        input_stream_name(File,!IO),
        Item = error(Msg,context(File,Line))
    ; Res = term(VS,Term),
        ( Term = variable(Var,Cxt),
            Item = unexpected_variable_error_item(VS,Cxt,Var)
        ; Term = functor(Cnst,Args,Cxt),
            ( Cnst = atom("import"),
              Args = [Import] ->
                parse_import(Import,VS,Item)
            ; Cnst = atom("pragma"),
              Args = [Pragma] ->
                Item = pragma(Pragma,VS)
            ; Cnst = atom("<=>"),
              Args = [Hd0,GdBd] ->
                parse_head(Hd0,MaybeCC,Hd),
                parse_guard_and_body(GdBd,MaybeGd,Bd),
                Item = rule(MaybeCC,Hd,MaybeGd,Bd,VS)
            ;   Item = error("expected a declaration or rule",Cxt)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred parse_head(term::in,maybe(term)::out,term::out) is det.

parse_head(Var,no,Var) :-
    Var = variable(_,_).
parse_head(Term,MaybeCC,Head) :-
    Term = functor(Cnst,Args,_),
    ( Cnst = atom("\\"),
      Args = [Arg1,Arg2] ->
        MaybeCC = yes(Arg1),
        Head = Arg2
    ;   MaybeCC = no,
        Head = Term
    ).

%---------------------------------------------------------------------------%

:- pred parse_guard_and_body(term::in,maybe(term)::out,term::out) is det.

parse_guard_and_body(Var,no,Var) :-
    Var = variable(_, _).
parse_guard_and_body(Term,MaybeGuard,Body) :-
    Term = functor(Cnst,Args,_),
    ( Cnst = atom("|"),
      Args = [Arg1,Arg2] ->
        MaybeGuard = yes(Arg1),
        Body = Arg2
    ;   MaybeGuard = no,
        Body = Term
    ).

%---------------------------------------------------------------------------%

:- pred state_var_expand(term::in,term::out,int::in,int::out,
    varset::in,varset::out,map(string,var)::in,map(string,var)::out,
    io::di,io::uo) is det.

state_var_expand(Var@variable(_,_),Var,!State,!VarSet,!SVars,!IO).
state_var_expand(Term@functor(Atom,Args,Cxt),NTerm,!State,!VarSet,!SVars,
        !IO) :-
    ( Atom = atom("!."),
      Args = [variable(Var,_)] ->
        lookup_name(!.VarSet,Var,Name),
        NName = Name ++ "_" ++ int_to_string(!.State),
        ( search(!.SVars,NName,NVar) ->
            NTerm = variable(NVar,Cxt)
        ;   Message = message(
            "no previous state variable !:%s for current state variable !.%s",
                [s(Name),s(Name)],context_to_cxt(Cxt)),
            compiler_error(Message,!IO),
            NTerm = Term
        )
    ; Atom = atom("!:"),
      Args = [variable(Var,_)] ->
        lookup_name(!.VarSet,Var,Name),
        !:State = !.State + 1,
        NName = Name ++ "_" ++ int_to_string(!.State),
        varset.new_named_var(NName,NVar,!VarSet),
        map.det_insert(NName,NVar,!SVars),
        NTerm = variable(NVar,Cxt)
    ; Atom = atom(":="),
      Args = [Arg1,Arg2] ->
        state_var_expand(Arg2,NArg2,!State,!VarSet,!SVars,!IO),
        state_var_expand(Arg1,NArg1,!State,!VarSet,!SVars,!IO),
        NTerm = functor(Atom,[NArg1,NArg2],Cxt)
    ;   map_foldl4(state_var_expand,Args,NArgs,!State,!VarSet,!SVars,!IO),
        NTerm = functor(Atom,NArgs,Cxt)
    ).

%---------------------------------------------------------------------------%

:- pred parse_import(term::in,varset::in,cadmium_item::out) is det.

parse_import(variable(Var,Cxt),VS,Item) :-
    Item = unexpected_variable_error_item(VS,Cxt,Var).
parse_import(functor(Cnst,Args,Cxt),_,Item) :-
    ( Cnst = atom(Module),
      Args = [] ->
        Item = import(Module)
    ;   Item = error("expected module name after 'import'",Cxt)
    ).

%---------------------------------------------------------------------------%

:- func unexpected_variable_error_item(varset,context,var) = cadmium_item.

unexpected_variable_error_item(VS,Cxt,Var) = Item :-
    lookup_name(VS,Var,Name),
    Msg = "unexpected variable '" ++ Name ++ "'",
    Item = error(Msg,Cxt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- instance op_table(cadmium_op_table) where [
    pred(lookup_infix_op/5) is lookup_cadmium_infix_op,
    pred(lookup_prefix_op/4) is lookup_cadmium_prefix_op,
    pred(lookup_binary_prefix_op/5) is lookup_cadmium_binary_prefix_op,
    pred(lookup_postfix_op/4) is lookup_cadmium_postfix_op,
    pred(lookup_op/2) is lookup_cadmium_op,
    pred(lookup_op_infos/4) is lookup_cadmium_op_infos,
    pred(lookup_operator_term/4) is lookup_cadmium_operator_term,
    func(max_priority/1) is cadmium_max_priority,
    func(arg_priority/1) is cadmium_arg_priority
].

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_op_infos(cadmium_op_table,string,op_info,list(op_info)).
:- mode lookup_cadmium_op_infos(in,in,out,out) is semidet.

lookup_cadmium_op_infos(_,Name,OpInfo,OpInfos) :-
    solutions(cadmium_op_info(Name),[OpInfo|OpInfos]).

%-----------------------------------------------------------------------------%

:- pred lookup_cadmium_op_infos(cadmium_op_table,string,list(op_info)).
:- mode lookup_cadmium_op_infos(in,in,out) is det.

lookup_cadmium_op_infos(_,Name,OpInfos) :-
    solutions(cadmium_op_info(Name),OpInfos).

%-----------------------------------------------------------------------------%

:- pred cadmium_op_info(string,op_info).
:- mode cadmium_op_info(in,out) is nondet.

cadmium_op_info(Name,OpInfo) :-
    cadmium_op_table(Name,OpInfo).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_infix_op(cadmium_op_table,string,priority,assoc,
            assoc).
:- mode lookup_cadmium_infix_op(in,in,out,out,out) is semidet.

lookup_cadmium_infix_op(_,Name,Priority,LeftAssoc,RightAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table,Name,OpInfos),
    find_first(is_infix_op,OpInfos,OpInfo),
    OpInfo = op_info(infix(LeftAssoc,RightAssoc),Priority).

%---------------------------------------------------------------------------%

:- func is_binary_operator(string) = int.

:- pragma foreign_export("C",is_binary_operator(in) = out,
    "CR_is_binary_operator").

is_binary_operator(Name) = Priority :-
    ( lookup_cadmium_infix_op(cadmium_op_table,Name,Priority0,_,_) ->
        Priority = Priority0
    ;   Priority = 0
    ).

%---------------------------------------------------------------------------%

:- pred is_infix_op(op_info::in) is semidet.

is_infix_op(op_info(infix(_,_),_)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_operator_term(cadmium_op_table,priority,assoc,
            assoc).
:- mode lookup_cadmium_operator_term(in,out,out,out) is semidet.

lookup_cadmium_operator_term(_,120,y,x) :- semidet_true.

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_prefix_op(cadmium_op_table,string,priority,assoc).
:- mode lookup_cadmium_prefix_op(in,in,out,out) is semidet.

lookup_cadmium_prefix_op(_,Name,Priority,LeftAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table,Name,OpInfos),
    find_first(is_prefix_op,OpInfos,OpInfo),
    OpInfo = op_info(prefix(LeftAssoc),Priority).

%---------------------------------------------------------------------------%

:- func is_unary_operator(string) = int.

:- pragma foreign_export("C",is_unary_operator(in) = out,
    "CR_is_unary_operator").

is_unary_operator(Name) = Priority :-
    ( lookup_cadmium_prefix_op(cadmium_op_table,Name,Priority0,_) ->
        Priority = Priority0
    ;   Priority = 0
    ).

%---------------------------------------------------------------------------%

:- pred is_prefix_op(op_info::in) is semidet.

is_prefix_op(op_info(prefix(_),_)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_binary_prefix_op(cadmium_op_table,string,priority,
            assoc,assoc).
:- mode lookup_cadmium_binary_prefix_op(in,in,out,out,out) is semidet.

lookup_cadmium_binary_prefix_op(_,Name,Priority,LeftAssoc,RightAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table,Name,OpInfos),
    find_first(is_binary_prefix_op,OpInfos,OpInfo),
    OpInfo = op_info(binary_prefix(LeftAssoc,RightAssoc),Priority).

%---------------------------------------------------------------------------%

:- pred is_binary_prefix_op(op_info::in) is semidet.

is_binary_prefix_op(op_info(binary_prefix(_,_),_)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_postfix_op(cadmium_op_table,string,priority,assoc).
:- mode lookup_cadmium_postfix_op(in,in,out,out) is semidet.

lookup_cadmium_postfix_op(_,Name,Priority,LeftAssoc) :-
    lookup_cadmium_op_infos(cadmium_op_table,Name,OpInfos),
    find_first(is_postfix_op,OpInfos,OpInfo),
    OpInfo = op_info(postfix(LeftAssoc),Priority).

%---------------------------------------------------------------------------%

:- pred is_postfix_op(op_info::in) is semidet.

is_postfix_op(op_info(postfix(_),_)).

%---------------------------------------------------------------------------%

:- pred lookup_cadmium_op(cadmium_op_table,string).
:- mode lookup_cadmium_op(in,in) is semidet.

lookup_cadmium_op(_,Name) :-
    cadmium_op_table(Name,_).

%---------------------------------------------------------------------------%

:- func cadmium_max_priority(cadmium_op_table) = priority.

cadmium_max_priority(_) = 1200.

%---------------------------------------------------------------------------%

:- func cadmium_arg_priority(cadmium_op_table) = priority.

cadmium_arg_priority(_) = 1201.

%---------------------------------------------------------------------------%

:- pred find_first(pred(T)::(pred(in) is semidet),list(T)::in,T::out)
    is semidet.

find_first(Pred,[X|Xs],Y) :-
    ( Pred(X) ->
        Y = X
    ;   find_first(Pred,Xs,Y)
    ).

%---------------------------------------------------------------------------%

:- pred cadmium_op_table(string,op_info).
:- mode cadmium_op_table(in,out) is nondet.

cadmium_op_table("..", op_info(infix(x,x), 550)).     % Mercury extension
cadmium_op_table("$*", op_info(infix(y,x), 399)).     % Cadmium extension
cadmium_op_table("*", op_info(infix(y,x), 400)).      % standard ISO Prolog
cadmium_op_table("**", op_info(infix(x,y), 200)).     % standard ISO Prolog
cadmium_op_table("$+", op_info(infix(y,x), 499)).     % Cadmium extension
cadmium_op_table("+", op_info(infix(y,x), 500)).      % standard ISO Prolog
cadmium_op_table("++", op_info(infix(x,y), 500)).     % Mercury extension
cadmium_op_table("+", op_info(prefix(x), 500)).       % traditional Prolog (not ISO)
cadmium_op_table("&", op_info(infix(x,y), 1025)).     % Mercury extension
cadmium_op_table("$-", op_info(infix(y,x), 499)).     % Cadmium extension
cadmium_op_table("-", op_info(infix(y,x), 500)).      % standard ISO Prolog
cadmium_op_table("$-", op_info(prefix(x), 199)).      % Cadmium extension
cadmium_op_table("-", op_info(prefix(x), 200)).       % standard ISO Prolog
cadmium_op_table("->", op_info(infix(x,y), 1050)).    % standard ISO Prolog
cadmium_op_table("$/", op_info(infix(y,x), 399)).     % Cadmium extension
cadmium_op_table("/", op_info(infix(y,x), 400)).      % standard ISO Prolog
cadmium_op_table("/\\", op_info(infix(y,x), 1000)).   % Cadmium extenstion
cadmium_op_table("$/\\", op_info(infix(y,x), 999)).   % Cadmium extenstion
cadmium_op_table("::", op_info(infix(x,y), 99)).      % Cadmium extension
cadmium_op_table("$::", op_info(infix(x,y), 98)).     % Cadmium extension
cadmium_op_table(":=", op_info(infix(x,x), 700)).     % Mercury extension
cadmium_op_table("<", op_info(infix(x,x), 700)).      % standard ISO Prolog
cadmium_op_table("$<", op_info(infix(x,x), 699)).     % Cadmium extension
cadmium_op_table("<->", op_info(infix(x,x), 700)).    % Cadmium extension
cadmium_op_table("$<->", op_info(infix(x,x), 699)).   % Cadmium extension
cadmium_op_table("$->", op_info(infix(x,x), 799)).    % Cadmium extension
cadmium_op_table("$<-", op_info(infix(x,x), 799)).    % Cadmium extension
cadmium_op_table("=", op_info(infix(x,x), 700)).      % standard ISO Prolog
cadmium_op_table("$=", op_info(infix(x,x), 699)).     % Cadmium extension
cadmium_op_table("<=", op_info(infix(x,x), 700)).     % standard ISO Prolog
cadmium_op_table("$<=", op_info(infix(x,x), 699)).    % Cadmium extension
cadmium_op_table("=>", op_info(infix(x,y), 920)).     % Mercury/NU-Prolog extension
cadmium_op_table(">", op_info(infix(x,x), 700)).      % standard ISO Prolog
cadmium_op_table("$>", op_info(infix(x,x), 699)).     % Cadmium extension
cadmium_op_table(">=", op_info(infix(x,x), 700)).     % standard ISO Prolog
cadmium_op_table("$>=", op_info(infix(x,x), 699)).    % Cadmium extension
cadmium_op_table("@", op_info(infix(x,x), 90)).       % Mercury extension
cadmium_op_table("\\", op_info(prefix(x), 200)).      % standard ISO Prolog
cadmium_op_table("\\/", op_info(infix(y,x), 1100)).   % Cadmium extension
cadmium_op_table("$\\/", op_info(infix(y,x), 1099)).  % Cadmium extension
cadmium_op_table("$xor", op_info(infix(y,x), 1099)).  % Cadmium extension
cadmium_op_table("!=", op_info(infix(x,x), 700)).     % Cadmium extension
cadmium_op_table("$!=", op_info(infix(x,x), 699)).    % Cadmium extension
cadmium_op_table("^", op_info(infix(x,y), 99)).       % ISO Prolog (prec. 200,
cadmium_op_table("\\", op_info(infix(y,x), 1110)).    % Cadmium extension
cadmium_op_table("|", op_info(infix(x,y), 1110)).     % Cadmium extension
cadmium_op_table("not", op_info(prefix(y), 900)).     % Cadmium extension
cadmium_op_table("$not", op_info(prefix(y), 899)).    % Cadmium extension
cadmium_op_table("<=>", op_info(infix(x,y), 1150)).   % Cadmium extension
cadmium_op_table("import", op_info(prefix(y), 1199)). % Cadmium extension
cadmium_op_table("pragma", op_info(prefix(y), 1199)). % Cadmium extension
cadmium_op_table("mod", op_info(infix(x,x), 400)).    % Standard ISO Prolog
cadmium_op_table("!.", op_info(prefix(x), 40)).       % Cadmium extension
cadmium_op_table("!:", op_info(prefix(x), 40)).       % Cadmium extension
cadmium_op_table("^", op_info(prefix(x), 40)).        % Cadmium extension

%-----------------------------------------------------------------------------%
:- end_module parse.
%-----------------------------------------------------------------------------%
