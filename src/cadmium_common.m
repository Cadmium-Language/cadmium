%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck <gjd@cs.mu.oz.au>
%
% Common stuff for Cadmium.
%
%---------------------------------------------------------------------------%

:- module cadmium_common.
:- interface.

:- import_module list.
:- import_module term.

    % Filename/line-number.
    %
:- type cxt
    ---> none
    ;    cxt(string,int).

    % A "symbol" is a Name/Arity pair.
    %
:- type hl_name  == string.
:- type hl_arity == int.
:- type hl_symbol.

    % Different types of models.
    %
:- type hl_model_type
    ---> int
    ;    float
    ;    string
    ;    var
    ;    functor.

    % Decides if the given functor is an AC operator.
    %
:- pred is_ac(string::in) is semidet.

    % Decides if the given functor is an ACD operator.
    %
:- pred is_acd(string::in) is semidet.

    % Decides if the given functor is a D-only operator.
    %
:- pred is_d_only(string::in) is semidet.

    % Various functor names that the Cd engine/compiler knows about.
    %
:- func int_name                = string.
:- func float_name              = string.
:- func string_name             = string.
:- func var_name                = string.
:- func nil_name                = string.
:- func cons_name               = string.
:- func true_name               = string.
:- func false_name              = string.
:- func annotate_name           = string.
:- func plus_name               = string.
:- func mult_name               = string.
:- func conj_name               = string.
:- func disj_name               = string.
:- func term_name               = string.
:- func is_int_name             = string.
:- func is_float_name           = string.
:- func is_string_name          = string.
:- func is_var_name             = string.
:- func is_functor_name         = string.
:- func top_level_name          = string.
:- func call_name               = string.
:- func cc_name                 = string.
:- func foreign_name            = string.
:- func undefined_name          = string.
:- func builtin_plus_name       = string.
:- func builtin_minus_name      = string.
:- func builtin_multiply_name   = string.
:- func builtin_divide_name     = string.
:- func builtin_mod_name        = string.
:- func builtin_and_name        = string.
:- func builtin_or_name         = string.
:- func builtin_iff_name        = string.
:- func builtin_xor_name        = string.
:- func builtin_impl_name       = string.
:- func builtin_not_name        = string.
:- func builtin_lt_name         = string.
:- func builtin_leq_name        = string.
:- func builtin_gt_name         = string.
:- func builtin_geq_name        = string.
:- func builtin_eq_name         = string.
:- func builtin_neq_name        = string.
:- func eq_name                 = string.
:- func lt_name                 = string.
:- func gt_name                 = string.

    % Construct a hl_symbol.
    %
:- func hl_symbol(hl_name,hl_arity) = hl_symbol.

    % Tests if the given symbol is Func/Aty.
    %
:- pred hl_is_symbol(hl_symbol::in,hl_name::in,hl_arity::in) is semidet.

    % Get the name of a hl_symbol.
    %
:- func hl_symbol_name(hl_symbol) = hl_name.

    % Get the arity of a hl_symbol.
    %
:- func hl_symbol_arity(hl_symbol) = hl_arity.

    % Parse a name/arity pair from a string.
    %
:- pred parse_name_arity(string::in,string::out,int::out) is semidet.

    % Convert a term.context into a cxt.
    %
:- func context_to_cxt(context) = cxt.

    % This should also go in list.m
    %
:- pred any_true(pred(A),list(A)).
:- mode any_true(pred(in) is semidet,in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- type hl_symbol
    ---> hl_symbol(hl_arity,hl_name).

%---------------------------------------------------------------------------%

hl_symbol(Func,Aty) = hl_symbol(Aty,Func).

%---------------------------------------------------------------------------%

hl_is_symbol(hl_symbol(Aty,Func),Func,Aty).

%---------------------------------------------------------------------------%

hl_symbol_name(hl_symbol(_,Func)) = Func.

%---------------------------------------------------------------------------%

hl_symbol_arity(hl_symbol(Aty,_)) = Aty.

%---------------------------------------------------------------------------%

parse_name_arity(Str,Name,Aty) :-
    sub_string_search(Str,"/",Idx0),
    find_last_slash_index(Str,Idx0,Idx),
    Name = substring(Str,0,Idx),
    AtyStr = substring(Str,Idx+1,length(Str)),
    to_int(AtyStr,Aty).

%---------------------------------------------------------------------------%

context_to_cxt(context(File,Line)) = cxt(File,Line).

%---------------------------------------------------------------------------%

    % Finds the index of the last slash, i.e. '/', in a string.
    %
:- pred find_last_slash_index(string::in,int::in,int::out) is det.

find_last_slash_index(Str,Idx0,Idx) :-
    ( sub_string_search_start(Str,"/",Idx0+1,Idx1) ->
        find_last_slash_index(Str,Idx1,Idx)
    ;   Idx = Idx0
    ).

%---------------------------------------------------------------------------%

is_ac(Func) :-
    ( ( Func = plus_name
      ; Func = conj_name
      ; Func = mult_name
      ; Func = disj_name 
      ; Func = annotate_name ) ->
        true
    ;   Func \= "",
        unsafe_index(Func,0,Char),
        ( Char = ('&')
        ; Char = ('~')
        )
    ).

%---------------------------------------------------------------------------%

is_acd(Func) :-
    (   Func = conj_name
    ;   Func \= "",
        unsafe_index(Func,0,'~')
    ).

%---------------------------------------------------------------------------%

is_d_only(Func) :-
    (   Func = cc_name
    ;   Func \= "",
        unsafe_index(Func,0,'{')
    ).

%---------------------------------------------------------------------------%

int_name                = "$int".
float_name              = "$float".
string_name             = "$string".
var_name                = "$var".
nil_name                = "[]".
cons_name               = "[|]".
true_name               = "true".
false_name              = "false".
annotate_name           = "::".
plus_name               = "+".
mult_name               = "*".
conj_name               = "/\\".
disj_name               = "\\/".
term_name               = "term".
is_int_name             = "is_int".
is_float_name           = "is_float".
is_string_name          = "is_string".
is_var_name             = "is_var".
is_functor_name         = "is_functor".
top_level_name          = "$top_level".
cc_name                 = "$cc".
foreign_name            = "$foreign".
undefined_name          = "undefined".
call_name               = "call".
builtin_plus_name       = "$+".
builtin_minus_name      = "$-".
builtin_multiply_name   = "$*".
builtin_divide_name     = "$/".
builtin_mod_name        = "$mod".
builtin_and_name        = "$/\\".
builtin_or_name         = "$\\/".
builtin_iff_name        = "$<->".
builtin_xor_name        = "$xor".
builtin_impl_name       = "$->".
builtin_not_name        = "$not".
builtin_lt_name         = "$<".
builtin_leq_name        = "$<=".
builtin_gt_name         = "$>".
builtin_geq_name        = "$>=".
builtin_eq_name         = "$=".
builtin_neq_name        = "$!=".
eq_name                 = "=".
lt_name                 = "<".
gt_name                 = ">".

%---------------------------------------------------------------------------%

any_true(P,[X|Xs]) :-
    ( P(X) ->
        true
    ;   any_true(P,Xs)
    ).

%---------------------------------------------------------------------------%
:- end_module cadmium_common.
%---------------------------------------------------------------------------%
