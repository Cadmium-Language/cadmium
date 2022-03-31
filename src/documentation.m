%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Cadmium documentation management.
%
%---------------------------------------------------------------------------%

:- module documentation.

:- interface.

:- import_module io.
:- import_module list.
:- import_module map.

:- import_module cadmium_common.
:- import_module hl_prog.

:- type doc_info == map(hl_symbol,doc_data).
:- type doc_data
    ---> doc_data(list(string),string).

    % Convert a 'pragma doc' info doc info.
    %
:- pred pragma_doc_to_doc_info(hl_pragma::in,doc_info::in,doc_info::out,
    io::di,io::uo) is det.

    % Query a doc info.
    %
:- pred doc_query(doc_info::in,string::in,io::di,io::uo) is det.

    % Query all documentation.
    %
:- pred doc_query_all(doc_info::in,io::di,io::uo) is det.

    % Like io.write_string, but ensures text fits into 80 char column.
    %
:- pred write_text(string::in,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module string.

:- import_module cadmium_error.

:- type space
    ---> none
    ;    one
    ;    two.

%---------------------------------------------------------------------------%

pragma_doc_to_doc_info(pragma_foreign(_,_,_),!DocInfo,!IO).
pragma_doc_to_doc_info(pragma_doc(Sym,Params,Text,Cxt),!DocInfo,!IO) :-
    DocData = doc_data(Params,Text),
    ( map.insert(Sym,DocData,!DocInfo) ->
        true
    ;   Message = message("more than one pragma doc exists for symbol %s/%d",
            [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],Cxt),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

doc_query(DocInfo,Query,!IO) :-
    ( parse_name_arity(Query,Name,Arity) ->
        Sym = hl_symbol(Name,Arity),
        ( search(DocInfo,Sym,DocData) ->
            print_help(Sym,DocData,!IO)
        ;   format("No help available for %s/%d.\n",[s(Name),i(Arity)],!IO)
        )
    ;   format("Expected name/arity pair, found \"%s\".\n",[s(Query)],!IO)
    ).

%---------------------------------------------------------------------------%

doc_query_all(DocInfo,!IO) :-
    foldl(print_help,DocInfo,!IO).

%---------------------------------------------------------------------------%

:- pred print_help(hl_symbol::in,doc_data::in,io::di,io::uo) is det.

print_help(Sym,DocData,!IO) :-
    DocData = doc_data(Params,Text),
    print_head(Sym,Params,!IO),
    print_text(Params,Text,!IO),
    nl(!IO).

%---------------------------------------------------------------------------%

:- pred print_head(hl_symbol::in,list(string)::in,io::di,io::uo) is det.

print_head(Sym,Params,!IO) :-
    write_string(hl_symbol_name(Sym),!IO),
    ( hl_symbol_arity(Sym) = 0 ->
        nl(!IO)
    ;   write_char('(',!IO),
        write_list(Params,",",print_highlighted,!IO),
        write_string(")\n",!IO)
    ).

%---------------------------------------------------------------------------%

:- pred print_text(list(string)::in,string::in,io::di,io::uo) is det.

print_text(Params,Text,!IO) :-
    Words = words(Text),
    print_words(Params,Words,!IO),
    nl(!IO).

%---------------------------------------------------------------------------%

:- pred print_words(list(string)::in,list(string)::in,io::di,io::uo) is det.

print_words(Params,Words,!IO) :-
    foldl3(print_word(Params),Words,none,Space,8,_,!IO),
    ( Space = one ->
        write_char('.',!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred print_word(list(string)::in,string::in,space::in,space::out,
    int::in,int::out,io::di,io::uo) is det.

print_word(Params,Word,!Space,!Line,!IO) :-
    length(Word,LenWord),
    ( !.Space = two,
        Len = LenWord + 2
    ; !.Space = one,
        Len = LenWord + 1
    ; !.Space = none,
        Len = LenWord
    ),
    !:Line = !.Line + Len,
    ( !.Line >= 79 ->
        nl(!IO),
        !:Line = Len + 8,
        !:Space = none
    ;   true
    ),
    ( !.Space = two,
        write_string("  ",!IO)
    ; !.Space = one,
        write_char(' ',!IO)
    ; !.Space = none,
        write_char('\t',!IO)
    ),
    det_index(Word,LenWord-1,LastChar),
    ( is_punctuation(LastChar) ->
        left(Word,LenWord-1,NWord),
        HaveLastChar = yes(LastChar)
    ;   NWord = Word,
        HaveLastChar = no
    ),
    ( member(NWord,Params) ->
        print_highlighted(NWord,!IO)
    ;   write_string(NWord,!IO)
    ),
    ( HaveLastChar = yes(Char),
        write_char(Char,!IO)
    ; HaveLastChar = no,
        true
    ),
    ( LastChar = ('.') ->
        !:Space = two
    ;   !:Space = one
    ).

%---------------------------------------------------------------------------%

:- pred print_highlighted(string::in,io::di,io::uo) is det.

print_highlighted(Name,!IO) :-
    start_highlight(!IO),
    write_string(Name,!IO),
    stop_highlight(!IO).

%---------------------------------------------------------------------------%

:- pred start_highlight(io::di,io::uo) is det.

start_highlight(!IO) :-
    write_string("\033\[1;4;37m",!IO).

%---------------------------------------------------------------------------%

:- pred stop_highlight(io::di,io::uo) is det.

stop_highlight(!IO) :-
    write_string("\033\[0m",!IO).

%---------------------------------------------------------------------------%

:- pred is_punctuation(char::in) is semidet.

is_punctuation('.').
is_punctuation(',').
is_punctuation(';').
is_punctuation(':').
is_punctuation('!').
is_punctuation('?').
is_punctuation(')').

%---------------------------------------------------------------------------%

write_text(Text,!IO) :-
    print_text([],Text,!IO).

