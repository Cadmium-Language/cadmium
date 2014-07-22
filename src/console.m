%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%-----------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% A hugs-style interface for cadmium.
%
%---------------------------------------------------------------------------%

:- module console.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % Launch the console.
    % 
:- pred console(bool::in,io::di,io::uo) is det.

    % A default prompt for the console.
    %
:- func default_prompt = string.

    % Set the prompt.
    %
:- pred set_console_prompt(string::in,io::di,io::uo) is det.

    % Set the special command handler.
    %
:- pred set_console_handler(pred(string,io,io)::pred(in,di,uo) is det,
    io::di,io::uo) is det.

    % Set the execute handler.
    %
:- pred set_console_execute(pred(term,varset,io,io)::pred(in,in,di,uo) is det,
    io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module model.
:- import_module parse.
:- import_module readline.

:- import_module list.
:- import_module parser.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

:- mutable(prompt, string, default_prompt, ground,
    [untrailed, attach_to_io_state]).

:- mutable(handler, pred(string, io, io), default_handler,
    pred(in, di, uo) is det, [untrailed, attach_to_io_state]).

:- mutable(execute, pred(term, varset, io, io), default_execute,
    pred(in, in, di, uo) is det, [untrailed, attach_to_io_state]).

%---------------------------------------------------------------------------%

set_console_prompt(Prmpt, !IO) :-
    set_prompt(Prmpt, !IO).

set_console_handler(Handler, !IO) :-
    set_handler(Handler, !IO).

set_console_execute(Exec, !IO) :-
    set_execute(Exec, !IO).

console(Silent, !IO) :-
    console_loop(Silent, init, !IO).

%---------------------------------------------------------------------------%

:- pred console_loop(bool::in,history_state::in,io::di,io::uo) is det.

console_loop(Silent, !.State, !IO) :-
    (
        Silent = no,
        get_prompt(Prmpt,!IO),
        Prompt = Prmpt ++ "> ",
        read_line_as_string(Prompt, match_symbol_names, Res, !State, !IO)
    ;
        Silent = yes,
        read_line_as_string("", match_symbol_names, Res, !State, !IO)
    ),
    (
        Res = eof,
        (
            Silent = no,
            write_string("[exit].\n", !IO)
        ;
            Silent = yes
        )
    ;
        Res = error(Err),
        format("%s\n", [s(error_message(Err))], !IO),
        console_loop(Silent, !.State, !IO)
    ;
        Res = ok(Line),
        ( if is_console_command(Line, Command) then
            get_handler(Handler, !IO),
            Handler(Command, !IO)
        else
            read_term_from_string_with_op_table(cadmium_op_table, "<stdin>",
                Line, _, ReadTerm),
            (
                ReadTerm = eof
            ;
                ReadTerm = error(Msg,_),
                format("%s\n", [s(Msg)], !IO)
            ;
                ReadTerm = term(VS,Term),
                get_execute(Exec,!IO),
                Exec(Term,VS,!IO)
            )
        ),
        console_loop(Silent, !.State, !IO)
    ).

%---------------------------------------------------------------------------%

    % Console commands are of the form ": command".
    %
:- pred is_console_command(string::in, string::out) is semidet.

is_console_command(In0, Command) :-
    In1 = lstrip(In0),
    first_char(In1, ':', Command).

%---------------------------------------------------------------------------%

default_prompt = "cadmium".

%---------------------------------------------------------------------------%

:- pred default_handler(string::in, io::di, io::uo) is det.

default_handler(_, !IO).

%---------------------------------------------------------------------------%

:- pred default_execute(term::in, varset::in, io::di, io::uo) is det.

default_execute(_, _, !IO).

%---------------------------------------------------------------------------%
:- end_module console.
%---------------------------------------------------------------------------%
