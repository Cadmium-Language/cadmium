%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
% See the file COPYING for license information.
%-----------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Error handling for Cadmium.
%
%-----------------------------------------------------------------------------%

:- module cadmium_error.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

:- import_module cadmium_common.

%-----------------------------------------------------------------------------%

:- type message
    --->    message(string, list(string.poly_type), cxt).

:- type cd_error_result(T)
    ---> cd_success(T, list(message) /* Warnings */)
    ;    cd_errors(list(message), list(message)).

%-----------------------------------------------------------------------------%
%
% Errors
%

    % Registers an error.
    %
:- impure pred compiler_error(message::in) is det.

    % Same as compiler_error/1 but pure.
    %
:- pred compiler_error(message::in,io::di,io::uo) is det.

    % Same as compiler_error/1, but also throws the error (& and existing
    % error).  Since it is known there is at least one error, this predicate
    % can be erroneous.
    %
:- pred throw_compiler_error(message::in,io::di,io::uo) is erroneous.

%-----------------------------------------------------------------------------%
%
% Warning
%
    % Registers a warning.
    %
:- impure pred compiler_warning(message::in) is det.

    % Same as compiler_warning/1 but pure.
    %
:- pred compiler_warning(message::in,io::di,io::uo) is det.

%-----------------------------------------------------------------------------%

    % Throws all registered errors.  Prints any warning messages.  Does 
    % nothing if there have been no errors.
    %
:- pred throw_errors(io::di,io::uo) is det.

    % Catch any errors generated by the goal.
    %
:- pred catch_cadmium_errors(pred(T, io, io)::in(pred(out, di, uo) is det),
    cd_error_result(T)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Print messages as warnings.
    %
:- pred print_warnings(list(message)::in, io::di, io::uo) is det.

    % Print messages as errors.
    %
:- pred print_errors(list(message)::in, io::di, io::uo) is det.

    % print_errors(Stream, Program, Message, !IO): 
    %
:- pred print_cadmium_errors(io.text_output_stream::in, string::in,
    list(message)::in, io::di, io::uo) is det.

:- pred print_cadmium_warnings(io.text_output_stream::in, string::in,
    list(message)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module string.
:- import_module univ.

%-----------------------------------------------------------------------------%

    % Errors seen so far.
    %
:- mutable(errors,list(message),[],ground,[untrailed,attach_to_io_state]).

    % Warnings seen so far.
    %
:- mutable(warnings,list(message),[],ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

compiler_error(Error) :-
    semipure get_errors(Errs),
    impure set_errors([Error|Errs]).

%---------------------------------------------------------------------------%

compiler_error(Error,!IO) :-
    get_errors(Errs,!IO),
    set_errors([Error|Errs],!IO).

%---------------------------------------------------------------------------%

throw_compiler_error(Error,!IO) :-
    get_errors(Errs,!IO),
    throw_errors([Error|Errs],!IO).

%---------------------------------------------------------------------------%

compiler_warning(Warning) :-
    semipure get_warnings(Wrns),
    impure set_warnings([Warning|Wrns]).

%---------------------------------------------------------------------------%

compiler_warning(Warning,!IO) :-
    get_warnings(Wrns,!IO),
    set_warnings([Warning|Wrns],!IO).

%---------------------------------------------------------------------------%

throw_errors(!IO) :-
    get_errors(Errs,!IO),
    (
        Errs = []
    ;   
        Errs = [_ | _],
        throw_errors(Errs,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred throw_errors(list(message)::in, io::di, io::uo) is erroneous.

throw_errors(Errs, !IO) :-
    list.sort_and_remove_dups(message_compare, Errs, Errs1),
    set_errors([], !IO),
    throw(Errs1).

%---------------------------------------------------------------------------%

catch_cadmium_errors(Goal, Result, !IO) :-
    promise_equivalent_solutions [!:IO, Result] (
        try_io(Goal, GoalResult, !IO),
        get_warnings(Warnings0, !IO),
        list.sort_and_remove_dups(message_compare, Warnings0, Warnings),
        set_warnings([], !IO),
        (
            GoalResult = succeeded(X),
            Result = cd_success(X, Warnings)
        ;
            GoalResult = exception(Excp),
            ( if    univ_to_type(Excp, Errors)
              then  Result = cd_errors(Errors, Warnings)
              else  rethrow(GoalResult)
            )
        )
    ).
%---------------------------------------------------------------------------%

:- pred message_compare(message::in, message::in, comparison_result::out)
    is det.

message_compare(A, B, Result) :-
    A = message(MsgA, ArgsA, CxtA),
    B = message(MsgB, ArgsB, CxtB),
    compare(CxtResult, CxtA, CxtB),
    (
        CxtResult = (=),
        compare(MsgResult, MsgA, MsgB),
        (
            MsgResult = (=),
            compare(Result, ArgsA, ArgsB)
        ;
            ( MsgResult = (<)
            ; MsgResult = (>)
            ),
            Result = MsgResult
        )
    ;
        ( CxtResult = (<)
        ; CxtResult = (>)
        ),
        Result = CxtResult
    ).

%---------------------------------------------------------------------------%

% XXX why don't these print errors to the standard error?

print_warnings(Warnings ,!IO) :-
    io.stdout_stream(StdOut, !IO),
    list.foldl(print_message(StdOut, "", no), Warnings, !IO).

print_errors(Errors, !IO) :-
    io.stdout_stream(StdOut, !IO),
    list.foldl(print_message(StdOut, "", yes), Errors, !IO).

%---------------------------------------------------------------------------%

print_cadmium_errors(Stream, ProgName, Errors, !IO) :-
    list.foldl(print_message(Stream, ProgName, yes), Errors, !IO).

print_cadmium_warnings(Stream, ProgName, Warnings, !IO) :-
    list.foldl(print_message(Stream, ProgName, no), Warnings, !IO).

%---------------------------------------------------------------------------%

:- pred print_message(io.text_output_stream::in, string::in, bool::in,
    message::in, io::di, io::uo) is det.

print_message(Stream, ProgName, IsError, Message, !IO) :-
    Message = message(FmtStr, Args, Context),
    (
        Context = none,
        % Use the program name if we have no context information.
        % If no program name is available then print nothing; the
        % latter behaviour is for compatibility with existing code
        % in the Cadmium engine only.
        ( if    ProgName = ""
          then  true
          else  io.format(Stream, "%s: ", [s(ProgName)], !IO)
        )
    ;
        Context = cxt(FileName, Line),
        io.format(Stream, "%s: %d: ", [s(FileName), i(Line)], !IO)
    ),
    (
        IsError = yes,
        io.write_string(Stream, "error: ", !IO)
    ;
        IsError = no,
        io.write_string(Stream, "warning: ", !IO)
    ),
    io.format(Stream, FmtStr, Args, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%
:- end_module cadmium_error.
%---------------------------------------------------------------------------%
