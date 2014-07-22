%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% A simple Mercury binding to the readline library.
%
%---------------------------------------------------------------------------%

:- module readline.
:- interface.

:- import_module io.
:- import_module list.

:- type history_state.

:- type completion_func == (func(string) = list(string)).
:- inst completion_func == (func(in) = out is det).

    % Create a new history_state.
    %
:- func init = history_state.

    % Like io.read_line_as_string but with readline.
    %
:- pred read_line_as_string(string::in,completion_func::in(completion_func),
    io.result(string)::out,history_state::in,history_state::out,io::di,io::uo)
     is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

    % XXX: doesn't work becuase history_state_t can't be exported.
    %
% :- pragma foreign_type("C",history_state,"history_state_t",
%     [can_pass_as_mercury_type,stable]).

:- type history_state == c_pointer.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C","
#ifdef CD_HAVE_READLINE

#include <readline/readline.h>
#include <readline/history.h>

typedef HISTORY_STATE *history_state_t;

extern char **completion(const char *,int,int);
extern char *completion_generator(const char *,int);

extern MR_Word completion_closure;
extern MR_Word completion_list;

#else       /* CD_HAVE_READLINE */

typedef MR_Word history_state_t;

#endif      /* CD_HAVE_READLINE */
").

:- pragma foreign_code("C","
#ifdef CD_HAVE_READLINE

MR_Word completion_closure = (MR_Word)NULL;
MR_Word completion_list    = (MR_Word)NULL;

char **completion(const char *prefix,int start,int end)
{
    completion_list =
        mercury_completion(completion_closure, (MR_String) prefix);

    if(MR_list_is_empty(completion_list))
        return NULL;

    return rl_completion_matches(prefix,completion_generator);
}

char *completion_generator(const char *prefix,int state)
{
    char *tmp, *cpy;
    
    do {
        if(MR_list_is_empty(completion_list))
            return NULL;

        tmp = (char *)MR_list_head(completion_list);
        completion_list = MR_list_tail(completion_list);
    
        /*
         * Don't bother displaying any non-printable strings.
         */
    } while(!isprint(tmp[0]));
    
    /*
     * We have to copy the string here, because libreadline free's it.
     */
    cpy = (char *)malloc(strlen(tmp)+1);
    strcpy(cpy,tmp);
    return cpy;
}

#endif      /* CD_HAVE_READLINE */
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    init = (State::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
#ifdef CD_HAVE_READLINE
    State = (MR_Word)malloc(sizeof(struct _hist_state));
    ((history_state_t)State)->entries = NULL;
    ((history_state_t)State)->offset  = 0;
    ((history_state_t)State)->length  = 0;
    ((history_state_t)State)->size    = 0;
    ((history_state_t)State)->flags   = 0;
#else
    State = (MR_Word)NULL;
#endif
").

%---------------------------------------------------------------------------%

read_line_as_string(Prompt,CompFunc,Result,!State,!IO) :-
    ( have_readline ->
        readline(Prompt,CompFunc,Str,!State,!IO),
        ( is_string_null_pointer(Str) ->
            Result = eof
        ;   Result = ok(Str)
        )
    ;   write_string(Prompt,!IO),
        io.read_line_as_string(Result,!IO)
    ).

%---------------------------------------------------------------------------%

:- func mercury_completion(completion_func::in(completion_func),string::in)
    = (list(string)::out) is det.

:- pragma foreign_export("C",mercury_completion(in(completion_func),in) = out,
    "mercury_completion").
mercury_completion(CompFunc,Prefix) = List :-
    List = CompFunc(Prefix).

%---------------------------------------------------------------------------%

:- pred readline(string::in,completion_func::in(completion_func),
    string::out,history_state::in,history_state::out,io::di,io::uo) is det.

:- pragma foreign_proc("C",
    readline(Prompt::in,CompFunc::in(completion_func),Str::out,State0::in,
        State1::out,_IO0::di,_IO1::uo),
        [may_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
#ifdef CD_HAVE_READLINE
    char *line;
    history_set_history_state((history_state_t)State0);
    completion_closure = CompFunc;
    rl_attempted_completion_function = completion;
    rl_completion_append_character = '\\0';
    line = readline(Prompt);
    if(line != NULL) {
        if(*line) {
            add_history(line);
            free((void *)State0);
            State1 = (MR_Word)history_get_history_state();
        } else
            State1 = State0;
        Str = MR_copy_string(line);
        free(line);
    } else {
        State1 = State0;
        Str = NULL;
    }
#endif
").

%---------------------------------------------------------------------------%

:- pred is_string_null_pointer(string::in) is semidet.

:- pragma foreign_proc("C",
    is_string_null_pointer(Str::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = (Str == NULL);
").

%---------------------------------------------------------------------------%

:- pred have_readline is semidet.

:- pragma foreign_proc("C",
    have_readline,
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
#ifdef CD_HAVE_READLINE
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%---------------------------------------------------------------------------%
:- end_module readline.
%---------------------------------------------------------------------------%
