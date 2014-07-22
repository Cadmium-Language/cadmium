%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%-----------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Cadmium trace debugger.
%
%-----------------------------------------------------------------------------%

:- module cadmium_debug.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module map.

:- import_module cadmium_common.
:- import_module hl_prog.
:- import_module ll_prog.
:- import_module model.

%-----------------------------------------------------------------------------%

:- type program_point(Sym)
    ---> call(Sym)
    ;    pass(rule_id)
    ;    fail(rule_id)
    ;    return(Sym,bool).

:- type var_reg_info(Reg) == map(string,Reg).
:- type var_reg_info == var_reg_info(register).

:- type debug_info(Sym,Reg) 
    ---> debug_info(Sym,program_point(Sym),cxt,var_reg_info(Reg)).

:- type debug_info == debug_info(symbol, register).

    % Do a debug step.
    %
:- impure pred debug_step(debug_info::in,io::di,io::uo) is det.

    % Rest the debugger.
    %
:- impure pred reset_debugger is det.
:- pred reset_debugger(io::di, io::uo) is det.

    % Set the debug pretty printer.
    %
:- pred set_debugger_pretty_printer(pred(bool,model,io,io),io,io).
:- mode set_debugger_pretty_printer(pred(in,in,di,uo) is det,di,uo) is det.

    % Set the debug break-point parser.
    %
:- pred set_debugger_break_point_parser(pred(string,string,int),io,io).
:- mode set_debugger_break_point_parser(pred(in,out,out) is semidet,di,uo) 
    is det.

    % Set the debugger in "silent" mode (i.e. no prompt).
    %
:- pred set_silent_mode(bool::in, io::di, io::uo) is det.

    % Set the debugger into a "step" state.
    %
:- pred set_debugger_step_state(io::di, io::uo) is det.

    % Print stats.
    %
:- pred write_stats(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module unit.

:- import_module machine.
:- import_module readline.

%---------------------------------------------------------------------------%

:- type step_state
    ---> continue
    ;    step.

:- type next_state
    ---> none
    ;    stack(int).

:- type program_point == program_point(symbol).

:- type break_points == map(program_point,unit).

:- type command
    ---> h
    ;    c
    ;    b(string)
    ;    d
    ;    s
    ;    n
    ;    v
    ;    p(string)
    ;    cc
    ;    a(string)
    ;    j(string)
    ;    i
    ;    t(string)
    ;    lc
    ;    sj(string)
    ;    rlc.

:- type stats 
    ---> stats(int,int).

:- type last_call ---> last_call(int, int).

%---------------------------------------------------------------------------%

:- mutable(break_points,break_points,init,ground,[untrailed]).

%---------------------------------------------------------------------------%

:- mutable(step_state,step_state,step,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(next_state,next_state,none,ground,[untrailed]).

%---------------------------------------------------------------------------%

:- mutable(last_call, last_call, last_call(0, 0), ground, [untrailed]).

%---------------------------------------------------------------------------%

:- mutable(pp_counter,int,0,ground,[untrailed]).

%---------------------------------------------------------------------------%

:- mutable(pp_counter_stop,int,-1,ground,[untrailed]).

%---------------------------------------------------------------------------%

:- mutable(print_annots,bool,no,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(print_trace,bool,no,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(history_state,history_state,init,ground,
    [untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(pprint_model,pred(bool,model,io,io),pprint_model_default,
    pred(in,in,di,uo) is det,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(break_point_parser,pred(string,string,int),parse_name_arity,
    pred(in,out,out) is semidet,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(stats_pass,int,0,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(stats_fail,int,0,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(silent,bool,no,ground,[untrailed,attach_to_io_state]).

%---------------------------------------------------------------------------%

:- mutable(stage_nb, int, 1, ground, [untrailed]).

%---------------------------------------------------------------------------%

:- mutable(stage_jump, maybe(int), no, ground, [untrailed]).

%---------------------------------------------------------------------------%
reset_debugger :-
    semipure get_stage_nb(StageNb),
    StageNb1 = StageNb + 1,
    impure set_stage_nb(StageNb1),
    impure set_break_points(init),
    semipure get_stage_jump(StageJump),
    ( if StageJump = yes(StageNb1) then
        impure set_step_state(step),
        impure set_stage_jump(no)
      else if StageJump = no then
        impure set_step_state(step)
      else
        impure set_step_state(continue)
    ),
    impure set_next_state(none),
    impure set_last_call(last_call(0, 0)),
    impure set_pp_counter(0),
    impure set_pp_counter_stop(-1),
    impure set_stats_pass(0),
    impure set_stats_fail(0).

reset_debugger(!IO) :-
    promise_pure (
        impure reset_debugger,
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

set_debugger_pretty_printer(PPrint,!IO) :-
    set_pprint_model(PPrint,!IO).

%---------------------------------------------------------------------------%

set_debugger_break_point_parser(BParser,!IO) :-
    set_break_point_parser(BParser,!IO).

%---------------------------------------------------------------------------%

set_debugger_step_state(!IO) :-
    set_step_state(step,!IO).

%---------------------------------------------------------------------------%

set_silent_mode(Silent,!IO) :-
    set_silent(Silent,!IO).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",debug_step(in,di,uo),"CR_debug").

debug_step(DI,!IO) :-
    DI = debug_info(Sym,PP,Cxt,_),
    ( PP = pass(_),
        semipure get_stats_pass(NumPass),
        impure set_stats_pass(NumPass+1)
    ; PP = fail(_),
        semipure get_stats_fail(NumFail),
        impure set_stats_fail(NumFail+1)
    ; ( PP = return(_,_)
      ; PP = call(_) ),
        true
    ),
    semipure get_pp_counter(PPC),
    PPC1 = PPC + 1,
    impure set_pp_counter(PPC1),
    semipure get_last_call(LC),
    LC = last_call(Level, Counter),
    ( if Level =< 0, PP = call(_) then
        impure set_last_call(last_call(1, PPC1))
      else if PP = call(_) then
        impure set_last_call(last_call(Level + 1, Counter))
      else if PP = return(_, _) then
        impure set_last_call(last_call(Level - 1, Counter))
      else true
    ),
    semipure get_step_state(FS),
    ( FS = step,
        semipure write_state_summary(Sym,PP,Cxt,!IO),
        impure debug_console(DI,!IO)
    ; FS = continue,
        semipure get_break_points(BPs),
        semipure get_next_state(NS0),
        ( semipure debug_stop(PP,PPC1,BPs,NS0) ->
            impure set_pp_counter_stop(-1),
            impure set_next_state(none),
            impure set_step_state(step),
            semipure write_state_summary(Sym,PP,Cxt,!IO),
            impure debug_console(DI,!IO)
        ;   update_next_state(PP,NS0,NS1),
            impure set_next_state(NS1),
            get_print_trace(PT,!IO),
            ( PT = yes,
                semipure write_state_summary(Sym,PP,Cxt,!IO)
            ; PT = no,
                true
            )
        )
    ).

%---------------------------------------------------------------------------%

:- semipure pred debug_stop(program_point::in,int::in,break_points::in,
    next_state::in) is semidet.

debug_stop(PP,PPC,BPs,NS) :-
    semipure get_pp_counter_stop(Stop),
    ( PPC = Stop ->
        true
    ; contains(BPs,PP) ->
        true
    ;   NS = stack(N),
        N =< 0,
        PP = return(_,_)
    ).

%---------------------------------------------------------------------------%

:- impure pred debug_console(debug_info::in,io::di,io::uo) is det.

debug_console(DI,!IO) :-
    get_history_state(State,!IO),
    get_silent(Silent,!IO),
    ( Silent = yes,
        Prompt = ""
    ; Silent = no,
        Prompt = "> "
    ),
    io.set_input_stream(io.stdin_stream, _, !IO),
    readline.read_line_as_string(Prompt,match_symbol_names,Res,State,NState,
        !IO),
    set_history_state(NState,!IO),
    ( Res = eof,
        Exit = construct("debugger_exit",[]),
        machine.throw(Exit)
    ; Res = error(Err),
        write_string(error_message(Err),!IO),
        nl(!IO),
        impure debug_console(DI,!IO)
    ; Res = ok(Line),
        Args = words(Line),
        DI = debug_info(_,PP,_,Vars),
        impure debug_command(Args,PP,Vars,Cont,!IO),
        ( Cont = yes,
            impure debug_console(DI,!IO)
        ; Cont = no,
            true
        )
    ).

%---------------------------------------------------------------------------%

:- impure pred debug_command(list(string)::in,program_point::in,
    var_reg_info::in,bool::out,io::di,io::uo) is det.

debug_command(Cmd0,PP,Vars,Cont,!IO) :-
    ( parse_command(Cmd0,Cmd1) ->
        impure debug_execute(Cmd1,PP,Vars,Cont,!IO)
    ;   write_string("Invalid command.\n",!IO),
        write_string("Try `h' for help.\n",!IO),
        Cont = yes
    ).

%---------------------------------------------------------------------------%

:- pred update_next_state(program_point::in,next_state::in,next_state::out)
    is det.

update_next_state(call(_),stack(N),stack(N+1)).
update_next_state(call(_),none,none).
update_next_state(pass(_),!NS).
update_next_state(fail(_),!NS).
update_next_state(return(_,_),stack(N),stack(N-1)).
update_next_state(return(_,_),none,none).

%---------------------------------------------------------------------------%

:- pred parse_command(list(string)::in,command::out) is semidet.

parse_command([],s).
parse_command(["c"],c).
parse_command(["continue"],c).
parse_command(["s"],s).
parse_command(["step"],s).
parse_command(["n"],n).
parse_command(["next"],n).
parse_command(["j",N],j(N)).
parse_command(["jump",N],j(N)).
parse_command(["b",Point],b(Point)).
parse_command(["break",Point],b(Point)).
parse_command(["d"],d).
parse_command(["delete"],d).
parse_command(["v"],v).
parse_command(["vars"],v).
parse_command(["p",Var],p(Var)).
parse_command(["print",Var],p(Var)).
parse_command(["cc"],cc).
parse_command(["h"],h).
parse_command(["help"],h).
parse_command(["a",Cmd],a(Cmd)).
parse_command(["annotations",Cmd],a(Cmd)).
parse_command(["i"],i).
parse_command(["info"],i).
parse_command(["t",Cmd],t(Cmd)).
parse_command(["trace",Cmd],t(Cmd)).
parse_command(["lc"], lc).
parse_command(["sj", N], sj(N)).
parse_command(["rlc"], rlc).

%---------------------------------------------------------------------------%

:- impure pred debug_execute(command::in,program_point::in,var_reg_info::in,
    bool::out,io::di,io::uo) is det.

debug_execute(h,_,_,yes,!IO) :-
    write_help(!IO).
debug_execute(c,_,_,no,!IO) :-
    impure set_step_state(continue).
debug_execute(b(Point),_,_,yes,!IO) :-
    get_break_point_parser(Parser,!IO),
    ( Parser(Point,Name,Aty) ->
        Sym = symbol(Name,Aty),
        ( is_proc(Sym,_) ->
            write_string("Break point on ",!IO),
            write_string(Point,!IO),
            write_string(".\n",!IO),
            semipure get_break_points(BPs0),
            map.set(call(Sym),unit,BPs0,BPs1),
            impure set_break_points(BPs1)
        ;   write_string("No such procedure ",!IO),
            write_name_arity(Name,Aty,!IO),
            write_string(".\n",!IO)
        )
    ;   write_string("Expected functor/arity pair, found ",!IO),
        write_string(Point,!IO),
        write_string(".\n",!IO)
    ).
debug_execute(d,_,_,yes,!IO) :-
    write_string("Deleting all breakpoints.\n",!IO),
    impure set_break_points(init).
debug_execute(j(NStr),_,_,Cont,!IO) :-
    ( string.to_int(NStr,N),
      N > 0 ->
        semipure get_pp_counter(PPC),
        Stop = PPC + N,
        impure set_pp_counter_stop(Stop),
        impure set_step_state(continue),
        Cont = no
    ;   write_string("Expected a positive integer, found ",!IO),
        write_string(NStr,!IO),
        write_string(".\n",!IO),
        Cont = yes
    ).
debug_execute(s,_,_,no,!IO).
debug_execute(n,PP,_,no,!IO) :-
        % "next" is the same as "step" if the program point is a return.
        %
    ( PP = return(_,_) ->
        true
    ;   impure set_step_state(continue),
        impure set_next_state(stack(0))
    ).
debug_execute(v,_,VRI,yes,!IO) :-
    foldl(write_var,VRI,!IO).
debug_execute(cc,_,_,yes,!IO) :-
    semipure lookup_cc(CC),
    cadmium_debug.write_cc(CC,!IO).
debug_execute(p(Name),_,VRI,yes,!IO) :-
    ( search(VRI,Name,Reg) ->
        write_var(Name,Reg,!IO)
    ;   write_string("No such variable `",!IO),
        write_string(Name,!IO),
        write_string("'.\n",!IO),
        write_string("Try `v' to list all variables.\n",!IO)
    ).
debug_execute(a(Cmd),_,_,yes,!IO) :-
    ( Cmd = "on" ->
        set_print_annots(yes,!IO)
    ; Cmd = "off" ->
        set_print_annots(no,!IO)
    ;   write_string("Expected `on' or `off', found ",!IO),
        write_string(Cmd,!IO),
        nl(!IO)
    ).
debug_execute(i,_,_,yes,!IO) :-
    write_stats(!IO).
debug_execute(t(Cmd),_,_,yes,!IO) :-
    ( Cmd = "on" ->
        set_print_trace(yes,!IO)
    ; Cmd = "off" ->
        set_print_trace(no,!IO)
    ;   write_string("Expected `on' or `off', found ",!IO),
        write_string(Cmd,!IO),
        nl(!IO)
    ).
debug_execute(lc, _, _, yes, !IO) :-
    semipure get_last_call(last_call(_, Counter)),
    write_int(Counter, !IO),
    nl(!IO).
debug_execute(sj(NStr), _, _, Cont, !IO) :-
    semipure get_stage_nb(StageNb),
    ( if string.to_int(NStr, N), N > StageNb then
        impure set_stage_jump(yes(N)),
        impure set_step_state(continue),
        Cont = no
      else
        write_string("Expected an integer larger than current stage, found ",
            !IO),
        write_string(NStr, !IO),
        write_string(".\n", !IO),
        Cont = yes
    ).
debug_execute(rlc, _, _, yes, !IO) :-
    semipure get_pp_counter(PPC),
    impure set_last_call(last_call(1, PPC)).

%---------------------------------------------------------------------------%

:- pred write_help(io::di,io::uo) is det.

write_help(!IO) :-
    nl(!IO),
    write_string("Cd Debugger Help:\n\n",!IO),
    write_string("\ta <on or off>, annotations <on or off>\n",!IO),
    write_string("\t\tShow annotations when printing expressions.\n",!IO),
    write_string("\tb <functor/arity>, break <functor/arity>\n",!IO),
    write_string("\t\tSet a breakpoint on functor/arity.\n",!IO),
    write_string("\tc, continue\n",!IO),
    write_string("\t\tContinue executing until the next break point.\n",!IO),
    write_string("\tcc\n",!IO),
    write_string("\t\tPrint the current Conjunctive Context (CC).\n",!IO),
    write_string("\td, delete\n",!IO),
    write_string("\t\tDelete all breakpoints.\n",!IO),
    write_string("\tj <N>, jump <N>\n",!IO),
    write_string("\t\tJump N program-points.\n",!IO), 
    write_string("\ts, step, \\r\n",!IO),
    write_string("\t\tSingle-step to the next program-point.\n",!IO),
    write_string("\tn, next\n",!IO),
    write_string("\t\tContinue executing until the current call returns.\n",
        !IO),
    write_string("\tsj <N>\n", !IO),
    write_string("\t\tContinue executing until the start of the N-th stage.\n",
        !IO),
    write_string("\tlc\n", !IO),
    write_string("\t\tPrint the program counter value at the most recent\n",
        !IO),
    write_string("\t\tleast-depth call since the beginning of the current\n",
        !IO),
    write_string("\t\tstage or since resetting this value using rlc.\n", !IO),
    write_string("\trlc\n", !IO),
    write_string("\t\tReset the value returned by lc so that subsequent\n",
        !IO),
    write_string("\t\tcalls to it return a value relative to the current\n",
        !IO),
    write_string("\t\tprogram counter value.\n", !IO),
    write_string("\tt <on or off>, trace <on or off>\n",!IO),
    write_string("\t\tPrint trace during continue mode.\n",!IO),
    write_string("\tv, vars\n",!IO),
    write_string("\t\tPrint all current variables.\n",!IO),
    write_string("\tp <var>, print <var>\n",!IO),
    write_string("\t\tPrint a variable.\n",!IO),
    write_string("\th, help\n",!IO),
    write_string("\t\tPrint this help message.\n",!IO),
    nl(!IO).

%---------------------------------------------------------------------------%

:- pred write_name_arity(string::in,int::in,io::di,io::uo) is det.

write_name_arity(Name,Aty,!IO) :-
    write_string(Name,!IO),
    write_char('/',!IO),
    write_int(Aty,!IO).

%---------------------------------------------------------------------------%

:- pred write_var(string::in,register::in,io::di,io::uo) is det.

write_var(Name,Reg,!IO) :-
    write_string(Name,!IO),
    write_string(" = ",!IO),
    promise_pure semipure lookup_register_value(Reg,Val),
    write_model_maybe_annots(Val,!IO),
    nl(!IO).

%---------------------------------------------------------------------------%

:- semipure pred write_state_summary(symbol::in,program_point::in,cxt::in,
    io::di,io::uo) is det.

write_state_summary(Sym,PP,Cxt,!IO) :-
    semipure get_pp_counter(PPC),
    write_int(PPC,!IO),
    write_char(' ',!IO),
    write_program_point(PP,!IO),
    write_char(' ',!IO),
    ( PP = return(_,yes) ->
        semipure lookup_stack_value(0,Val),
        write_model_maybe_annots(Val,!IO)
    ;   semipure write_goal(Sym,!IO)
    ),
    write_char(' ',!IO),
    write_location(Cxt,!IO),
    nl(!IO).

%---------------------------------------------------------------------------%

:- pred write_cc(list(model)::in,io::di,io::uo) is det.

write_cc([],!IO) :-
    nl(!IO).
write_cc([Model|CC],!IO) :-
    write_model_maybe_annots(Model,!IO),
    ( CC = [] ->
        nl(!IO)
    ;   write_string(" /\\\n",!IO),
        cadmium_debug.write_cc(CC,!IO)
    ).

%---------------------------------------------------------------------------%

:- semipure pred write_goal(symbol::in,io::di,io::uo) is det.

write_goal(Sym,!IO) :-
    ( ( is_ac(Sym)
      ; Sym = var_symbol ) ->
        semipure lookup_stack_value(0,Val)
    ;   semipure lookup_stack_model(Sym,Val)
    ),
    write_model_maybe_annots(Val,!IO).

%---------------------------------------------------------------------------%

:- pred write_annotation(symbol::in,model::in,io::di,io::uo) is det.

write_annotation(_,Model,!IO) :-
    write_string("::",!IO),
    write_model_maybe_annots(Model,!IO).

%---------------------------------------------------------------------------%

:- semipure pred write_goal_args(int::in,int::in,io::di,io::uo) is det.

write_goal_args(N,M,!IO) :-
    ( N > M ->
        true
    ;   semipure lookup_stack_value(M-N,Val),
        write_model_maybe_annots(Val,!IO),
        NN = N + 1,
        ( NN > M ->
            true
        ;   write_char(',',!IO),
            semipure write_goal_args(NN,M,!IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred write_program_point(program_point::in,io::di,io::uo) is det.

write_program_point(call(_),!IO) :-
    write_string("CALL",!IO).
write_program_point(pass(_),!IO) :-
    write_string("PASS",!IO).
write_program_point(fail(_),!IO) :-
    write_string("FAIL",!IO).
write_program_point(return(_,_),!IO) :-
    write_string("RETURN",!IO).

%---------------------------------------------------------------------------%

:- pred write_location(cxt::in,io::di,io::uo) is det.

write_location(none,!IO) :-
    write_string("?",!IO).
write_location(cxt(File,Line),!IO) :-
    write_string(File,!IO),
    write_char(':',!IO),
    write_int(Line,!IO).

%---------------------------------------------------------------------------%

:- pred write_model_maybe_annots(model::in,io::di,io::uo) is det.

write_model_maybe_annots(Model,!IO) :-
    get_print_annots(PrintAnnots,!IO),
    get_pprint_model(PPrint,!IO),
    PPrint(PrintAnnots,Model,!IO).

%---------------------------------------------------------------------------%

:- pragma promise_pure(write_stats/2).

write_stats(!IO) :-
    get_stats_pass(NumPass,!IO),
    get_stats_fail(NumFail,!IO),
    semipure get_stage_nb(StageNb),
    io.format("\n%%%%----  End of stage %d (%d passes, %d fails)  ----%%%%\n\n",
        [i(StageNb), i(NumPass), i(NumFail)], !IO).

%---------------------------------------------------------------------------%

:- pred pprint_model_default(bool::in,model::in,io::di,io::uo) is det.

pprint_model_default(PrintAnnots,Model,!IO) :-
    ( PrintAnnots = yes,
        write_model_with_annotations(Model,!IO)
    ; PrintAnnots = no,
        write_model(Model,!IO)
    ).

%---------------------------------------------------------------------------%
:- end_module cadmium_debug.
%---------------------------------------------------------------------------%
