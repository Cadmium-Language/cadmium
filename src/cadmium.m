%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% The top-level of the Cadmium interpreter.
%
%---------------------------------------------------------------------------%

:- module cadmium.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cadmium_common.
:- import_module cadmium_debug.
:- import_module cadmium_error.
:- import_module cadmium_make.
:- import_module console.
:- import_module documentation.
:- import_module ll_prog.
:- import_module machine.
:- import_module model.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module unit.
:- import_module varset.

:- type option
    ---> help
    ;    version
    ;    search_dir
    ;    silent
    ;    debug
    ;    compile
    ;    install_prefix.

:- type option_table == option_table(option).

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        short_option_0,
        long_option_0,
        option_defaults
    ),
    process_options(OptionOps, Args, NonOptionArgs, Result),
    (
        Result = error(OptionError),
        ErrorMsg = option_error_to_string(OptionError),
        Error = message(ErrorMsg, [], none),
        print_errors([Error], !IO)
    ;
        Result = ok(Options),
        lookup_bool_option(Options, help, Help),
        (
            Help = yes,
            help(!IO)
        ;
            Help = no,
            lookup_bool_option(Options, version, Version),
            (
                Version = yes,
                version(!IO)
            ;
                Version = no,
                machine.startup(!IO),
                init_console(Options, !IO),
                lookup_bool_option(Options, silent, Silent),
                lookup_bool_option(Options, compile, Compile),
                cadmium_debug.set_silent_mode(Silent, !IO),
                ( if
                    Silent = no,
                    Compile = no
                then
                    print_banner(!IO)
                else
                    true
                ),
                append(NonOptionArgs, ["prelude"], Names),
                load_action(Options, no, Names, !IO),
                (
                    Compile = no,
                    console(Silent, !IO)
                ;
                    Compile = yes
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

:- func version_number = float.

version_number = 1.0.

%---------------------------------------------------------------------------%

:- pred usage(text_output_stream::in, io::di, io::uo) is det.

usage(Stream, !IO) :-
    progname_base("cadmium", Prog, !IO),
    write_string(Stream, "usage: ", !IO),
    write_string(Stream, Prog, !IO),
    write_string(Stream, " [OPTIONS] [file1.acd [file2.acd ...]]\n", !IO),
    set_exit_status(1, !IO).

%---------------------------------------------------------------------------%

:- pred help(io::di,io::uo) is det.

help(!IO) :-
    version(!IO),
    nl(!IO),
    usage(stdout_stream, !IO),
    nl(!IO),
    write_string("OPTIONS:\n\n", !IO),
    Options0 = [
        help,
        version,
        search_dir,
        install_prefix,
        debug,
        silent,
        compile
    ],
    sort(option_compare, Options0, Options),
    foldl(option_help, Options, !IO),
    nl(!IO).

%---------------------------------------------------------------------------%

:- pred option_compare(option::in, option::in, comparison_result::out)
    is det.

option_compare(Option1, Option2, Result) :-
    long_option(Name1, Option1),
    long_option(Name2, Option2),
    compare(Result, Name1, Name2).

%---------------------------------------------------------------------------%

:- pred version(io::di,io::uo) is det.

version(!IO) :-
    write_string("Cadmium, version ",!IO),
    write_float(version_number,!IO),
    nl(!IO),
    copyright("", !IO).

%---------------------------------------------------------------------------%

:- pred copyright(string::in, io::di, io::uo) is det.

copyright(LinePrefix, !IO) :-
    format("%sCopyright (C) 2013-2014 Opturion Pty Ltd.\n", [s(LinePrefix)],
        !IO),
    format("%sCopyright (C) 2005-2010 NICTA.\n", [s(LinePrefix)], !IO).

%---------------------------------------------------------------------------%

:- pred init_console(option_table::in,io::di,io::uo) is det.

init_console(Options,!IO) :-
    set_console_handler(handler(Options,no,init),!IO),
    lookup_bool_option(Options,debug,Debug),
    set_console_execute(execute_goal(Debug,no),!IO),
    set_console_prompt(default_prompt,!IO).

%---------------------------------------------------------------------------%

:- pred execute_goal(bool::in,bool::in,term::in,varset::in,io::di,io::uo) 
    is det.

execute_goal(Debug, HaveProg, Term, VarSet, !IO) :-
    Goal = term_to_model(VarSet, Term),
    (
        HaveProg = yes,
        machine.interpret_main(Goal, Result, !IO),
        (
            Result = interpret_ok(Res),
            write_model_with_annotations(Res,!IO),
            write_string(".\n",!IO)
        ;
            Result = interpret_exception(Err),
            io.write_string("*** exception: ",!IO),
            write_model(Err,!IO),
            io.nl(!IO)
        ),
        (
            Debug = yes,
            write_stats(!IO)
        ;
            Debug = no
        ),
        machine.reset(!IO),
        cadmium_debug.reset_debugger(!IO)
    ;
        HaveProg = no,
        write_model_with_annotations(Goal,!IO),
        io.write_string(".\n",!IO)
    ).

%---------------------------------------------------------------------------%

:- type command
    ---> help
    ;    doc_help
    ;    doc_help_all
    ;    load.

:- pred handler(option_table::in,maybe(ll_prog)::in,doc_info::in,string::in,
    io::di,io::uo) is det.

handler(Options,MaybeLLProg,DocInfo,Str,!IO) :-
    Words = string.words(Str),
    ( if
        Words = [CommStr | Args],
        string_to_command(CommStr,length(Args),Comm)
    then
        execute_command(Options,MaybeLLProg,DocInfo,Comm,Args,!IO)
    else
        default_action(!IO)
    ).

%---------------------------------------------------------------------------%

:- pred string_to_command(string::in,int::in,command::out) is semidet.

string_to_command("?",0,help).
string_to_command("h",1,doc_help).
string_to_command("help",1,doc_help).
string_to_command("h",0,doc_help_all).
string_to_command("help",0,doc_help_all).
string_to_command("l",1,load).
string_to_command("load",1,load).

%---------------------------------------------------------------------------%

:- pred execute_command(option_table::in,maybe(ll_prog)::in,doc_info::in,
    command::in,list(string)::in,io::di,io::uo) is det.

execute_command(_,_,_,help,_,!IO) :-
    help_action(!IO).
execute_command(_,_,DocInfo,doc_help,Args,!IO) :-
    Arg = det_head(Args),
    doc_query(DocInfo,Arg,!IO).
execute_command(_,_,DocInfo,doc_help_all,_,!IO) :-
    doc_query_all(DocInfo,!IO).
execute_command(Options,MaybeLLProg,_,load,Args,!IO) :-
    load_action(Options,MaybeLLProg,Args,!IO).

%---------------------------------------------------------------------------%

:- pred load_action(option_table::in,maybe(ll_prog)::in,list(string)::in,
    io::di,io::uo) is det.

load_action(Options,MaybeLLProg,Names,!IO) :-
    catch_cadmium_errors(load_action_2(Options,MaybeLLProg,Names),Result,!IO),
    (
        Result = cd_errors(Errs,Warns),
        print_warnings(Warns,!IO),
        print_errors(Errs,!IO),
        init_console(Options,!IO)
    ;
        Result = cd_success(_,Warns),
        print_warnings(Warns,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred load_action_2(option_table::in,maybe(ll_prog)::in,list(string)::in,
    unit::out,io::di,io::uo) is det.

load_action_2(Options,MaybeOldLLProg,Names,unit,!IO) :-
    reset_model(!IO),
    (
        MaybeOldLLProg = yes(OldLLProg),
         disassemble(OldLLProg, !IO)
    ;
        MaybeOldLLProg = no
    ),
    lookup_accumulating_option(Options,search_dir,Dirs),
    lookup_bool_option(Options,debug,Debug),
    lookup_string_option(Options,install_prefix,InstallDir),
    MakeRuleACD2CDO   = make_rule(".acd",".cdo",compile_acd_file(Debug),
        load_cdo_file(Debug)),
    MakeRuleACD2CDDOC = make_rule(".acd",".cddoc",compile_acd_to_cddoc,
        load_cddoc_file),
    MakeRules = [MakeRuleACD2CDO,MakeRuleACD2CDDOC],
    Name = det_head(Names),
    make_target_name(Name,BaseName,UseSubDirs,Dirs,SrcDirs,Dirs,TgtDirs),
    Target = BaseName ++ ".cdo",
    DocTarget = BaseName ++ ".cddoc",
    make(MakeRules,InstallDir,SrcDirs,TgtDirs,UseSubDirs,Target,Result,init,_,
        !IO),
    make(MakeRules,InstallDir,SrcDirs,TgtDirs,UseSubDirs,DocTarget,DocResult,
        init,_,!IO),
    ( if
        Result = cdo(LLProg0,Imports),
        DocResult = cddoc(DocInfo0)
    then
        Seen0 = make_singleton_set(BaseName),
        ImportsLs = append(Names,to_sorted_list(Imports)),
        foldl5(load_module(MakeRules,InstallDir,Dirs),ImportsLs,Seen0,_,1,_,
            LLProg0,LLProg,DocInfo0,DocInfo,!IO),
        machine.assemble(Debug, LLProg, !IO),
        throw_errors(!IO),
        set_console_prompt(BaseName,!IO),
        set_console_handler(handler(Options,yes(LLProg),DocInfo),!IO),
        set_console_execute(execute_goal(Debug,yes),!IO)
    else
        unexpected($file, $pred, "unexpected make result")
    ).

%---------------------------------------------------------------------------%

:- pred load_module(make_rules(make_result)::in(make_rules),string::in,
    list(string)::in,string::in,set(string)::in,set(string)::out,
    ll_prog_id::in,ll_prog_id::out,ll_prog::in,ll_prog::out,doc_info::in,
    doc_info::out,io::di,io::uo) is det.

load_module(MakeRules,InstallDir,Dirs,Name,!Seen,!LLProgId,!LLProg,!DocInfo,
        !IO) :-
    make_target_name(Name,BaseName,UseSubDirs,Dirs,SrcDirs,Dirs,TgtDirs),
    ( if contains(!.Seen,BaseName) then
        true
    else
        Target = BaseName ++ ".cdo",
        DocTarget = BaseName ++ ".cddoc",
        set.insert(BaseName,!Seen),
        make(MakeRules,InstallDir,SrcDirs,TgtDirs,UseSubDirs,Target,Result,
            init,_,!IO),
        make(MakeRules,InstallDir,SrcDirs,TgtDirs,UseSubDirs,DocTarget,
            DocResult,init,_,!IO),
        ( if
            Result = cdo(LLProg0,Imports),
            DocResult = cddoc(DocInfo0)
        then
            overlay(DocInfo0,!DocInfo),
            ll_prog_link(!.LLProgId,LLProg0,!LLProg),
            !:LLProgId = !.LLProgId + 1,
            foldl5(load_module(MakeRules,InstallDir,Dirs),
                to_sorted_list(Imports),!Seen,!LLProgId,!LLProg,!DocInfo,!IO)
        else
            unexpected($file, $pred, "unexpected make result")
        )
    ).

%---------------------------------------------------------------------------%

:- pred make_target_name(string::in,string::out,bool::out,list(string)::in,
    list(string)::out,list(string)::in,list(string)::out) is det.

make_target_name(Name,BaseName,UseSubDirs,!SrcDirs,!TgtDirs) :-
    some [!BaseName] (
        !:BaseName = Name,
        ( split_name(!.BaseName,DirName,!:BaseName) ->
            ( remove_suffix(!.BaseName,".acd",!:BaseName) ->
                BaseName = !.BaseName,
                UseSubDirs = yes,
                !:SrcDirs = [DirName],
                !:TgtDirs = [DirName,this_directory]
            ; remove_suffix(!.BaseName,".cdo",!:BaseName) ->
                BaseName = !.BaseName,
                UseSubDirs = no,
                !:SrcDirs = [],
                !:TgtDirs = [DirName]
            ;   BaseName = !.BaseName,
                UseSubDirs = yes,
                !:SrcDirs = [DirName],
                !:TgtDirs = [DirName,this_directory]
            )
        ;   ( remove_suffix(!.BaseName,".acd",!:BaseName) ->
                BaseName = !.BaseName,
                UseSubDirs = yes
            ; remove_suffix(!.BaseName,".cdo",!:BaseName) ->
                BaseName = !.BaseName,
                !:SrcDirs = [],
                UseSubDirs = yes
            ;   BaseName = !.BaseName,
                UseSubDirs = yes
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred help_action(io::di,io::uo) is det.

help_action(!IO) :-
    write_string("COMMANDS:\n",!IO),
    write_string("    :?\n",!IO),
    write_string("        Print this help message.\n",!IO),
    write_string("    :h <name>/<arity>, :help <name>/<arity>\n",!IO),
    write_string("        Print any available help for <name>/<arity>.\n",!IO),
    write_string("    :h, :help\n",!IO),
    write_string("        Print all available help.\n",!IO),
    write_string("    :l <file1> [<file2> ...], :load <file1> [<file2> ...]\n",
        !IO),
    write_string("        Load map(s) <file1>, <file2>, ...\n",!IO).

%---------------------------------------------------------------------------%

:- pred default_action(io::di,io::uo) is det.

default_action(!IO) :-
    write_string("Type :? for help.\n",!IO).

%---------------------------------------------------------------------------%

:- pred option_help(option::in,io::di,io::uo) is det.

option_help(Option,!IO) :-
    long_option(LongName,Option),
    write_string("--",!IO),
    write_string(LongName,!IO),
    write_char(' ',!IO),
    write_option_arg(Option,!IO),
    ( short_option(ShortName,Option) ->
        write_string(", -",!IO),
        write_char(ShortName,!IO),
        write_char(' ',!IO),
        write_option_arg(Option,!IO)
    ;   true
    ),
    nl(!IO),
    write_option_text(Option,!IO).

%---------------------------------------------------------------------------%

:- pred write_option_arg(option::in,io::di,io::uo) is det.

write_option_arg(help,!IO).
write_option_arg(version,!IO).
write_option_arg(search_dir,!IO) :-
    write_string("<path>",!IO).
write_option_arg(install_prefix,!IO) :-
    write_string("<path>",!IO).
write_option_arg(silent,!IO).
write_option_arg(debug,!IO).
write_option_arg(compile,!IO).

%---------------------------------------------------------------------------%

:- pred write_option_text(option::in,io::di,io::uo) is det.

write_option_text(help,!IO) :-
    write_text("Print this help message",!IO).
write_option_text(version,!IO) :-
    write_text("Print version information",!IO).
write_option_text(search_dir,!IO) :-
    write_text("Add <path> to the list of directories to be searched for " ++
        "imported modules",!IO).
write_option_text(install_prefix,!IO) :-
    write_text("Specify the directory where the Cadmium installs " ++
        "automatically generated .cdo and .cddoc files",!IO).
write_option_text(silent,!IO) :-
    write_text("Suppress all output except compiler warnings/errors and " ++
        "answers to queries",!IO).
write_option_text(debug,!IO) :-
    write_text("Enable the Cadmium trace debugger when executing goals",!IO).
write_option_text(compile,!IO) :-
    write_text("Compile only, do not enter interactive prompt",!IO).

%---------------------------------------------------------------------------%

:- pred short_option_0(char::in,option::out) is semidet.
:- pred long_option_0(string::in,option::out) is semidet.

short_option_0(Char,Option) :-
    short_option(Char,Option).
long_option_0(Str,Option) :-
    long_option(Str,Option).

%---------------------------------------------------------------------------%

:- pred short_option(char,option).
:- mode short_option(in,out) is semidet.
:- mode short_option(out,in) is semidet.

short_option('I',search_dir).

%---------------------------------------------------------------------------%

:- pred long_option(string,option).
:- mode long_option(in,out) is semidet.
:- mode long_option(out,in) is det.

long_option("help",help).
long_option("version",version).
long_option("search-dir",search_dir).
long_option("install-prefix",install_prefix).
long_option("silent",silent).
long_option("debug",debug).
long_option("compile",compile).

%---------------------------------------------------------------------------%

:- pred option_defaults(option::out,option_data::out) is multi.

option_defaults(help,bool(no)).
option_defaults(version,bool(no)).
option_defaults(search_dir,accumulating([this_directory])).
option_defaults(install_prefix,string(this_directory)).
option_defaults(silent,bool(no)).
option_defaults(debug,bool(no)).
option_defaults(compile,bool(no)).

%---------------------------------------------------------------------------%

:- pred print_banner(io::di,io::uo) is det.

print_banner(!IO) :-
    write_string("\033\[1;91m",!IO),
    write_string("               _           _\n",!IO),
    write_string("  ___ __ _  __| |_ __ ___ (_)_   _ _ __ ___\n",!IO),
    write_string(" / __/ _` |/ _` | '_ ` _ \\| | | | | '_ ` _ \\\n",!IO),
    write_string("| (_| (_| | (_| | | | | | | | |_| | | | | | |\n",!IO),
    write_string(" \\___\\__,_|\\__,_|_| |_| |_|_|\\__,_|_| |_| |_|\n",!IO),
    nl(!IO),
    write_string("\033\[0;92m",!IO),
    write_string("\tVersion: ",!IO),
    write_float(version_number,!IO),
    nl(!IO),
    copyright("\t", !IO),
    nl(!IO),
    write_string("\033\[96m",!IO),
    write_string("Type :? for help.\n\n",!IO),
    write_string("\033\[0m",!IO).

%---------------------------------------------------------------------------%
:- end_module cadmium.
%---------------------------------------------------------------------------%
