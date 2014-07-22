%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%-----------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% This module contains the Cadmium make system.
%
%-----------------------------------------------------------------------------%

:- module cadmium_make.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- import_module documentation.
:- import_module ll_prog.

%-----------------------------------------------------------------------------%

:- type make_result
    --->    cdo(ll_prog, set(string))
    ;       cddoc(doc_info).

:- type load_action(T) == pred(string,maybe(T),io,io).
:- inst load_action    == (pred(in,out,di,uo) is det).

:- type compile_action(T) == pred(string,string,maybe(T),io,io).
:- inst compile_action    == (pred(in,in,out,di,uo) is det).

:- type make_rule(T) 
    ---> make_rule(string, string, compile_action(T), load_action(T)).
:- inst make_rule 
    ---> make_rule(ground, ground, compile_action, load_action).

:- type make_rules(T) == list(make_rule(T)).
:- inst make_rules    == list(make_rule).

    % XXX this should be abstract.
    %
:- type make_cache(T) == map(string, T).

%-----------------------------------------------------------------------------%

    % Directory where all .cdo files are stored.
    %
:- func cadmium_dir = string.

    % make(Rules, InstallDir, SrcDirs, TgtDirs, UseSubDirs, Target, Result,
    %   !IO):
    %
:- pred make(make_rules(T)::in(make_rules),string::in,list(string)::in,
    list(string)::in,bool::in,string::in,T::out,make_cache(T)::in,
    make_cache(T)::out,io::di,io::uo) is det.

    % Load action for .cdo files.
    %
:- pred load_cdo_file(bool::in,string::in,maybe(make_result)::out,
    io::di,io::uo) is det.

    % Load action for .cddoc files.
    %
:- pred load_cddoc_file(string::in,maybe(make_result)::out,io::di,io::uo) 
    is det.

    % Compile action for .acd files.
    %
:- pred compile_acd_file(bool::in,string::in,string::in,
    maybe(make_result)::out,io::di,io::uo) is det.

    % Compile action for .acd to .cddoc
    %
:- pred compile_acd_to_cddoc(string::in,string::in,maybe(make_result)::out,
    io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module cadmium_common.
:- import_module compile.
:- import_module cadmium_error.
:- import_module hl_prog.
:- import_module parse.

%---------------------------------------------------------------------------%

cadmium_dir = "Cadmium".

%---------------------------------------------------------------------------%

make(Rules, InstallDir, SrcDirs, TgtDirs, UseSubDirs, Target, Result, !Cache,
        !IO) :-
    ( if map.search(!.Cache, Target, Result0) then
        Result = Result0
    else
        make(Rules, [], InstallDir, SrcDirs, TgtDirs, UseSubDirs, Target,
            Result, !IO),
        map.det_insert(Target,Result,!Cache)
    ).

%---------------------------------------------------------------------------%

:- pred make(make_rules(T)::in(make_rules), list(string)::in, string::in,
    list(string)::in, list(string)::in, bool::in, string::in,
    T::out, io::di, io::uo) is det.

make(Rules0, Seen, InstallDir, SrcDirs, TgtDirs, UseSubDirs, Target,
        Result, !IO) :-
    (
        Rules0 = [],
        (
            Seen = [],
            Message = message("no rule to make target %s", [s(Target)], none)
        ;
            Seen = [_ | _],
            strings_to_comma_string(Seen, SeenStr),
            Message = message(
                "failed to make target %s, no candidate (%s) file",
                [s(Target), s(SeenStr)], none)
        ),
        throw_compiler_error(Message,!IO)
    ;
        Rules0 = [Rule | Rules],
        Rule = make_rule(SrcExt, DstExt, Compile, Load),
        ( if target_matches(Target, DstExt, BaseName) then 
            Source = BaseName ++ SrcExt,
            search_for_file(SrcDirs, no, Source, MaybeSrc, !IO),
            (
                UseSubDirs = yes,
                ( if remove_prefix(".", DstExt, DstExtSuff)
                then SubDir = DstExtSuff ++ "s"
                else error($pred ++ ": expected '.' prefix to filename extension")
                ),
                MaybeSubDir = yes(SubDir)
            ;
                UseSubDirs = no,
                MaybeSubDir = no
            ),
            search_for_file(TgtDirs, MaybeSubDir, Target, MaybeTgt, !IO),
            (
                MaybeTgt = yes(DirTarget),
                (
                    MaybeSrc = yes(DirSource),
                    % Both the target and source are available.
                    % Compile if source is newer, other simply load the
                    % target.
                    %
                    file_modification_time(DirSource, SrcTimeRes, !IO),
                    file_modification_time(DirTarget, TgtTimeRes, !IO),
                    ( if
                        SrcTimeRes = ok(SrcTime),
                        TgtTimeRes = ok(TgtTime)
                    then
                        compare(ResTime, TgtTime, SrcTime),
                        (
                            ResTime = (>),
                            % The target is up-to-date.
                            Load(DirTarget,LoadResult,!IO),
                            (
                                LoadResult = yes(Result0),
                                Result = Result0
                            ;
                                LoadResult = no,
                                % The load failed, so we compile instead.
                                compile_action(Compile, DirSource, Target,
                                    InstallDir, MaybeSubDir, Result, !IO)
                            )
                        ;       
                            ( ResTime = (=)
                            ; ResTime = (<)
                            ),
                            % The target is out-of-date, so compile.
                            compile_action(Compile, DirSource, Target, 
                                InstallDir, MaybeSubDir, Result, !IO)
                        )
                      
                    else 
                        unexpected($file, $pred, "failed to get file timestamps")
                    )
                ;
                    MaybeSrc = no,
                    % The source is unavailable, but we have the target.
                    % We assume that the target is up-to-date and simply load
                    % it.
                    Load(DirTarget,LoadResult,!IO),
                    (
                        LoadResult = yes(Result0),
                        Result = Result0
                    ;
                        LoadResult = no,
                        % Load failed, but other rules may work.
                        make(Rules, [SrcExt | Seen], InstallDir, SrcDirs,
                            TgtDirs, UseSubDirs, Target, Result, !IO)
                    )
                )
            ;
                MaybeTgt = no,
                (
                    % The target is unavailable, but we have the source.
                    % Therefore we compile the source.
                    %
                    MaybeSrc = yes(DirSource),
                    compile_action(Compile, DirSource, Target, InstallDir,
                        MaybeSubDir, Result, !IO)
                ;
                    % This make rule failed because both the target is not
                    % already made, and the source doesn't exist.  Therefore
                    % we try the other make rules.
                    %
                    MaybeSrc = no,
                    make(Rules, [SrcExt | Seen], InstallDir, SrcDirs, TgtDirs,
                        UseSubDirs, Target, Result, !IO)
                )
            )
        else
            % This make rule failed becuase the target does not agree with the
            % rule's target.
            make(Rules, Seen, InstallDir, SrcDirs, TgtDirs, UseSubDirs,
                Target, Result, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred compile_action(compile_action(T)::in(compile_action), string::in,
    string::in, string::in, maybe(string)::in, T::out, io::di, io::uo) is det.

compile_action(Compile, Source, Target, InstallDir, MaybeSubDir, Result,
        !IO) :-
    (
        MaybeSubDir = yes(SubDir),
        FullSubDir = InstallDir / cadmium_dir / SubDir,
        make_directory(FullSubDir, CdDirRes, !IO),
        (
            CdDirRes = ok,
            NTarget = FullSubDir / Target
        ;
            CdDirRes = error(Error),
            Message = message("failed to create directory ""%s"": %s",
                [s(cadmium_dir), s(error_message(Error))], none),
            throw_compiler_error(Message,!IO)
        )
    ;
        MaybeSubDir = no,
        NTarget = Target
    ),
    Compile(Source, NTarget, MaybeResult, !IO),
    (
        MaybeResult = yes(Result0),
        Result = Result0
    ;
        MaybeResult = no,
        Message = message("failed to make target %s", [s(Target)], none),
        throw_compiler_error(Message,!IO)
    ).

%-----------------------------------------------------------------------------%

:- pred search_for_file(list(string)::in, maybe(string)::in,
    string::in, maybe(string)::out, io::di, io::uo) is det.

search_for_file(Dirs0, MaybeSubDir, File, Result, !IO) :-
    (
        Dirs0 = [],
        Result = no
    ;
        Dirs0 = [Dir | Dirs],
        Reading = [read],
        (
            MaybeSubDir = yes(SubDir),
            DirFile = Dir / cadmium_dir / SubDir / File
        ;
            MaybeSubDir = no,
            DirFile = Dir / File
        ),
        io.check_file_accessibility(DirFile, Reading, CanRead, !IO),
        (
            CanRead = ok,
            Result = yes(DirFile)
        ;   
            % XXX Is this really the right thing to do?
            CanRead = error(_),
            search_for_file(Dirs, MaybeSubDir, File, Result, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

compile_acd_file(Debug, ACDFile, CDOFile, Result, !IO) :-
    io.see(ACDFile,ACDStreamRes,!IO),
    (
        ACDStreamRes = ok,
        parse_acd_file(Rules, Imports, Pragmas, !IO),
        io.seen(!IO),
        map.init(HLProg0),
        hl_rules_to_hl_prog(Rules, HLProg0, HLProg, !IO),
        compile(Debug, HLProg, Pragmas, Module, !IO),
        io.open_binary_output(CDOFile, CDOStreamRes, !IO),
        (
            CDOStreamRes = ok(CDOStream),
            write_ll_prog(CDOStream, Debug, Module, Imports, !IO),
            io.close_binary_output(CDOStream,!IO)
        ;
            CDOStreamRes = error(CDOError),
            % We can't create the .cdo file.  This is a warning
            % only, because we can still run the program.
            %
            Message = message("failed to open binary file %s: %s",
                [s(CDOFile), s(error_message(CDOError))], none),
            compiler_warning(Message,!IO)
        ),
        Result = yes(cdo(Module, Imports))
    ;
        ACDStreamRes = error(ACDError),
        Message = message("failed to open file %s: %s",
            [s(ACDFile), s(error_message(ACDError))], none),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

compile_acd_to_cddoc(ACDFile, CDDocFile, yes(cddoc(DocInfo)), !IO) :-
    io.see(ACDFile,ACDStreamRes,!IO),
    (
        ACDStreamRes = ok,
        parse_acd_file(_, _, Pragmas, !IO),
        io.seen(!IO),
        map.init(DocInfo0),
        list.foldl2(pragma_doc_to_doc_info, Pragmas, DocInfo0, DocInfo, !IO),
        io.open_output(CDDocFile, CDDocStreamRes, !IO),
        (
            CDDocStreamRes = ok(CDDocStream),
            io.write(CDDocStream, DocInfo, !IO),
            io.write_string(CDDocStream, ".\n", !IO),
            io.close_output(CDDocStream, !IO)
        ;
            CDDocStreamRes = error(CDDocError),
            Message = message("failed to open file %s: %s",
                [s(CDDocFile), s(error_message(CDDocError))], none),
            compiler_warning(Message, !IO)
        )
    ;
        ACDStreamRes = error(ACDError),
        Message = message("failed to open file %s: %s",
            [s(ACDFile), s(error_message(ACDError))], none),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

load_cdo_file(Debug, CDOFile, MaybeResult, !IO) :-
    io.open_binary_input(CDOFile,CDOStreamRes,!IO),
    (
        CDOStreamRes = ok(CDOStream),
        read_ll_prog(CDOStream, Debug, ModuleRes, !IO),
        io.close_binary_input(CDOStream,!IO),
        (
            ModuleRes = ok(Module, Imports),
            MaybeResult = yes(cdo(Module, Imports))
        ;
            ModuleRes = error,
            MaybeResult = no
        )
    ;
        CDOStreamRes = error(CDOError),
        Message = message("failed to open binary file %s: %s",
            [s(CDOFile), s(error_message(CDOError))], none),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

load_cddoc_file(CDDocFile, MaybeResult, !IO) :-
    io.open_input(CDDocFile, CDDocStreamRes, !IO),
    (
        CDDocStreamRes = ok(CDDocStream),
        io.read(CDDocStream, DocInfoRes, !IO),
        io.close_input(CDDocStream, !IO),
        (
            DocInfoRes = ok(DocInfo),
            MaybeResult = yes(cddoc(DocInfo))
        ;
            DocInfoRes = eof,
            MaybeResult = no
        ;
            DocInfoRes = error(_ , _),
            MaybeResult = no
        )
    ;
        CDDocStreamRes = error(CDDocError),
        Message = message("failed to open file %s: %s",
            [s(CDDocFile), s(error_message(CDDocError))], none),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

    % A Target matches if it is of the form BaseName[.*].Ext.
    % The optional .* allows for submodules.
    %
:- pred target_matches(string::in, string::in, string::out) is semidet.

target_matches(Target, Ext, BaseName) :-
    remove_suffix(Target, Ext, BaseName0),
    ( if sub_string_search(BaseName0, ".", Idx)
    then left(BaseName0, Idx, BaseName)
    else BaseName = BaseName0
    ).

%-----------------------------------------------------------------------------%

:- pred strings_to_comma_string(list(string)::in, string::out) is det.

strings_to_comma_string([], "").
strings_to_comma_string([Str|Strs],Result) :-
    (
        Strs = [],
        Result = Str
    ;
        Strs = [_ | _],
        strings_to_comma_string(Strs, Result0),
        Result = Str ++ "," ++ Result0
    ).

%-----------------------------------------------------------------------------%
:- end_module cadmium_make.
%-----------------------------------------------------------------------------%
