%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Mercury definitions for ac_indexes.
%
%---------------------------------------------------------------------------%

:- module ac_index.
:- interface.

:- import_module list.
:- import_module model.

%---------------------------------------------------------------------------%

:- type ac_index.

    % Empty index.
    %
:- func ac_index_empty = ac_index.

    % Convert an index into a (sorted) list.
    %
:- pred ac_index_to_list(ac_index::in, list(model)::out) is det.

    % Insert a model into an AC index.
    % NOTE: The model is assumed to be unique(!)
    %
:- pred ac_index_insert(model::in, ac_index::in, ac_index::out) is det.

    % Delete the least element from an AC index and return it.
    %
:- pred ac_index_delete_least(model::out, ac_index::in, ac_index::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_import_module("C", machine).
:- pragma foreign_import_module("C", model).

:- type ac_index == c_pointer.

:- pragma foreign_decl("C","
#include ""ac_index_impl.h""
").

:- pragma foreign_code("C","
#include ""ac_index_impl.c""
").

:- pragma foreign_proc("C",
    ac_index_empty = (Idx::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    Idx = CR_EMPTY_IDX;
").

:- pragma foreign_proc("C",
    ac_index_to_list(Idx::in, Ls::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    Ls = CR_index_to_list(Idx);
").

:- pragma foreign_proc("C",
    ac_index_insert(Model::in, Idx0::in, Idx1::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    Idx1 = CR_index_insert(Idx0,Model);
").

:- pragma foreign_proc("C",
    ac_index_delete_least(Model::out, Idx0::in, Idx1::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    MR_Bool red;
    Idx1 = CR_index_delete_least(Idx0,&Model,&red);
").

%---------------------------------------------------------------------------%
:- end_module ac_index.
%---------------------------------------------------------------------------%
