%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Extract lookups from ml_b_blocks.
%
% ASSUMPTIONS: 
% - 'loop' optimisation has not yet being performed.  Otherwise invalid 
%   lookups may be generated.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_lookups.

:- interface.

:- import_module io.

:- import_module ml_prog.

    %
    %
:- pred ml_optimise_lookups(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.

:- import_module cadmium_common.
:- import_module hl_prog.

:- type itr_info == map(hl_var,ml_l_block_id).

%---------------------------------------------------------------------------%

ml_optimise_lookups(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_lookups,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_lookups(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_lookups(_Sym,!MLProc,!IO) :-
    ml_s_block_optimise_lookups(ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_lookups(ml_s_block_id::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_s_block_optimise_lookups(SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        foldl2(ml_b_block_optimise_lookups,BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_lookups(ml_b_block_id::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_b_block_optimise_lookups(BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    some [!Instrs,!InVars,!OutVars,!Lookup] (
        BBlk = ml_b_block(_,BBlkInfo,!:Instrs,SId),
        BBlkInfo = ml_b_block_info(MLItr,_,_,_),
        ( MLItr = ml_none,
            true
        ; MLItr = ml_ac_itr(_,LId,_,ASrc),
            !:Lookup = [],
            !:InVars = init,
            !:OutVars = init,
            extract_lookup(ASrc,_,!Lookup,!InVars,!OutVars,!Instrs),
            reverse(!Lookup),
            LBlk = ml_l_block(LId,!.InVars,!.OutVars,!.Lookup),
            ml_l_block_insert(LBlk,!MLProc),
            NBBlk = ml_b_block(BId,BBlkInfo,!.Instrs,SId),
            ml_b_block_insert(NBBlk,!MLProc)
        )
    ),
    ml_s_block_optimise_lookups(SId,!MLProc,!IO).

%----------------------------------------------------------------------------%

:- pred extract_lookup(hl_var::in,bool::out,list(ml_instr)::in,
    list(ml_instr)::out,set(hl_var)::in,set(hl_var)::out,set(hl_var)::in,
    set(hl_var)::out,list(ml_instr)::in,list(ml_instr)::out) is det.

extract_lookup(Src,IsFixed,!Lookup,!InVars,!OutVars,!Instrs) :-
    ( !.Instrs = [Instr|!:Instrs] ->
        ( Instr = is_functor(Src,Sym,Dst) ->
            !:Lookup = [Instr|!.Lookup],
            set.insert(Dst,!OutVars),
            extract_lookup_on_functor(Dst,1,hl_symbol_arity(Sym),IsFixed,
                !Lookup,!InVars,!OutVars,!Instrs)
        ; Instr = is_type(Src,_) ->
            !:Lookup = [Instr|!.Lookup],
            IsFixed = no
        ; Instr = is_int(Src,_) ->
            !:Lookup = [Instr|!.Lookup],
            IsFixed = yes
        ; Instr = is_float(Src,_) ->
            !:Lookup = [Instr|!.Lookup],
            IsFixed = yes
        ; Instr = is_string(Src,_) ->
            !:Lookup = [Instr|!.Lookup],
            IsFixed = yes
        ; Instr = is_named_var(Src,_) ->
            !:Lookup = [Instr|!.Lookup],
            IsFixed = yes
        ; Instr = is_ac_functor(Src,_,_,Dst) ->
            !:Lookup = [Instr|!.Lookup],
            set.insert(Dst,!OutVars),
            IsFixed = no
        ; Instr = eq(Src,InVar) ->
            !:Lookup = [Instr|!.Lookup],
            set.insert(InVar,!InVars),
            IsFixed = yes
        ;   extract_lookup(Src,IsFixed,!Lookup,!InVars,!OutVars,!Instrs),
            !:Instrs = [Instr|!.Instrs]
        )
    ;   IsFixed = no
    ).   

%----------------------------------------------------------------------------%

:- pred extract_lookup_on_functor(hl_var::in,int::in,int::in,bool::out,
    list(ml_instr)::in,list(ml_instr)::out,set(hl_var)::in,set(hl_var)::out,
    set(hl_var)::in,set(hl_var)::out,list(ml_instr)::in,list(ml_instr)::out) 
    is det.

extract_lookup_on_functor(Src,N,M,IsFixed,!Lookup,!InVars,!OutVars,!Instrs) :-
    ( N > M ->
        IsFixed = yes
    ;   extract_lookup_on_get_arg(Src,N,IsFixed0,!Lookup,!InVars,!OutVars,
            !Instrs),
        ( IsFixed0 = yes ->
            extract_lookup_on_functor(Src,N+1,M,IsFixed,!Lookup,!InVars,
                !OutVars,!Instrs)
        ;   IsFixed = no
        )
    ).

%-----------------------------------------------------------------------------%

:- pred extract_lookup_on_get_arg(hl_var::in,int::in,bool::out,
    list(ml_instr)::in,list(ml_instr)::out,set(hl_var)::in,set(hl_var)::out,
    set(hl_var)::in,set(hl_var)::out,list(ml_instr)::in,list(ml_instr)::out)
    is det.

extract_lookup_on_get_arg(Src,N,IsFixed,!Lookup,!InVars,!OutVars,!Instrs) :-
    ( !.Instrs = [Instr|!:Instrs],
        ( Instr = get_arg(Src,N,Dst) ->
            set.insert(Dst,!OutVars),
            !:Lookup = [Instr|!.Lookup],
            extract_lookup(Dst,IsFixed,!Lookup,!InVars,!OutVars,!Instrs)
        ;   extract_lookup_on_get_arg(Src,N,IsFixed,!Lookup,!InVars,!OutVars,
                !Instrs),
            !:Instrs = [Instr|!.Instrs]
        )
    ; !.Instrs = [],
        IsFixed = no
    ).

