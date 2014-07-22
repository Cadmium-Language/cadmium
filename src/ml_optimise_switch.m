%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% Find switches that can be used to compile more efficient ll_prog.  Switches
% are added to the ml_prog.
%
%---------------------------------------------------------------------------%

:- module ml_optimise_switch.
:- interface.

:- import_module io.

:- import_module ml_prog.

:- pred ml_optimise_switch(ml_prog::in,ml_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- import_module cadmium_common.
:- import_module hl_prog.

:- type switch_info == map(hl_var,list(ml_switch)).

%---------------------------------------------------------------------------%

    % Minimum switch size.
    % Experimental results show minimum_switch_size = 2 gives a speed-up.
    %
:- func minimum_switch_size = int.

minimum_switch_size = 2.

%---------------------------------------------------------------------------%

ml_optimise_switch(!MLProg,!IO) :-
    map_foldl(ml_proc_optimise_switch,!MLProg,!IO).

%---------------------------------------------------------------------------%

:- pred ml_proc_optimise_switch(hl_symbol::in,ml_proc::in,ml_proc::out,
    io::di,io::uo) is det.

ml_proc_optimise_switch(_Sym,!MLProc,!IO) :-
    ml_proc_get_src_var(!.MLProc,Src),
    ml_fix(Src,init,Fixed),
    ml_s_block_optimise_switch(Fixed,ml_entry_s_block_id,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_s_block_optimise_switch(ml_fixed::in,ml_s_block_id::in,ml_proc::in,
    ml_proc::out,io::di,io::uo) is det.

ml_s_block_optimise_switch(Fixed,SId,!MLProc,!IO) :-
    ml_s_block_lookup(!.MLProc,SId,SBlk),
    ( SBlk = ml_s_body_block(_,_,_),
        true
    ; SBlk = ml_s_block(_,_,BIds),
        ( BIds = [_,_|_] ->
            foldl3(ml_b_block_switch_analysis(!.MLProc,Fixed),BIds,1,_,init,
                SwitchInfo,!IO),
            pick_best_switches(SwitchInfo,Switches0),
            map(reverse_switch,Switches0,Switches),
            ( Switches = [_|_],
                NSBlk = ml_s_block(SId,Switches,BIds),
                ml_s_block_insert(NSBlk,!MLProc)
            ; Switches = [],
                true
            )
        ;   true
        ),
        foldl2(ml_b_block_optimise_switch(Fixed),BIds,!MLProc,!IO)
    ).

%---------------------------------------------------------------------------%

    % Switches are constructed in the wrong order -- fix it here.
    %
:- pred reverse_switch(ml_switch::in,ml_switch::out) is det.

reverse_switch(ml_switch(N,M,Var,Syms0),ml_switch(N,M,Var,Syms1)) :-
    reverse(Syms0,Syms1).

%---------------------------------------------------------------------------%

:- pred ml_b_block_optimise_switch(ml_fixed::in,ml_b_block_id::in,
    ml_proc::in,ml_proc::out,io::di,io::uo) is det.

ml_b_block_optimise_switch(Fixed,BId,!MLProc,!IO) :-
    ml_b_block_lookup(!.MLProc,BId,BBlk),
    BBlk = ml_b_block(_,_BBlkInfo,Instrs,SId),
    foldl(ml_instr_update_fixed,Instrs,Fixed,NFixed),
    ml_s_block_optimise_switch(NFixed,SId,!MLProc,!IO).

%---------------------------------------------------------------------------%

:- pred ml_b_block_switch_analysis(ml_proc::in,ml_fixed::in,ml_b_block_id::in,
    int::in,int::out,switch_info::in,switch_info::out,io::di,io::uo) is det.

ml_b_block_switch_analysis(MLProc,Fixed,BId,!N,!SwitchInfo,!IO) :-
    ml_b_block_lookup(MLProc,BId,BBlk),
    BBlk = ml_b_block(_,_,Instrs,_),
    ml_instrs_extend_switches(Instrs,!.N,Fixed,!SwitchInfo,!IO),
    !:N = !.N + 1.

%---------------------------------------------------------------------------%

:- pred ml_instrs_extend_switches(list(ml_instr)::in,int::in,ml_fixed::in,
    switch_info::in,switch_info::out,io::di,io::uo) is det.

ml_instrs_extend_switches([],_,_,!SwitchInfo,!IO).
ml_instrs_extend_switches([Instr|Instrs],N,Fixed,!SwitchInfo,!IO) :-
    ( is_switchable_instruction(Instr,Var,Sym),
      ml_is_fixed(Fixed,Var) ->
        ( search(!.SwitchInfo,Var,Switches) ->
            ( Switches = [ml_switch(L,M,_,Syms)|Switches0],
              M = N - 1,
              not member(Sym,Syms) ->
                NSwitches = [ml_switch(L,N,Var,[Sym|Syms])|Switches0]
            ;   NSwitches = [ml_switch(N,N,Var,[Sym])|Switches]
            ),
            map.set(Var,NSwitches,!SwitchInfo)
        ;   NSwitches = [ml_switch(N,N,Var,[Sym])],
            map.det_insert(Var,NSwitches,!SwitchInfo)
        )
    ;   true
    ),
    ml_instrs_extend_switches(Instrs,N,Fixed,!SwitchInfo,!IO).

%---------------------------------------------------------------------------%

:- pred is_switchable_instruction(ml_instr::in,hl_var::out,hl_symbol::out)
    is semidet.

is_switchable_instruction(Instr,Var,Sym) :-
    ( Instr = is_functor(Var0,Sym0,_),
        Var = Var0,
        Sym = Sym0
    ; Instr = is_ac_functor(Var0,_,Sym0,_),
        Var = Var0,
        Sym = hl_symbol(hl_symbol_name(Sym0),0)
    ; Instr = is_int(Var0,_),
        Var = Var0,
        Sym = hl_symbol(int_name,0)
    ; Instr = is_float(Var0,_),
        Var = Var0,
        Sym = hl_symbol(float_name,0)
    ; Instr = is_string(Var0,_),
        Var = Var0,
        Sym = hl_symbol(string_name,0)
    ; Instr = is_named_var(Var0,_),
        Var = Var0,
        Sym = hl_symbol(var_name,0)
    ; Instr = is_type(Var0,Type),
        Var = Var0,
        ( Type = int,
            Sym = hl_symbol(int_name,0)
        ; Type = float,
            Sym = hl_symbol(float_name,0)
        ; Type = string,
            Sym = hl_symbol(string_name,0)
        ; Type = var,
            Sym = hl_symbol(var_name,0)
        )
    ).

%---------------------------------------------------------------------------%

:- pred pick_best_switches(switch_info::in,list(ml_switch)::out) is det.

pick_best_switches(SwitchInfo,Switches) :-
    pick_best_switches_2([],Switches,SwitchInfo,_).

%---------------------------------------------------------------------------%

:- pred pick_best_switches_2(list(ml_switch)::in,list(ml_switch)::out,
    switch_info::in,switch_info::out) is det.

pick_best_switches_2(!Switches,!SwitchInfo) :-
    ( pick_best_switch(Switch,!SwitchInfo) ->
        !:Switches = [Switch|!.Switches],
        pick_best_switches_2(!Switches,!SwitchInfo)
    ;   true
    ).

%---------------------------------------------------------------------------%

    % Choose the "best" switch out of all the possibilities.  The best switch
    % is simply the largest.
    %
:- pred pick_best_switch(ml_switch::out,switch_info::in,switch_info::out)
    is semidet.

pick_best_switch(Switch,!SwitchInfo) :-
    foldl(pick_best_switch_2,!.SwitchInfo,no,MaybeSwitch),
    MaybeSwitch = yes(Switch),
    delete_conflicting_switches(Switch,!SwitchInfo).

%---------------------------------------------------------------------------%

:- pred pick_best_switch_2(hl_var::in,list(ml_switch)::in,maybe(ml_switch)::in,
    maybe(ml_switch)::out) is det.

pick_best_switch_2(_Var,Switches,!MaybeSwitch) :-
    foldl(pick_best_switch_3,Switches,!MaybeSwitch).

%---------------------------------------------------------------------------%

:- pred pick_best_switch_3(ml_switch::in,maybe(ml_switch)::in,
    maybe(ml_switch)::out) is det.

pick_best_switch_3(Switch0,!MaybeSwitch) :-
    Switch0 = ml_switch(L0,U0,_,_),
        
        % A switch must have at least minimum_switch_size symbols.
        %
    ( U0 - L0 + 1 >= minimum_switch_size ->
        ( !.MaybeSwitch = yes(Switch),
            Switch = ml_switch(L1,U1,_,_),
            ( U0-L0 < U1-L1 ->
                !:MaybeSwitch = yes(Switch0)
            ;   true
            )
        ; !.MaybeSwitch = no,
            !:MaybeSwitch = yes(Switch0)
        )
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred delete_conflicting_switches(ml_switch::in,switch_info::in,
    switch_info::out) is det.

delete_conflicting_switches(Switch,!SwitchInfo) :-
    Switch = ml_switch(L,U,_,_),
    foldl(delete_conflicting_switches_range(L,U),!.SwitchInfo,!SwitchInfo).

%---------------------------------------------------------------------------%

:- pred delete_conflicting_switches_range(int::in,int::in,hl_var::in,
    list(ml_switch)::in,switch_info::in,switch_info::out) is det.

delete_conflicting_switches_range(L,U,Var,Switches,!SwitchInfo) :-
    filter(delete_conflicting_switch_range(L,U),Switches,NSwitches),
    ( NSwitches = [],
        map.delete(Var,!SwitchInfo)
    ; NSwitches = [_|_],
        map.set(Var,NSwitches,!SwitchInfo)
    ).

%---------------------------------------------------------------------------%

:- pred delete_conflicting_switch_range(int::in,int::in,ml_switch::in) 
    is semidet.

delete_conflicting_switch_range(L,U,Switch) :-
    Switch = ml_switch(X,Y,_,_),
    Y - X + 1 >= minimum_switch_size,
    ( Y < L
    ; X > U ).

