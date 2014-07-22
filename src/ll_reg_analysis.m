%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Decides how "registers" (which are really just places on the cstack) are 
% implemented.
%
%---------------------------------------------------------------------------%

:- module ll_reg_analysis.
:- interface.

:- import_module io.
:- import_module ll_prog.

    % Perform register analysis to convert a ll_prog(ll_var) to a ll_prog(reg).
    %
:- pred ll_reg_analysis(ll_prog_0::in,ll_prog::out,io::di,io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module set.

:- import_module cadmium_common.
:- import_module cadmium_debug.

%---------------------------------------------------------------------------%

:- type reg == int.

:- type reg_info == map(hl_symbol,reg_info_2).

:- type reg_info_2
    ---> reg_info_2(int,reg_graph_coloring).

:- type reg_occ_info == map(ll_var,list(int)).

:- type reg_graph == multi_map(ll_var,ll_var).

:- type reg_graph_coloring == map(ll_var,reg).

%---------------------------------------------------------------------------%

ll_reg_analysis(LLProg0,LLProg,!IO) :-
    map_foldl(reg_analysis_on_ll_proc_0,LLProg0,RegInfo,!IO),
    map_values(ll_proc_0_to_ll_proc(RegInfo),LLProg0,LLProg).

%---------------------------------------------------------------------------%

:- pred ll_proc_0_to_ll_proc(reg_info::in,hl_symbol::in,ll_proc_0::in,
    ll_proc::out) is det.

ll_proc_0_to_ll_proc(RegInfo,Sym,LLProc0,LLProc) :-
    LLProc0 = ll_proc(Label,Instrs0,LookupInstrs0,Creates),
    lookup(RegInfo,Sym,RegInfo2),
    map(ll_instr_0_to_ll_instr(RegInfo2),Instrs0,Instrs),
    map_values(ll_instr_0s_to_ll_instrs(RegInfo2),LookupInstrs0,LookupInstrs),
    LLProc = ll_proc(Label,Instrs,LookupInstrs,Creates).

%---------------------------------------------------------------------------%

:- pred ll_instr_0s_to_ll_instrs(reg_info_2::in,label::in,
    list(ll_instr(ll_var))::in,list(ll_instr)::out) is det.

ll_instr_0s_to_ll_instrs(RegInfo,_,!Instrs) :-
    map(ll_instr_0_to_ll_instr(RegInfo),!Instrs).

%---------------------------------------------------------------------------%

:- pred ll_instr_0_to_ll_instr(reg_info_2::in,ll_instr(ll_var)::in,
    ll_instr::out) is det.

ll_instr_0_to_ll_instr(RegInfo,is_int(A,X),is_int(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,is_float(A,X),is_float(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,is_string(A,X),is_string(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,is_named_var(A,X),is_named_var(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,is_functor(A,X),is_functor(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,is_ac_functor(A,X),is_ac_functor(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(_,is_ac_functor(X),is_ac_functor(X)).
ll_instr_0_to_ll_instr(RegInfo,is_geq_ac_functor(A,X),is_geq_ac_functor(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(_,is_geq_ac_functor(X),is_geq_ac_functor(X)).
ll_instr_0_to_ll_instr(RegInfo,is_type(A,X),is_type(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,get_arg(A,X,B),get_arg(R,X,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,get_arg(X,A),get_arg(X,R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,pre_guard(A),pre_guard(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,post_guard(A),post_guard(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,post_match_guard(A,B),post_match_guard(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,switch(A,X,Y),switch(R,X,Y)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(_,pop(X),pop(X)).
ll_instr_0_to_ll_instr(_,construct(X),construct(X)).
ll_instr_0_to_ll_instr(_,construct_ac(X,Y),construct_ac(X,Y)).
ll_instr_0_to_ll_instr(RegInfo,construct_acd(A,X,Y),construct_acd(R,X,Y)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,construct_acd(A,X,Y,B),construct_acd(R,X,Y,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(_,interpret,interpret).
ll_instr_0_to_ll_instr(RegInfo,interpret(A),interpret(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(_,return,return).
ll_instr_0_to_ll_instr(_,push_new_var,push_new_var).
ll_instr_0_to_ll_instr(_,push_int(X),push_int(X)).
ll_instr_0_to_ll_instr(_,push_float(X),push_float(X)).
ll_instr_0_to_ll_instr(_,push_string(X),push_string(X)).
ll_instr_0_to_ll_instr(_,push_named_var(X),push_named_var(X)).
ll_instr_0_to_ll_instr(RegInfo,ccpush(A),ccpush(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(_,ccpop,ccpop).
ll_instr_0_to_ll_instr(RegInfo,cpush(_),cpush(N)) :-
    lookup_frame_size(RegInfo,N).
ll_instr_0_to_ll_instr(RegInfo,cset(A),cset(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,cget(A),cget(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,ccopy(A,B),ccopy(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,ceqeq(A,B),ceqeq(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,cpop(_),cpop(N)) :-
    lookup_frame_size(RegInfo,N).
ll_instr_0_to_ll_instr(_,call_sym(X),call_sym(X)).
ll_instr_0_to_ll_instr(_,call_sym_lco(X),call_sym_lco(X)).
ll_instr_0_to_ll_instr(_,call_ac_arg_sym(X,Y),call_ac_arg_sym(X,Y)).
ll_instr_0_to_ll_instr(_,call_ac_sym_lco(X),call_ac_sym_lco(X)).
ll_instr_0_to_ll_instr(_,call_label(X),call_label(X)).
ll_instr_0_to_ll_instr(_,call_label_lco(X),call_label_lco(X)).
ll_instr_0_to_ll_instr(_,call_ac_arg_label(X,Y),call_ac_arg_label(X,Y)).
ll_instr_0_to_ll_instr(_,call_ac_label_lco(X),call_ac_label_lco(X)).
ll_instr_0_to_ll_instr(_,call_ho(X),call_ho(X)).
ll_instr_0_to_ll_instr(_,create_cp(X),create_cp(X)).
ll_instr_0_to_ll_instr(_,commit(X,Y),commit(X,Y)).
ll_instr_0_to_ll_instr(_,commit,commit).
ll_instr_0_to_ll_instr(_,call_foreign(X,Y),call_foreign(X,Y)).
ll_instr_0_to_ll_instr(_,halt,halt).
ll_instr_0_to_ll_instr(RegInfo,lookup_(A,X,B,C,D),lookup_(R,X,S,T,U)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T),
    lookup_reg(RegInfo,D,U).
ll_instr_0_to_ll_instr(RegInfo,lookup_(X,A,B,C),lookup_(X,R,S,T)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T).
ll_instr_0_to_ll_instr(RegInfo,lookup_first(A,X,B),lookup_first(R,X,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,lookup_first(X,A),lookup_first(X,R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,split(A,B),split(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,lookup_cc(X,A,B,C,D),lookup_cc(X,R,S,T,U)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T),
    lookup_reg(RegInfo,D,U).
ll_instr_0_to_ll_instr(RegInfo,lookup_cc(A,X,B,C,D,E),lookup_cc(R,X,S,T,U,V)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T),
    lookup_reg(RegInfo,D,U),
    lookup_reg(RegInfo,E,V).
ll_instr_0_to_ll_instr(RegInfo,lookup_first_cc(X,A),lookup_first_cc(X,R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_first_cc(A,X,B),lookup_first_cc(R,X,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,get_next(A,X,Y,B,C),get_next(R,X,Y,S,T)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T).
ll_instr_0_to_ll_instr(RegInfo,init_itr(A,B,C),init_itr(R,S,T)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T).
ll_instr_0_to_ll_instr(RegInfo,get_next_cc(A,B,X,Y,C,D),
        get_next_cc(R,S,X,Y,T,U)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S),
    lookup_reg(RegInfo,C,T),
    lookup_reg(RegInfo,D,U).
ll_instr_0_to_ll_instr(_,top_level,top_level).
ll_instr_0_to_ll_instr(RegInfo,is_diff(A,B),is_diff(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,ll_prog.delete(A,B),ll_prog.delete(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(RegInfo,flatten(A),flatten(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,set_annots(A),set_annots(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,get_annots(A),get_annots(R)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,get_annots(A,B),get_annots(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(_,clear_annots,clear_annots).
ll_instr_0_to_ll_instr(_,restore_annots,restore_annots).
ll_instr_0_to_ll_instr(_,setup_annots,setup_annots).
ll_instr_0_to_ll_instr(_,call_annots,call_annots).
ll_instr_0_to_ll_instr(_,attach_annots,attach_annots).
ll_instr_0_to_ll_instr(_,wakeup_on_redo,wakeup_on_redo).
ll_instr_0_to_ll_instr(_,wakeup_on(X),wakeup_on(X)).
ll_instr_0_to_ll_instr(_,add,add).
ll_instr_0_to_ll_instr(_,subtract,subtract).
ll_instr_0_to_ll_instr(_,multiply,multiply).
ll_instr_0_to_ll_instr(_,divide,divide).
ll_instr_0_to_ll_instr(_,(mod),(mod)).
ll_instr_0_to_ll_instr(_,and,and).
ll_instr_0_to_ll_instr(_,or,or).
ll_instr_0_to_ll_instr(_,iff,iff).
ll_instr_0_to_ll_instr(_,xor,xor).
ll_instr_0_to_ll_instr(_,impl,impl).
ll_instr_0_to_ll_instr(_,not,not).
ll_instr_0_to_ll_instr(_,lt,lt).
ll_instr_0_to_ll_instr(_,leq,leq).
ll_instr_0_to_ll_instr(_,gt,gt).
ll_instr_0_to_ll_instr(_,geq,geq).
ll_instr_0_to_ll_instr(_,eq,eq).
ll_instr_0_to_ll_instr(_,neq,neq).
ll_instr_0_to_ll_instr(_,negate,negate).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_int(A,X),lookup_is_int(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_float(A,X),lookup_is_float(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_string(A,X),lookup_is_string(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_named_var(A,X),
        lookup_is_named_var(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_functor(A,X),
        lookup_is_functor(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_ac_functor(A,X),
        lookup_is_ac_functor(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_geq_ac_functor(A,X),
        lookup_is_geq_ac_functor(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_is_type(A,X),lookup_is_type(R,X)) :-
    lookup_reg(RegInfo,A,R).
ll_instr_0_to_ll_instr(RegInfo,lookup_compare(A,B),lookup_compare(R,S)) :-
    lookup_reg(RegInfo,A,R),
    lookup_reg(RegInfo,B,S).
ll_instr_0_to_ll_instr(_,lookup_accept,lookup_accept).
ll_instr_0_to_ll_instr(RegInfo,debug(DebugInfo0),debug(DebugInfo)) :-
    ll_debug_info_0_to_ll_debug_info(RegInfo,DebugInfo0,DebugInfo).
ll_instr_0_to_ll_instr(_,label(X),label(X)).

%---------------------------------------------------------------------------%

:- pred ll_debug_info_0_to_ll_debug_info(reg_info_2::in,
    debug_info(hl_symbol,ll_var)::in,debug_info(hl_symbol,reg)::out) is det.

ll_debug_info_0_to_ll_debug_info(RegInfo,debug_info(Sym,PP,Cxt,LLVars),
        debug_info(Sym,PP,Cxt,Regs)) :-
    map_values(lookup_reg_2(RegInfo),LLVars,Regs).

%---------------------------------------------------------------------------%

:- pred lookup_reg_2(reg_info_2::in,string::in,ll_var::in,reg::out) is det.

lookup_reg_2(RegInfo,_,Var,Reg) :-
    lookup_reg(RegInfo,Var,Reg).

%---------------------------------------------------------------------------%

:- pred lookup_reg(reg_info_2::in,ll_var::in,reg::out) is det.

lookup_reg(reg_info_2(_,VarRegs),Var,Reg) :-
    lookup(VarRegs,Var,Reg0),
    Reg = Reg0 + 1.

%---------------------------------------------------------------------------%

:- pred lookup_frame_size(reg_info_2::in,int::out) is det.

lookup_frame_size(reg_info_2(Size0,_),Size) :-
    Size = Size0 + 1.

%---------------------------------------------------------------------------%

:- pred reg_analysis_on_ll_proc_0(hl_symbol::in,ll_proc_0::in,reg_info_2::out,
    io::di,io::uo) is det.

reg_analysis_on_ll_proc_0(_,LLProc,SI2,!IO) :-
    LLProc = ll_proc(_,Instrs,Lookups,_),
    some [!ROI,!RO,!Gbls,!CPs] (
        !:ROI = map.init,
        !:RO = 0,
        !:Gbls = set.init,
        !:CPs = [],
        foldl4(generate_reg_occ_info_on_ll_instr(Lookups),Instrs,!RO,!Gbls,
            !CPs,!ROI),
        !.Gbls = _,
        !.CPs = _,
        !.RO = _,
        map_values(do_reverse,!ROI),
        generate_reg_graph(!.ROI,RG),
        color_graph(RG,RC),
        foldl(maximum_reg_number,RC,-1,MxReg),
        SI2 = reg_info_2(MxReg+1,RC)
    ).

%---------------------------------------------------------------------------%

:- pred maximum_reg_number(ll_var::in,reg::in,reg::in,reg::out) is det.

maximum_reg_number(_,Reg0,Reg1,Reg2) :-
    Reg2 = max(Reg0,Reg1).

%---------------------------------------------------------------------------%

:- pred do_reverse(T::in,list(U)::in,list(U)::out) is det.

do_reverse(_,Xs,Ys) :-
    reverse(Xs,Ys).

%---------------------------------------------------------------------------%

    % Generates a ll_var dependency graph.  Two ll_vars share a dependency
    % (and hence an edge in the graph) if they are both "live" at the some
    % point during the match/body.
    %
:- pred generate_reg_graph(reg_occ_info::in,reg_graph::out) is det.

generate_reg_graph(ROI,RG) :-
    multi_map.init(RG0),
    foldl(generate_reg_graph_node(ROI),ROI,RG0,RG).

%---------------------------------------------------------------------------%

:- pred generate_reg_graph_node(reg_occ_info::in,ll_var::in,list(int)::in,
    reg_graph::in,reg_graph::out) is det.

generate_reg_graph_node(ROI,Var,Occs,!RG) :-
    FstOcc = det_head(Occs),
    LstOcc = det_last(Occs),
    foldl(generate_reg_graph_node_dependency(FstOcc,LstOcc),ROI,[],Deps),
    map.set(Var,Deps,!RG).

%---------------------------------------------------------------------------%

:- pred generate_reg_graph_node_dependency(int::in,int::in,ll_var::in,
    list(int)::in,list(ll_var)::in,list(ll_var)::out) is det.

generate_reg_graph_node_dependency(Fst,Lst,Var,Occs,!Deps) :-
    ( exists_dependency(Fst,Lst,Occs) ->
        !:Deps = [Var|!.Deps]
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred exists_dependency(int::in,int::in,list(int)::in) is semidet.

exists_dependency(Fst,Lst,Occs) :-
    Fst1 = det_head(Occs),
    Lst > Fst1,
    Lst1 = det_last(Occs),
    ( Fst < Fst1 ->
        Lst > Fst1
    ;   Lst1 > Fst
    ).

%---------------------------------------------------------------------------%

    % Uses a naive graph coloring algorithm: we assign the first "free" color 
    % to each node we see in order.
    %
:- pred color_graph(reg_graph::in,reg_graph_coloring::out) is det.

color_graph(RG,RC) :-
    foldl(color_node,RG,init,RC).

%---------------------------------------------------------------------------%

:- pred color_node(ll_var::in,list(ll_var)::in,reg_graph_coloring::in,
    reg_graph_coloring::out) is det.

color_node(HLReg,Nbs,!RC) :-
    filter_map(search(!.RC),Nbs,Used0),
    sort_and_remove_dups(Used0,Used),
    pick_lowest_free_color(Used,0,Free),
    map.set(HLReg,Free,!RC).

%---------------------------------------------------------------------------%

:- pred pick_lowest_free_color(list(reg)::in,reg::in,reg::out)
    is det.

pick_lowest_free_color([],!RN).
pick_lowest_free_color([Reg|Regs],RN0,RN) :-
    ( Reg = RN0 ->
        pick_lowest_free_color(Regs,RN0+1,RN)
    ;   RN = RN0
    ).

%---------------------------------------------------------------------------%

:- pred generate_reg_occ_info_on_ll_instr(ll_lookup_instrs(ll_var)::in,
    ll_instr(ll_var)::in,int::in,int::out,set(ll_var)::in,set(ll_var)::out,
    list(int)::in,list(int)::out,reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_occ_info_on_ll_instr(_,is_int(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_float(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_string(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_named_var(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_functor(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_ac_functor(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_ac_functor(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_geq_ac_functor(X,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_geq_ac_functor(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_type(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,get_arg(X,_,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,get_arg(_,X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,pre_guard(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,post_guard(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,post_match_guard(X,Y),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,switch(X,_,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,pop(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,construct(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,construct_ac(_,_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,construct_acd(X,_,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,construct_acd(X,_,_,Y),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,interpret,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,interpret(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,return,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,push_new_var,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,push_int(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,push_float(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,push_string(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,push_named_var(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,ccpush(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,ccpop,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,cpush(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,cset(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,cget(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,ccopy(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,ceqeq(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,cpop(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_sym(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_sym_lco(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_ac_arg_sym(_,_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_ac_sym_lco(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_label(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_label_lco(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_ac_arg_label(_,_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_ac_label_lco(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_ho(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,create_cp(_),!RO,!Gbls,!CPs,!ROI) :-
    !:CPs = [!.RO|!.CPs],
    !:RO = !.RO + 1.
generate_reg_occ_info_on_ll_instr(_,commit(_,_),!RO,!Gbls,!CPs,!ROI) :-
    reverse(!CPs),
    foldl2(generate_reg_info_on_cp_var(!.CPs),!.ROI,!RO,!ROI),
    !:CPs = [],
    to_sorted_list(!.Gbls,GblsLs), 
    foldl2(generate_reg_occ_info_on_var_0,GblsLs,!RO,!ROI),
    !:Gbls = set.init.
generate_reg_occ_info_on_ll_instr(_,commit,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_foreign(_,_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,halt,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_(X,Lookup,Y,Z,W),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(W,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_(Lookup,X,Y,Z),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_first(X,Lookup,Y),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_first(Lookup,X),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,split(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_cc(Lookup,X,Y,Z,W),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(W,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_cc(X,Lookup,Y,Z,W,T),!RO,
        !Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(W,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(T,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_first_cc(Lookup,X),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,lookup_first_cc(X,Lookup,Y),!RO,
        !Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,init_itr(X,Y,Z),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,get_next(X,_,Lookup,Y,Z),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    !:CPs = [!.RO|!.CPs],
    !:RO = !.RO + 1,
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(Lookups,get_next_cc(X,Y,_,Lookup,Z,W),!RO,
        !Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI),
    !:CPs = [!.RO|!.CPs],
    !:RO = !.RO + 1,
    generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI),
    generate_reg_occ_info_on_var(Z,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(W,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,top_level,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,is_diff(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,delete(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,flatten(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,set_annots(X),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,get_annots(X),!RO,!Gbls,!CPs,!ROI) :- 
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,get_annots(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,clear_annots,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,restore_annots,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,setup_annots,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,call_annots,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,attach_annots,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,wakeup_on_redo,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,wakeup_on(_),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,add,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,subtract,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,multiply,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,divide,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,(mod),!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,and,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,or,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,iff,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,xor,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,impl,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,not,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,lt,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,leq,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,gt,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,geq,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,eq,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,neq,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,negate,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_int(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_float(X,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_string(X,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_named_var(X,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_functor(X,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_ac_functor(X,_),!RO,!Gbls,!CPs,
        !ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_geq_ac_functor(X,_),!RO,!Gbls,
        !CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_is_type(X,_),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_compare(X,Y),!RO,!Gbls,!CPs,!ROI) :-
    generate_reg_occ_info_on_var(X,!RO,!Gbls,!ROI),
    generate_reg_occ_info_on_var(Y,!RO,!Gbls,!ROI).
generate_reg_occ_info_on_ll_instr(_,lookup_accept,!RO,!Gbls,!CPs,!ROI).
generate_reg_occ_info_on_ll_instr(_,debug(DebugInfo),!RO,!Gbls,!CPs,!ROI) :-
     generate_reg_occ_info_on_debug_info(DebugInfo,!RO,!ROI).
generate_reg_occ_info_on_ll_instr(_,label(_),!RO,!Gbls,!CPs,!ROI).

%---------------------------------------------------------------------------%

:- pred generate_reg_occ_info_on_lookup(ll_lookup_instrs(ll_var)::in,label::in,
    int::in,int::out,reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_occ_info_on_lookup(Lookups,Lookup,!RO,!ROI) :-
    map.lookup(Lookups,Lookup,Instrs),
        % A lookup cannot contain any globals nor create a choice point.
        % 
    CPs = [],
    Gbls = set.init,
    foldl4(generate_reg_occ_info_on_ll_instr(Lookups),Instrs,!RO,Gbls,_,CPs,_,
        !ROI).

%---------------------------------------------------------------------------%

:- pred generate_reg_occ_info_on_debug_info(debug_info(hl_symbol,ll_var)::in,
    int::in,int::out,reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_occ_info_on_debug_info(debug_info(_,_,_,VarRegInfo),!RO,!ROI) :-
    foldl2(generate_reg_occ_info_on_named_var,VarRegInfo,!RO,!ROI).

%---------------------------------------------------------------------------%

:- pred generate_reg_occ_info_on_named_var(string::in,ll_var::in,int::in,
    int::out,reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_occ_info_on_named_var(_,Var,!RO,!ROI) :-
    generate_reg_occ_info_on_var_0(Var,!RO,!ROI).

%---------------------------------------------------------------------------%

:- pred generate_reg_occ_info_on_var(ll_var::in,int::in,int::out,
    set(ll_var)::in,set(ll_var)::out,reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_occ_info_on_var(Var,!RO,!Gbls,!ROI) :-
    generate_reg_occ_info_on_var_0(Var,!RO,!ROI),
    ( is_global_ll_var(Var) ->
        set.insert(Var,!Gbls)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred generate_reg_occ_info_on_var_0(ll_var::in,int::in,int::out,
    reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_occ_info_on_var_0(Var,!RO,!ROI) :-
    ( map.search(!.ROI,Var,ROccs0) ->
        ROccs1 = [!.RO|ROccs0],
        map.set(Var,ROccs1,!ROI)
    ;   map.set(Var,[!.RO],!ROI)
    ),
    !:RO = !.RO + 1.

%---------------------------------------------------------------------------%

    % Variables that are referenced before and after a choicepoint must
    % remain live until the end of the matching.  Otherwise the concrete
    % register assigned to the variable may be used for something else, which
    % will cause problems on backtracking.
    %
:- pred generate_reg_info_on_cp_var(list(int)::in,ll_var::in,list(int)::in,
    int::in,int::out,reg_occ_info::in,reg_occ_info::out) is det.

generate_reg_info_on_cp_var(CPs,Var,Occs,!RO,!ROI) :-
    ( live_across_choice_points(Occs,CPs) ->
        generate_reg_occ_info_on_var_0(Var,!RO,!ROI)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred live_across_choice_points(list(int)::in,list(int)::in) is semidet.

live_across_choice_points(Occs,[CP|CPs]) :-
    ( live_across_choice_point(Occs,CP) ->
        true
    ;   live_across_choice_points(Occs,CPs)
    ).

%---------------------------------------------------------------------------%

:- pred live_across_choice_point(list(int)::in,int::in) is semidet.

live_across_choice_point(Occs,CP) :-
    live_before_choice_point(Occs,CP),
    live_after_choice_point(Occs,CP).

%---------------------------------------------------------------------------%

:- pred live_before_choice_point(list(int)::in,int::in) is semidet.

live_before_choice_point([Occ|Occs],CP) :-
    ( Occ < CP ->
        true
    ;   live_before_choice_point(Occs,CP)
    ).

%---------------------------------------------------------------------------%

:- pred live_after_choice_point(list(int)::in,int::in) is semidet.

live_after_choice_point([Occ|Occs],CP) :-
    ( Occ > CP ->
        true
    ;   live_after_choice_point(Occs,CP)
    ).

%---------------------------------------------------------------------------%
:- end_module ll_reg_analysis.
%---------------------------------------------------------------------------%
