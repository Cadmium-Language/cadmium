%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% The "Low-Level" representation is a representation of the virtual machine
% instructions of the final executable.
%
% The representation is designed to be linkable: so sets of instructions
% for each procedure are indexed by the procedure name.
%
%---------------------------------------------------------------------------%

:- module ll_prog.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.

:- import_module cadmium_common.
:- import_module cadmium_debug.
:- import_module hl_prog.

%---------------------------------------------------------------------------%

:- type ll_prog_id == int.

    % A single hl_var can be mapped down to multiple ll_vars by changing the
    % first integer argument.
    %
:- type ll_var
    ---> ll_var(int,hl_var).

:- type ll_proc == ll_proc(register).
:- type ll_proc_0 == ll_proc(ll_var).
:- type ll_lookup_instrs(Place) == map(label,list(ll_instr(Place))).
:- type ll_lookup_instrs == ll_lookup_instrs(register).
:- type ll_proc(Place)
    ---> ll_proc(label,list(ll_instr(Place)),ll_lookup_instrs(Place),
            list(hl_symbol)).

:- type ll_prog == ll_prog(register).
:- type ll_prog_0 == ll_prog(ll_var).
:- type ll_prog(Place) == map(hl_symbol,ll_proc(Place)).

:- type label == int.
:- type register == int.

:- type ll_instr == ll_instr(register).

:- type ll_instr(Place)
        % is_int(Reg,Int)
        %   Tests if the value in Reg is the integer Int.
        %
    ---> is_int(Place,int)

        % is_float(Reg,Flt)
        %   Tests if the value in Reg is the float Flt.
        %
    ;    is_float(Place,float)

        % is_string(Reg,Str)
        %   Tests if the value in Reg is the string Str.
        %
    ;    is_string(Place,string)

        % is_named_var(Reg,NameStr)
        %   Tests if the value in Reg is a variable with name NameStr.
        %
    ;    is_named_var(Place,string)

        % is_functor(Reg,Sym)
        %   Tests if the value in Reg is a functor model with symbol Sym and
        %   with the same arity.
        %   Note: Not for AC operators (use is_ac_functor instead).
        %
    ;    is_functor(Place,hl_symbol)

        % is_ac_functor(Reg,Sym)
        %   Tests if the value in Reg is an AC functor model with symbol Sym
        %   and with the same arity.
        %
    ;    is_ac_functor(Place,hl_symbol)

        % is_ac_functor(Sym)
        %   Tests if the 1st stack element is an AC functor model with symbol
        %   Sym and with the same arity.
        %
    ;    is_ac_functor(hl_symbol)

        % is_geq_ac_functor(Reg,Sym)
        %   The same as is_ac_functor/2 but the arity of the value in Reg may
        %   be >= the arity of Sym.
        %
    ;    is_geq_ac_functor(Place,hl_symbol)

        % is_geq_ac_functor(Sym)
        %   The same as is_ac_functor/1 but the arity of the value in Reg may
        %   be >= the arity of Sym.
        %
    ;    is_geq_ac_functor(hl_symbol)

        % is_type(Reg,Type)
        %   Tests if the value in Reg is of Type.
        %
    ;    is_type(Place,hl_model_type)

        % get_arg(SrcReg,Idx,DstReg)
        %   Assigns the value in SrcReg's Idx^th argument to DstReg.
        %
    ;    get_arg(Place,int,Place)

        % get_arg(Idx,Reg)
        %   Assigns the value in the Idx^th stack element (starting from 1) to
        %   Reg.
        %
    ;    get_arg(int,Place)

        % pre_guard(ChReg)
        %   Must be executed before calling a user-defined guard.  Stores the
        %   current changed-global-register into ChReg.
        %
    ;    pre_guard(Place)

        % post_guard(ChReg)
        %   Must be called after executing a user-defined guard.  Fails if the
        %   top of the stack is not `true'.  Restores the
        %   changed-global-register from ChReg.
        %
    ;    post_guard(Place)

        % post_match_guard(ResReg,ChReg)
        %   Like post_guard/1 except only fails if the top of the stack is 
        %   `false'.  The result of the guard computation is put in `ResReg'.
        % 
    ;    post_match_guard(Place,Place)

        % switch(SrcReg,Table,TbLength)
        %   A "switch" jumbo instruction.  Given the symbol of the value in 
        %   SrcReg, jump to the corresponding label in Table if it exists, 
        %   otherwise fail.
        %
    ;    switch(Place,ll_switch_table,int)

        % pop(N)
        %   Pop off N elements from the stack.
        %
    ;    pop(int)

        % construct(Sym)
        %   Constructs a functor model with symbol Sym based on the first
        %   arity(Sym) elements on the stack.  Pops off the arity(Sym) elements
        %   and then pushes the result back onto the stack.  Note: cannot be
        %   used for AC operators (use construct_ac instead).
        %
    ;    construct(hl_symbol)

        % construct_ac(Sym,Aty)
        %   Construct an AC functor model with symbol Sym and arity Aty based 
        %   on the first Aty elements on the stack.  Pops off the Aty elements.
        %
    ;    construct_ac(hl_symbol,int)

        % construct_acd(EventsReg,Sym,Aty)
        %   Like construct_ac/2 but for ACD functors.  Also computes the set
        %   of events for interpret/1.
        %
    ;    construct_acd(Place,hl_symbol,int)

        % construct_acd(EventsReg,Sym,Aty,CollReg)
        %   Like construct_acd/3 but merge into existing ACD collector in 
        %   CollReg.
        %
    ;    construct_acd(Place,hl_symbol,int,Place)

        % interpret
        %   Execute the 1st stack element as if it were a goal.
        %
    ;    interpret

        % interpret(EventsReg)
        %   Like interpret/0 but where the initial events contains is the 
        %   contents of `EventsReg'.
        %
    ;    interpret(Place)

        % return
        %   Return from a call instruction.
        %
    ;    return

        % push_new_var
        %   Create a new variable and push it onto the stack.
        %
    ;    push_new_var

        % push_int(Int)
        %   Push the integer Int onto the stack.
        %
    ;    push_int(int)

        % push_float(Flt)
        %   Push the float Flt onto the stack.
        %
    ;    push_float(float)

        % push_string(Str)
        %   Push the string Str onto the stack.
        %
    ;    push_string(string)

        % push_named_var(NameStr)
        %   Push the variable with name NameStr onto the stack.
        %
    ;    push_named_var(string)

        % ccpush(Reg)
        %   Push the contents of Reg on to the CC stack.
        %
    ;    ccpush(Place)

        % ccpop
        %   Pop an entry off the CC stack.
        %
    ;    ccpop

        % cpush(N)
        %   Create space for N elements (i.e. registers) on the call stack.
        %
    ;    cpush(int)

        % cset(Reg)
        %   Set register Reg to the 1st element of the stack.
        %
    ;    cset(Place)

        % cget(Reg)
        %   Push the value of register Reg onto the stack.
        %
    ;    cget(Place)

        % ccopy(SrcReg,DstReg)
        %   Copy the contents of SrcReg to DstReg.
        %
    ;    ccopy(Place,Place)

        % ceqeq(Reg0,Reg1)
        %   Fails if the models in registers Reg0 and Reg1 are not equal.
        %
    ;    ceqeq(Place,Place)

        % cpop(N)
        %   Pop off N place holders from the call stack.
        %
    ;    cpop(int)

        % call_sym(Symbol)
        %   Call the procedure for Symbol.
        %
    ;    call_sym(hl_symbol)

        % call_sym_lco(Symbol)
        %   Like call_sym/1 except use Last Call Optimisation (LCO) if Symbol
        %   is a procedure.
        %
    ;    call_sym_lco(hl_symbol)

        % call_ac_arg_sym(ACSymbol,Symbol)
        %   Like call_sym/1 except the caller is from an AC argument context.
        %   e.g. if reducing (f(1)+Z), the call to f/1 will be 
        %   call_ac_arg_sym(+/0,f/1).
        %
    ;    call_ac_arg_sym(hl_symbol,hl_symbol)

        % call_ac_sym_lco(Symbol)
        %   Special call operation for the last call of an AC operator.  This
        %   will call the procedure for 'Symbol' if it exists, or will be a NOP
        %   otherwise.  If we are in the AC context of 'Symbol', we will 
        %   instead simply return the current value without calling the 
        %   procedure.
        %
    ;    call_ac_sym_lco(hl_symbol)

        % call_label(label)
        %   Like call_sym/1 but when the label is known.
        %
    ;    call_label(label)

        % call_label_lco(label)
        %   Like call_sym_lco/1 but when the label is known.
        %
    ;    call_label_lco(label)

        % call_ac_arg_label(ACSymbol,Label)
        %   Like call_ac_sym/1 but when the label is known.
        %
    ;    call_ac_arg_label(hl_symbol,label)

        % call_ac_label_lco(Label)
        %   Like call_ac_sym_lco/1 but when the label is known.
        %
    ;    call_ac_label_lco(label)

        % call_ho(N)
        %   A higher-order call.  The Nth+1 stack argument is the closure, and
        %   the following N arguments are extra.
        %
    ;    call_ho(int)

        % create_cp(Label)
        %   Create a choicepoint.  Any future failure will cause execution to
        %   return to the nearest choicepoint.  Restores the stack pointers,
        %   but does not restore stack contents.
        %
    ;    create_cp(label)

        % commit(N,S)
        %   Pops off N choicepoints from the dstack and S arguments off the 
        %   stack.  Also sets the `changed' register to 1.
        %
    ;    commit(int,int)

        % commit
        %   Pops off a choicepoint from the dstack.
        %
    ;    commit

        % call_foreign(N,Name)
        %   Execute a foreign procedure which takes `N' arguments and has C
        %   symbol name `Name'.
        %
    ;    call_foreign(int,string)

        % halt
        %   Cause execution to stop.
        %
    ;    halt

        % lookup_(SrcReg,LookupLabel,ArgReg,NodeReg,PosReg)
        %   Given an AC model in SrcReg and a lookup and LookupLabel, search 
        %   for a suitable match and put the result in NodeReg and PosReg.
        %   Fail if there are no suitable matches.  ArgReg is needed by the 
        %   lookup.
        %
    ;    lookup_(Place,label,Place,Place,Place)

        % lookup_(LookupLabel,ArgReg,NodeReg,PosReg)
        %   Like lookup_/4, but uses the top stack value instead of SrcReg.
        %
    ;    lookup_(label,Place,Place,Place)

        % lookup_first(SrcReg,LookupLabel,ArgReg)
        %   Like lookup_, but for when we want to commit to the first match.
        %
    ;    lookup_first(Place,label,Place)

        % lookup_first(LookupLabel,ArgReg)
        %   Like lookup_first/1, but uses the top stack value instead of 
        %   SrcReg.
        %   
    ;    lookup_first(label,Place)

        % init_itr(NodeReg,PosReg,ItrReg)
        %   Given the results from a lookup (NodeReg and PosReg), construct an
        %   iterator and put it in ItrReg.
        %
    ;    init_itr(Place,Place,Place)

        % split(SrcReg,ArgReg)
        %   Pick an argument (any argument) from the AC term in SrcReg and
        %   put it in ArgReg.  Also updates SrcReg.  For determinisitic AC
        %   matching.
        %
    ;    split(Place,Place)

        % lookup_cc(LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg)
        %   Like lookup_/4, but uses the CC stack to find matches.
        %   ItrNoReg will contain the current "position" in the CC stack.  
        %   If/When the iterator is exhausted, a new iterator can be created 
        %   from the next element of the CC stack.
        %
    ;    lookup_cc(label,Place,Place,Place,Place)

        % lookup_cc(CollReg,LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg)
        %   Same as lookup_cc/4 but considers the contents of `CollReg' as 
        %   part of the CC.
        %
    ;    lookup_cc(Place,label,Place,Place,Place,Place)

        % lookup_first_cc(LookupLabel,ArgReg)
        %   Like lookup_first/2, but uses the CC stack to find matches.
        %
    ;    lookup_first_cc(label,Place)

        % lookup_first_cc(CollReg,LookupLabel,ArgReg)
        %   Same as lookup_first_cc/1 but considers the contents of `CollReg'
        %   as part of the CC.
        %
    ;    lookup_first_cc(Place,label,Place)

        % get_next(ItrReg,Label,LookupLabel,ArgReg,IdReg)
        %   Gets an element from the iterator in ItrReg and puts it into 
        %   ArgReg.  If the iterator is empty the instruction fails.  Also
        %   implicitly executes a create_cp(Label) instruction.  IdReg will
        %   be set to a unique identifier for difference tests.
        %
    ;    get_next(Place,label,label,Place,Place)

        % get_next_cc(ItrNoReg,ItrReg,Label,LookupLabel,ArgReg,IdReg)
        %   Like get_next/4 but for CC iterators.  See the documentation for
        %   lookup_cc/3.
        %
    ;    get_next_cc(Place,Place,label,label,Place,Place)

        % top_level
        %   Put the top-most stack element into the top-level conjunction if it
        %   exists, otherwise throw an error.
        %
    ;    top_level

        % is_diff(Reg1,Reg2)
        %   Fails if the ID in register Reg1 is the same as the ID in register
        %   Reg2.
        %
    ;    is_diff(Place,Place)

        % delete(SrcReg,ArgReg)
        %   Deletes the value in register ArgReg from the value in SrcReg. 
        %
    ;    delete(Place,Place)

        % flatten(SrcReg)
        %   Flattens a singular AC model in SrcReg.
        %
    ;    flatten(Place)

        % set_annots(SrcReg)
        %   Copy the contents of SrcReg to the annotation register.
        %
    ;    set_annots(Place)

        % get_annots(DstReg)
        %   Get the annotations from the top-most stack element and put them in
        %   DstReg.
        %
    ;    get_annots(Place)

        % get_annots(SrcReg,DstReg)
        %   Like get_annots/1, except get the annotations from SrcReg
        %
    ;    get_annots(Place,Place)

        % clear_annots
        %   Clear the annotation register.
        %
    ;    clear_annots

        % restore_annots
        %   Copy the contents of the local annotation register to the (global)
        %   annotation register.
        %
    ;    restore_annots

        % setup_annots
        %   Sets the annotation register to the top-most stack element.
        %   Pops the stack by 1.
        %
    ;    setup_annots

        % call_annots
        %   Call the top-most stack element again, since its annotations have
        %   changed.
        %
    ;    call_annots

        % attach_annots
        %   Attach annotations in the 1st stack position to the model in the
        %   2nd stack position.  Pops the stack by one.
        %
    ;    attach_annots

        % wakeup_on_redo
        %   Cause the current conjunct to wakeup on the `redo' event.
        %
    ;    wakeup_on_redo

        % wakeup_on(Syms)
        %   Cause the current conjunct to wakeup on `create(Sym)' event for
        %   Sym in Syms.
        %
    ;    wakeup_on(list(hl_symbol))

        % add, subtract, multiply, divide, mod, negate, and, or, iff, xor,
        % impl, not, lt, leq, gt, geq, eq, neg
        %   Standard operations.  Operates on stack values.
        %
    ;    add
    ;    subtract
    ;    multiply
    ;    divide
    ;    (mod)
    ;    negate
    ;    (and)
    ;    (or)
    ;    iff
    ;    xor
    ;    impl
    ;    (not)
    ;    lt
    ;    leq
    ;    gt
    ;    geq
    ;    eq
    ;    neq

        % 'lookup' versions of (some of) the above instructions.
        % Instead of failing, these versions return (<), (>), or (=), which
        % is used for efficiently looking-up values in an AC index structure
        % during matching.
        %
        % lookup_compare(X,Y) would be analagous to ceqeq(X,Y).
        %
    ;    lookup_is_int(Place,int)
    ;    lookup_is_float(Place,float)
    ;    lookup_is_string(Place,string)
    ;    lookup_is_named_var(Place,string)
    ;    lookup_is_functor(Place,hl_symbol)
    ;    lookup_is_ac_functor(Place,hl_symbol)
    ;    lookup_is_geq_ac_functor(Place,hl_symbol)
    ;    lookup_is_type(Place,hl_model_type)
    ;    lookup_compare(Place,Place)
    ;    lookup_accept

        % debug(Info)
        %   A debug point.
        %
    ;    debug(debug_info(hl_symbol,Place))

        % Pseudo instruction that defines a label.
        %
    ;    label(label).

    % Zero-arity atomic symbol associated to each ll_instr
    %
:- type ll_instr_0
    ---> is_int_2
    ;    is_float_2
    ;    is_string_2
    ;    is_named_var_2
    ;    is_functor_2
    ;    is_ac_functor_3
    ;    is_ac_functor_2
    ;    is_geq_ac_functor_3
    ;    is_geq_ac_functor_2
    ;    is_type_2
    ;    get_arg_3
    ;    get_arg_2
    ;    pre_guard_1
    ;    post_guard_1
    ;    post_match_guard_2
    ;    switch_3
    ;    pop_1
    ;    construct_1
    ;    construct_ac_2
    ;    construct_acd_3
    ;    construct_acd_4
    ;    interpret_0
    ;    interpret_1
    ;    return_0
    ;    push_new_var_0
    ;    push_int_1
    ;    push_float_1
    ;    push_string_1
    ;    push_named_var_1
    ;    ccpush_1
    ;    ccpop_0
    ;    cpush_1
    ;    cset_1
    ;    cget_1
    ;    ccopy_2
    ;    ceqeq_2
    ;    cpop_1
    ;    call_label_1
    ;    call_label_lco_1
    ;    call_ac_arg_label_2
    ;    call_ac_label_lco_1
    ;    call_sym_1
    ;    call_sym_lco_1
    ;    call_ac_arg_sym_2
    ;    call_ac_sym_lco_1
    ;    call_ho_1
    ;    create_cp_1
    ;    commit_0
    ;    commit_2
    ;    call_foreign_2
    ;    lookup_5
    ;    lookup_4
    ;    lookup_first_3
    ;    lookup_first_2
    ;    split_2
    ;    init_itr_3
    ;    get_next_5
    ;    lookup_cc_5
    ;    lookup_cc_6
    ;    lookup_first_cc_2
    ;    lookup_first_cc_3
    ;    get_next_cc_6
    ;    top_level_0
    ;    is_diff_2
    ;    delete_2
    ;    flatten_1
    ;    set_annots_1
    ;    get_annots_1
    ;    get_annots_2
    ;    clear_annots_0
    ;    restore_annots_0
    ;    setup_annots_0
    ;    call_annots_0
    ;    attach_annots_0
    ;    wakeup_on_redo_0
    ;    wakeup_on_1
    ;    add_0
    ;    subtract_0
    ;    multiply_0
    ;    divide_0
    ;    mod_0
    ;    negate_0
    ;    and_0
    ;    or_0
    ;    iff_0
    ;    xor_0
    ;    impl_0
    ;    not_0
    ;    lt_0
    ;    leq_0
    ;    gt_0
    ;    geq_0
    ;    eq_0
    ;    neq_0
    ;    lookup_is_int_2
    ;    lookup_is_float_2
    ;    lookup_is_string_2
    ;    lookup_is_named_var_2
    ;    lookup_is_functor_2
    ;    lookup_is_ac_functor_3
    ;    lookup_is_geq_ac_functor_3
    ;    lookup_is_type_2
    ;    lookup_compare_2
    ;    lookup_accept_0
    ;    debug_1
    ;    halt_0
    ;    label_1.

:- type ll_switch_table
    ---> ll_switch_table(list(hl_symbol),list(label)).

:- type ll_res
    ---> ok(ll_prog,set(string))
    ;    error.

    % The first available label (all others < this are reserved).
    %
:- func label_min = label.

    % A global ll_var should never has its value overridden.
    %
:- pred is_global_ll_var(ll_var::in) is semidet.

    % Make a global var.
    %
:- func ll_global(int,hl_var) = ll_var.

    % Write a ll_prog to a binary output stream.
    %
:- pred write_ll_prog(binary_output_stream::in,bool::in,ll_prog::in,
    set(string)::in,io::di,io::uo) is det.

    % Read a ll_prog to a binary input stream.
    %
:- pred read_ll_prog(binary_input_stream::in,bool::in,ll_res::out,
    io::di,io::uo) is det.

    % Pretty print a ll_prog.
    %
:- pred pprint_ll_prog(ll_prog::in,io::di,io::uo) is det.

    % Convert a ll_instr into a ll_instr_0
    %
:- pred ll_instr_to_ll_instr_0(ll_instr::in,ll_instr_0::out) is det.

    % The number of arguments in a ll_instr_0
    %
:- pred ll_instr_0_length(ll_instr_0::in,int::out) is det.

    % Link the first ll_prog into the second.
    %
:- pred ll_prog_link(ll_prog_id::in,ll_prog::in,ll_prog::in,ll_prog::out)
    is det.

    % Fold predicate.
    %
:- pred ll_prog_fold(pred(ll_instr,T,T),ll_prog,T,T).
:- mode ll_prog_fold(pred(in,in,out) is det,in,in,out) is det.
% :- mode ll_prog_fold(pred(in,di,uo) is det,in,di,uo) is det.

    % Fold predicate (for io.io).
    %
:- pred ll_prog_fold_io(pred(ll_instr,io,io),ll_prog,io,io).
:- mode ll_prog_fold_io(pred(in,di,uo) is det,in,di,uo) is det.

    % Fold2 predicate.  
    % 
:- pred ll_prog_fold2(pred(ll_instr,T,T,U,U),ll_prog,T,T,U,U).
:- mode ll_prog_fold2(pred(in,in,out,in,out) is det,in,in,out,in,out) is det.
% :- mode ll_prog_fold2(pred(in,in,out,di,uo) is det,in,in,out,di,uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cadmium_error.
:- import_module hl_prog.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type pseudo_symbol == int.
:- type pseudo_sym_info_w == map(string,pseudo_symbol).
:- type pseudo_sym_info_r == map(pseudo_symbol,string).

:- type string_info_w == map(string,pseudo_symbol).
:- type string_info_r == map(pseudo_symbol,string).

:- type code == int.

%---------------------------------------------------------------------------%

label_min = 100.

%---------------------------------------------------------------------------%

    % Increment this number each time the file format changes.
    %
:- func version_number = int.

version_number = 24.

%---------------------------------------------------------------------------%

    % Global-ness is marked by the number being negative.
    %
is_global_ll_var(ll_var(N,_)) :-
    N < 0.

%---------------------------------------------------------------------------%

ll_global(N,Var) = LLVar :-
    LLVar = ll_var(-N,Var).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reading
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

read_ll_prog(Stream,Debug,LLProgRes,!IO) :-
    read_binary_chars(Stream,9,Start,!IO),
    ( Start = "#!cadmium" ->
        read_tag(Stream,VTag,!IO),
        ( VTag = 'V' ->
            read_binary_16_bit_int(Stream,Version,!IO),
            ( Version = version_number ->
                read_binary_byte(Stream,BitsPerInt,!IO),
                read_binary_byte(Stream,Code,!IO),
                ( Code = 0 ->
                    DebugIn = no
                ;   DebugIn = yes
                ),
                ( BitsPerInt = bits_per_int,
                  Debug = DebugIn ->
                    read_tag(Stream,MTag,!IO),
                    ( MTag = 'M' ->
                        read_imports(Stream,init,Imports,TTag,!IO),
                        ( TTag = 'T' ->
                            read_symbol_table(Stream,1,init,SymInfo,init,
                                StrInfo,FTag,!IO),
                            ( FTag = 'C' ->
                                read_ll_procs(Stream,SymInfo,StrInfo,init,
                                    LLProg,!IO),
                                % pprint_ll_prog(LLProg,!IO),
                                LLProgRes = ok(LLProg,Imports)
                            ; FTag = 'E' ->
                                LLProgRes = ok(init,Imports)
                            ;   LLProgRes = error
                            )
                        ;   LLProgRes = error
                        )
                    ;   LLProgRes = error
                    )
                ;   LLProgRes = error
                )
            ;   LLProgRes = error
            )
        ;   LLProgRes = error
        )
    ;   LLProgRes = error
    ).

%---------------------------------------------------------------------------%

:- pred read_imports(binary_input_stream::in,set(string)::in,set(string)::out,
    char::out,io::di,io::uo) is det.

read_imports(Stream,!Imports,Tag,!IO) :-
    read_tag(Stream,ITag,!IO),
    ( ITag = 'I' ->
        read_binary_string(Stream,Import,!IO),
        set.insert(Import,!Imports),
        read_imports(Stream,!Imports,Tag,!IO)
    ;   Tag = ITag
    ).

%---------------------------------------------------------------------------%

:- pred read_ll_procs(binary_input_stream::in,pseudo_sym_info_r::in,
    string_info_r::in,ll_prog::in,ll_prog::out,io::di,io::uo) is det.

read_ll_procs(Stream,SymInfo,StrInfo,!LLProg,!IO) :-
    read_tag(Stream,Tag,!IO),
    ( Tag = 'P' ->
        read_symbol(Stream,SymInfo,Sym,!IO),
        read_symbols(Stream,SymInfo,Conds,!IO),
        read_label(Stream,Label,!IO),
        read_ll_instrs(Stream,SymInfo,StrInfo,[],Instrs,!IO),
        read_ll_lookup_instrs(Stream,SymInfo,StrInfo,init,LookupInstrs,!IO),
        LLProc = ll_proc(Label,Instrs,LookupInstrs,Conds),
        map.set(Sym,LLProc,!LLProg),
        read_ll_procs(Stream,SymInfo,StrInfo,!LLProg,!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred read_ll_lookup_instrs(binary_input_stream::in,pseudo_sym_info_r::in,
    string_info_r::in,ll_lookup_instrs::in,ll_lookup_instrs::out,io::di,io::uo)
    is det.

read_ll_lookup_instrs(Stream,SymInfo,StrInfo,!LookupInstrs,!IO) :-
    read_binary_byte(Stream,Code,!IO),
    ( Code = have_lookup_code ->
        read_label(Stream,Label,!IO),
        read_ll_instrs(Stream,SymInfo,StrInfo,[],Instrs,!IO),
        map.det_insert(Label,Instrs,!LookupInstrs),
        read_ll_lookup_instrs(Stream,SymInfo,StrInfo,!LookupInstrs,!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred read_ll_instrs(binary_input_stream::in,pseudo_sym_info_r::in,
    string_info_r::in,list(ll_instr)::in,list(ll_instr)::out,
    io::di,io::uo) is det.

read_ll_instrs(Stream,SymInfo,StrInfo,!Instrs,!IO) :-
    read_binary_byte(Stream,Code,!IO),
    ( Code = end_instrs_code ->
        true
    ; ll_instr_0_code(Instr0,Code) ->
        read_ll_instr_args(Stream,SymInfo,StrInfo,Instr0,Instr,!IO),
        !:Instrs = [Instr|!.Instrs],
        read_ll_instrs(Stream,SymInfo,StrInfo,!Instrs,!IO)
    ;   Message = message("invalid instruction code (%d)",[i(Code)],none),
        throw_compiler_error(Message,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred read_ll_instr_args(binary_input_stream::in,pseudo_sym_info_r::in,
    string_info_r::in,ll_instr_0::in,ll_instr::out,io::di,io::uo) is det.

read_ll_instr_args(Stream,_,_,is_int_2,is_int(Reg,Int),!IO) :-
    read_register(Stream,Reg,!IO),
    read_binary_int(Stream,Int,!IO).
read_ll_instr_args(Stream,_,_,is_float_2,is_float(Reg,Flt),!IO) :-
    read_register(Stream,Reg,!IO),
    read_binary_float(Stream,Flt,!IO).
read_ll_instr_args(Stream,_,StrInfo,is_string_2,is_string(Reg,Str),!IO) :-
    read_register(Stream,Reg,!IO),
    read_string(Stream,StrInfo,Str,!IO).
read_ll_instr_args(Stream,_,StrInfo,is_named_var_2,is_named_var(Reg,NameStr),
        !IO) :-
    read_register(Stream,Reg,!IO),
    read_string(Stream,StrInfo,NameStr,!IO).
read_ll_instr_args(Stream,SymInfo,_,is_functor_2,is_functor(Reg,Sym),!IO) :-
    read_register(Stream,Reg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,is_ac_functor_3,is_ac_functor(Reg,Sym),
        !IO) :-
    read_register(Stream,Reg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,is_ac_functor_2,is_ac_functor(Sym),!IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,is_geq_ac_functor_3,
        is_geq_ac_functor(Reg,Sym),!IO) :-
    read_register(Stream,Reg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,is_geq_ac_functor_2,is_geq_ac_functor(Sym),
        !IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,_,_,is_type_2,is_type(Reg,Type),!IO) :-
    read_register(Stream,Reg,!IO),
    read_type(Stream,Type,!IO).
read_ll_instr_args(Stream,_,_,get_arg_3,get_arg(SrcReg,Idx,DstReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_binary_byte(Stream,Idx,!IO),
    read_register(Stream,DstReg,!IO).
read_ll_instr_args(Stream,_,_,get_arg_2,get_arg(Idx,DstReg),!IO) :-
    read_binary_byte(Stream,Idx,!IO),
    read_register(Stream,DstReg,!IO).
read_ll_instr_args(Stream,_,_,pre_guard_1,pre_guard(ChReg),!IO) :-
    read_register(Stream,ChReg,!IO).
read_ll_instr_args(Stream,_,_,post_guard_1,post_guard(ChReg),!IO) :-
    read_register(Stream,ChReg,!IO).
read_ll_instr_args(Stream,_,_,post_match_guard_2,post_match_guard(ResReg,ChReg),
        !IO) :-
    read_register(Stream,ResReg,!IO),
    read_register(Stream,ChReg,!IO).
read_ll_instr_args(Stream,SymInfo,_,switch_3,switch(SrcReg,Table,TbLen),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_binary_24_bit_int(Stream,TbLen,!IO),
    read_switch_table(Stream,SymInfo,TbLen,Table,!IO).
read_ll_instr_args(Stream,_,_,pop_1,pop(N),!IO) :-
    read_binary_byte(Stream,N,!IO).
read_ll_instr_args(Stream,SymInfo,_,construct_1,construct(Sym),!IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,construct_ac_2,construct_ac(Sym,Aty),
        !IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO),
    read_binary_byte(Stream,Aty,!IO).
read_ll_instr_args(Stream,SymInfo,_,construct_acd_3,
        construct_acd(DstReg,Sym,Aty),!IO) :-
    read_register(Stream,DstReg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO),
    read_binary_byte(Stream,Aty,!IO).
read_ll_instr_args(Stream,SymInfo,_,construct_acd_4,
        construct_acd(DstReg,Sym,Aty,CollReg),!IO) :-
    read_register(Stream,DstReg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO),
    read_binary_byte(Stream,Aty,!IO),
    read_register(Stream,CollReg,!IO).
read_ll_instr_args(_,_,_,interpret_0,interpret,!IO).
read_ll_instr_args(Stream,_,_,interpret_1,interpret(SrcReg),!IO) :-
    read_register(Stream,SrcReg,!IO).
read_ll_instr_args(_,_,_,push_new_var_0,push_new_var,!IO).
read_ll_instr_args(Stream,_,_,push_int_1,push_int(Int),!IO) :-
    read_binary_int(Stream,Int,!IO).
read_ll_instr_args(Stream,_,_,push_float_1,push_float(Flt),!IO) :-
    read_binary_float(Stream,Flt,!IO).
read_ll_instr_args(Stream,_,StrInfo,push_string_1,push_string(Str),!IO) :-
    read_string(Stream,StrInfo,Str,!IO).
read_ll_instr_args(Stream,_,StrInfo,push_named_var_1,push_named_var(NameStr),
        !IO) :-
    read_string(Stream,StrInfo,NameStr,!IO).
read_ll_instr_args(Stream,_,_,ccpush_1,ccpush(Reg),!IO) :-
    read_register(Stream,Reg,!IO).
read_ll_instr_args(_,_,_,ccpop_0,ccpop,!IO).
read_ll_instr_args(Stream,_,_,cpush_1,cpush(N),!IO) :-
    read_binary_byte(Stream,N,!IO).
read_ll_instr_args(Stream,_,_,cset_1,cset(Reg),!IO) :-
    read_register(Stream,Reg,!IO).
read_ll_instr_args(Stream,_,_,cget_1,cget(Reg),!IO) :-
    read_register(Stream,Reg,!IO).
read_ll_instr_args(Stream,_,_,ccopy_2,ccopy(SrcReg,DstReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_register(Stream,DstReg,!IO).
read_ll_instr_args(Stream,_,_,ceqeq_2,ceqeq(Reg0,Reg1),!IO) :-
    read_register(Stream,Reg0,!IO),
    read_register(Stream,Reg1,!IO).
read_ll_instr_args(Stream,_,_,cpop_1,cpop(N),!IO) :-
    read_binary_byte(Stream,N,!IO).
read_ll_instr_args(Stream,_,_,call_label_1,call_label(Label),!IO) :-
    read_label(Stream,Label,!IO).
read_ll_instr_args(Stream,_,_,call_label_lco_1,call_label_lco(Label),!IO) :-
    read_label(Stream,Label,!IO).
read_ll_instr_args(Stream,SymInfo,_,call_ac_arg_label_2,call_ac_arg_label(ACSym,Label),
        !IO) :-
    read_symbol(Stream,SymInfo,ACSym,!IO),
    read_label(Stream,Label,!IO).
read_ll_instr_args(Stream,_,_,call_ac_label_lco_1,call_ac_label_lco(Label),
        !IO) :-
    read_label(Stream,Label,!IO).
read_ll_instr_args(Stream,SymInfo,_,call_sym_1,call_sym(Sym),!IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,call_sym_lco_1,call_sym_lco(Sym),!IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,call_ac_arg_sym_2,call_ac_arg_sym(ACSym,Sym),!IO) :-
    read_symbol(Stream,SymInfo,ACSym,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,call_ac_sym_lco_1,call_ac_sym_lco(Sym),
        !IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,_,_,call_ho_1,call_ho(N),!IO) :-
    read_binary_byte(Stream,N,!IO).
read_ll_instr_args(_,_,_,return_0,return,!IO).
read_ll_instr_args(Stream,_,_,create_cp_1,create_cp(Label),!IO) :-
    read_label(Stream,Label,!IO).
read_ll_instr_args(_,_,_,commit_0,commit,!IO).
read_ll_instr_args(Stream,_,_,commit_2,commit(N,S),!IO) :-
    read_binary_byte(Stream,N,!IO),
    read_binary_byte(Stream,S,!IO).
read_ll_instr_args(Stream,_,StrInfo,call_foreign_2,call_foreign(N,Name),!IO) :-
    read_binary_byte(Stream,N,!IO),
    read_string(Stream,StrInfo,Name,!IO).
read_ll_instr_args(_,_,_,halt_0,halt,!IO).
read_ll_instr_args(Stream,_,_,lookup_5,
        lookup_(SrcReg,LookupLabel,ArgReg,NodeReg,PosReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO),
    read_register(Stream,NodeReg,!IO),
    read_register(Stream,PosReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_4,
        lookup_(LookupLabel,ArgReg,NodeReg,PosReg),!IO) :-
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO),
    read_register(Stream,NodeReg,!IO),
    read_register(Stream,PosReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_first_3,
        lookup_first(SrcReg,LookupLabel,ArgReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_first_2,lookup_first(LookupLabel,ArgReg),
        !IO) :-
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO).
read_ll_instr_args(Stream,_,_,split_2,split(SrcReg,ArgReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_register(Stream,ArgReg,!IO).
read_ll_instr_args(Stream,_,_,init_itr_3,init_itr(NodeReg,PosReg,ItrReg),
        !IO) :-
    read_register(Stream,NodeReg,!IO),
    read_register(Stream,PosReg,!IO),
    read_register(Stream,ItrReg,!IO).
read_ll_instr_args(Stream,_,_,get_next_5,
        get_next(ItrReg,Label,LookupLabel,ArgReg,IdReg),!IO) :-
    read_register(Stream,ItrReg,!IO),
    read_label(Stream,Label,!IO),
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO),
    read_register(Stream,IdReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_cc_5,
        lookup_cc(LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),!IO) :-
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ItrNoReg,!IO),
    read_register(Stream,ArgReg,!IO),
    read_register(Stream,NodeReg,!IO),
    read_register(Stream,PosReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_cc_6,
        lookup_cc(CollReg,LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),!IO) :-
    read_register(Stream,CollReg,!IO),
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ItrNoReg,!IO),
    read_register(Stream,ArgReg,!IO),
    read_register(Stream,NodeReg,!IO),
    read_register(Stream,PosReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_first_cc_2,
        lookup_first_cc(LookupLabel,ArgReg),!IO) :-
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO).
read_ll_instr_args(Stream,_,_,lookup_first_cc_3,
        lookup_first_cc(CollReg,LookupLabel,ArgReg),!IO) :-
    read_register(Stream,CollReg,!IO),
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO).
read_ll_instr_args(Stream,_,_,get_next_cc_6,
        get_next_cc(ItrNoReg,ItrReg,Label,LookupLabel,ArgReg,IdReg),!IO) :-
    read_register(Stream,ItrNoReg,!IO),
    read_register(Stream,ItrReg,!IO),
    read_label(Stream,Label,!IO),
    read_label(Stream,LookupLabel,!IO),
    read_register(Stream,ArgReg,!IO),
    read_register(Stream,IdReg,!IO).
read_ll_instr_args(_,_,_,top_level_0,top_level,!IO).
read_ll_instr_args(Stream,_,_,is_diff_2,is_diff(Reg1,Reg2),!IO) :-
    read_register(Stream,Reg1,!IO),
    read_register(Stream,Reg2,!IO).
read_ll_instr_args(Stream,_,_,delete_2,delete(SrcReg,ArgReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_register(Stream,ArgReg,!IO).
read_ll_instr_args(Stream,_,_,flatten_1,flatten(SrcReg),!IO) :-
    read_register(Stream,SrcReg,!IO).
read_ll_instr_args(Stream,_,_,set_annots_1,set_annots(SrcReg),!IO) :-
    read_register(Stream,SrcReg,!IO).
read_ll_instr_args(Stream,_,_,get_annots_1,get_annots(DstReg),!IO) :-
    read_register(Stream,DstReg,!IO).
read_ll_instr_args(Stream,_,_,get_annots_2,get_annots(SrcReg,DstReg),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_register(Stream,DstReg,!IO).
read_ll_instr_args(_,_,_,clear_annots_0,clear_annots,!IO).
read_ll_instr_args(_,_,_,restore_annots_0,restore_annots,!IO).
read_ll_instr_args(_,_,_,setup_annots_0,setup_annots,!IO).
read_ll_instr_args(_,_,_,call_annots_0,call_annots,!IO).
read_ll_instr_args(_,_,_,attach_annots_0,attach_annots,!IO).
read_ll_instr_args(_,_,_,wakeup_on_redo_0,wakeup_on_redo,!IO).
read_ll_instr_args(Stream,SymInfo,_,wakeup_on_1,wakeup_on(Syms),!IO) :-
    read_symbols(Stream,SymInfo,Syms,!IO).
read_ll_instr_args(_,_,_,add_0,add,!IO).
read_ll_instr_args(_,_,_,subtract_0,subtract,!IO).
read_ll_instr_args(_,_,_,multiply_0,multiply,!IO).
read_ll_instr_args(_,_,_,divide_0,divide,!IO).
read_ll_instr_args(_,_,_,mod_0,mod,!IO).
read_ll_instr_args(_,_,_,negate_0,negate,!IO).
read_ll_instr_args(_,_,_,and_0,and,!IO).
read_ll_instr_args(_,_,_,or_0,or,!IO).
read_ll_instr_args(_,_,_,iff_0,iff,!IO).
read_ll_instr_args(_,_,_,xor_0,xor,!IO).
read_ll_instr_args(_,_,_,impl_0,impl,!IO).
read_ll_instr_args(_,_,_,not_0,not,!IO).
read_ll_instr_args(_,_,_,lt_0,lt,!IO).
read_ll_instr_args(_,_,_,leq_0,leq,!IO).
read_ll_instr_args(_,_,_,geq_0,geq,!IO).
read_ll_instr_args(_,_,_,gt_0,gt,!IO).
read_ll_instr_args(_,_,_,eq_0,eq,!IO).
read_ll_instr_args(_,_,_,neq_0,neq,!IO).
read_ll_instr_args(Stream,_,_,lookup_is_int_2,lookup_is_int(SrcReg,Int),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_binary_int(Stream,Int,!IO).
read_ll_instr_args(Stream,_,_,lookup_is_float_2,lookup_is_float(SrcReg,Flt),
        !IO) :-
    read_register(Stream,SrcReg,!IO),
    read_binary_float(Stream,Flt,!IO).
read_ll_instr_args(Stream,_,StrInfo,lookup_is_string_2,
        lookup_is_string(SrcReg,Str),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_string(Stream,StrInfo,Str,!IO).
read_ll_instr_args(Stream,_,StrInfo,lookup_is_named_var_2,
        lookup_is_named_var(SrcReg,Name),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_string(Stream,StrInfo,Name,!IO).
read_ll_instr_args(Stream,SymInfo,_,lookup_is_functor_2,
        lookup_is_functor(SrcReg,Sym),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,lookup_is_ac_functor_3,
        lookup_is_ac_functor(SrcReg,Sym),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,SymInfo,_,lookup_is_geq_ac_functor_3,
        lookup_is_geq_ac_functor(SrcReg,Sym),!IO) :-
    read_register(Stream,SrcReg,!IO),
    read_symbol(Stream,SymInfo,Sym,!IO).
read_ll_instr_args(Stream,_,_,lookup_is_type_2,lookup_is_type(SrcReg,Type),
        !IO) :-
    read_register(Stream,SrcReg,!IO),
    read_type(Stream,Type,!IO).
read_ll_instr_args(Stream,_,_,lookup_compare_2,lookup_compare(Reg0,Reg1),
        !IO) :-
    read_register(Stream,Reg0,!IO),
    read_register(Stream,Reg1,!IO).
read_ll_instr_args(_,_,_,lookup_accept_0,lookup_accept,!IO).
read_ll_instr_args(Stream,SymInfo,StrInfo,debug_1,debug(DebugInfo),!IO) :-
    read_symbol(Stream,SymInfo,Sym,!IO),
    read_program_point(Stream,SymInfo,PP,!IO),
    read_cxt(Stream,StrInfo,Cxt,!IO),
    read_var_reg_info(Stream,StrInfo,init,VarRegInfo,!IO),
    DebugInfo = debug_info(Sym,PP,Cxt,VarRegInfo).
read_ll_instr_args(Stream,_,_,label_1,label(Label),!IO) :-
    read_label(Stream,Label,!IO).

%---------------------------------------------------------------------------%

:- pred read_switch_table(binary_input_stream::in,pseudo_sym_info_r::in,
    int::in,ll_switch_table::out,io::di,io::uo) is det.

read_switch_table(Stream,SymInfo,TbLen,Table,!IO) :-
    read_switch_table_2(Stream,SymInfo,TbLen,[],Syms,[],Labels,!IO),
    Table = ll_switch_table(Syms,Labels).

%---------------------------------------------------------------------------%

:- pred read_switch_table_2(binary_input_stream::in,pseudo_sym_info_r::in,
    int::in,list(hl_symbol)::in,list(hl_symbol)::out,list(label)::in,
    list(label)::out,io::di,io::uo) is det.

read_switch_table_2(Stream,SymInfo,TbLen,!Syms,!Labels,!IO) :-
    ( TbLen = 0 ->
        true
    ;   read_symbol(Stream,SymInfo,Sym,!IO),
        read_label(Stream,Label,!IO),
        !:Syms = [Sym|!.Syms],
        !:Labels = [Label|!.Labels],
        read_switch_table_2(Stream,SymInfo,TbLen-1,!Syms,!Labels,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred read_program_point(binary_input_stream::in,pseudo_sym_info_r::in,
    program_point(hl_symbol)::out,io::di,io::uo) is det.
    
read_program_point(Stream,SymInfo,PP,!IO) :-
    read_binary_byte(Stream,Code,!IO),
    ( Code = 0 ->
        read_symbol(Stream,SymInfo,Sym,!IO),
        PP = call(Sym)
    ; Code = 1 ->
        read_rule_id(Stream,RuleId,!IO),
        PP = pass(RuleId)
    ; Code = 2 ->
        read_rule_id(Stream,RuleId,!IO),
        PP = fail(RuleId)
    ;   read_symbol(Stream,SymInfo,Sym,!IO),
        read_binary_byte(Stream,BoolCode,!IO),
        ( BoolCode = 0 ->
            Bool = no
        ;   Bool = yes
        ),
        PP = return(Sym,Bool)
    ).
    
%---------------------------------------------------------------------------%

:- pred read_cxt(binary_input_stream::in,string_info_r::in,cxt::out,
    io::di,io::uo) is det.

read_cxt(Stream,StrInfo,Cxt,!IO) :-
    read_binary_byte(Stream,Code,!IO),
    ( Code = 0 ->
        Cxt = none
    ;   read_string(Stream,StrInfo,File,!IO),
        read_binary_24_bit_int(Stream,Line,!IO),
        Cxt = cxt(File,Line)
    ).

%---------------------------------------------------------------------------%

:- pred read_var_reg_info(binary_input_stream::in,string_info_r::in,
    var_reg_info::in,var_reg_info::out,io::di,io::uo) is det.

read_var_reg_info(Stream,StrInfo,!VarRegInfo,!IO) :-
    read_binary_byte(Stream,Code,!IO),
    ( Code = 0 ->
        read_string(Stream,StrInfo,Name,!IO),
        read_register(Stream,Reg,!IO),
        map.set(Name,Reg,!VarRegInfo),
        read_var_reg_info(Stream,StrInfo,!VarRegInfo,!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred read_rule_id(binary_input_stream::in,rule_id::out,io::di,io::uo)
    is det.

read_rule_id(Stream,RuleId,!IO) :-
    read_binary_16_bit_int(Stream,RuleId,!IO).

%---------------------------------------------------------------------------%

:- pred read_symbol_table(binary_input_stream::in,pseudo_symbol::in,
    pseudo_sym_info_r::in,pseudo_sym_info_r::out,string_info_r::in,
    string_info_r::out,char::out,io::di,io::uo) is det.

read_symbol_table(Stream,PSym,!SymInfo,!StrInfo,Tag0,!IO) :-
    read_tag(Stream,Tag,!IO),
    ( Tag = 'S' ->
        read_binary_byte(Stream,Code,!IO),
        read_binary_string(Stream,Str,!IO),
        ( Code = symbol_code ->
            map.det_insert(PSym,Str,!SymInfo)
        ;   map.det_insert(PSym,Str,!StrInfo)
        ),
        read_symbol_table(Stream,PSym+1,!SymInfo,!StrInfo,Tag0,!IO)
    ;   Tag0 = Tag
    ).

%---------------------------------------------------------------------------%

    % Returns ' ' on an error.
    %
:- pred read_tag(binary_input_stream::in,char::out,io::di,io::uo) is det.

read_tag(Stream,Tag,!IO) :-
    read_binary_char(Stream,Ch0,!IO),
    ( Ch0 = '\n' ->
        read_binary_char(Stream,Ch1,!IO),
        ( Ch1 = '#' ->
            read_binary_char(Stream,Tag,!IO)
        ;   Tag = ' '
        )
    ;   Tag = ' '
    ).

%---------------------------------------------------------------------------%

:- pred read_register(binary_input_stream::in,register::out,io::di,io::uo)
    is det.

read_register(Stream,Reg,!IO) :-
    read_binary_16_bit_int(Stream,Reg,!IO).

%---------------------------------------------------------------------------%

:- pred read_label(binary_input_stream::in,label::out,io::di,io::uo) is det.

read_label(Stream,Label,!IO) :-
    read_binary_24_bit_int(Stream,Label,!IO).

%---------------------------------------------------------------------------%

:- pred read_symbol(binary_input_stream::in,pseudo_sym_info_r::in,
    hl_symbol::out,io::di,io::uo) is det.

read_symbol(Stream,SymInfo,Sym,!IO) :-
    read_binary_24_bit_int(Stream,FakeSym,!IO),
    read_binary_byte(Stream,Aty,!IO),
    lookup(SymInfo,FakeSym,Name),
    Sym = hl_symbol(Name,Aty).

%---------------------------------------------------------------------------%

:- pred read_symbols(binary_input_stream::in,pseudo_sym_info_r::in,
    list(hl_symbol)::out,io::di,io::uo) is det.

read_symbols(Stream,SymInfo,Syms,!IO) :-
    read_binary_16_bit_int(Stream,Len,!IO),
    read_symbols_2(Stream,SymInfo,Len,[],Syms,!IO).

%---------------------------------------------------------------------------%

:- pred read_symbols_2(binary_input_stream::in,pseudo_sym_info_r::in,
    int::in,list(hl_symbol)::in,list(hl_symbol)::out,io::di,io::uo) is det.

read_symbols_2(Stream,SymInfo,Len,!Syms,!IO) :-
    ( Len =< 0 ->
        true
    ;   read_symbol(Stream,SymInfo,Sym,!IO),
        !:Syms = [Sym|!.Syms],
        read_symbols_2(Stream,SymInfo,Len-1,!Syms,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred read_string(binary_input_stream::in,string_info_r::in,string::out,
    io::di,io::uo) is det.

read_string(Stream,StrInfo,Str,!IO) :-
    read_binary_24_bit_int(Stream,FakeStr,!IO),
    lookup(StrInfo,FakeStr,Str).

%---------------------------------------------------------------------------%

:- pred read_type(binary_input_stream::in,hl_model_type::out,io::di,io::uo)
    is det.

read_type(Stream,Type,!IO) :-
    read_binary_byte(Stream,Code,!IO),
    ( model_type_code(Type0,Code) ->
        Type = Type0
    ;   Message = message("invalid type code (%d)",[i(Code)],none),
        throw_compiler_error(Message,!IO),
        Type = int
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

write_ll_prog(Stream,Debug,LLProg,Imports,!IO) :-
    write_binary_chars(Stream,"#!cadmium",!IO),
    write_tag(Stream,'V',!IO),
    write_binary_16_bit_int(Stream,version_number,!IO),
    write_binary_byte(Stream,bits_per_int,!IO),
    ( Debug = yes,
        write_binary_byte(Stream,1,!IO)
    ; Debug = no,
        write_binary_byte(Stream,0,!IO)
    ),
    write_tag(Stream,'M',!IO),
    fold(write_import(Stream),Imports,!IO),
    write_tag(Stream,'T',!IO),
    foldl4(write_symbol_table(Stream),LLProg,1,_,init,SymInfo,init,StrInfo,
        !IO),
    write_tag(Stream,'C',!IO),
    foldl(write_ll_proc(Stream,SymInfo,StrInfo),LLProg,!IO),
    write_tag(Stream,'E',!IO).

%---------------------------------------------------------------------------%

:- pred write_import(binary_output_stream::in,string::in,io::di,io::uo)
    is det.

write_import(Stream,Import,!IO) :-
    write_tag(Stream,'I',!IO),
    write_binary_string(Stream,Import,!IO).

%---------------------------------------------------------------------------%

:- pred write_symbol_table(binary_output_stream::in,hl_symbol::in,ll_proc::in,
    pseudo_symbol::in,pseudo_symbol::out,pseudo_sym_info_w::in,
    pseudo_sym_info_w::out,string_info_w::in,string_info_w::out,
    io::di,io::uo) is det.

write_symbol_table(Stream,Sym,Proc,!PSym,!SymInfo,!StrInfo,!IO) :-
    write_symbol(Stream,Sym,!PSym,!SymInfo,!IO),
    Proc = ll_proc(_,Instrs,LookupInstrs,Creates),
    foldl3(write_symbol(Stream),Creates,!PSym,!SymInfo,!IO),
    foldl4(write_symbol_table_instr(Stream),Instrs,!PSym,!SymInfo,!StrInfo,
        !IO),
    foldl4(write_symbol_table_instrs(Stream),LookupInstrs,!PSym,!SymInfo,
        !StrInfo,!IO).

%---------------------------------------------------------------------------%

:- pred write_symbol_table_instr(binary_output_stream::in,ll_instr::in,
    pseudo_symbol::in,pseudo_symbol::out,pseudo_sym_info_w::in,
    pseudo_sym_info_w::out,string_info_w::in,string_info_w::out,io::di,io::uo)
    is det.

write_symbol_table_instr(Stream,Instr,!PSym,!SymInfo,!StrInfo,!IO) :-
    ( get_ll_instr_symbols(Instr,Syms) ->
        foldl3(write_symbol(Stream),Syms,!PSym,!SymInfo,!IO)
    ;   true
    ),
    ( get_ll_instr_strings(Instr,Strs) ->
        foldl3(write_string(Stream),Strs,!PSym,!StrInfo,!IO)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- pred write_symbol_table_instrs(binary_output_stream::in,label::in,
    list(ll_instr)::in,pseudo_symbol::in,pseudo_symbol::out,
    pseudo_sym_info_w::in,pseudo_sym_info_w::out,string_info_w::in,
    string_info_w::out,io::di,io::uo) is det.

write_symbol_table_instrs(Stream,_,Instrs,!PSym,!SymInfo,!StrInfo,!IO) :-
    foldl4(write_symbol_table_instr(Stream),Instrs,!PSym,!SymInfo,!StrInfo,
        !IO).

%---------------------------------------------------------------------------%

:- pred get_ll_instr_symbols(ll_instr::in,list(hl_symbol)::out) is semidet.

get_ll_instr_symbols(is_functor(_,Sym),[Sym]).
get_ll_instr_symbols(is_ac_functor(_,Sym),[Sym]).
get_ll_instr_symbols(is_ac_functor(Sym),[Sym]).
get_ll_instr_symbols(is_geq_ac_functor(_,Sym),[Sym]).
get_ll_instr_symbols(is_geq_ac_functor(Sym),[Sym]).
get_ll_instr_symbols(switch(_,Table,_),Syms) :-
    Table = ll_switch_table(Syms,_).
get_ll_instr_symbols(call_sym(Sym),[Sym]).
get_ll_instr_symbols(call_sym_lco(Sym),[Sym]).
get_ll_instr_symbols(call_ac_arg_sym(Sym1,Sym2),[Sym1,Sym2]).
get_ll_instr_symbols(call_ac_sym_lco(Sym),[Sym]).
get_ll_instr_symbols(call_ac_arg_label(Sym,_),[Sym]).
get_ll_instr_symbols(construct(Sym),[Sym]).
get_ll_instr_symbols(construct_ac(Sym,_),[Sym]).
get_ll_instr_symbols(construct_acd(_,Sym,_),[Sym]).
get_ll_instr_symbols(construct_acd(_,Sym,_,_),[Sym]).
get_ll_instr_symbols(wakeup_on(Syms),Syms).
get_ll_instr_symbols(lookup_is_functor(_,Sym),[Sym]).
get_ll_instr_symbols(lookup_is_ac_functor(_,Sym),[Sym]).
get_ll_instr_symbols(lookup_is_geq_ac_functor(_,Sym),[Sym]).
get_ll_instr_symbols(debug(DebugInfo),Syms) :-
    DebugInfo = debug_info(Sym0,PP,_,_),
    ( PP = call(Sym1),
        Syms = [Sym0,Sym1]
    ; PP = return(Sym1,_),
        Syms = [Sym0,Sym1]
    ; ( PP = pass(_)
      ; PP = fail(_) ),
        Syms = [Sym0]
    ).

%---------------------------------------------------------------------------%

:- pred get_ll_instr_strings(ll_instr::in,list(string)::out) is semidet.

get_ll_instr_strings(is_string(_,Str),[Str]).
get_ll_instr_strings(is_named_var(_,Str),[Str]).
get_ll_instr_strings(push_string(Str),[Str]).
get_ll_instr_strings(push_named_var(Str),[Str]).
get_ll_instr_strings(call_foreign(_,Str),[Str]).
get_ll_instr_strings(lookup_is_string(_,Str),[Str]).
get_ll_instr_strings(lookup_is_named_var(_,Str),[Str]).
get_ll_instr_strings(debug(DebugInfo),Strs) :-
    DebugInfo = debug_info(_,_,Cxt,VarRegInfo),
    keys(VarRegInfo,Strs0),
    ( Cxt = cxt(File,_),
        Strs = [File|Strs0]
    ; Cxt = none,
        Strs = Strs0
    ).

%---------------------------------------------------------------------------%

:- pred write_symbol(binary_output_stream::in,hl_symbol::in,pseudo_symbol::in,
    pseudo_symbol::out,pseudo_sym_info_w::in,pseudo_sym_info_w::out,
    io::di,io::uo) is det.

write_symbol(Stream,Sym,!PSym,!SymInfo,!IO) :-
    Name = hl_symbol_name(Sym),
    ( contains(!.SymInfo,Name) ->
        true
    ;   write_tag(Stream,'S',!IO),
        write_binary_byte(Stream,symbol_code,!IO),
        write_binary_string(Stream,Name,!IO),
        map.det_insert(Name,!.PSym,!SymInfo),
        !:PSym = !.PSym + 1
    ).

%---------------------------------------------------------------------------%

:- pred write_string(binary_output_stream::in,string::in,pseudo_symbol::in,
    pseudo_symbol::out,string_info_w::in,string_info_w::out,io::di,io::uo) 
    is det.

write_string(Stream,Str,!PSym,!StrInfo,!IO) :-
    ( contains(!.StrInfo,Str) ->
        true
    ;   write_tag(Stream,'S',!IO),
        write_binary_byte(Stream,string_code,!IO),
        write_binary_string(Stream,Str,!IO),
        map.det_insert(Str,!.PSym,!StrInfo),
        !:PSym = !.PSym + 1
    ).

%---------------------------------------------------------------------------%

:- pred write_ll_proc(binary_output_stream::in,pseudo_sym_info_w::in,
    string_info_w::in,hl_symbol::in,ll_proc::in,io::di,io::uo) is det.

write_ll_proc(Stream,SymInfo,StrInfo,Sym,Proc,!IO) :-
    Proc = ll_proc(Label,Instrs,LookupInstrs,Conds),
    write_tag(Stream,'P',!IO),
    write_symbol(Stream,SymInfo,Sym,!IO),
    write_symbols(Stream,SymInfo,Conds,!IO),
    write_label(Stream,Label,!IO),
    foldr(write_ll_instr(Stream,SymInfo,StrInfo),Instrs,!IO),
    write_binary_byte(Stream,end_instrs_code,!IO),
    foldl(write_ll_lookup_instrs(Stream,SymInfo,StrInfo),LookupInstrs,!IO),
    write_binary_byte(Stream,no_lookup_code,!IO).

%---------------------------------------------------------------------------%

:- pred write_ll_lookup_instrs(binary_output_stream::in,pseudo_sym_info_w::in,
    string_info_w::in,label::in,list(ll_instr)::in,io::di,io::uo) is det.

write_ll_lookup_instrs(Stream,SymInfo,StrInfo,Label,Instrs,!IO) :-
    write_binary_byte(Stream,have_lookup_code,!IO),
    write_label(Stream,Label,!IO),
    foldr(write_ll_instr(Stream,SymInfo,StrInfo),Instrs,!IO),
    write_binary_byte(Stream,end_instrs_code,!IO).

%---------------------------------------------------------------------------%

:- pred write_ll_instr(binary_output_stream::in,pseudo_sym_info_w::in,
    string_info_w::in,ll_instr::in,io::di,io::uo) is det.

write_ll_instr(Stream,SymInfo,StrInfo,Instr,!IO) :-
    write_ll_instr_0_code(Stream,Instr,!IO),
    write_ll_instr_args(Stream,SymInfo,StrInfo,Instr,!IO).
    
%---------------------------------------------------------------------------%

:- pred write_ll_instr_args(binary_output_stream::in,pseudo_sym_info_w::in,
    string_info_w::in,ll_instr::in,io::di,io::uo) is det.

write_ll_instr_args(Stream,_,_,is_int(Reg,Int),!IO) :-
    write_register(Stream,Reg,!IO),
    write_binary_int(Stream,Int,!IO).
write_ll_instr_args(Stream,_,_,is_float(Reg,Flt),!IO) :-
    write_register(Stream,Reg,!IO),
    write_binary_float(Stream,Flt,!IO).
write_ll_instr_args(Stream,_,StrInfo,is_string(Reg,Str),!IO) :-
    write_register(Stream,Reg,!IO),
    write_string(Stream,StrInfo,Str,!IO).
write_ll_instr_args(Stream,_,StrInfo,is_named_var(Reg,NameStr),!IO) :-
    write_register(Stream,Reg,!IO),
    write_string(Stream,StrInfo,NameStr,!IO).
write_ll_instr_args(Stream,SymInfo,_,is_functor(Reg,Sym),!IO) :-
    write_register(Stream,Reg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,is_ac_functor(Reg,Sym),!IO) :-
    write_register(Stream,Reg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,is_ac_functor(Sym),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,is_geq_ac_functor(Reg,Sym),!IO) :-
    write_register(Stream,Reg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,is_geq_ac_functor(Sym),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,_,_,is_type(Reg,Type),!IO) :-
    write_register(Stream,Reg,!IO),
    write_type(Stream,Type,!IO).
write_ll_instr_args(Stream,_,_,get_arg(SrcReg,Idx,DstReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_binary_byte(Stream,Idx,!IO),
    write_register(Stream,DstReg,!IO).
write_ll_instr_args(Stream,_,_,get_arg(Idx,DstReg),!IO) :-
    write_binary_byte(Stream,Idx,!IO),
    write_register(Stream,DstReg,!IO).
write_ll_instr_args(Stream,_,_,pre_guard(ChReg),!IO) :-
    write_register(Stream,ChReg,!IO).
write_ll_instr_args(Stream,_,_,post_guard(ChReg),!IO) :-
    write_register(Stream,ChReg,!IO).
write_ll_instr_args(Stream,_,_,post_match_guard(ResReg,ChReg),!IO) :-
    write_register(Stream,ResReg,!IO),
    write_register(Stream,ChReg,!IO).
write_ll_instr_args(Stream,SymInfo,_,switch(SrcReg,Table,TbLen),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_binary_24_bit_int(Stream,TbLen,!IO),
    write_switch_table(Stream,SymInfo,TbLen,Table,!IO).
write_ll_instr_args(Stream,_,_,pop(N),!IO) :-
    write_binary_byte(Stream,N,!IO).
write_ll_instr_args(Stream,SymInfo,_,construct(Sym),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,construct_ac(Sym,Aty),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO),
    write_binary_byte(Stream,Aty,!IO).
write_ll_instr_args(Stream,SymInfo,_,construct_acd(DstReg,Sym,Aty),!IO) :-
    write_register(Stream,DstReg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO),
    write_binary_byte(Stream,Aty,!IO).
write_ll_instr_args(Stream,SymInfo,_,construct_acd(DstReg,Sym,Aty,CollReg),
        !IO) :-
    write_register(Stream,DstReg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO),
    write_binary_byte(Stream,Aty,!IO),
    write_register(Stream,CollReg,!IO).
write_ll_instr_args(_,_,_,interpret,!IO).
write_ll_instr_args(Stream,_,_,interpret(SrcReg),!IO) :-
    write_register(Stream,SrcReg,!IO).
write_ll_instr_args(_,_,_,push_new_var,!IO).
write_ll_instr_args(Stream,_,_,push_int(Int),!IO) :-
    write_binary_int(Stream,Int,!IO).
write_ll_instr_args(Stream,_,_,push_float(Flt),!IO) :-
    write_binary_float(Stream,Flt,!IO).
write_ll_instr_args(Stream,_,StrInfo,push_string(Str),!IO) :-
    write_string(Stream,StrInfo,Str,!IO).
write_ll_instr_args(Stream,_,StrInfo,push_named_var(NameStr),!IO) :-
    write_string(Stream,StrInfo,NameStr,!IO).
write_ll_instr_args(Stream,_,_,ccpush(Reg),!IO) :-
    write_register(Stream,Reg,!IO).
write_ll_instr_args(_,_,_,ccpop,!IO).
write_ll_instr_args(Stream,_,_,cpush(N),!IO) :-
    write_binary_byte(Stream,N,!IO).
write_ll_instr_args(Stream,_,_,cset(Reg),!IO) :-
    write_register(Stream,Reg,!IO).
write_ll_instr_args(Stream,_,_,cget(Reg),!IO) :-
    write_register(Stream,Reg,!IO).
write_ll_instr_args(Stream,_,_,ccopy(SrcReg,DstReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_register(Stream,DstReg,!IO).
write_ll_instr_args(Stream,_,_,ceqeq(Reg0,Reg1),!IO) :-
    write_register(Stream,Reg0,!IO),
    write_register(Stream,Reg1,!IO).
write_ll_instr_args(Stream,_,_,cpop(N),!IO) :-
    write_binary_byte(Stream,N,!IO).
write_ll_instr_args(Stream,_,_,call_label(Label),!IO) :-
    write_label(Stream,Label,!IO).
write_ll_instr_args(Stream,_,_,call_label_lco(Label),!IO) :-
    write_label(Stream,Label,!IO).
write_ll_instr_args(Stream,SymInfo,_,call_ac_arg_label(ACSym,Label),!IO) :-
    write_symbol(Stream,SymInfo,ACSym,!IO),
    write_label(Stream,Label,!IO).
write_ll_instr_args(Stream,_,_,call_ac_label_lco(Label),!IO) :-
    write_label(Stream,Label,!IO).
write_ll_instr_args(Stream,SymInfo,_,call_sym(Sym),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,call_sym_lco(Sym),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,call_ac_arg_sym(ACSym,Sym),!IO) :-
    write_symbol(Stream,SymInfo,ACSym,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,call_ac_sym_lco(Sym),!IO) :-
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,_,_,call_ho(N),!IO) :-
    write_binary_byte(Stream,N,!IO).
write_ll_instr_args(_,_,_,return,!IO).
write_ll_instr_args(Stream,_,_,create_cp(Label),!IO) :-
    write_label(Stream,Label,!IO).
write_ll_instr_args(_,_,_,commit,!IO).
write_ll_instr_args(Stream,_,_,commit(N,S),!IO) :-
    write_binary_byte(Stream,N,!IO),
    write_binary_byte(Stream,S,!IO).
write_ll_instr_args(Stream,_,StrInfo,call_foreign(N,Name),!IO) :-
    write_binary_byte(Stream,N,!IO),
    write_string(Stream,StrInfo,Name,!IO).
write_ll_instr_args(_,_,_,halt,!IO).
write_ll_instr_args(Stream,_,_,
        lookup_(SrcReg,LookupLabel,ArgReg,NodeReg,PosReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO),
    write_register(Stream,NodeReg,!IO),
    write_register(Stream,PosReg,!IO).
write_ll_instr_args(Stream,_,_,lookup_(LookupLabel,ArgReg,NodeReg,PosReg),
        !IO) :-
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO),
    write_register(Stream,NodeReg,!IO),
    write_register(Stream,PosReg,!IO).
write_ll_instr_args(Stream,_,_,lookup_first(SrcReg,LookupLabel,ArgReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO).
write_ll_instr_args(Stream,_,_,lookup_first(LookupLabel,ArgReg),!IO) :-
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO).
write_ll_instr_args(Stream,_,_,split(SrcReg,ArgReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_register(Stream,ArgReg,!IO).
write_ll_instr_args(Stream,_,_,init_itr(NodeReg,PosReg,ItrReg),!IO) :-
    write_register(Stream,NodeReg,!IO),
    write_register(Stream,PosReg,!IO),
    write_register(Stream,ItrReg,!IO).
write_ll_instr_args(Stream,_,_,get_next(ItrReg,Label,LookupLabel,ArgReg,IdReg),
        !IO) :-
    write_register(Stream,ItrReg,!IO),
    write_label(Stream,Label,!IO),
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO),
    write_register(Stream,IdReg,!IO).
write_ll_instr_args(Stream,_,_,
        lookup_cc(LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),!IO) :-
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ItrNoReg,!IO),
    write_register(Stream,ArgReg,!IO),
    write_register(Stream,NodeReg,!IO),
    write_register(Stream,PosReg,!IO).
write_ll_instr_args(Stream,_,_,
        lookup_cc(CollReg,LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),!IO) :-
    write_register(Stream,CollReg,!IO),
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ItrNoReg,!IO),
    write_register(Stream,ArgReg,!IO),
    write_register(Stream,NodeReg,!IO),
    write_register(Stream,PosReg,!IO).
write_ll_instr_args(Stream,_,_,lookup_first_cc(LookupLabel,ArgReg),!IO) :-
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO).
write_ll_instr_args(Stream,_,_,lookup_first_cc(CollReg,LookupLabel,ArgReg),
        !IO) :-
    write_register(Stream,CollReg,!IO),
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO).
write_ll_instr_args(Stream,_,_,
        get_next_cc(ItrNoReg,ItrReg,Label,LookupLabel,ArgReg,IdReg),!IO) :-
    write_register(Stream,ItrNoReg,!IO),
    write_register(Stream,ItrReg,!IO),
    write_label(Stream,Label,!IO),
    write_label(Stream,LookupLabel,!IO),
    write_register(Stream,ArgReg,!IO),
    write_register(Stream,IdReg,!IO).
write_ll_instr_args(_,_,_,top_level,!IO).
write_ll_instr_args(Stream,_,_,is_diff(Reg1,Reg2),!IO) :-
    write_register(Stream,Reg1,!IO),
    write_register(Stream,Reg2,!IO).
write_ll_instr_args(Stream,_,_,delete(SrcReg,ArgReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_register(Stream,ArgReg,!IO).
write_ll_instr_args(Stream,_,_,flatten(SrcReg),!IO) :-
    write_register(Stream,SrcReg,!IO).
write_ll_instr_args(Stream,_,_,set_annots(SrcReg),!IO) :-
    write_register(Stream,SrcReg,!IO).
write_ll_instr_args(Stream,_,_,get_annots(DstReg),!IO) :-
    write_register(Stream,DstReg,!IO).
write_ll_instr_args(Stream,_,_,get_annots(SrcReg,DstReg),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_register(Stream,DstReg,!IO).
write_ll_instr_args(_,_,_,clear_annots,!IO).
write_ll_instr_args(_,_,_,restore_annots,!IO).
write_ll_instr_args(_,_,_,setup_annots,!IO).
write_ll_instr_args(_,_,_,call_annots,!IO).
write_ll_instr_args(_,_,_,attach_annots,!IO).
write_ll_instr_args(_,_,_,wakeup_on_redo,!IO).
write_ll_instr_args(Stream,SymInfo,_,wakeup_on(Syms),!IO) :-
    write_symbols(Stream,SymInfo,Syms,!IO).
write_ll_instr_args(_,_,_,add,!IO).
write_ll_instr_args(_,_,_,subtract,!IO).
write_ll_instr_args(_,_,_,multiply,!IO).
write_ll_instr_args(_,_,_,divide,!IO).
write_ll_instr_args(_,_,_,mod,!IO).
write_ll_instr_args(_,_,_,negate,!IO).
write_ll_instr_args(_,_,_,and,!IO).
write_ll_instr_args(_,_,_,or,!IO).
write_ll_instr_args(_,_,_,iff,!IO).
write_ll_instr_args(_,_,_,xor,!IO).
write_ll_instr_args(_,_,_,impl,!IO).
write_ll_instr_args(_,_,_,not,!IO).
write_ll_instr_args(_,_,_,lt,!IO).
write_ll_instr_args(_,_,_,leq,!IO).
write_ll_instr_args(_,_,_,gt,!IO).
write_ll_instr_args(_,_,_,geq,!IO).
write_ll_instr_args(_,_,_,eq,!IO).
write_ll_instr_args(_,_,_,neq,!IO).
write_ll_instr_args(Stream,_,_,lookup_is_int(SrcReg,Int),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_binary_int(Stream,Int,!IO).
write_ll_instr_args(Stream,_,_,lookup_is_float(SrcReg,Flt),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_binary_float(Stream,Flt,!IO).
write_ll_instr_args(Stream,_,StrInfo,lookup_is_string(SrcReg,Str),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_string(Stream,StrInfo,Str,!IO).
write_ll_instr_args(Stream,_,StrInfo,lookup_is_named_var(SrcReg,Name),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_string(Stream,StrInfo,Name,!IO).
write_ll_instr_args(Stream,SymInfo,_,lookup_is_functor(SrcReg,Sym),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,lookup_is_ac_functor(SrcReg,Sym),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,SymInfo,_,lookup_is_geq_ac_functor(SrcReg,Sym),
        !IO) :-
    write_register(Stream,SrcReg,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_ll_instr_args(Stream,_,_,lookup_is_type(SrcReg,Type),!IO) :-
    write_register(Stream,SrcReg,!IO),
    write_type(Stream,Type,!IO).
write_ll_instr_args(Stream,_,_,lookup_compare(Reg0,Reg1),!IO) :-
    write_register(Stream,Reg0,!IO),
    write_register(Stream,Reg1,!IO).
write_ll_instr_args(_,_,_,lookup_accept,!IO).
write_ll_instr_args(Stream,SymInfo,StrInfo,debug(DebugInfo),!IO) :-
    DebugInfo = debug_info(Sym,PP,Cxt,VarRegInfo),
    write_symbol(Stream,SymInfo,Sym,!IO),
    write_program_point(Stream,SymInfo,PP,!IO),
    write_cxt(Stream,StrInfo,Cxt,!IO),
    write_var_reg_info(Stream,StrInfo,VarRegInfo,!IO).
write_ll_instr_args(Stream,_,_,label(Label),!IO) :-
    write_label(Stream,Label,!IO).

%---------------------------------------------------------------------------%

:- pred write_switch_table(binary_output_stream::in,pseudo_sym_info_w::in,
    int::in,ll_switch_table::in,io::di,io::uo) is det.

write_switch_table(Stream,SymInfo,TbLen,Table,!IO) :-
    Table = ll_switch_table(Syms,Labels),
    write_switch_table_2(Stream,SymInfo,TbLen,Syms,Labels,!IO).

%---------------------------------------------------------------------------%

:- pred write_switch_table_2(binary_output_stream::in,pseudo_sym_info_w::in,
    int::in,list(hl_symbol)::in,list(label)::in,io::di,io::uo) is det.

write_switch_table_2(Stream,SymInfo,TbLen,Syms,Labels,!IO) :-
    ( TbLen = 0 ->
        true
    ;   write_symbol(Stream,SymInfo,det_head(Syms),!IO),
        write_label(Stream,det_head(Labels),!IO),
        write_switch_table_2(Stream,SymInfo,TbLen-1,det_tail(Syms),
            det_tail(Labels),!IO)
    ).

%---------------------------------------------------------------------------%

:- pred write_program_point(binary_output_stream::in,pseudo_sym_info_w::in,
    program_point(hl_symbol)::in,io::di,io::uo) is det.

write_program_point(Stream,SymInfo,call(Sym),!IO) :-
    write_binary_byte(Stream,0,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO).
write_program_point(Stream,_,pass(RuleId),!IO) :-
    write_binary_byte(Stream,1,!IO),
    write_rule_id(Stream,RuleId,!IO).
write_program_point(Stream,_,fail(RuleId),!IO) :-
    write_binary_byte(Stream,2,!IO),
    write_rule_id(Stream,RuleId,!IO).
write_program_point(Stream,SymInfo,return(Sym,Bool),!IO) :-
    write_binary_byte(Stream,3,!IO),
    write_symbol(Stream,SymInfo,Sym,!IO),
    ( Bool = yes,
        write_binary_byte(Stream,1,!IO)
    ; Bool = no,
        write_binary_byte(Stream,0,!IO)
    ).

%---------------------------------------------------------------------------%

:- pred write_cxt(binary_output_stream::in,string_info_w::in,cxt::in,
    io::di,io::uo) is det.

write_cxt(Stream,_,none,!IO) :-
    write_binary_byte(Stream,0,!IO).
write_cxt(Stream,StrInfo,cxt(File,Line),!IO) :-
    write_binary_byte(Stream,1,!IO),
    write_string(Stream,StrInfo,File,!IO),
    write_binary_24_bit_int(Stream,Line,!IO).

%---------------------------------------------------------------------------%

:- pred write_var_reg_info(binary_output_stream::in,string_info_w::in,
    var_reg_info::in,io::di,io::uo) is det.

write_var_reg_info(Stream,StrInfo,VarRegInfo,!IO) :-
    foldl(write_var_reg_pair(Stream,StrInfo),VarRegInfo,!IO),
    write_binary_byte(Stream,1,!IO).

%---------------------------------------------------------------------------%

:- pred write_var_reg_pair(binary_output_stream::in,string_info_w::in,
    string::in,register::in,io::di,io::uo) is det.

write_var_reg_pair(Stream,StrInfo,Name,Reg,!IO) :-
    write_binary_byte(Stream,0,!IO),
    write_string(Stream,StrInfo,Name,!IO),
    write_register(Stream,Reg,!IO).

%---------------------------------------------------------------------------%

:- pred write_rule_id(binary_output_stream::in,rule_id::in,io::di,io::uo)
    is det.

write_rule_id(Stream,RuleId,!IO) :-
    write_binary_16_bit_int(Stream,RuleId,!IO).

%---------------------------------------------------------------------------%  

:- pred write_ll_instr_0_code(binary_output_stream::in,ll_instr::in,
    io::di,io::uo) is det.

write_ll_instr_0_code(Stream,Instr,!IO) :-
    ll_instr_to_ll_instr_0(Instr,Instr0),
    ll_instr_0_code(Instr0,Code),
    write_binary_byte(Stream,Code,!IO).

%---------------------------------------------------------------------------%

:- pred write_register(binary_output_stream::in,register::in,io::di,io::uo) 
    is det.

write_register(Stream,Reg,!IO) :-
    write_binary_16_bit_int(Stream,Reg,!IO).

%---------------------------------------------------------------------------%

:- pred write_label(binary_output_stream::in,label::in,io::di,io::uo) is det.

write_label(Stream,Label,!IO) :-
    write_binary_24_bit_int(Stream,Label,!IO).

%---------------------------------------------------------------------------%

:- pred write_symbol(binary_output_stream::in,pseudo_sym_info_w::in,
    hl_symbol::in,io::di,io::uo) is det.

write_symbol(Stream,SymInfo,Sym,!IO) :-
    Name = hl_symbol_name(Sym),
    lookup(SymInfo,Name,FakeSym),
    write_binary_24_bit_int(Stream,FakeSym,!IO),
    Aty = hl_symbol_arity(Sym),
    write_binary_byte(Stream,Aty,!IO).

%---------------------------------------------------------------------------%

:- pred write_symbols(binary_output_stream::in,pseudo_sym_info_w::in,
    list(hl_symbol)::in,io::di,io::uo) is det.

write_symbols(Stream,SymInfo,Syms,!IO) :-
    write_binary_16_bit_int(Stream,length(Syms),!IO),
    foldl(write_symbol(Stream,SymInfo),Syms,!IO).

%---------------------------------------------------------------------------%

:- pred write_string(binary_output_stream::in,string_info_w::in,string::in,
    io::di,io::uo) is det.

write_string(Stream,StrInfo,Str,!IO) :-
    lookup(StrInfo,Str,FakeStr),
    write_binary_24_bit_int(Stream,FakeStr,!IO).

%---------------------------------------------------------------------------%

:- pred write_type(binary_output_stream::in,hl_model_type::in,io::di,io::uo) 
    is det.

write_type(Stream,Type,!IO) :-
    model_type_code(Type,Code),
    write_binary_byte(Stream,Code,!IO).

%---------------------------------------------------------------------------%

:- pred write_tag(binary_output_stream::in,char::in,io::di,io::uo) is det.

write_tag(Stream,Tag,!IO) :-
    write_binary_chars(Stream,"\n#",!IO),
    write_binary_byte(Stream,to_int(Tag),!IO).

%---------------------------------------------------------------------------%

:- func symbol_code      = code.
:- func string_code      = code.
:- func no_lookup_code   = code.
:- func have_lookup_code = code.
:- func end_instrs_code  = code.

symbol_code      = 0.
string_code      = 1.
no_lookup_code   = 0.
have_lookup_code = 1.
end_instrs_code   = 0xFF.

%---------------------------------------------------------------------------%

:- pred model_type_code(hl_model_type,code).
:- mode model_type_code(in,out) is det.
:- mode model_type_code(out,in) is semidet.

model_type_code(int    ,0).
model_type_code(float  ,1).
model_type_code(string ,2).
model_type_code(var    ,3).
model_type_code(functor,4).

%---------------------------------------------------------------------------%

:- pred ll_instr_0_code(ll_instr_0,code).
:- mode ll_instr_0_code(in,out) is det.
:- mode ll_instr_0_code(out,in) is semidet.

ll_instr_0_code(is_int_2,                   1).
ll_instr_0_code(is_float_2,                 2).
ll_instr_0_code(is_string_2,                3).
ll_instr_0_code(is_functor_2,               4).
ll_instr_0_code(is_ac_functor_3,            5).
ll_instr_0_code(is_ac_functor_2,            6).
ll_instr_0_code(is_geq_ac_functor_3,        7).
ll_instr_0_code(is_geq_ac_functor_2,        8).
ll_instr_0_code(is_type_2,                  9).
ll_instr_0_code(get_arg_3,                  10).
ll_instr_0_code(get_arg_2,                  11).
ll_instr_0_code(pre_guard_1,                12).
ll_instr_0_code(post_guard_1,               13).
ll_instr_0_code(pop_1,                      14).
ll_instr_0_code(construct_1,                15).
ll_instr_0_code(construct_ac_2,             16).
ll_instr_0_code(interpret_0,                17).
ll_instr_0_code(return_0,                   18).
ll_instr_0_code(push_new_var_0,             19).
ll_instr_0_code(cpush_1,                    20).
ll_instr_0_code(cset_1,                     21).
ll_instr_0_code(cget_1,                     22).
ll_instr_0_code(ceqeq_2,                    23).
ll_instr_0_code(cpop_1,                     24).
ll_instr_0_code(call_label_1,               25).
ll_instr_0_code(call_label_lco_1,           26).
ll_instr_0_code(call_sym_1,                 27).
ll_instr_0_code(call_sym_lco_1,             28).
ll_instr_0_code(call_ho_1,                  29).
ll_instr_0_code(create_cp_1,                30).
ll_instr_0_code(commit_2,                   31).
ll_instr_0_code(call_foreign_2,             32).
ll_instr_0_code(lookup_5,                   33).
ll_instr_0_code(lookup_4,                   34).
ll_instr_0_code(get_next_5,                 35).
ll_instr_0_code(lookup_cc_5,                36).
ll_instr_0_code(get_next_cc_6,              37).
ll_instr_0_code(top_level_0,                38).
ll_instr_0_code(is_diff_2,                  39).
ll_instr_0_code(delete_2,                   40).
ll_instr_0_code(flatten_1,                  41).
ll_instr_0_code(debug_1,                    42).
ll_instr_0_code(halt_0,                     43).
ll_instr_0_code(label_1,                    44).
ll_instr_0_code(push_int_1,                 45).
ll_instr_0_code(push_float_1,               46).
ll_instr_0_code(push_string_1,              47).
ll_instr_0_code(call_ac_arg_sym_2,          48).
ll_instr_0_code(call_ac_arg_label_2,        49).
ll_instr_0_code(ccpop_0,                    50).
ll_instr_0_code(set_annots_1,               51).
ll_instr_0_code(commit_0,                   52).
ll_instr_0_code(split_2,                    53).
ll_instr_0_code(ccopy_2,                    54).
ll_instr_0_code(switch_3,                   55).
ll_instr_0_code(post_match_guard_2,         56).
ll_instr_0_code(ccpush_1,                   57).
ll_instr_0_code(is_named_var_2,             58).
ll_instr_0_code(push_named_var_1,           59).
ll_instr_0_code(get_annots_1,               60).
ll_instr_0_code(get_annots_2,               61).
ll_instr_0_code(setup_annots_0,             62).
ll_instr_0_code(call_annots_0,              63).
ll_instr_0_code(add_0,                      64).
ll_instr_0_code(subtract_0,                 65).
ll_instr_0_code(multiply_0,                 66).
ll_instr_0_code(divide_0,                   67).
ll_instr_0_code(mod_0,                      68).
ll_instr_0_code(negate_0,                   69).
ll_instr_0_code(restore_annots_0,           70).
ll_instr_0_code(wakeup_on_redo_0,           71).
ll_instr_0_code(wakeup_on_1,                72).
ll_instr_0_code(interpret_1,                73).
ll_instr_0_code(attach_annots_0,            74).
ll_instr_0_code(construct_acd_3,            75).
ll_instr_0_code(init_itr_3,                 76).
ll_instr_0_code(lookup_cc_6,                77).
ll_instr_0_code(call_ac_sym_lco_1,          78).
ll_instr_0_code(call_ac_label_lco_1,        79).
ll_instr_0_code(construct_acd_4,            80).
ll_instr_0_code(clear_annots_0,             81).
ll_instr_0_code(lookup_is_int_2,            82).
ll_instr_0_code(lookup_is_float_2,          83).
ll_instr_0_code(lookup_is_string_2,         84).
ll_instr_0_code(lookup_is_named_var_2,      85).
ll_instr_0_code(lookup_is_functor_2,        86).
ll_instr_0_code(lookup_is_ac_functor_3,     87).
ll_instr_0_code(lookup_is_geq_ac_functor_3, 88).
ll_instr_0_code(lookup_is_type_2,           89).
ll_instr_0_code(lookup_compare_2,           90).
ll_instr_0_code(lookup_accept_0,            91).
ll_instr_0_code(lookup_first_3,             92).
ll_instr_0_code(lookup_first_2,             93).
ll_instr_0_code(lookup_first_cc_2,          94).
ll_instr_0_code(lookup_first_cc_3,          95).
ll_instr_0_code(and_0,                      96).
ll_instr_0_code(or_0,                       97).
ll_instr_0_code(iff_0,                      98).
ll_instr_0_code(xor_0,                      99).
ll_instr_0_code(impl_0,                     100).
ll_instr_0_code(not_0,                      101).
ll_instr_0_code(lt_0,                       102).
ll_instr_0_code(leq_0,                      103).
ll_instr_0_code(gt_0,                       104).
ll_instr_0_code(geq_0,                      105).
ll_instr_0_code(eq_0,                       106).
ll_instr_0_code(neq_0,                      107).

%---------------------------------------------------------------------------%

ll_instr_0_length(is_int_2,                     2).
ll_instr_0_length(is_float_2,                   2).
ll_instr_0_length(is_string_2,                  2).
ll_instr_0_length(is_named_var_2,               2).
ll_instr_0_length(is_functor_2,                 2).
ll_instr_0_length(is_ac_functor_3,              3).
ll_instr_0_length(is_ac_functor_2,              2).
ll_instr_0_length(is_geq_ac_functor_3,          3).
ll_instr_0_length(is_geq_ac_functor_2,          2).
ll_instr_0_length(is_type_2,                    2).
ll_instr_0_length(get_arg_3,                    3).
ll_instr_0_length(get_arg_2,                    2).
ll_instr_0_length(pre_guard_1,                  1).
ll_instr_0_length(post_guard_1,                 1).
ll_instr_0_length(post_match_guard_2,           2).
ll_instr_0_length(switch_3,                     3).
ll_instr_0_length(pop_1,                        1).
ll_instr_0_length(construct_1,                  1).
ll_instr_0_length(construct_ac_2,               2).
ll_instr_0_length(construct_acd_3,              3).
ll_instr_0_length(construct_acd_4,              4).
ll_instr_0_length(interpret_0,                  0).
ll_instr_0_length(interpret_1,                  1).
ll_instr_0_length(return_0,                     0).
ll_instr_0_length(push_new_var_0,               0).
ll_instr_0_length(push_int_1,                   1).
ll_instr_0_length(push_float_1,                 1).
ll_instr_0_length(push_string_1,                1).
ll_instr_0_length(push_named_var_1,             1).
ll_instr_0_length(ccpush_1,                     1).
ll_instr_0_length(ccpop_0,                      0).
ll_instr_0_length(cpush_1,                      1).
ll_instr_0_length(cset_1,                       1).
ll_instr_0_length(cget_1,                       1).
ll_instr_0_length(ccopy_2,                      2).
ll_instr_0_length(ceqeq_2,                      2).
ll_instr_0_length(cpop_1,                       1).
ll_instr_0_length(call_label_1,                 1).
ll_instr_0_length(call_label_lco_1,             1).
ll_instr_0_length(call_ac_arg_label_2,          2).
ll_instr_0_length(call_ac_label_lco_1,          1).
ll_instr_0_length(call_sym_1,                   1).
ll_instr_0_length(call_sym_lco_1,               1).
ll_instr_0_length(call_ac_arg_sym_2,            2).
ll_instr_0_length(call_ac_sym_lco_1,            1).
ll_instr_0_length(call_ho_1,                    1).
ll_instr_0_length(create_cp_1,                  1).
ll_instr_0_length(commit_0,                     0).
ll_instr_0_length(commit_2,                     2).
ll_instr_0_length(call_foreign_2,               2).
ll_instr_0_length(lookup_5,                     5).
ll_instr_0_length(lookup_4,                     4).
ll_instr_0_length(lookup_first_3,               3).
ll_instr_0_length(lookup_first_2,               2).
ll_instr_0_length(split_2,                      2).
ll_instr_0_length(init_itr_3,                   3).
ll_instr_0_length(get_next_5,                   5).
ll_instr_0_length(lookup_cc_5,                  5).
ll_instr_0_length(lookup_cc_6,                  6).
ll_instr_0_length(lookup_first_cc_2,            2).
ll_instr_0_length(lookup_first_cc_3,            3).
ll_instr_0_length(get_next_cc_6,                6).
ll_instr_0_length(top_level_0,                  0).
ll_instr_0_length(is_diff_2,                    2).
ll_instr_0_length(delete_2,                     2).
ll_instr_0_length(flatten_1,                    1).
ll_instr_0_length(set_annots_1,                 1).
ll_instr_0_length(get_annots_1,                 1).
ll_instr_0_length(get_annots_2,                 2).
ll_instr_0_length(clear_annots_0,               0).
ll_instr_0_length(restore_annots_0,             0).
ll_instr_0_length(setup_annots_0,               0).
ll_instr_0_length(call_annots_0,                0).
ll_instr_0_length(attach_annots_0,              0).
ll_instr_0_length(wakeup_on_redo_0,             0).
ll_instr_0_length(wakeup_on_1,                  1).
ll_instr_0_length(add_0,                        0).
ll_instr_0_length(subtract_0,                   0).
ll_instr_0_length(multiply_0,                   0).
ll_instr_0_length(divide_0,                     0).
ll_instr_0_length(mod_0,                        0).
ll_instr_0_length(negate_0,                     0).
ll_instr_0_length(and_0,                        0).
ll_instr_0_length(or_0,                         0).
ll_instr_0_length(iff_0,                        0).
ll_instr_0_length(xor_0,                        0).
ll_instr_0_length(impl_0,                       0).
ll_instr_0_length(not_0,                        0).
ll_instr_0_length(lt_0,                         0).
ll_instr_0_length(leq_0,                        0).
ll_instr_0_length(gt_0,                         0).
ll_instr_0_length(geq_0,                        0).
ll_instr_0_length(eq_0,                         0).
ll_instr_0_length(neq_0,                        0).
ll_instr_0_length(lookup_is_int_2,              2).
ll_instr_0_length(lookup_is_float_2,            2).
ll_instr_0_length(lookup_is_string_2,           2).
ll_instr_0_length(lookup_is_named_var_2,        2).
ll_instr_0_length(lookup_is_functor_2,          2).
ll_instr_0_length(lookup_is_ac_functor_3,       3).
ll_instr_0_length(lookup_is_geq_ac_functor_3,   3).
ll_instr_0_length(lookup_is_type_2,             2).
ll_instr_0_length(lookup_compare_2,             2).
ll_instr_0_length(lookup_accept_0,              0).
ll_instr_0_length(debug_1,                      1).
ll_instr_0_length(halt_0,                       0).
ll_instr_0_length(label_1,                      1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

ll_instr_to_ll_instr_0(is_int(_,_),                 is_int_2).
ll_instr_to_ll_instr_0(is_float(_,_),               is_float_2).
ll_instr_to_ll_instr_0(is_string(_,_),              is_string_2).
ll_instr_to_ll_instr_0(is_named_var(_,_),           is_named_var_2).
ll_instr_to_ll_instr_0(is_functor(_,_),             is_functor_2).
ll_instr_to_ll_instr_0(is_ac_functor(_,_),          is_ac_functor_3).
ll_instr_to_ll_instr_0(is_ac_functor(_),            is_ac_functor_2).
ll_instr_to_ll_instr_0(is_geq_ac_functor(_,_),      is_geq_ac_functor_3).
ll_instr_to_ll_instr_0(is_geq_ac_functor(_),        is_geq_ac_functor_2).
ll_instr_to_ll_instr_0(is_type(_,_),                is_type_2).
ll_instr_to_ll_instr_0(get_arg(_,_,_),              get_arg_3).
ll_instr_to_ll_instr_0(get_arg(_,_),                get_arg_2).
ll_instr_to_ll_instr_0(pre_guard(_),                pre_guard_1).
ll_instr_to_ll_instr_0(post_guard(_),               post_guard_1).
ll_instr_to_ll_instr_0(post_match_guard(_,_),       post_match_guard_2).
ll_instr_to_ll_instr_0(switch(_,_,_),               switch_3).
ll_instr_to_ll_instr_0(pop(_),                      pop_1).
ll_instr_to_ll_instr_0(construct(_),                construct_1).
ll_instr_to_ll_instr_0(construct_ac(_,_),           construct_ac_2).
ll_instr_to_ll_instr_0(construct_acd(_,_,_),        construct_acd_3).
ll_instr_to_ll_instr_0(construct_acd(_,_,_,_),      construct_acd_4).
ll_instr_to_ll_instr_0(interpret,                   interpret_0).
ll_instr_to_ll_instr_0(interpret(_),                interpret_1).
ll_instr_to_ll_instr_0(return,                      return_0).
ll_instr_to_ll_instr_0(push_new_var,                push_new_var_0).
ll_instr_to_ll_instr_0(push_int(_),                 push_int_1).
ll_instr_to_ll_instr_0(push_float(_),               push_float_1).
ll_instr_to_ll_instr_0(push_string(_),              push_string_1).
ll_instr_to_ll_instr_0(push_named_var(_),           push_named_var_1).
ll_instr_to_ll_instr_0(ccpush(_),                   ccpush_1).
ll_instr_to_ll_instr_0(ccpop,                       ccpop_0).
ll_instr_to_ll_instr_0(cpush(_),                    cpush_1).
ll_instr_to_ll_instr_0(cset(_),                     cset_1).
ll_instr_to_ll_instr_0(cget(_),                     cget_1).
ll_instr_to_ll_instr_0(ccopy(_,_),                  ccopy_2).
ll_instr_to_ll_instr_0(ceqeq(_,_),                  ceqeq_2).
ll_instr_to_ll_instr_0(cpop(_),                     cpop_1).
ll_instr_to_ll_instr_0(call_label(_),               call_label_1).
ll_instr_to_ll_instr_0(call_label_lco(_),           call_label_lco_1).
ll_instr_to_ll_instr_0(call_ac_arg_label(_,_),      call_ac_arg_label_2).
ll_instr_to_ll_instr_0(call_ac_label_lco(_),        call_ac_label_lco_1).
ll_instr_to_ll_instr_0(call_sym(_),                 call_sym_1).
ll_instr_to_ll_instr_0(call_sym_lco(_),             call_sym_lco_1).
ll_instr_to_ll_instr_0(call_ac_arg_sym(_,_),        call_ac_arg_sym_2).
ll_instr_to_ll_instr_0(call_ac_sym_lco(_),          call_ac_sym_lco_1).
ll_instr_to_ll_instr_0(call_ho(_),                  call_ho_1).
ll_instr_to_ll_instr_0(create_cp(_),                create_cp_1).
ll_instr_to_ll_instr_0(commit,                      commit_0).
ll_instr_to_ll_instr_0(commit(_,_),                 commit_2).
ll_instr_to_ll_instr_0(call_foreign(_,_),           call_foreign_2).
ll_instr_to_ll_instr_0(lookup_(_,_,_,_,_),          lookup_5).
ll_instr_to_ll_instr_0(lookup_(_,_,_,_),            lookup_4).
ll_instr_to_ll_instr_0(lookup_first(_,_,_),         lookup_first_3).
ll_instr_to_ll_instr_0(lookup_first(_,_),           lookup_first_2).
ll_instr_to_ll_instr_0(split(_,_),                  split_2).
ll_instr_to_ll_instr_0(init_itr(_,_,_),             init_itr_3).
ll_instr_to_ll_instr_0(get_next(_,_,_,_,_),         get_next_5).
ll_instr_to_ll_instr_0(lookup_cc(_,_,_,_,_),        lookup_cc_5).
ll_instr_to_ll_instr_0(lookup_cc(_,_,_,_,_,_),      lookup_cc_6).
ll_instr_to_ll_instr_0(lookup_first_cc(_,_),        lookup_first_cc_2).
ll_instr_to_ll_instr_0(lookup_first_cc(_,_,_),      lookup_first_cc_3).
ll_instr_to_ll_instr_0(get_next_cc(_,_,_,_,_,_),    get_next_cc_6).
ll_instr_to_ll_instr_0(top_level,                   top_level_0).
ll_instr_to_ll_instr_0(is_diff(_,_),                is_diff_2).
ll_instr_to_ll_instr_0(delete(_,_),                 delete_2).
ll_instr_to_ll_instr_0(flatten(_),                  flatten_1).
ll_instr_to_ll_instr_0(set_annots(_),               set_annots_1).
ll_instr_to_ll_instr_0(get_annots(_),               get_annots_1).
ll_instr_to_ll_instr_0(get_annots(_,_),             get_annots_2).
ll_instr_to_ll_instr_0(clear_annots,                clear_annots_0).
ll_instr_to_ll_instr_0(restore_annots,              restore_annots_0).
ll_instr_to_ll_instr_0(setup_annots,                setup_annots_0).
ll_instr_to_ll_instr_0(call_annots,                 call_annots_0).
ll_instr_to_ll_instr_0(attach_annots,               attach_annots_0).
ll_instr_to_ll_instr_0(wakeup_on_redo,              wakeup_on_redo_0).
ll_instr_to_ll_instr_0(wakeup_on(_),                wakeup_on_1).
ll_instr_to_ll_instr_0(add,                         add_0).
ll_instr_to_ll_instr_0(subtract,                    subtract_0).
ll_instr_to_ll_instr_0(multiply,                    multiply_0).
ll_instr_to_ll_instr_0(divide,                      divide_0).
ll_instr_to_ll_instr_0(mod,                         mod_0).
ll_instr_to_ll_instr_0(negate,                      negate_0).
ll_instr_to_ll_instr_0(and,                         and_0).
ll_instr_to_ll_instr_0(or,                          or_0).
ll_instr_to_ll_instr_0(iff,                         iff_0).
ll_instr_to_ll_instr_0(xor,                         xor_0).
ll_instr_to_ll_instr_0(impl,                        impl_0).
ll_instr_to_ll_instr_0(not,                         not_0).
ll_instr_to_ll_instr_0(lt,                          lt_0).
ll_instr_to_ll_instr_0(leq,                         leq_0).
ll_instr_to_ll_instr_0(gt,                          gt_0).
ll_instr_to_ll_instr_0(geq,                         geq_0).
ll_instr_to_ll_instr_0(eq,                          eq_0).
ll_instr_to_ll_instr_0(neq,                         neq_0).
ll_instr_to_ll_instr_0(lookup_is_int(_,_),          lookup_is_int_2).
ll_instr_to_ll_instr_0(lookup_is_float(_,_),        lookup_is_float_2).
ll_instr_to_ll_instr_0(lookup_is_string(_,_),       lookup_is_string_2).
ll_instr_to_ll_instr_0(lookup_is_named_var(_,_),    lookup_is_named_var_2).
ll_instr_to_ll_instr_0(lookup_is_functor(_,_),      lookup_is_functor_2).
ll_instr_to_ll_instr_0(lookup_is_ac_functor(_,_),   lookup_is_ac_functor_3).
ll_instr_to_ll_instr_0(lookup_is_geq_ac_functor(_,_),
    lookup_is_geq_ac_functor_3).
ll_instr_to_ll_instr_0(lookup_is_type(_,_),         lookup_is_type_2).
ll_instr_to_ll_instr_0(lookup_compare(_,_),         lookup_compare_2).
ll_instr_to_ll_instr_0(lookup_accept,               lookup_accept_0).
ll_instr_to_ll_instr_0(debug(_),                    debug_1).
ll_instr_to_ll_instr_0(halt,                        halt_0).
ll_instr_to_ll_instr_0(label(_),                    label_1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty Printing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

pprint_ll_prog(LLProg,!IO) :-
    ll_prog_fold_io(pprint_ll_instr,LLProg,!IO).

%---------------------------------------------------------------------------%

:- pred pprint_ll_instr(ll_instr::in,io::di,io::uo) is det.

pprint_ll_instr(is_int(Reg,Int),!IO) :-
    format("\t\tis_int\t\t\tr%d,%d\n",[i(Reg),i(Int)],!IO).
pprint_ll_instr(is_float(Reg,Flt),!IO) :-
    format("\t\tis_float\t\tr%d,%f\n",[i(Reg),f(Flt)],!IO).
pprint_ll_instr(is_string(Reg,Str),!IO) :-
    format("\t\tis_string\t\tr%d,""%s""\n",[i(Reg),s(Str)],!IO).
pprint_ll_instr(is_named_var(Reg,NameStr),!IO) :-
    format("\t\tis_named_var\t\tr%d,""%s""\n",[i(Reg),s(NameStr)],!IO).
pprint_ll_instr(is_functor(Reg,Sym),!IO) :-
    format("\t\tis_functor\t\tr%d,%s/%d\n",
        [i(Reg),s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(is_ac_functor(Reg,Sym),!IO) :-
    format("\t\tis_ac_functor\t\tr%d,%s/%d\n",
        [i(Reg),s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(is_ac_functor(Sym),!IO) :-
    format("\t\tis_ac_functor\t%s/%d\n",
        [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(is_geq_ac_functor(Reg,Sym),!IO) :-
    format("\t\tis_geq_ac_functor\tr%d,%s/%d\n",
        [i(Reg),s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(is_geq_ac_functor(Sym),!IO) :-
    format("\t\tis_geq_ac_functor\t%s/%d\n",
        [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(is_type(Reg,Type),!IO) :-
    format("\t\tis_type\t\t\tr%d,",[i(Reg)],!IO),
    write(Type,!IO),
    nl(!IO).
pprint_ll_instr(get_arg(SrcReg,Idx,DstReg),!IO) :-
    format("\t\tget_arg\t\t\tr%d,%d,r%d\n",[i(SrcReg),i(Idx),i(DstReg)],!IO).
pprint_ll_instr(get_arg(Idx,Reg),!IO) :-
    format("\t\tget_arg\t\t\t%d,r%d\n",[i(Idx),i(Reg)],!IO).
pprint_ll_instr(pre_guard(ChReg),!IO) :-
    format("\t\tpre_guard\t\tr%d\n",[i(ChReg)],!IO).
pprint_ll_instr(post_guard(ChReg),!IO) :-
    format("\t\tpost_guard\t\tr%d\n",[i(ChReg)],!IO).
pprint_ll_instr(post_match_guard(ResReg,ChReg),!IO) :-
    format("\t\tpost_match_guard\tr%d,r%d\n",[i(ResReg),i(ChReg)],!IO).
pprint_ll_instr(switch(SrcReg,Table,Len),!IO) :-
    format("\t\tswitch\t\t\tr%d,%d\n",[i(SrcReg),i(Len)],!IO),
    write_string("\t\t{\n",!IO),
    Table = ll_switch_table(Syms,Labels),
    pprint_ll_switch_table(Syms,Labels,!IO),
    write_string("\t\t}\n",!IO).
pprint_ll_instr(pop(N),!IO) :-
    format("\t\tpop\t\t\t%d\n",[i(N)],!IO).
pprint_ll_instr(construct(Sym),!IO) :-
    format("\t\tconstruct\t\t%s/%d\n",
        [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(construct_ac(Sym,Aty),!IO) :-
    format("\t\tconstruct_ac\t\t%s/%d\n",[s(hl_symbol_name(Sym)),i(Aty)],
        !IO).
pprint_ll_instr(construct_acd(DstReg,Sym,Aty),!IO) :-
    format("\t\tconstruct_acd\t\tr%d,%s/%d\n",[i(DstReg),
        s(hl_symbol_name(Sym)),i(Aty)],!IO).
pprint_ll_instr(construct_acd(DstReg,Sym,Aty,CollReg),!IO) :-
    format("\t\tconstruct_acd\t\tr%d,%s/%d,r%d\n",[i(DstReg),
        s(hl_symbol_name(Sym)),i(Aty),i(CollReg)],!IO).
pprint_ll_instr(interpret,!IO) :-
    write_string("\t\tinterpret\n",!IO).
pprint_ll_instr(interpret(SrcReg),!IO) :-
    format("\t\tinterpret\t\tr%d\n",[i(SrcReg)],!IO).
pprint_ll_instr(push_new_var,!IO) :-
    write_string("\t\tpush_new_var\n",!IO).
pprint_ll_instr(push_int(Int),!IO) :-
    format("\t\tpush_int\t\t%d\n",[i(Int)],!IO).
pprint_ll_instr(push_float(Flt),!IO) :-
    format("\t\tpush_float\t\t%f\n",[f(Flt)],!IO).
pprint_ll_instr(push_string(Str),!IO) :-
    format("\t\tpush_string\t\t""%s""\n",[s(Str)],!IO).
pprint_ll_instr(push_named_var(NameStr),!IO) :-
    format("\t\tpush_name_var\t\t""%s""\n",[s(NameStr)],!IO).
pprint_ll_instr(ccpush(Reg),!IO) :-
    format("\t\tccpush\t\t\tr%d",[i(Reg)],!IO).
pprint_ll_instr(ccpop,!IO) :-
    write_string("\t\tccpop\n",!IO).
pprint_ll_instr(cpush(N),!IO) :-
    format("\t\tcpush\t\t\t%d\n",[i(N)],!IO).
pprint_ll_instr(cset(Reg),!IO) :-
    format("\t\tcset\t\t\tr%d\n",[i(Reg)],!IO).
pprint_ll_instr(cget(Reg),!IO) :-
    format("\t\tcget\t\t\tr%d\n",[i(Reg)],!IO).
pprint_ll_instr(ccopy(SrcReg,DstReg),!IO) :-
    format("\t\tccopy\t\t\tr%d,r%d\n",[i(SrcReg),i(DstReg)],!IO).
pprint_ll_instr(ceqeq(Reg0,Reg1),!IO) :-
    format("\t\tceqeq\t\t\tr%d,r%d\n",[i(Reg0),i(Reg1)],!IO).
pprint_ll_instr(cpop(N),!IO) :-
    format("\t\tcpop\t\t\t%d\n",[i(N)],!IO).
pprint_ll_instr(call_label(Label),!IO) :-
    format("\t\tcall_label\t\tl%d\n",[i(Label)],!IO).
pprint_ll_instr(call_label_lco(Label),!IO) :-
    format("\t\tcall_label_lco\t\tl%d\n",[i(Label)],!IO).
pprint_ll_instr(call_ac_arg_label(ACSym,Label),!IO) :-
    format("\t\tcall_ac_arg_label\t%s/%d,l%d\n",
        [s(hl_symbol_name(ACSym)),i(hl_symbol_arity(ACSym)),i(Label)],!IO).
pprint_ll_instr(call_ac_label_lco(Label),!IO) :-
    format("\t\tcall_ac_label_lco\tl%d\n",[i(Label)],!IO).
pprint_ll_instr(call_sym(Sym),!IO) :-
    format("\t\tcall_sym\t\t%s/%d\n",[s(hl_symbol_name(Sym)),
        i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(call_sym_lco(Sym),!IO) :-
    format("\t\tcall_sym_lco\t\t%s/%d\n",[s(hl_symbol_name(Sym)),
        i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(call_ac_arg_sym(ACSym,Sym),!IO) :-
    format("\t\tcall_ac_arg_sym\t\t%s/%d,%s/%d\n",
        [s(hl_symbol_name(ACSym)),i(hl_symbol_arity(ACSym)),
         s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(call_ac_sym_lco(Sym),!IO) :-
    format("\t\tcall_ac_sym_lco\t\t%s/%d\n",
        [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(call_ho(N),!IO) :-
    format("\t\tcall_ho\t\t\t%d\n",[i(N)],!IO).
pprint_ll_instr(return,!IO) :-
    write_string("\t\treturn\n",!IO).
pprint_ll_instr(create_cp(Label),!IO) :-
    format("\t\tcreate_cp\t\tl%d\n",[i(Label)],!IO).
pprint_ll_instr(commit,!IO) :-
    write_string("\t\tcommit\n",!IO).
pprint_ll_instr(commit(N,S),!IO) :-
    format("\t\tcommit\t\t\t%d,%d\n",[i(N),i(S)],!IO).
pprint_ll_instr(call_foreign(N,Name),!IO) :-
    format("\t\tcall_foreign\t\t%d,""%s""\n",[i(N),s(Name)],!IO).
pprint_ll_instr(halt,!IO) :-
    write_string("\t\thalt\n",!IO).
pprint_ll_instr(lookup_(SrcReg,LookupLabel,ArgReg,NodeReg,PosReg),!IO) :-
    format("\t\tlookup\t\t\tr%d,l%d,r%d,r%d,r%d\n",
        [i(SrcReg),i(LookupLabel),i(ArgReg),i(NodeReg),i(PosReg)],!IO).
pprint_ll_instr(lookup_(LookupLabel,ArgReg,NodeReg,PosReg),!IO) :-
    format("\t\tlookup\t\t\tl%d,r%d,r%d,r%d\n",
        [i(LookupLabel),i(ArgReg),i(NodeReg),i(PosReg)],!IO).
pprint_ll_instr(lookup_first(SrcReg,LookupLabel,ArgReg),!IO) :-
    format("\t\tlookup_first\t\tr%d,l%d,r%d\n",
        [i(SrcReg),i(LookupLabel),i(ArgReg)],!IO).
pprint_ll_instr(lookup_first(LookupLabel,ArgReg),!IO) :-
    format("\t\tlookup_first\t\tl%d,r%d\n",[i(LookupLabel),i(ArgReg)],!IO).
pprint_ll_instr(split(SrcReg,ArgReg),!IO) :-
    format("\t\tsplit\t\t\tr%d,r%d\n",[i(SrcReg),i(ArgReg)],!IO).
pprint_ll_instr(init_itr(NodeReg,PosReg,ItrReg),!IO) :-
    format("\t\tinit_itr\t\tr%d,r%d,r%d\n",
        [i(NodeReg),i(PosReg),i(ItrReg)],!IO).
pprint_ll_instr(get_next(ItrReg,Label,LookupLabel,ArgReg,IdReg),!IO) :-
    format("\t\tget_next\t\tr%d,l%d,l%d,r%d,r%d\n",
        [i(ItrReg),i(Label),i(LookupLabel),i(ArgReg),i(IdReg)],!IO).
pprint_ll_instr(lookup_cc(LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),!IO) :-
    format("\t\tlookup_cc\t\tl%d,r%d,r%d,r%d,r%d\n",
        [i(LookupLabel),i(ItrNoReg),i(ArgReg),i(NodeReg),i(PosReg)],!IO).
pprint_ll_instr(lookup_cc(CollReg,LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),
        !IO) :-
    format("\t\tlookup_cc\t\tr%d,l%d,r%d,r%d,r%d,r%d\n",
        [i(CollReg),i(LookupLabel),i(ItrNoReg),i(ArgReg),i(NodeReg),i(PosReg)],
        !IO).
pprint_ll_instr(lookup_first_cc(LookupLabel,ArgReg),!IO) :-
    format("\t\tlookup_first_cc\t\tl%d,r%d\n",[i(LookupLabel),i(ArgReg)],!IO).
pprint_ll_instr(lookup_first_cc(CollReg,LookupLabel,ArgReg),!IO) :-
    format("\t\tlookup_first_cc\t\tr%d,l%d,r%d\n",
        [i(CollReg),i(LookupLabel),i(ArgReg)],!IO).
pprint_ll_instr(get_next_cc(ItrNoReg,ItrReg,Label,LookupLabel,ArgReg,IdReg),
        !IO) :-
    format("\t\tget_next_cc\t\tr%d,r%d,l%d,l%d,r%d,r%d\n",
        [i(ItrNoReg),i(ItrReg),i(Label),i(LookupLabel),i(ArgReg),i(IdReg)],
        !IO).
pprint_ll_instr(top_level,!IO) :-
    write_string("\t\ttop_level\n",!IO).
pprint_ll_instr(is_diff(Reg1,Reg2),!IO) :-
    format("\t\tis_diff\t\t\tr%d,r%d\n",[i(Reg1),i(Reg2)],!IO).
pprint_ll_instr(delete(SrcReg,ArgReg),!IO) :-
    format("\t\tdelete\t\t\tr%d,r%d\n",[i(SrcReg),i(ArgReg)],!IO).
pprint_ll_instr(flatten(SrcReg),!IO) :-
    format("\t\tflatten\t\t\tr%d\n",[i(SrcReg)],!IO).
pprint_ll_instr(set_annots(SrcReg),!IO) :-
    format("\t\tset_annots\t\tr%d\n",[i(SrcReg)],!IO).
pprint_ll_instr(get_annots(DstReg),!IO) :-
    format("\t\tget_annots\t\tr%d\n",[i(DstReg)],!IO).
pprint_ll_instr(get_annots(SrcReg,DstReg),!IO) :-
    format("\t\tget_annots\t\tr%d,r%d\n",[i(SrcReg),i(DstReg)],!IO).
pprint_ll_instr(clear_annots,!IO) :-
    write_string("\t\tclear_annots\n",!IO).
pprint_ll_instr(restore_annots,!IO) :-
    write_string("\t\trestore_annots\n",!IO).
pprint_ll_instr(setup_annots,!IO) :-
    write_string("\t\tsetup_annots\n",!IO).
pprint_ll_instr(call_annots,!IO) :-
    write_string("\t\tcall_annots\n",!IO).
pprint_ll_instr(attach_annots,!IO) :-
    write_string("\t\tattach_annots\n",!IO).
pprint_ll_instr(add,!IO) :-
    write_string("\t\tadd\n",!IO).
pprint_ll_instr(subtract,!IO) :-
    write_string("\t\tsubtract\n",!IO).
pprint_ll_instr(multiply,!IO) :-
    write_string("\t\tmultiply\n",!IO).
pprint_ll_instr(divide,!IO) :-
    write_string("\t\tdivide\n",!IO).
pprint_ll_instr(mod,!IO) :-
    write_string("\t\tmod\n",!IO).
pprint_ll_instr(negate,!IO) :-
    write_string("\t\tnegate\n",!IO).
pprint_ll_instr(and,!IO) :-
    write_string("\t\tand\n",!IO).
pprint_ll_instr(or,!IO) :-
    write_string("\t\tor\n",!IO).
pprint_ll_instr(iff,!IO) :-
    write_string("\t\tiff\n",!IO).
pprint_ll_instr(xor,!IO) :-
    write_string("\t\txor\n",!IO).
pprint_ll_instr(impl,!IO) :-
    write_string("\t\timpl\n",!IO).
pprint_ll_instr(not,!IO) :-
    write_string("\t\tnot\n",!IO).
pprint_ll_instr(lt,!IO) :-
    write_string("\t\tlt\n",!IO).
pprint_ll_instr(leq,!IO) :-
    write_string("\t\tleq\n",!IO).
pprint_ll_instr(gt,!IO) :-
    write_string("\t\tgt\n",!IO).
pprint_ll_instr(geq,!IO) :-
    write_string("\t\tgeq\n",!IO).
pprint_ll_instr(eq,!IO) :-
    write_string("\t\teq\n",!IO).
pprint_ll_instr(neq,!IO) :-
    write_string("\t\tneq\n",!IO).
pprint_ll_instr(wakeup_on_redo,!IO) :-
    write_string("\t\twakeup_on_redo\n",!IO).
pprint_ll_instr(wakeup_on(Syms),!IO) :-
    write_string("\t\twakeup_on\t\t[\n",!IO),
    pprint_symbols(Syms,!IO),
    write_string("\t\t]\n",!IO).
pprint_ll_instr(lookup_is_int(Src,Int),!IO) :-
    format("\t\tlookup_is_int\t\tr%d,%d\n",[i(Src),i(Int)],!IO).
pprint_ll_instr(lookup_is_float(Src,Flt),!IO) :-
    format("\t\tlookup_is_float\t\tr%d,%f\n",[i(Src),f(Flt)],!IO).
pprint_ll_instr(lookup_is_string(Src,Str),!IO) :-
    format("\t\tlookup_is_string\t\tr%d,""%s""\n",[i(Src),s(Str)],!IO).
pprint_ll_instr(lookup_is_named_var(Src,Name),!IO) :-
    format("\t\tlookup_is_named_var\tr%d,""%s""\n",[i(Src),s(Name)],!IO).
pprint_ll_instr(lookup_is_functor(Src,Sym),!IO) :-
    format("\t\tlookup_is_functor\tr%d,%s/%d\n",
        [i(Src),s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(lookup_is_ac_functor(Src,Sym),!IO) :-
    format("\t\tlookup_is_ac_functor\tr%d,%s/%d\n",
        [i(Src),s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(lookup_is_geq_ac_functor(Src,Sym),!IO) :-
    format("\t\tlookup_is_geq_ac_functor\tr%d,%s/%d\n",
        [i(Src),s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],!IO).
pprint_ll_instr(lookup_is_type(Src,Type),!IO) :-
    format("\t\tlookup_is_type\t\tr%d,",[i(Src)],!IO),
    write(Type,!IO),
    nl(!IO).
pprint_ll_instr(lookup_compare(Reg0,Reg1),!IO) :-
    format("\t\tlookup_compare\t\tr%d,r%d\n",[i(Reg0),i(Reg1)],!IO).
pprint_ll_instr(lookup_accept,!IO) :-
    write_string("\t\tlookup_accept\n",!IO).
pprint_ll_instr(debug(_),!IO) :-
    write_string("\t\t/* debug point */\n",!IO).
pprint_ll_instr(label(Label),!IO) :-
    format("l%d:\n",[i(Label)],!IO).

%---------------------------------------------------------------------------%

:- pred pprint_symbols(list(hl_symbol)::in,io::di,io::uo) is det.

pprint_symbols([],!IO).
pprint_symbols([Sym|Syms],!IO) :-
    format("\t\t\t%s/%d\n",[s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym))],
        !IO),
    pprint_symbols(Syms,!IO).

%---------------------------------------------------------------------------%

:- pred pprint_ll_switch_table(list(hl_symbol)::in,list(label)::in,
    io::di,io::uo) is det.

pprint_ll_switch_table([],_,!IO).
pprint_ll_switch_table([Sym|Syms],Labels,!IO) :-
    format("\t\t\t%s/%d,l%d\n",
        [s(hl_symbol_name(Sym)),i(hl_symbol_arity(Sym)),i(det_head(Labels))],
        !IO),
    pprint_ll_switch_table(Syms,det_tail(Labels),!IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

ll_prog_link(LLProgId,LLProg0,!LLProg) :-
    foldl(ll_proc_link(LLProgId),LLProg0,!LLProg).

%---------------------------------------------------------------------------%

:- pred ll_proc_link(ll_prog_id::in,hl_symbol::in,ll_proc::in,ll_prog::in,
    ll_prog::out) is det.

ll_proc_link(LLProgId,Sym,LLProc,!LLProg) :-
    LLProc = ll_proc(Label,Instrs,LookupInstrs,Conds),
    NLabel = offset_label(LLProgId,Label),
    ( search(!.LLProg,Sym,LLProc0) ->
        LLProc0 = ll_proc(_,Instrs0,LookupInstrs0,Conds0),
        ll_instrs_offset_label(LLProgId,Instrs,Instrs0,NInstrs),
        foldl(ll_lookup_instrs_offset_label(LLProgId),LookupInstrs,
            LookupInstrs0,NLookupInstrs),
        merge_and_remove_dups(Conds0,Conds,NConds)
    ;   ll_instrs_offset_label(LLProgId,Instrs,[],NInstrs),
        foldl(ll_lookup_instrs_offset_label(LLProgId),LookupInstrs,init,
            NLookupInstrs),
        NConds = Conds
    ),
    NLLProc = ll_proc(NLabel,NInstrs,NLookupInstrs,NConds),
    map.set(Sym,NLLProc,!LLProg).

%---------------------------------------------------------------------------%

:- pred ll_lookup_instrs_offset_label(ll_prog_id::in,label::in,
    list(ll_instr)::in,ll_lookup_instrs::in,ll_lookup_instrs::out) is det.

ll_lookup_instrs_offset_label(LLProgId,Label,Instrs,!LookupInstrs) :-
    NLabel = offset_label(LLProgId,Label),
    ll_instrs_offset_label(LLProgId,Instrs,[],NInstrs),
    map.det_insert(NLabel,NInstrs,!LookupInstrs).

%---------------------------------------------------------------------------%

:- pred ll_instrs_offset_label(ll_prog_id::in,list(ll_instr)::in,
    list(ll_instr)::in,list(ll_instr)::out) is det.

ll_instrs_offset_label(_,[],!Instrs).
ll_instrs_offset_label(LLProgId,[Instr|Instrs],Instrs0,NInstrs) :-
    ( Instr = create_cp(Label),
        NInstr = create_cp(offset_label(LLProgId,Label))
    ; Instr = lookup_(Label,R1,R2,R3),
        NInstr = lookup_(offset_label(LLProgId,Label),R1,R2,R3)
    ; Instr = lookup_(R1,Label,R2,R3,R4),
        NInstr = lookup_(R1,offset_label(LLProgId,Label),R2,R3,R4)
    ; Instr = lookup_first(R1,Label,R2),
        NInstr = lookup_first(R1,offset_label(LLProgId,Label),R2)
    ; Instr = lookup_first(Label,R1),
        NInstr = lookup_first(offset_label(LLProgId,Label),R1)
    ; Instr = lookup_cc(Label,R1,R2,R3,R4),
        NInstr = lookup_cc(offset_label(LLProgId,Label),R1,R2,R3,R4)
    ; Instr = lookup_cc(R1,Label,R2,R3,R4,R5),
        NInstr = lookup_cc(R1,offset_label(LLProgId,Label),R2,R3,R4,R5)
    ; Instr = lookup_first_cc(Label,R1),
        NInstr = lookup_first_cc(offset_label(LLProgId,Label),R1)
    ; Instr = lookup_first_cc(R1,Label,R2),
        NInstr = lookup_first_cc(R1,offset_label(LLProgId,Label),R2)
    ; Instr = get_next(R1,Label1,Label2,R2,R3),
        NInstr = get_next(R1,offset_label(LLProgId,Label1),
            offset_label(LLProgId,Label2),R2,R3)
    ; Instr = get_next_cc(R1,R2,Label1,Label2,R3,R4),
        NInstr = get_next_cc(R1,R2,offset_label(LLProgId,Label1),
            offset_label(LLProgId,Label2),R3,R4)
    ; Instr = call_label(Label),
        NInstr = call_label(offset_label(LLProgId,Label))
    ; Instr = call_label_lco(Label),
        NInstr = call_label_lco(offset_label(LLProgId,Label))
    ; Instr = call_ac_arg_label(Sym,Label),
        NInstr = call_ac_arg_label(Sym,offset_label(LLProgId,Label))
    ; Instr = call_ac_label_lco(Label),
        NInstr = call_ac_label_lco(offset_label(LLProgId,Label))
    ; Instr = label(Label),
        NInstr = label(offset_label(LLProgId,Label))
    ; Instr = switch(X,Table,Y),
        Table = ll_switch_table(Syms,Labels),
        NTable = ll_switch_table(Syms,list.map(offset_label(LLProgId),Labels)),
        NInstr = switch(X,NTable,Y)
    ;   ( Instr = is_int(_,_)
        ; Instr = is_float(_,_)
        ; Instr = is_string(_,_)
        ; Instr = is_named_var(_,_)
        ; Instr = is_functor(_,_)
        ; Instr = is_ac_functor(_,_)
        ; Instr = is_ac_functor(_)
        ; Instr = is_geq_ac_functor(_,_)
        ; Instr = is_geq_ac_functor(_)
        ; Instr = is_type(_,_)
        ; Instr = get_arg(_,_,_)
        ; Instr = get_arg(_,_)
        ; Instr = pre_guard(_)
        ; Instr = post_guard(_)
        ; Instr = post_match_guard(_,_)
        ; Instr = pop(_)
        ; Instr = construct(_)
        ; Instr = construct_ac(_,_)
        ; Instr = construct_acd(_,_,_)
        ; Instr = construct_acd(_,_,_,_)
        ; Instr = interpret
        ; Instr = interpret(_)
        ; Instr = return
        ; Instr = push_new_var
        ; Instr = push_int(_)
        ; Instr = push_float(_)
        ; Instr = push_string(_)
        ; Instr = push_named_var(_)
        ; Instr = ccpush(_)
        ; Instr = ccpop
        ; Instr = cpush(_)
        ; Instr = cset(_)
        ; Instr = cget(_)
        ; Instr = ccopy(_,_)
        ; Instr = ceqeq(_,_)
        ; Instr = cpop(_)
        ; Instr = call_sym(_)
        ; Instr = call_sym_lco(_)
        ; Instr = call_ac_arg_sym(_,_)
        ; Instr = call_ac_sym_lco(_)
        ; Instr = call_ho(_)
        ; Instr = commit
        ; Instr = commit(_,_)
        ; Instr = call_foreign(_,_)
        ; Instr = split(_,_)
        ; Instr = init_itr(_,_,_)
        ; Instr = top_level
        ; Instr = is_diff(_,_)
        ; Instr = delete(_,_)
        ; Instr = flatten(_)
        ; Instr = set_annots(_)
        ; Instr = get_annots(_)
        ; Instr = get_annots(_,_)
        ; Instr = clear_annots
        ; Instr = restore_annots
        ; Instr = setup_annots
        ; Instr = call_annots
        ; Instr = attach_annots
        ; Instr = wakeup_on_redo
        ; Instr = wakeup_on(_)
        ; Instr = add
        ; Instr = subtract
        ; Instr = multiply
        ; Instr = divide
        ; Instr = (mod)
        ; Instr = negate
        ; Instr = (and)
        ; Instr = (or)
        ; Instr = iff
        ; Instr = xor
        ; Instr = impl
        ; Instr = (not)
        ; Instr = lt
        ; Instr = leq
        ; Instr = gt
        ; Instr = geq
        ; Instr = eq
        ; Instr = neq
        ; Instr = lookup_is_int(_,_)
        ; Instr = lookup_is_float(_,_)
        ; Instr = lookup_is_string(_,_)
        ; Instr = lookup_is_named_var(_,_)
        ; Instr = lookup_is_functor(_,_)
        ; Instr = lookup_is_ac_functor(_,_)
        ; Instr = lookup_is_geq_ac_functor(_,_)
        ; Instr = lookup_is_type(_,_)
        ; Instr = lookup_compare(_,_)
        ; Instr = lookup_accept
        ; Instr = debug(_)
        ; Instr = halt ),
        NInstr = Instr
    ),
    ll_instrs_offset_label(LLProgId,Instrs,Instrs0,NInstrs0),
    NInstrs = [NInstr|NInstrs0].

%---------------------------------------------------------------------------%

    % The ll_prog ID is the upper 10 bits, and the actual label is the lower
    % 22 bits.
    %
:- func offset_label(ll_prog_id,label) = label.
:- func ll_prog_id_shift = int.

offset_label(LLProgId,Label0) = Label1 :-
    Label1 = Label0 \/ (LLProgId << ll_prog_id_shift).

ll_prog_id_shift = 22.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Higher-order Stuff.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

ll_prog_fold_io(Pred,LLProg,!IO) :-
        % This is a hack to get around Hg's address-of-multi-mode-pred
        % problem.
        %
    NPred = ll_prog_fold_ignore_args_io(Pred),
    ll_prog_fold2(NPred,LLProg,unit,_,unit,_).

%---------------------------------------------------------------------------%

ll_prog_fold(Pred,LLProg,!X) :-
    NPred = ll_prog_fold_ignore_args(Pred),
    ll_prog_fold2(NPred,LLProg,unit,_,!X).

%---------------------------------------------------------------------------%

:- pred ll_prog_fold_ignore_args(pred(ll_instr,T,T),ll_instr,unit,unit,T,T).
:- mode ll_prog_fold_ignore_args(pred(in,in,out) is det,in,in,out,in,out) 
    is det.
% :- mode ll_prog_fold_ignore_args(pred(in,di,uo) is det,in,in,out,di,uo) is det.

ll_prog_fold_ignore_args(Pred,Instr,!U,!X) :-
    Pred(Instr,!X).

%---------------------------------------------------------------------------%

:- pred ll_prog_fold_ignore_args_io(pred(ll_instr,io,io),ll_instr,unit,unit,
    unit,unit).
:- mode ll_prog_fold_ignore_args_io(pred(in,di,uo) is det,in,in,out,in,out)
    is det.

ll_prog_fold_ignore_args_io(Pred,Instr,!U1,!U2) :-
    Pred(Instr,unsafe_make_io,_).

%---------------------------------------------------------------------------%

ll_prog_fold2(Pred,LLProg,!X,!Y) :-
        % First up are all builtin procedures.
        %
    ll_builtin_procs_foldl2(Pred,!X,!Y),

        % The rest of the program is any remaining procedures.
        %
    foldl2(ll_proc_fold2(LLProg,Pred),LLProg,!X,!Y).

%---------------------------------------------------------------------------%

:- pred ll_builtin_procs_foldl2(pred(ll_instr,T,T,U,U),T,T,U,U).
:- mode ll_builtin_procs_foldl2(pred(in,in,out,in,out) is det,in,out,in,out)
    is det.

ll_builtin_procs_foldl2(Pred,!X,!Y) :-
    some [!Label] (
        !:Label = 0,
        ll_builtin_proc_foldl2(Pred,add,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,subtract,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,multiply,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,divide,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,mod,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,negate,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,and,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,or,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,iff,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,xor,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,impl,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,not,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,lt,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,leq,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,gt,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,geq,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,eq,!Label,!X,!Y),
        ll_builtin_proc_foldl2(Pred,neq,!.Label,_,!X,!Y)
    ).

%---------------------------------------------------------------------------%

:- pred ll_builtin_proc_foldl2(pred(ll_instr,T,T,U,U),ll_instr,label,label,
    T,T,U,U).
:- mode ll_builtin_proc_foldl2(pred(in,in,out,in,out) is det,in,in,out,in,out,
    in,out) is det.

ll_builtin_proc_foldl2(Pred,Instr,!Label,!X,!Y) :-
    Pred(label(!.Label),!X,!Y),
    !:Label = !.Label + 1,
    Pred(Instr,!X,!Y),
    Pred(return,!X,!Y).

%---------------------------------------------------------------------------%

:- pred ll_proc_fold2(ll_prog,pred(ll_instr,T,T,U,U),hl_symbol,ll_proc,
    T,T,U,U).
:- mode ll_proc_fold2(in,pred(in,in,out,in,out) is det,in,in,in,out,in,out)
    is det.
% :- mode ll_proc_fold2(in,pred(in,in,out,di,uo) is det,in,in,in,out,di,uo)
%     is det.

ll_proc_fold2(LLProg,Pred,Sym,LLProc,!X,!Y) :-
    LLProc = ll_proc(Label,Instrs,LookupInstrs,Conds),
    Pred(label(Label),!X,!Y),
    foldl2(ll_instr_process(LLProg,Pred),Instrs,!X,!Y),

        % Generate the wakeup_on instruction for this procedure.
        %
    ( Conds \= [] ->
        Pred(wakeup_on(Conds),!X,!Y)
    ;   true
    ),

        % The following instructions are executed if no rule was 
        % applicable.  If the Sym is AC, or for a variable, then we simply
        % `return'.  Otherwise, we first must `construct' the term before
        % we return it.
        %
    ( ( is_ac(hl_symbol_name(Sym))
      ; hl_is_symbol(Sym,var_name,0) ) ->
        true
    ;   Pred(construct(Sym),!X,!Y)
    ),
    Pred(return,!X,!Y),

        % Generate the lookup instructions.
        %
    foldl2(ll_lookup_instrs_process(LLProg,Pred),LookupInstrs,!X,!Y).

%---------------------------------------------------------------------------%

:- pred ll_lookup_instrs_process(ll_prog,pred(ll_instr,T,T,U,U),label,
    list(ll_instr), T,T,U,U).
:- mode ll_lookup_instrs_process(in,pred(in,in,out,in,out) is det,in,in,in,out,
    in,out) is det.

ll_lookup_instrs_process(LLProg,Pred,Label,Instrs,!X,!Y) :-
    Pred(label(Label),!X,!Y),
    foldl2(ll_instr_process(LLProg,Pred),Instrs,!X,!Y),
    Pred(lookup_accept,!X,!Y).

%---------------------------------------------------------------------------%

:- pred ll_instr_process(ll_prog,pred(ll_instr,T,T,U,U),ll_instr,T,T,U,U).
:- mode ll_instr_process(in,pred(in,in,out,in,out) is det,in,in,out,in,out)
    is det.
% :- mode ll_instr_process(in,pred(in,in,out,di,uo) is det,in,in,out,di,uo)
%     is det.

ll_instr_process(LLProg,Pred,Instr,!X,!Y) :-
        % Here if decide if a `call' is really a `construct' or a NOP.
        %
    ( is_call_sym_instr(Instr,Sym) ->
        Name = hl_symbol_name(Sym),
        ( search(LLProg,Sym,LLProc) ->
            LLProc = ll_proc(Label,_,_,_),
            ( Instr = call_sym(_) ->
                Pred(call_label(Label),!X,!Y)
            ; Instr = call_sym_lco(_) ->
                Pred(call_label_lco(Label),!X,!Y)
            ; Instr = call_ac_arg_sym(ACSym,_) ->
                Pred(call_ac_arg_label(ACSym,Label),!X,!Y)
            ; Instr = call_ac_sym_lco(_) ->
                Pred(call_ac_label_lco(Label),!X,!Y)
            ;   true
            )
        ; is_builtin(Name,hl_symbol_arity(Sym),BInstr) ->
                % A built-in instruction, simply execute it.
                %
            Pred(BInstr,!X,!Y)
        ; is_ac(Name) ->
                % An AC term but not a procedure.  The term should have
                % already being constructed, thus its a NOP here.
                %
            true
        ; Name = var_name ->
                % Again a NOP, the variable is already on the stack.
                %
            true
        ;   Pred(construct(Sym),!X,!Y)
        )
    ;   Pred(Instr,!X,!Y)
    ).

%---------------------------------------------------------------------------%

:- pred is_builtin(string::in,int::in,ll_instr::out) is semidet.

is_builtin(Func,Aty,Instr) :-
    ( Aty = 1,
        Func = builtin_minus_name,
        Instr = negate
    ; Aty = 2,
        ( Func = builtin_plus_name ->
            Instr = add
        ; Func = builtin_minus_name ->
            Instr = subtract
        ; Func = builtin_multiply_name ->
            Instr = multiply
        ; Func = builtin_divide_name ->
            Instr = divide
        ; Func = builtin_mod_name ->
            Instr = (mod)
        ; Func = builtin_and_name ->
            Instr = (and)
        ; Func = builtin_or_name ->
            Instr = (or)
        ; Func = builtin_iff_name ->
            Instr = iff
        ; Func = builtin_xor_name ->
            Instr = xor
        ; Func = builtin_impl_name ->
            Instr = impl
        ; Func = builtin_not_name ->
            Instr = (not)
        ; Func = builtin_lt_name ->
            Instr = lt
        ; Func = builtin_leq_name ->
            Instr = leq
        ; Func = builtin_gt_name ->
            Instr = gt
        ; Func = builtin_geq_name ->
            Instr = geq
        ; Func = builtin_eq_name ->
            Instr = eq
        ; Func = builtin_neq_name ->
            Instr = neq
        ;   fail
        ) 
    ).

%---------------------------------------------------------------------------%

:- pred is_call_sym_instr(ll_instr::in,hl_symbol::out) is semidet.

is_call_sym_instr(call_sym(Sym),Sym).
is_call_sym_instr(call_sym_lco(Sym),Sym).
is_call_sym_instr(call_ac_arg_sym(_,Sym),Sym).
is_call_sym_instr(call_ac_sym_lco(Sym),Sym).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Low-level I/O
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XXX: Mercury has biniary input/output streams, however mercury actually
% only reads/writes plain old text, so I wrote my own predicates.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

:- pred write_binary_byte(binary_output_stream::in,int::in,io::di,io::uo) 
    is det.

:- pragma foreign_proc("C",
    write_binary_byte(Stream::in,Byte::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    putc(Byte,MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_byte(binary_input_stream::in,int::out,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    read_binary_byte(Stream::in,Byte::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Byte = getc(MR_file(*MR_unwrap_input_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_char(binary_input_stream::in,char::out,io::di,io::uo)
    is det.

read_binary_char(Stream,Char,!IO) :-
    read_binary_byte(Stream,Byte,!IO),
    det_from_int(Byte,Char).

%---------------------------------------------------------------------------%

:- pred write_binary_24_bit_int(binary_output_stream::in,int::in,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    write_binary_24_bit_int(Stream::in,Int::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fwrite(&Int,3,1,MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_24_bit_int(binary_input_stream::in,int::out,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    read_binary_24_bit_int(Stream::in,Int::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Int = 0;
    fread(&Int,3,1,MR_file(*MR_unwrap_input_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred write_binary_16_bit_int(binary_output_stream::in,int::in,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    write_binary_16_bit_int(Stream::in,Int::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fwrite(&Int,2,1,MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_16_bit_int(binary_input_stream::in,int::out,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    read_binary_16_bit_int(Stream::in,Int::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Int = 0;
    fread(&Int,2,1,MR_file(*MR_unwrap_input_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred write_binary_int(binary_output_stream::in,int::in,io::di,io::uo) 
    is det.

:- pragma foreign_proc("C",
    write_binary_int(Stream::in,Int::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fwrite(&Int,sizeof(MR_Integer),1,
        MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_int(binary_input_stream::in,int::out,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    read_binary_int(Stream::in,Int::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fread(&Int,sizeof(MR_Integer),1,
        MR_file(*MR_unwrap_input_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred write_binary_float(binary_output_stream::in,float::in,io::di,io::uo) 
    is det.

:- pragma foreign_proc("C",
    write_binary_float(Stream::in,Flt::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fwrite(&Flt,sizeof(MR_Float),1,MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_float(binary_input_stream::in,float::out,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    read_binary_float(Stream::in,Flt::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fread(&Flt,sizeof(MR_Float),1,MR_file(*MR_unwrap_input_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred write_binary_chars(binary_output_stream::in,string::in,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    write_binary_chars(Stream::in,Str::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    fwrite((char *)Str,sizeof(char),strlen((const char *)Str),
        MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_chars(binary_input_stream::in,int::in,string::out,
    io::di,io::uo) is det.

:- pragma foreign_proc("C",
    read_binary_chars(Stream::in,Len::in,Str::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Str = MR_GC_malloc(Len + 1);
    fread((char *)Str, sizeof(char), Len,
        MR_file(*MR_unwrap_input_stream(Stream)));
    ((char *)Str)[Len] = '\\0';
").

%---------------------------------------------------------------------------%

:- pred write_binary_string(binary_output_stream::in,string::in,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    write_binary_string(Stream::in,Str::in,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    unsigned short len = strlen((const char *)Str);
    fwrite(&len,sizeof(unsigned short),1,
        MR_file(*MR_unwrap_output_stream(Stream)));
    fwrite((char *)Str,sizeof(char),len,
        MR_file(*MR_unwrap_output_stream(Stream)));
").

%---------------------------------------------------------------------------%

:- pred read_binary_string(binary_input_stream::in,string::out,io::di,io::uo)
    is det.

:- pragma foreign_proc("C",
    read_binary_string(Stream::in,Str::out,_IO0::di,_IO1::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    unsigned short len;
    fread(&len,sizeof(unsigned short),1,
        MR_file(*MR_unwrap_input_stream(Stream)));
    Str = MR_malloc(len+1);
    fread((char *)Str,sizeof(char),len,
        MR_file(*MR_unwrap_input_stream(Stream)));
    ((char *)Str)[len] = '\\0';
").

%---------------------------------------------------------------------------%

:- func unsafe_make_io = (io::uo) is det.

:- pragma foreign_proc("C",
    unsafe_make_io = (IO::uo),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    IO = (MR_Word)NULL;
").

%---------------------------------------------------------------------------%
:- end_module ll_prog.
%---------------------------------------------------------------------------%
