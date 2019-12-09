%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%-----------------------------------------------------------------------------%
%
% Author: Gregory J. Duck
%
% Implements the ACDTR abstract machine.
%
% Description: The abstract machine consists of four stacks:
%   (1) Stack -- for constructing new terms.
%   (2) Call stack -- which stores the matching information (i.e. variable
%       bindings), and the return address for procedure calls.
%   (3) Determinism stack -- which stores information about what state the
%       abstract machine should be in should failure occur.  Failure only
%       occurs in matching.
%   (4) Conjunctive Context (CC) stack -- Stack containing the conjunctive
%       context for the current subterm.
%
%---------------------------------------------------------------------------%

:- module machine.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module ll_prog.
:- import_module model.

%---------------------------------------------------------------------------%

:- type interpret_result
    ---> interpret_ok(model)
    ;    interpret_exception(model).

    % Load a ll_prog into the Cadmium VM.
    %
:-  pred assemble(bool::in, ll_prog::in, io::di, io::uo) is det.

    % Must be called before a new assemble/2.
    %
:- impure pred disassemble(ll_prog::in) is det.
:- pred disassemble(ll_prog::in, io::di, io::uo) is det.

    % Execute the program (initial-goal version).
    %
:- impure pred interpret_main(model::in, interpret_result::out) is det.
:- pred interpret_main(model::in, interpret_result::out, io::di, io::uo) is det.

    % Execute the program (re-entry version).
    %
:- impure pred interpret(model::in, model::out) is det.

    % Throw a (cadmium) exception.
    %
:- pred throw(model::in) is erroneous.

    % Start-up the machine.
    %
:- impure pred startup is det.
:- pred startup(io::di, io::uo) is det.

    % Reset the machine.
    %
:- impure pred reset is det.
:- pred reset(io::di, io::uo) is det.

    % Shut-down the machine.
    %
:- impure pred shutdown is det.
:- pred shutdown(io::di, io::uo) is det.

    % A label that can never be used.
    %
:- func dummy_label = label.

    % Lookup the value of a register.   Assumes the value is a model.
    % Used by the debugger.
    %
:- semipure pred lookup_register_value(register::in,model::out) is det.

    % Lookup the Nth value on the stack.  Used by the debugger.
    %
:- semipure pred lookup_stack_value(int::in,model::out) is det.

    % Lookup (construct) the model on top of the stack.
    %
:- semipure pred lookup_stack_model(symbol::in,model::out) is det.

    % Return the contents of the CC stack into a list.
    %
:- semipure pred lookup_cc(list(model)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module lib.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module unit.

:- import_module ac_index.
:- import_module cadmium_common.
:- import_module cadmium_debug.
:- import_module cadmium_error.

:- pragma foreign_import_module("C", ac_index).
:- pragma foreign_import_module("C", cadmium_debug).
:- pragma foreign_import_module("C", lib).
:- pragma foreign_import_module("C", model).

%---------------------------------------------------------------------------%

:- type label_info == map(label,address).
:- type switch_table == c_pointer.
:- type events == int.

:- pragma foreign_decl("C","

/*
 * In debug mode the current instruction is printed to the screen before it
 * is executed.  Does not work in CR_FAST_INTERPETER mode.
 */

/* #define CR_DEBUG */

/*
 * If CR_FAST_INTERPETER is defined, the Cadmium interpreter represents each
 * opcode as the address of the code that implements the instruction.
 * Otherwise, the opcode is just an integer constant.
 *
 * In CR_FAST_INTERPETER mode, we can jump directly to the instruction
 * implementation.  Otherwise we rely on a switch statement to find the
 * implementation.  The latter is slower, since it requires a switch
 * lookup and jump.
 */

#define CR_FAST_INTERPETER

#ifdef CR_DEBUG
#undef CR_FAST_INTERPETER
#endif

/*
 * If CR_USE_EVENTS is defined, the engine will attempt to avoid unnecessary
 * wakeup.
 */

#define CR_USE_EVENTS

/*
 * Cadmium uses some GNU CC extensions, namely taking the address of a label
 * (&&label).
 */
#ifndef __GNUC__
#error ""Cadmium must be compiled with GNUC CC""
#endif

#ifdef CR_FAST_INTERPETER

#define CR_make_local_label(label)                  \\
    local_op_##label

#endif /*CR_FAST_INTERPETER*/

").

:- inst exec_instr
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
    ;    wakeup_on_redo_0
    ;    wakeup_on_1
    ;    debug_1
    ;    halt_0.

:- pragma foreign_export_enum("C", ll_instr_0/0, [prefix("op_")], []).

%---------------------------------------------------------------------------%

:- type word == c_pointer.

:- type memory == c_pointer.
:- type stack == c_pointer.
:- type prog  == c_pointer.

    % The deconstruct/construct stack.
    %
:- mutable(stack,stack,null_stack,ground,[foreign_name("C","stk"),untrailed]).
:- mutable(stk_ptr,int,-1,ground,[foreign_name("C","ptr"),untrailed]).
:- mutable(stk_limit,int,0,ground,[foreign_name("C","limit"),untrailed]).

    % The call-stack.
    %
:- mutable(cstack,stack,null_stack,ground,[foreign_name("C","cstk"),untrailed]).
:- mutable(cstk_ptr,int,-1,ground,[foreign_name("C","cptr"),untrailed]).
:- mutable(cstk_limit,int,0,ground,[foreign_name("C","climit"),untrailed]).

    % The determinism stack.
    %
:- mutable(dstack,stack,null_stack,ground,[foreign_name("C","dstk"),untrailed]).
:- mutable(dstk_ptr,int,-1,ground,[foreign_name("C","dptr"),untrailed]).
:- mutable(dstk_limit,int,0,ground,[foreign_name("C","dlimit"),untrailed]).

    % The CC stack.
    %
:- mutable(ccstack,stack,null_stack,ground,
    [foreign_name("C","ccstk"),untrailed]).
:- mutable(ccstk_ptr,int,-1,ground,[foreign_name("C","ccptr"),untrailed]).
:- mutable(ccstk_limit,int,0,ground,[foreign_name("C","cclimit"),untrailed]).

    % Register that is set to 1 each time a rule fires (i.e. a commit/2
    % instruction is executed).
    %
:- mutable(changed_reg,int,0,ground,[foreign_name("C","ch_reg"),untrailed]).

    % Register that contains the set of wakeup conditions generated during
    % normalisation.
    %
:- mutable(wakeup_reg,events,0,ground,
    [foreign_name("C","wakeup_reg"),untrailed]).

    % Register that contains "extra" events that are to propagate upwards.
    % This is necessary for the correct implementation of top_level.
    %
:- mutable(extra_reg,events,0,ground,[foreign_name("C","extra_reg"),untrailed]).

    % Records if the Cd engine has been initialised or not.
    %
:- mutable(engine_initialised,bool,no,ground,[untrailed]).

    % Records if we are in debug mode or not.
    %
:- mutable(debug,bool,no,ground,[untrailed,attach_to_io_state]).

    % Records the current program.
    %
:- mutable(prog,prog,null_memory,ground,[foreign_name("C","CR_prog"),
    untrailed]).

%---------------------------------------------------------------------------%

startup :-
    lib_dummy,
    impure model.startup,
    impure reset_regs,
    impure Stack = init_stack,
    impure set_stack(Stack),
    impure set_stk_limit(init_stack_size),
    impure CStack = init_stack,
    impure set_cstack(CStack),
    impure set_cstk_limit(init_stack_size),
    impure DStack = init_stack,
    impure set_dstack(DStack),
    impure set_dstk_limit(init_stack_size),
    impure CCStack = init_stack,
    impure set_ccstack(CCStack),
    impure set_ccstk_limit(init_stack_size),
    impure set_halt_return_instrs,
    impure ccstack_push(ac_index_empty),
    impure set_engine_initialised(yes).

startup(!IO) :-
    promise_pure (
        impure machine.startup,
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

:- impure pred set_halt_return_instrs is det.
:- pragma foreign_proc("C",
    set_halt_return_instrs,
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    CR_halt_0_instr = CR_execute(CR_TRANSLATE, (MR_Word) NULL, op_halt_0);
    CR_return_0_instr = CR_execute(CR_TRANSLATE, (MR_Word) NULL, op_return_0);
    CR_interpret_0_instrs[0] = CR_execute(CR_TRANSLATE, (MR_Word) NULL,
        op_interpret_0);
    CR_interpret_0_instrs[1] = CR_return_0_instr;
").

%---------------------------------------------------------------------------%

reset :-
    impure reset_regs,
        % Zero the stacks to remove any left-over pointers so the GC can
        % collect them.
        %
    semipure get_stack(Stk),
    semipure get_stk_limit(Lim),
    impure zero_memory(Lim,Stk),
    semipure get_cstack(CStk),
    semipure get_cstk_limit(CLim),
    impure zero_memory(CLim,CStk),
    semipure get_dstack(DStk),
    semipure get_dstk_limit(DLim),
    impure zero_memory(DLim,DStk),
    semipure get_ccstack(CCStk),
    semipure get_ccstk_limit(CCLim),
    impure zero_memory(CCLim,CCStk),
    impure restore_sigint_handler,
    impure ccstack_push(ac_index_empty).

reset(!IO) :-
    promise_pure (
        impure machine.reset,
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

:- impure pred reset_regs is det.

reset_regs :-
    impure set_stk_ptr(-1),
    impure set_cstk_ptr(-1),
    impure set_dstk_ptr(-1),
    impure set_ccstk_ptr(-1),
    impure set_changed_reg(-1),
    impure set_annotation_reg(empty_annotations),
    impure set_wakeup_reg(0),
    impure set_extra_reg(0).

%---------------------------------------------------------------------------%

shutdown :-
    impure model.shutdown,
    impure set_stack(null_stack),
    impure set_stk_limit(0),
    impure set_cstack(null_stack),
    impure set_cstk_limit(0),
    impure set_dstack(null_stack),
    impure set_dstk_limit(0),
    impure set_ccstack(null_stack),
    impure set_ccstk_limit(0),
    impure set_engine_initialised(no).

shutdown(!IO) :-
    promise_pure (
        impure machine.shutdown,
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

interpret_main(Model, Result) :-
    impure interpret_main(Model, ResModel, Succ),
    ( if Succ \= 0 then
        Result = interpret_ok(ResModel)
    else
        Result = interpret_exception(ResModel)
    ).

interpret_main(Model, Result, !IO) :-
    promise_pure (
        impure interpret_main(Model, Result),
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

:- impure pred interpret_main(model::in, model::out, int::out) is det.
:- pragma foreign_proc("C",
    interpret_main(Model0::in, Model1::out, Succ::out),
    [may_call_mercury, thread_safe, will_not_modify_trail],
"
    Model1 = CR_interpret_main(Model0, (MR_Bool *) &Succ);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    interpret(Model0::in,Model1::out),
    [may_call_mercury, thread_safe, will_not_modify_trail],
"
    Model1 = CR_interpret(Model0);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    throw(Model::in),
    [may_call_mercury,thread_safe,promise_pure,will_not_modify_trail],
"
    CR_throw_exception(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C","
MR_Word CR_ho_call_setup(int N);
").

:- pragma foreign_decl("C",local,"
static MR_Word CR_shallow_copy(MR_Word model);
").

:- pragma foreign_code("C","

/*
 * Copy a term T but don't copy T's arguments.
 */
MR_Word CR_shallow_copy(MR_Word model)
{
    MR_Word sym;
    MR_Word copy;
    MR_Integer int_val;
    MR_Float float_val;
    MR_String string_val;
    MR_Integer string_len;
    MR_Integer var_val;
    MR_Integer aty;
    MR_Word idx;
    MR_Word arg;
    MR_Integer i;

    switch(CR_tag(model)) {
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(model,sym);
            if(CR_IS_AC_SYM(sym)) {
                CR_GET_AC_FUNCTOR_ATY(model,aty);
                CR_GET_AC_FUNCTOR_IDX(model,idx);
                CR_MAKE_AC_FUNCTOR(sym,aty,idx,copy);
            } else {
                aty = CR_SYM_ATY(sym);
                CR_MAKE_FUNCTOR(sym,copy);
                for(i = 1; i <= aty; i++) {
                    CR_GET_FUNCTOR_ARG(i,model,arg);
                    CR_SET_FUNCTOR_ARG(i,copy,arg);
                }
            }
            break;
        case CR_INT_TAG: case CR_UBI_TAG:
            CR_GET_INT_VAL(model,int_val);
            CR_MAKE_INT(int_val,copy);
            break;
        case CR_VAR_TAG:
            CR_GET_VAR_VAL(model,var_val);
            CR_MAKE_VAR(var_val,copy);
            break;
        case CR_FLT_TAG:
            CR_GET_FLOAT_VAL(model,float_val);
            CR_MAKE_FLOAT(float_val,copy);
            break;
        case CR_STR_TAG:
            CR_GET_STRING_VAL(model,string_val);
            CR_GET_STRING_LENGTH(model,string_len);
            CR_MAKE_STRING(string_val,string_len,copy);
            break;
        case CR_FGN_TAG:
            CR_GET_FOREIGN_VAL(model,arg);
            CR_MAKE_FOREIGN(arg,copy);
            break;
        default:
            MR_assert(0);
    }

    return copy;
}

/*
 * Set-up a higher-order call(f,...)
 * - if 'f' is a function, sets the stacks to the correct state and returns
 *   the address of f's procedure.  But does not call f (upto the caller of
 *   CR_ho_call_setup).
 * - if 'f' is not a function, then construct the f-term.
 */
MR_Word CR_ho_call_setup(int N)
{
    MR_Word closure = CR_stk_val(N);
    MR_Word sym;
    MR_Integer aty;
    MR_Word addr;
    MR_Word model;
    MR_Integer i;

    if(CR_IS_FUNCTOR(closure)) {
        CR_GET_FUNCTOR_SYM(closure,sym);
        if(CR_IS_AC_SYM(sym)) {
            if(N == 0)
                CR_top_stk_val() = CR_shallow_copy(CR_top_stk_val());
            else {
                while(N) {
                    CR_stk_val(1) = CR_ac_merge(CR_stk_val(0),sym,
                        CR_stk_val(1));
                    CR_pop_stk(1);
                    N--;
                }
            }
            aty = 0;
        } else {
            aty = CR_SYM_ATY(sym);

            /*
             * If aty == 0, then we must shift to the left by 1.
             * If aty == 1, then there is no need to shift; all arguments just
             * happen to be in their correct positions.
             * Otherwise, we must shift to the right by N-1.
             */
            if(aty == 0) {
                for(i = N; i > 0; i--)
                    CR_stk_val(i) = CR_stk_val(i-1);
            } else if(aty != 1) {
                for(i = 0; i < N; i++)
                   CR_stk_val(i-(aty-1)) = CR_stk_val(i);
            }

            ptr += (aty - 1);

            /*
             * Copy the arguments from the closure onto the stack.
             */
            for(i = 1; i <= aty; i++) {
                CR_GET_FUNCTOR_ARG(i,closure,model);
                CR_stk_val(aty+N-i) = model;
            }

            sym = CR_set_arity(sym,aty+N);
        }

        addr = CR_SYM_PROC(sym);
        if(CR_IS_AC_SYM(sym)) {
            /*
             * For ACD symbols we need to wakeup all of the arguments, since
             * the CC of each argument has changed.
             */
            if(CR_IS_D_SYM(sym))
                return CR_INTERPRET_ADDRESS;
            if(addr == (MR_Word)0)
                return CR_RETURN_ADDRESS;
            return addr;
        } else {
            if(addr == (MR_Word)0) {
                CR_MAKE_FUNCTOR_STACK(sym,(((MR_Word *)stk)+ptr),model);
                CR_pop_stk(aty+N);
                CR_SET_ANNOTS(annot_reg,model,model);
                CR_push_stk(model);
                return CR_RETURN_ADDRESS;
            } else
                return addr;
        }
    } else if(N == 0) {
        CR_SET_ANNOTS(annot_reg,closure,CR_top_stk_val());
        if(CR_IS_VAR(closure)) {
            addr = CR_SYM_PROC(CR_VAR_SYMBOL());
            if(addr == (MR_Word)0)
                return CR_RETURN_ADDRESS;
            return addr;
        } else {
            return CR_RETURN_ADDRESS;
        }
    } else {
        MR_Word err;
        sym = CR_symbol((MR_String)""runtime_error"",1);
        CR_MAKE_STRING_0(""first argument to call/N where N>0 not a closure"",
            err);
        CR_MAKE_FUNCTOR_1(sym,err,err);
        CR_throw_exception(err);
    }

    return CR_RETURN_ADDRESS;
}
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C","

MR_Word CR_halt_0_instr;
MR_Word CR_return_0_instr;
MR_Word CR_interpret_0_instrs[2];

MR_Word CR_exception;

#define CR_HALT_ADDRESS         ((MR_Word)(&CR_halt_0_instr))
#define CR_RETURN_ADDRESS       ((MR_Word)(&CR_return_0_instr))
#define CR_INTERPRET_ADDRESS    ((MR_Word)(CR_interpret_0_instrs))

/*
 * Interpreter modes.
 * CR_TRANSLATE = translate the VM instruction into the address of the code
 *                that implements that function.
 * CR_INTERPRET = execute a goal (from the stack)
 * CR_LOOKUP    = execute a lookup.
 */
#define CR_TRANSLATE            0
#define CR_INTERPRET            1
#define CR_LOOKUP               2

/*
 * Various macros to make the C-code more readable.
 */
#ifdef CR_FAST_INTERPETER

/*
 * CR_FAST_INTERPETER = each opcode is the address of the C code that
 * implements the VM instruction.  Thus we can jump to the code directly.
 */
#define CR_jump()                                                   \\
    do {                                                            \\
        goto **prog;                                                \\
    } while(0)

#define CR_begin_code_block()                                       \\
    CR_jump();
#define CR_end_code_block()                                         \\
    CR_end:

#define CR_code(label)                                              \\
    CR_make_local_label(label)

#else   /*CR_FAST_INTERPETER*/

/*
 * !CR_FAST_INTERPETER = use a switch(...) {...} statement to interpret
 * opcodes.
 */
#define CR_jump()                                                   \\
    do {                                                            \\
        goto CR_entry;                                              \\
    } while(0)

#define CR_begin_code_block()                                       \\
    CR_entry:                                                       \\
    CR_maybe_debug_instruction(prog,CR_opand(0));                   \\
    switch(CR_opand(0)) {
#define CR_end_code_block()                                         \\
    }                                                               \\
    CR_end:

#define CR_code(label)                                              \\
    case op_##label

#endif  /*CR_FAST_INTERPETER*/

#define CR_continue(len)                                            \\
    do {                                                            \\
        prog += (MR_Unsigned)len+1;                                 \\
        CR_jump();                                                  \\
    } while(0)

#define CR_goto(new_prog)                                           \\
    do {                                                            \\
        prog = (MR_Word *)(new_prog);                               \\
        CR_jump();                                                  \\
    } while(0)

#define CR_fail()                                                   \\
    do {                                                            \\
        prog =                                                      \\
            (MR_Word *)(((MR_Word *)dstk)[(MR_Unsigned)dptr--]);    \\
        CR_jump();                                                  \\
    } while(0)

#define CR_ret_addr(frame_size)                                     \\
    CR_reg_val(frame_size)

#define CR_halt()                                                   \\
    do {                                                            \\
        MR_Word ret_addr = CR_ret_addr(0);                          \\
        CR_pop_cstk(1);                                             \\
        goto *((void *)ret_addr);                                   \\
    } while(0)

#define CR_create_cp(addr)                                          \\
    CR_push_dstk(addr)

#define CR_opand(idx)                                               \\
    (((MR_Word *)prog)[((MR_Unsigned)(idx))])

#define CR_stk_val(idx)                                             \\
    (((MR_Word *)stk)[ptr-((MR_Unsigned)(idx))])

#define CR_top_stk_val()                                            \\
    CR_stk_val(0)

#define CR_maybe_grow_stack(stk,ptr,limit)                          \\
    if((ptr) >= (limit))                                            \\
        CR_grow_stack((MR_Word **)&(stk),&(limit))

#define CR_pop_stk(n)                                               \\
    (ptr -= (MR_Unsigned)(n))

#define CR_push_stk(val)                                            \\
    do {                                                            \\
        ptr++;                                                      \\
        CR_maybe_grow_stack(stk,ptr,limit);                         \\
        ((MR_Word *)stk)[ptr] = (MR_Word)(val);                     \\
    } while(0)

#define CR_reg_val(reg)                                             \\
    CR_cstk_val(reg)

#define CR_cstk_val(idx)                                            \\
    (((MR_Word *)cstk)[cptr-(MR_Unsigned)(idx)])

#define CR_push_cstk(n)                                             \\
    do {                                                            \\
        cptr += (MR_Integer)(n);                                    \\
        CR_maybe_grow_stack(cstk,cptr,climit);                      \\
    } while(0)

#define CR_push_cstk_val(val)                                       \\
    do {                                                            \\
        cptr++;                                                     \\
        CR_maybe_grow_stack(cstk,cptr,climit);                      \\
        ((MR_Word *)cstk)[cptr] = (MR_Word)(val);                   \\
    } while(0)

#define CR_pop_cstk(n)                                              \\
    (cptr -= (MR_Unsigned)(n))

#define CR_ccstk_val(idx)                                           \\
    (((MR_Word *)ccstk)[(MR_Unsigned)(idx)])

#define CR_pop_ccstk(n)                                             \\
    (ccptr -= (MR_Unsigned)(n))

#define CR_push_ccstk(val)                                          \\
    do {                                                            \\
        ccptr++;                                                    \\
        CR_maybe_grow_stack(ccstk,ccptr,cclimit);                   \\
        ((MR_Word *)ccstk)[ccptr] = (MR_Word)(val);                 \\
    } while(0)

#define CR_pop_dstk(n)                                              \\
    (dptr -= (MR_Unsigned)(n))

#define CR_push_dstk(val)                                           \\
    do {                                                            \\
        dptr++;                                                     \\
        CR_maybe_grow_stack(dstk,dptr,dlimit);                      \\
        ((MR_Word *)dstk)[dptr] = (MR_Word)(val);                   \\
    } while(0)

#define CR_AC_CXT_TAG   MR_mktag(1)

#define CR_true_sym()                                               \\
    CR_TRUE_SYMBOL()

#define CR_false_sym()                                              \\
    CR_FALSE_SYMBOL()

#define CR_dummy_sym()                                              \\
    CR_DUMMY_SYMBOL()

#define CR_empty_itr()                                              \\
    MR_list_empty()

/*
 * Implementation of switches uses binary search.
 * TODO: Possible to replace this with O(1) switching?
 */
#define CR_switch_table_goto(tb_len,table,sym)                      \\
    do {                                                            \\
        MR_Integer mid_sym;                                         \\
        MR_Integer min = 0;                                         \\
        MR_Integer max = ((int)(tb_len))-1;                         \\
        MR_Integer mid;                                             \\
                                                                    \\
        while(min <= max) {                                         \\
            mid = (min + max)/2;                                    \\
            mid_sym = ((MR_Word *)(table))[mid];                    \\
            if((MR_Integer)(sym) < mid_sym)                         \\
                max = mid-1;                                        \\
            else if((MR_Integer)(sym) > mid_sym)                    \\
                min = mid+1;                                        \\
            else                                                    \\
                CR_goto(                                            \\
                 ((MR_Word *)(table))[((MR_Integer)(tb_len))+mid]); \\
        }                                                           \\
        CR_fail();                                                  \\
    } while(0)

#define CR_FOREIGN_CALL_0                                           \\
    (*((MR_Word (*)(void))CR_opand(2)))
#define CR_FOREIGN_CALL_1                                           \\
    (*((MR_Word (*)(MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_2                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_3                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word,MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_4                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word,MR_Word,MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_5                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word,MR_Word,MR_Word,MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_6                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word,MR_Word,MR_Word,MR_Word,        \\
        MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_7                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word,MR_Word,MR_Word,MR_Word,        \\
        MR_Word,MR_Word))CR_opand(2)))
#define CR_FOREIGN_CALL_8                                           \\
    (*((MR_Word (*)(MR_Word,MR_Word,MR_Word,MR_Word,MR_Word,        \\
        MR_Word,MR_Word,MR_Word))CR_opand(2)))

/*
 * Variables used by the interpret_0 implementation.
 */
#define CR_VAR_RET_ADDR         CR_reg_val(0)
#define CR_VAR_AC_CXT           CR_reg_val(1)
#define CR_VAR_MODEL            CR_reg_val(2)
#define CR_VAR_ANNOTS           CR_reg_val(3)
#define CR_VAR_SYM              CR_reg_val(4)
#define CR_VAR_I                CR_reg_val(5)
#define CR_VAR_J                CR_reg_val(6)
#define CR_VAR_IDX              CR_reg_val(7)
#define CR_VAR_EVENTS           CR_reg_val(8)
#define CR_VAR_SYMS             CR_reg_val(9)
#define CR_VAR_HEAD             CR_reg_val(10)
#define CR_VAR_DEPTH            CR_reg_val(11)
#define CR_VAR_NEXT_EVENTS      CR_reg_val(12)
#define CR_VAR_LCL_CH_REG       CR_reg_val(13)
#define CR_VAR_LCL_WAKEUP_REG   CR_reg_val(14)
#define CR_VAR_LCL_EXTRA_REG    CR_reg_val(15)
#define CR_VAR_LCL_PC           CR_reg_val(16)
#define CR_VAR_OLD_ARG          CR_reg_val(17)
#define CR_VAR_ARG              CR_VAR_NEXT_EVENTS
#define CR_INTERPRET_FRAME_SIZE 18

#define CR_INTERPRET_CALL(ret_addr,events,ac_cxt)                   \\
    do {                                                            \\
        CR_INTERPRET_CALL_0(ret_addr,events,ac_cxt);                \\
ret_addr:;                                                          \\
    } while(0)

#define CR_INTERPRET_CALL_0(ret_addr,events,ac_cxt)                 \\
    do {                                                            \\
        CR_push_cstk(CR_INTERPRET_FRAME_SIZE);                      \\
        CR_VAR_RET_ADDR = (MR_Word)(&&ret_addr);                    \\
        CR_VAR_EVENTS   = (events);                                 \\
        CR_VAR_AC_CXT   = (ac_cxt);                                 \\
        goto interpret_0_entry;                                     \\
    } while(0)

#define CR_INTERPRET_RETURN()                                       \\
    do {                                                            \\
        MR_Word tmp = CR_VAR_RET_ADDR;                              \\
        CR_pop_cstk(CR_INTERPRET_FRAME_SIZE);                       \\
        goto *(void *)tmp;                                          \\
    } while(0)

#define CR_INTERPRET_PROC_CALL(proc_addr,ret_addr)                  \\
    do {                                                            \\
        CR_VAR_LCL_PC = (MR_Word)prog;                              \\
        CR_push_cstk(2);                                            \\
        CR_reg_val(0) = (MR_Word)CR_HALT_ADDRESS;                   \\
        CR_reg_val(1) = (MR_Word)(&&ret_addr);                      \\
        CR_goto(proc_addr);                                         \\
ret_addr:                                                           \\
        prog = (MR_Word *)CR_VAR_LCL_PC;                            \\
    } while(0)

/*
 * Wakeup-conditions/events are represented as single-word bitsets.  The first
 * two bits are reserved.  If we run out of bits, then we start recycling
 * existing bits.  This is safe, however, it makes wakeups less accurate.  In
 * practice, there hasn't been an application that runs out of bits (yet).
 */
#define CR_EVENTS_EMPTY                     0x0
#define CR_EVENTS_CHANGED                   0x1
#define CR_EVENTS_REDO                      0x2
#define CR_EVENTS_UNION(xs,ys)              ((xs)|(ys))
#define CR_EVENTS_INTERSECTION(xs,ys)       ((xs)&(ys))
#define CR_EVENTS_MINUS(xs,ys)              ((xs)&(~(ys)))

#define CR_EVENT_MAX                                                \\
    (sizeof(MR_Integer)==sizeof(int)?(MR_Word)0x80000000:           \\
        (MR_Word)0x8000000000000000)
#define CR_EVENT_MIN                                                \\
    ((MR_Word)(((MR_Unsigned)CR_EVENTS_REDO)<<1))

#define CR_EVENT_NEXT(curr)                                         \\
    (((curr) == CR_EVENT_MAX)?CR_EVENT_MIN:((MR_Unsigned)(curr))<<1)

#define CR_lookup_return(res)                                       \\
    do {                                                            \\
        return (res);                                               \\
    } while(0)

#ifdef CR_DEBUG
#define CR_maybe_debug_instruction(prog,instr)                      \\
    CR_debug_instruction(prog,instr)
#else
#define CR_maybe_debug_instruction(prog,instr)
#endif

").

%---------------------------------------------------------------------------%

:- impure pred ll_instr_0_to_exec_ll_instr_0(ll_instr_0::in,int::out) is det.

:- pragma foreign_proc("C",
    ll_instr_0_to_exec_ll_instr_0(Instr::in,EInstr::out),
    [will_not_call_mercury,thread_safe,will_not_modify_trail],"
    EInstr = CR_execute(CR_TRANSLATE,(MR_Word)NULL,Instr);
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C","
MR_Word CR_interpret_main(MR_Word model,MR_Bool *success);
MR_Word CR_interpret(MR_Word model);
MR_Word CR_execute(MR_Integer Mode,MR_Word Prog,MR_Word Instr0);
void CR_throw_exception(MR_Word model) __attribute__((noreturn));
").

:- pragma foreign_code("C","

MR_Word CR_halt_0_instr;
MR_Word CR_return_0_instr;
MR_Word CR_interpret_0_instrs[2];

MR_Word CR_exception = (MR_Word)NULL;

static jmp_buf CR_jmp_env;

MR_Word CR_interpret_main(MR_Word model,MR_Bool *success)
{
    MR_Word sym;

    if(setjmp(CR_jmp_env)) {
        /*
         * An exception occurred.
         */
        *success = MR_FALSE;
        if(CR_exception != (MR_Word)NULL) {
            model = CR_exception;
            CR_exception = (MR_Word)NULL;
            return model;
        } else
            return CR_undefined_model;
    } else {
        model = CR_interpret(model);
        /*
         * No exception occurred.
         */
        *success = MR_TRUE;
        return model;
    }
}

MR_Word CR_interpret(MR_Word model)
{
    MR_Integer lcl_ptr;
    MR_Word sym;

    CR_push_stk(model);
    lcl_ptr = ptr;
    CR_execute(CR_INTERPRET,CR_prog,(MR_Word)NULL);

    if(ptr == lcl_ptr) {
        model = CR_stk_val(0);
        CR_pop_stk(1);
        return model;
    } else {
        CR_MAKE_INT(ptr,model);
        sym = CR_symbol((MR_String)""invalid_stack_state"",1);
        CR_MAKE_FUNCTOR_1(sym,model,model);
        CR_throw_exception(model);
        return (MR_Word)NULL;
    }
}

void CR_throw_exception(MR_Word model)
{
    CR_exception = model;
    longjmp(CR_jmp_env,0);
}

MR_Word CR_execute(MR_Integer Mode,MR_Word Prog,MR_Word Instr0)
{
    /* prog = Prog points to the compiled Cadmium machine code. */
    MR_Word *prog;

    switch(Mode) {
        case CR_TRANSLATE:
#ifdef CR_FAST_INTERPETER

#define CR_instr_map(label)                                                \\
    case op_##label: return (MR_Word)(&&CR_make_local_label(label));       \\
    break

            switch(Instr0) {
                CR_instr_map(is_int_2);
                CR_instr_map(is_float_2);
                CR_instr_map(is_string_2);
                CR_instr_map(is_named_var_2);
                CR_instr_map(is_functor_2);
                CR_instr_map(is_ac_functor_3);
                CR_instr_map(is_ac_functor_2);
                CR_instr_map(is_geq_ac_functor_3);
                CR_instr_map(is_geq_ac_functor_2);
                CR_instr_map(is_type_2);
                CR_instr_map(get_arg_3);
                CR_instr_map(get_arg_2);
                CR_instr_map(pre_guard_1);
                CR_instr_map(post_guard_1);
                CR_instr_map(post_match_guard_2);
                CR_instr_map(switch_3);
                CR_instr_map(pop_1);
                CR_instr_map(construct_1);
                CR_instr_map(construct_ac_2);
                CR_instr_map(construct_acd_3);
                CR_instr_map(construct_acd_4);
                CR_instr_map(interpret_0);
                CR_instr_map(interpret_1);
                CR_instr_map(return_0);
                CR_instr_map(push_new_var_0);
                CR_instr_map(push_int_1);
                CR_instr_map(push_float_1);
                CR_instr_map(push_string_1);
                CR_instr_map(push_named_var_1);
                CR_instr_map(ccpush_1);
                CR_instr_map(ccpop_0);
                CR_instr_map(cpush_1);
                CR_instr_map(cset_1);
                CR_instr_map(cget_1);
                CR_instr_map(ccopy_2);
                CR_instr_map(ceqeq_2);
                CR_instr_map(cpop_1);
                CR_instr_map(call_label_1);
                CR_instr_map(call_label_lco_1);
                CR_instr_map(call_ac_arg_label_2);
                CR_instr_map(call_ac_label_lco_1);
                CR_instr_map(call_ho_1);
                CR_instr_map(create_cp_1);
                CR_instr_map(commit_0);
                CR_instr_map(commit_2);
                CR_instr_map(call_foreign_2);
                CR_instr_map(lookup_5);
                CR_instr_map(lookup_4);
                CR_instr_map(lookup_first_3);
                CR_instr_map(lookup_first_2);
                CR_instr_map(split_2);
                CR_instr_map(init_itr_3);
                CR_instr_map(get_next_5);
                CR_instr_map(lookup_cc_5);
                CR_instr_map(lookup_cc_6);
                CR_instr_map(lookup_first_cc_2);
                CR_instr_map(lookup_first_cc_3);
                CR_instr_map(get_next_cc_6);
                CR_instr_map(top_level_0);
                CR_instr_map(is_diff_2);
                CR_instr_map(delete_2);
                CR_instr_map(flatten_1);
                CR_instr_map(set_annots_1);
                CR_instr_map(get_annots_1);
                CR_instr_map(get_annots_2);
                CR_instr_map(clear_annots_0);
                CR_instr_map(restore_annots_0);
                CR_instr_map(setup_annots_0);
                CR_instr_map(call_annots_0);
                CR_instr_map(attach_annots_0);
                CR_instr_map(add_0);
                CR_instr_map(subtract_0);
                CR_instr_map(multiply_0);
                CR_instr_map(divide_0);
                CR_instr_map(mod_0);
                CR_instr_map(negate_0);
                CR_instr_map(and_0);
                CR_instr_map(or_0);
                CR_instr_map(iff_0);
                CR_instr_map(xor_0);
                CR_instr_map(impl_0);
                CR_instr_map(not_0);
                CR_instr_map(lt_0);
                CR_instr_map(leq_0);
                CR_instr_map(gt_0);
                CR_instr_map(geq_0);
                CR_instr_map(eq_0);
                CR_instr_map(neq_0);
                CR_instr_map(lookup_is_int_2);
                CR_instr_map(lookup_is_float_2);
                CR_instr_map(lookup_is_string_2);
                CR_instr_map(lookup_is_named_var_2);
                CR_instr_map(lookup_is_functor_2);
                CR_instr_map(lookup_is_ac_functor_3);
                CR_instr_map(lookup_is_geq_ac_functor_3);
                CR_instr_map(lookup_is_type_2);
                CR_instr_map(lookup_compare_2);
                CR_instr_map(lookup_accept_0);
                CR_instr_map(wakeup_on_redo_0);
                CR_instr_map(wakeup_on_1);
                CR_instr_map(debug_1);
                CR_instr_map(halt_0);
                default:
                    {
                        MR_Word sym, err;
                        sym = CR_symbol((MR_String)""runtime_error"",1);
                        CR_MAKE_STRING_0(""invalid virtual machine ""
                            ""instruction"",err);
                        CR_MAKE_FUNCTOR_1(sym,err,err);
                        CR_throw_exception(err);
                    }
            }
#endif /*CR_FAST_INTERPETER*/
            return Instr0;

        case CR_INTERPRET:
            prog = (MR_Word *)Prog;

                /*
                 * Enter the Cadmium interpreter.   This jumps to the code for
                 * interpret_0 below, which executes the top-most stack element
                 * as if it were a goal.  After normalisation is complete, a
                 * halt_0 instruction will be executed, which causes us to jump
                 * to the CR_engine_exit label below.
                 */
            CR_INTERPRET_CALL_0(CR_engine_exit,CR_EVENTS_EMPTY,
                CR_DUMMY_SYMBOL());

        case CR_LOOKUP:
            prog = (MR_Word *)Prog;

                /*
                 * Like CR_INTERPRET but simply jump to the given address and
                 * do not interpret to the top stack value.  Used for lookups.
                 */
            CR_jump();

        default:
            {
                MR_Word sym, err;
                sym = CR_symbol((MR_String)""runtime_error"",1);
                CR_MAKE_STRING_0(""invalid virtual machine mode"",err);
                CR_MAKE_FUNCTOR_1(sym,err,err);
                CR_throw_exception(err);
            }
    }

    /* NOT REACHED */

    /* Cadmium's execution loop. */
CR_begin_code_block()
CR_code(is_int_2):
    if(CR_IS_INT_VAL(CR_reg_val(CR_opand(1)),CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_float_2):
    if(CR_IS_FLOAT_VAL(CR_reg_val(CR_opand(1)),
       MR_word_to_float(CR_opand(2))))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_string_2):
    if(CR_IS_STRING_VAL(CR_reg_val(CR_opand(1)),CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_named_var_2):
    if(CR_IS_VAR_VAL(CR_reg_val(CR_opand(1)),CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_functor_2):
    if(CR_IS_FUNCTOR_SYM(CR_reg_val(CR_opand(1)),CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_ac_functor_3):
    if(CR_IS_FUNCTOR_SYM(CR_reg_val(CR_opand(1)),CR_opand(2)) &&
       CR_HAS_AC_FUNCTOR_ATY(CR_reg_val(CR_opand(1)),CR_opand(3)))
        CR_continue(3);
    else
        CR_fail();
CR_code(is_ac_functor_2):
    if(CR_IS_FUNCTOR_SYM(CR_top_stk_val(),CR_opand(1)) &&
       CR_HAS_AC_FUNCTOR_ATY(CR_top_stk_val(),CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_geq_ac_functor_3):
    if(CR_IS_FUNCTOR_SYM(CR_reg_val(CR_opand(1)),CR_opand(2)) &&
       CR_HAS_GEQ_AC_FUNCTOR_ATY(CR_reg_val(CR_opand(1)),CR_opand(3)))
        CR_continue(3);
    else
        CR_fail();
CR_code(is_geq_ac_functor_2):
    if(CR_IS_FUNCTOR_SYM(CR_top_stk_val(),CR_opand(1)) &&
       CR_HAS_GEQ_AC_FUNCTOR_ATY(CR_top_stk_val(),CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(is_type_2):
    {
        MR_Word type;
        CR_GET_TYPE(CR_reg_val(CR_opand(1)),type);
        if(type == CR_opand(2))
            CR_continue(2);
        else
            CR_fail();
    }
CR_code(get_arg_3):
    CR_GET_FUNCTOR_ARG(CR_opand(2),CR_reg_val(CR_opand(1)),
        CR_reg_val(CR_opand(3)));
    CR_continue(3);
CR_code(get_arg_2):
    CR_reg_val(CR_opand(2)) = CR_stk_val(CR_opand(1));
    CR_continue(2);
CR_code(pre_guard_1):
    if(ch_reg)
        CR_reg_val(CR_opand(1)) =
            CR_EVENTS_UNION(wakeup_reg,CR_EVENTS_CHANGED);
    else
        CR_reg_val(CR_opand(1)) = wakeup_reg;
    wakeup_reg = CR_EVENTS_EMPTY;
    CR_reg_val(0) = annot_reg;
    annot_reg = CR_empty_annots_model;
    CR_continue(1);
CR_code(post_guard_1):
    ch_reg = CR_EVENTS_INTERSECTION(CR_reg_val(CR_opand(1)),CR_EVENTS_CHANGED);
    annot_reg = CR_reg_val(0);
    if(CR_IS_FUNCTOR_SYM(CR_top_stk_val(),CR_true_sym())) {
        /*
         * `redo' event is removed under the snapshot semantics for guards.
         */
        wakeup_reg = CR_EVENTS_UNION(CR_reg_val(CR_opand(1)),
            CR_EVENTS_MINUS(wakeup_reg,
                CR_EVENTS_UNION(CR_EVENTS_CHANGED,CR_EVENTS_REDO)));
        CR_pop_stk(1);
        CR_continue(1);
    } else {
        wakeup_reg = CR_EVENTS_MINUS(CR_reg_val(CR_opand(1)),
            CR_EVENTS_CHANGED);
        CR_pop_stk(1);
        CR_fail();
    }
CR_code(post_match_guard_2):
    ch_reg = CR_EVENTS_INTERSECTION(CR_reg_val(CR_opand(2)),CR_EVENTS_CHANGED);
    annot_reg = CR_reg_val(0);
    /*if(!CR_IS_FUNCTOR_SYM(CR_top_stk_val(),CR_UNDEFINED_SYMBOL())) {*/
        /*
         * `redo' event cannot be removed, since the result of a pattern guard
         * may be copied to the body of a rule.
         */
        wakeup_reg = CR_EVENTS_UNION(CR_reg_val(CR_opand(2)),
            CR_EVENTS_MINUS(wakeup_reg,CR_EVENTS_CHANGED));
        CR_reg_val(CR_opand(1)) = CR_top_stk_val();
        CR_pop_stk(1);
        CR_continue(2);
    /*} else {
        wakeup_reg = CR_EVENTS_MINUS(CR_reg_val(CR_opand(2)),
            CR_EVENTS_CHANGED);
        CR_pop_stk(1);
        CR_fail();
    }*/
CR_code(switch_3):
    {
        MR_Word sym;
        CR_GET_SYMBOL(CR_reg_val(CR_opand(1)),sym);
        CR_switch_table_goto(CR_opand(3),CR_opand(2),sym);
    }
CR_code(pop_1):
    CR_pop_stk(CR_opand(1));
    CR_continue(1);
CR_code(construct_1):
    {
        MR_Word sym = CR_opand(1);
        MR_Word model;
        CR_MAKE_FUNCTOR_STACK(sym,(((MR_Word *)stk)+ptr),model);
        CR_pop_stk(CR_SYM_ATY(sym));
        CR_push_stk(model);
        CR_continue(1);
    }
CR_code(construct_ac_2):
    {
        MR_Integer aty = CR_opand(2);
        if(aty == (MR_Integer)0)
            CR_push_stk(CR_make_atom(CR_opand(1)));
        else {
            while(aty > (MR_Integer)1) {
                CR_stk_val(1) = CR_ac_merge(CR_stk_val(1),CR_opand(1),
                    CR_stk_val(0));
                CR_pop_stk(1);
                aty--;
            }
        }
        CR_continue(2);
    }
CR_code(construct_acd_3):
    {
        MR_Integer aty = CR_opand(3);
        if(aty == (MR_Integer)0)
            CR_push_stk(CR_make_atom(CR_opand(2)));
        else {
            CR_reg_val(CR_opand(1)) = CR_create_events(CR_stk_val(0),
                CR_opand(2));
            CR_stk_val(0) = CR_set_wakeups(CR_EVENTS_REDO,CR_stk_val(0),
                CR_opand(2));
            while(aty > (MR_Integer)1) {
                CR_reg_val(CR_opand(1)) = CR_EVENTS_UNION(
                    CR_reg_val(CR_opand(1)),
                    CR_create_events(CR_stk_val(1),CR_opand(2)));
                CR_stk_val(1) =
                    CR_set_wakeups(CR_EVENTS_REDO,CR_stk_val(1),
                        CR_opand(2));
                CR_stk_val(1) = CR_ac_merge(CR_stk_val(1),CR_opand(2),
                    CR_stk_val(0));
                CR_pop_stk(1);
                aty--;
            }
        }
        CR_continue(3);
    }
CR_code(construct_acd_4):
    {
        MR_Integer aty = CR_opand(3);
        CR_reg_val(CR_opand(1)) = CR_create_events(CR_stk_val(0),CR_opand(2));
        CR_stk_val(0) = CR_set_wakeups(CR_EVENTS_REDO,CR_stk_val(0),
            CR_opand(2));
        while(aty > (MR_Integer)1) {
            CR_reg_val(CR_opand(1)) = CR_EVENTS_UNION(
                CR_reg_val(CR_opand(1)),
                CR_create_events(CR_stk_val(1),CR_opand(2)));
            CR_stk_val(1) =
                CR_set_wakeups(CR_EVENTS_REDO,CR_stk_val(1),CR_opand(2));
            CR_stk_val(1) = CR_ac_merge(CR_stk_val(1),CR_opand(2),
                CR_stk_val(0));
            CR_pop_stk(1);
            aty--;
        }
        CR_stk_val(0) = CR_ac_merge(CR_stk_val(0),CR_opand(2),
            CR_reg_val(CR_opand(4)));
        CR_continue(4);
    }
CR_code(return_0):
    {
        MR_Word ret_pc = CR_ret_addr(0);

        if(MR_tag(ret_pc) == CR_AC_CXT_TAG) {
            /*
             * The call_ac bit is set.  This means we are returning
             * from an AC context, thus the call-stack must be popped
             * twice: once for the return address, and once for the
             * AC symbol.
             */
            CR_pop_cstk(2);
            CR_goto(MR_body(ret_pc,CR_AC_CXT_TAG));
        } else {
            CR_pop_cstk(1);
            CR_goto(ret_pc);
        }
    }
CR_code(call_ac_label_lco_1):
    {
        MR_Word ret_pc = CR_ret_addr(0);
        MR_Word sym;
        if(MR_tag(ret_pc) == CR_AC_CXT_TAG) {
            CR_GET_SYMBOL(CR_top_stk_val(),sym);
            if(sym == CR_cstk_val(1)) {
                /*
                 * The return value has the same AC functor as the
                 * AC context.  Simply return the current value
                 */
                CR_pop_cstk(2);
                CR_goto(MR_body(ret_pc,CR_AC_CXT_TAG));
            } else {
                CR_push_cstk_val(prog+2);
                CR_goto(CR_opand(1));
            }
        } else {
            CR_push_cstk_val(prog+2);
            CR_goto(CR_opand(1));
        }
    }
CR_code(push_new_var_0):
    {
        MR_Word model;
        CR_MAKE_VAR(CR_new_mvar(),model);
        CR_push_stk(model);
        CR_continue(0);
    }
CR_code(push_int_1):
    {
        MR_Word model;
        CR_MAKE_INT(CR_opand(1),model);
        CR_push_stk(model);
        CR_continue(1);
    }
CR_code(push_float_1):
    {
        MR_Word model;
        CR_MAKE_FLOAT(MR_word_to_float(CR_opand(1)),model);
        CR_push_stk(model);
        CR_continue(1);
    }
CR_code(push_string_1):
    {
        MR_Word model;
        CR_MAKE_STRING_0(CR_opand(1),model);
        CR_push_stk(model);
        CR_continue(1);
    }
CR_code(push_named_var_1):
    {
        MR_Word model;
        CR_MAKE_VAR(CR_opand(1),model);
        CR_push_stk(model);
        CR_continue(1);
    }
CR_code(ccpush_1):
    CR_push_ccstk(CR_ac_idx(CR_reg_val(CR_opand(1))));
    CR_continue(1);
CR_code(ccpop_0):
    CR_pop_ccstk(1);
    CR_continue(0);
CR_code(cpush_1):
    CR_push_cstk(CR_opand(1));
    CR_continue(1);
CR_code(cset_1):
    CR_reg_val(CR_opand(1)) = CR_top_stk_val();
    CR_continue(1);
CR_code(cget_1):
    CR_push_stk(CR_reg_val(CR_opand(1)));
    CR_continue(1);
CR_code(ccopy_2):
    CR_reg_val(CR_opand(2)) = CR_reg_val(CR_opand(1));
    CR_continue(2);
CR_code(ceqeq_2):
    if(CR_eqeq(CR_reg_val(CR_opand(1)),CR_reg_val(CR_opand(2))))
        CR_continue(2);
    else
        CR_fail();
CR_code(cpop_1):
    CR_pop_cstk(CR_opand(1));
    CR_continue(1);
CR_code(call_label_1):
    CR_push_cstk_val(prog+2);
    CR_goto(CR_opand(1));
CR_code(call_ac_arg_label_2):
    CR_push_cstk_val(CR_opand(1));
    CR_push_cstk_val(MR_mkword(CR_AC_CXT_TAG,prog+3));
    CR_goto(CR_opand(2));
CR_code(call_label_lco_1):
    CR_goto(CR_opand(1));
CR_code(call_ho_1):
    CR_push_cstk_val(prog+2);
    CR_goto(CR_ho_call_setup(CR_opand(1)));
CR_code(create_cp_1):
    CR_create_cp(CR_opand(1));
    CR_continue(1);
CR_code(commit_0):
    CR_pop_dstk(1);
    CR_continue(0);
CR_code(commit_2):
    CR_pop_dstk(CR_opand(1));
    CR_pop_stk(CR_opand(2));
    CR_reg_val(0) = annot_reg;
    annot_reg = CR_empty_annots_model;
    ch_reg = 1;
    CR_continue(2);
CR_code(lookup_5):
    {
        MR_Word idx;
        CR_GET_AC_FUNCTOR_IDX(CR_reg_val(CR_opand(1)),idx);
        if(CR_index_find_first_node(idx,CR_opand(2),&CR_reg_val(CR_opand(3)),
           &CR_reg_val(CR_opand(4)),(MR_Integer *)&CR_reg_val(CR_opand(5))))
            CR_continue(5);
        else
            CR_fail();
    }
CR_code(lookup_4):
    {
        MR_Word idx;
        CR_GET_AC_FUNCTOR_IDX(CR_top_stk_val(),idx);
        if(CR_index_find_first_node(idx,CR_opand(1),&CR_reg_val(CR_opand(2)),
           &CR_reg_val(CR_opand(3)),(MR_Integer *)&CR_reg_val(CR_opand(4))))
            CR_continue(4);
        else
            CR_fail();
    }
CR_code(lookup_first_3):
    {
        MR_Word idx, dummy_node;
        MR_Integer dummy_pos;
        CR_GET_AC_FUNCTOR_IDX(CR_reg_val(CR_opand(1)),idx);
        if(CR_index_find_first_node(idx,CR_opand(2),&CR_reg_val(CR_opand(3)),
           &dummy_node,&dummy_pos))
            CR_continue(3);
        else
            CR_fail();
    }
CR_code(lookup_first_2):
    {
        MR_Word idx, dummy_node;
        MR_Integer dummy_pos;
        CR_GET_AC_FUNCTOR_IDX(CR_top_stk_val(),idx);
        if(CR_index_find_first_node(idx,CR_opand(1),&CR_reg_val(CR_opand(2)),
           &dummy_node,&dummy_pos))
            CR_continue(2);
        else
            CR_fail();
    }
CR_code(lookup_cc_5):
    {
        MR_Integer i = ccptr;

        do {
            if(CR_index_find_first_node(CR_ccstk_val(i),CR_opand(1),
               &CR_reg_val(CR_opand(3)),&CR_reg_val(CR_opand(4)),
               (MR_Integer *)&CR_reg_val(CR_opand(5)))) {
                CR_reg_val(CR_opand(2)) = i;
                CR_continue(5);
            }
            i--;
        } while(i >= (MR_Integer)0);
        CR_fail();
    }
CR_code(lookup_cc_6):
    {
        MR_Integer i;

        /*
         * First check the implicit conjunction collector.
         */
        if(CR_index_find_first_node(CR_ac_idx(CR_reg_val(CR_opand(1))),
           CR_opand(2),&CR_reg_val(CR_opand(4)),&CR_reg_val(CR_opand(5)),
           (MR_Integer *)&CR_reg_val(CR_opand(6)))) {
            CR_reg_val(CR_opand(3)) = ccptr+1;
            CR_continue(6);
        }

        /*
         * The rest proceeds like lookup_cc_5
         */
        i = ccptr;
        do {
            if(CR_index_find_first_node(CR_ccstk_val(i),CR_opand(2),
               &CR_reg_val(CR_opand(4)),&CR_reg_val(CR_opand(5)),
               (MR_Integer *)&CR_reg_val(CR_opand(6)))) {
                CR_reg_val(CR_opand(3)) = i;
                CR_continue(6);
            }
            i--;
        } while(i >= (MR_Integer)0);
        CR_fail();
    }
CR_code(lookup_first_cc_2):
    {
        MR_Integer i = ccptr, dummy_pos;
        MR_Word dummy_node;

        i = ccptr;
        do {
            if(CR_index_find_first_node(CR_ccstk_val(i),CR_opand(1),
               &CR_reg_val(CR_opand(2)),&dummy_node,&dummy_pos))
                CR_continue(2);
            i--;
        } while(i >= (MR_Integer)0);
        CR_fail();
    }
CR_code(lookup_first_cc_3):
    {
        MR_Integer i = ccptr, dummy_pos;
        MR_Word dummy_node;

        if(CR_index_find_first_node(CR_ac_idx(CR_reg_val(CR_opand(1))),
           CR_opand(2),&CR_reg_val(CR_opand(3)),&dummy_node,&dummy_pos))
            CR_continue(3);

        do {
            if(CR_index_find_first_node(CR_ccstk_val(i),CR_opand(2),
               &CR_reg_val(CR_opand(3)),&dummy_node,&dummy_pos))
                CR_continue(3);
            i--;
        } while(i >= (MR_Integer)0);
        CR_fail();
    }
CR_code(init_itr_3):
    {
        CR_iterator itr;

        itr = (CR_iterator)GC_MALLOC(sizeof(CR_iterator_s));
        CR_ITR_INIT(itr);
        CR_ITR_PUSH(itr,CR_reg_val(CR_opand(2)),CR_reg_val(CR_opand(1)));
        CR_reg_val(CR_opand(3)) = (MR_Word)itr;
        CR_continue(3);
    }
CR_code(split_2):
    {
        MR_Bool dummy;
        MR_Integer aty;
        MR_Word idx, sym, annots;

        CR_GET_FUNCTOR_SYM(CR_reg_val(CR_opand(1)),sym);
        CR_GET_AC_FUNCTOR_ATY(CR_reg_val(CR_opand(1)),aty);
        CR_GET_AC_FUNCTOR_IDX(CR_reg_val(CR_opand(1)),idx);
        CR_GET_AC_FUNCTOR_ANNOTS(CR_reg_val(CR_opand(1)),annots);
        idx = CR_index_delete_least(idx,&CR_reg_val(CR_opand(2)),&dummy);
        CR_MAKE_ANNOTATED_AC_FUNCTOR(sym,aty-1,idx,CR_reg_val(CR_opand(1)),
            annots);

        CR_continue(2);
    }
CR_code(get_next_5):
    if(CR_iterator_get_next((CR_iterator)CR_reg_val(CR_opand(1)),
       CR_opand(3),&CR_reg_val(CR_opand(4)),
       (MR_Integer *)&CR_reg_val(CR_opand(5)))) {
        CR_create_cp(CR_opand(2));
        CR_continue(5);
    } else
        CR_fail();
CR_code(get_next_cc_6):
    {
        MR_Integer i;
        MR_Integer pos;
        CR_iterator itr = (CR_iterator)CR_reg_val(CR_opand(2));
        MR_Word node;

        if(CR_iterator_get_next(itr,CR_opand(4),&CR_reg_val(CR_opand(5)),
           (MR_Integer *)&CR_reg_val(CR_opand(6)))) {
            CR_create_cp(CR_opand(3));
            CR_continue(6);
        } else {
            /*
             * The current iterator is empty, so we continue searching
             * the CC stack for more matches.
             */
            for(i = CR_reg_val(CR_opand(1))-1; i >= (MR_Integer)0; i--) {
                if(CR_index_find_first_node(CR_ccstk_val(i),CR_opand(4),
                   &CR_reg_val(CR_opand(5)),&node,&pos)) {
                    CR_ITR_INIT(itr);
                    CR_ITR_PUSH(itr,pos,node);
                    /*
                     * The following call to CR_iterator_get_next must always
                     * succeed.
                     */
                    CR_iterator_get_next(itr,CR_opand(4),
                        &CR_reg_val(CR_opand(5)),
                        (MR_Integer *)&CR_reg_val(CR_opand(6)));
                    CR_reg_val(CR_opand(1)) = i;
                    CR_create_cp(CR_opand(3));
                    CR_continue(6);
                }
            }

            /*
             * No more entries on the CC stack -- fail.
             */
            CR_fail();
        }
    }
CR_code(top_level_0):
    {
        MR_Word sym;
        MR_Word err;
        if(ccptr == 0) {
            sym = CR_symbol((MR_String)""runtime_error"",1);
            CR_MAKE_STRING_0(""attempt to modify the top-level ACD context ""
                ""when no such context exists"",err);
            CR_MAKE_FUNCTOR_1(sym,err,err);
            CR_throw_exception(err);
        } else {
            CR_GET_SYMBOL(CR_top_stk_val(),sym);
            if(!CR_IS_ACD_SYM(sym))
                sym = CR_DUMMY_SYMBOL();
            extra_reg = CR_EVENTS_UNION(extra_reg,
                CR_create_events(CR_top_stk_val(),sym));
            CR_top_stk_val() = CR_set_wakeups(CR_EVENTS_REDO,CR_top_stk_val(),
                sym);
            if(CR_IS_ACD_SYM(sym))
                CR_ccstk_val(0) = CR_index_merge(CR_ccstk_val(0),
                    CR_ac_idx(CR_top_stk_val()));
            else
                CR_ccstk_val(0) = CR_index_insert(CR_ccstk_val(0),
                    CR_top_stk_val());
            CR_pop_stk(1);
            CR_continue(0);
        }
    }
CR_code(is_diff_2):
    if(CR_reg_val(CR_opand(1)) != CR_reg_val(CR_opand(2)))
        CR_continue(2);
    else
        CR_fail();
CR_code(delete_2):
    {
        MR_Bool dummy;
        MR_Integer aty;
        MR_Word idx, sym, annots;

        CR_GET_FUNCTOR_SYM(CR_reg_val(CR_opand(1)),sym);
        CR_GET_AC_FUNCTOR_ATY(CR_reg_val(CR_opand(1)),aty);
        CR_GET_AC_FUNCTOR_IDX(CR_reg_val(CR_opand(1)),idx);
        CR_GET_AC_FUNCTOR_ANNOTS(CR_reg_val(CR_opand(1)),annots);
        idx = CR_index_delete(idx,CR_reg_val(CR_opand(2)),&dummy);
        CR_MAKE_ANNOTATED_AC_FUNCTOR(sym,aty-1,idx,CR_reg_val(CR_opand(1)),
            annots);

        CR_continue(2);
    }
CR_code(flatten_1):
    CR_reg_val(CR_opand(1)) = CR_ac_flatten(CR_reg_val(CR_opand(1)));
    CR_continue(1);
CR_code(set_annots_1):
    {
        MR_Integer aty;
        if(CR_IS_FUNCTOR_SYM(CR_reg_val(CR_opand(1)),CR_ANNOTATE_SYMBOL())) {
            CR_GET_AC_FUNCTOR_ATY(CR_reg_val(CR_opand(1)),aty);
            if(aty == 0)
                annot_reg = CR_empty_annots_model;
            else
                annot_reg = CR_reg_val(CR_opand(1));
        } else
            annot_reg = CR_ac_unflatten(CR_ANNOTATE_SYMBOL(),
                CR_reg_val(CR_opand(1)));
        CR_continue(1);
    }
CR_code(get_annots_1):
    CR_reg_val(CR_opand(1)) = annot_reg;
    CR_continue(1);
CR_code(get_annots_2):
    CR_GET_ANNOTS(CR_reg_val(CR_opand(1)),CR_reg_val(CR_opand(2)));
    CR_continue(2);
CR_code(clear_annots_0):
    annot_reg = CR_empty_annots_model;
    CR_continue(0);
CR_code(restore_annots_0):
    annot_reg = CR_reg_val(0);
    CR_continue(0);
CR_code(setup_annots_0):
    annot_reg = CR_ac_merge_annotations(CR_top_stk_val(),annot_reg);
    CR_pop_stk(1);
    CR_continue(0);
CR_code(call_annots_0):
    {
        MR_Word annots;
        CR_GET_ANNOTS(CR_top_stk_val(),annots);
        annot_reg = CR_ac_merge_annotations(annots,annot_reg);
        CR_push_cstk_val(prog+1);
        CR_goto(CR_ho_call_setup(0));
    }
CR_code(attach_annots_0):
    {
        MR_Word annots;
        CR_GET_ANNOTS(CR_stk_val(1),annots);
        annots = CR_ac_merge_annotations(CR_top_stk_val(),annots);
        CR_pop_stk(1);
        CR_SET_ANNOTS(annots,CR_top_stk_val(),CR_top_stk_val());
        CR_continue(0);
    }
CR_code(wakeup_on_redo_0):
    wakeup_reg = CR_EVENTS_UNION(wakeup_reg,CR_EVENTS_REDO);
    CR_continue(0);
CR_code(wakeup_on_1):
    wakeup_reg = CR_EVENTS_UNION(wakeup_reg,CR_opand(1));
    CR_continue(1);
CR_code(add_0):
    {
        MR_Integer i0, i1;
        MR_Float f0, f1;
        if(CR_IS_INT(CR_stk_val(0)) && CR_IS_INT(CR_stk_val(1))) {
            CR_GET_INT_VAL(CR_stk_val(0),i0);
            CR_GET_INT_VAL(CR_stk_val(1),i1);
            CR_MAKE_INT(i1+i0,CR_stk_val(1));
            ch_reg = 1;
        } else if(CR_IS_FLOAT(CR_stk_val(0)) && CR_IS_FLOAT(CR_stk_val(1))) {
            CR_GET_FLOAT_VAL(CR_stk_val(0),f0);
            CR_GET_FLOAT_VAL(CR_stk_val(1),f1);
            CR_MAKE_FLOAT(f1+f0,CR_stk_val(1));
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_2(CR_BPLUS_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
                CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(subtract_0):
    {
        MR_Integer i0, i1;
        MR_Float f0, f1;
        if(CR_IS_INT(CR_stk_val(0)) && CR_IS_INT(CR_stk_val(1))) {
            CR_GET_INT_VAL(CR_stk_val(0),i0);
            CR_GET_INT_VAL(CR_stk_val(1),i1);
            CR_MAKE_INT(i1-i0,CR_stk_val(1));
            ch_reg = 1;
        } else if(CR_IS_FLOAT(CR_stk_val(0)) && CR_IS_FLOAT(CR_stk_val(1))) {
            CR_GET_FLOAT_VAL(CR_stk_val(0),f0);
            CR_GET_FLOAT_VAL(CR_stk_val(1),f1);
            CR_MAKE_FLOAT(f1-f0,CR_stk_val(1));
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_2(CR_BMINUS_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
                CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(multiply_0):
    {
        MR_Integer i0, i1;
        MR_Float f0, f1;
        if(CR_IS_INT(CR_stk_val(0)) && CR_IS_INT(CR_stk_val(1))) {
            CR_GET_INT_VAL(CR_stk_val(0),i0);
            CR_GET_INT_VAL(CR_stk_val(1),i1);
            CR_MAKE_INT(i1*i0,CR_stk_val(1));
            ch_reg = 1;
        } else if(CR_IS_FLOAT(CR_stk_val(0)) && CR_IS_FLOAT(CR_stk_val(1))) {
            CR_GET_FLOAT_VAL(CR_stk_val(0),f0);
            CR_GET_FLOAT_VAL(CR_stk_val(1),f1);
            CR_MAKE_FLOAT(f1*f0,CR_stk_val(1));
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_2(CR_BMULTIPLY_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
                CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(divide_0):
    {
        MR_Integer i0, i1;
        MR_Float f0, f1;
        if(CR_IS_INT(CR_stk_val(0)) && CR_IS_INT(CR_stk_val(1))) {
            CR_GET_INT_VAL(CR_stk_val(0),i0);
            CR_GET_INT_VAL(CR_stk_val(1),i1);
            CR_MAKE_INT(i1/i0,CR_stk_val(1));
            ch_reg = 1;
        } else if(CR_IS_FLOAT(CR_stk_val(0)) && CR_IS_FLOAT(CR_stk_val(1))) {
            CR_GET_FLOAT_VAL(CR_stk_val(0),f0);
            CR_GET_FLOAT_VAL(CR_stk_val(1),f1);
            CR_MAKE_FLOAT(f1/f0,CR_stk_val(1));
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_2(CR_BDIVIDE_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
                CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(mod_0):
    {
        MR_Integer i0, i1;
        if(CR_IS_INT(CR_stk_val(0)) && CR_IS_INT(CR_stk_val(1))) {
            CR_GET_INT_VAL(CR_stk_val(0),i0);
            CR_GET_INT_VAL(CR_stk_val(1),i1);
            CR_MAKE_INT(i1%i0,CR_stk_val(1));
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_2(CR_BMOD_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
                CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(negate_0):
    {
        MR_Integer i;
        MR_Float f;
        if(CR_IS_INT(CR_stk_val(0))) {
            CR_GET_INT_VAL(CR_stk_val(0),i);
            CR_MAKE_INT(-i,CR_stk_val(0));
            ch_reg = 1;
        } else if(CR_IS_FLOAT(CR_stk_val(0))) {
            CR_GET_FLOAT_VAL(CR_stk_val(0),f);
            CR_MAKE_FLOAT(-f,CR_stk_val(0));
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_1(CR_BUMINUS_SYMBOL(),CR_stk_val(0),CR_stk_val(0));
        CR_continue(0);
    }
CR_code(and_0):
    {
        MR_Word sym1, sym2;

        if(CR_IS_FUNCTOR(CR_stk_val(0)) && CR_IS_FUNCTOR(CR_stk_val(1))) {
            CR_GET_FUNCTOR_SYM(CR_stk_val(0),sym1);
            CR_GET_FUNCTOR_SYM(CR_stk_val(1),sym2);
            switch(sym1) {
                case CR_TRUE_SYMBOL():
                    switch(sym2) {
                        case CR_TRUE_SYMBOL():
                            CR_pop_stk(1);
                            ch_reg = 1;
                            CR_top_stk_val() = CR_true_model;
                            CR_continue(0);
                        case CR_FALSE_SYMBOL():
                            CR_pop_stk(1);
                            ch_reg = 1;
                            CR_top_stk_val() = CR_false_model;
                            CR_continue(0);
                        default:
                            break;
                    }
                    break;
                case CR_FALSE_SYMBOL():
                    if((sym2 == CR_TRUE_SYMBOL()) ||
                       (sym2 == CR_FALSE_SYMBOL())) {
                        CR_pop_stk(1);
                        ch_reg = 1;
                        CR_top_stk_val() = CR_false_model;
                        CR_continue(0);
                    }
                    break;
                default:
                    break;
            }
        }
        CR_MAKE_FUNCTOR_2(CR_BAND_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
            CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(or_0):
    {
        MR_Word sym1, sym2;

        if(CR_IS_FUNCTOR(CR_stk_val(0)) && CR_IS_FUNCTOR(CR_stk_val(1))) {
            CR_GET_FUNCTOR_SYM(CR_stk_val(0),sym1);
            CR_GET_FUNCTOR_SYM(CR_stk_val(1),sym2);
            switch(sym1) {
                case CR_TRUE_SYMBOL():
                    if((sym2 == CR_TRUE_SYMBOL()) ||
                       (sym2 == CR_FALSE_SYMBOL())) {
                        CR_pop_stk(1);
                        ch_reg = 1;
                        CR_top_stk_val() = CR_true_model;
                        CR_continue(0);
                    }
                    break;
                case CR_FALSE_SYMBOL():
                    switch(sym2) {
                        case CR_TRUE_SYMBOL():
                            CR_pop_stk(1);
                            ch_reg = 1;
                            CR_top_stk_val() = CR_true_model;
                            CR_continue(0);
                        case CR_FALSE_SYMBOL():
                            CR_pop_stk(1);
                            ch_reg = 1;
                            CR_top_stk_val() = CR_false_model;
                            CR_continue(0);
                        default:
                            break;
                    }
                    break;
                default:
                    break;
            }
        }
        CR_MAKE_FUNCTOR_2(CR_BOR_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
            CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(iff_0):
    {
        MR_Word sym1, sym2;

        if(CR_IS_FUNCTOR(CR_stk_val(0)) && CR_IS_FUNCTOR(CR_stk_val(1))) {
            CR_GET_FUNCTOR_SYM(CR_stk_val(0),sym1);
            CR_GET_FUNCTOR_SYM(CR_stk_val(1),sym2);
            if(((sym1 == CR_TRUE_SYMBOL()) || (sym1 == CR_FALSE_SYMBOL())) &&
               ((sym2 == CR_TRUE_SYMBOL()) || (sym2 == CR_FALSE_SYMBOL()))) {
                CR_pop_stk(1);
                ch_reg = 1;
                CR_top_stk_val() = (sym1 == sym2)?CR_true_model:CR_false_model;
                CR_continue(0);
            }
        }
        CR_MAKE_FUNCTOR_2(CR_BIFF_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
            CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(xor_0):
    {
        MR_Word sym1, sym2;

        if(CR_IS_FUNCTOR(CR_stk_val(0)) && CR_IS_FUNCTOR(CR_stk_val(1))) {
            CR_GET_FUNCTOR_SYM(CR_stk_val(0),sym1);
            CR_GET_FUNCTOR_SYM(CR_stk_val(1),sym2);
            if(((sym1 == CR_TRUE_SYMBOL()) || (sym1 == CR_FALSE_SYMBOL())) &&
               ((sym2 == CR_TRUE_SYMBOL()) || (sym2 == CR_FALSE_SYMBOL()))) {
                CR_pop_stk(1);
                ch_reg = 1;
                CR_top_stk_val() = (sym1 == sym2)?CR_false_model:CR_true_model;
                CR_continue(0);
            }
        }
        CR_MAKE_FUNCTOR_2(CR_BXOR_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
            CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(impl_0):
    {
        MR_Word sym1, sym2;

        if(CR_IS_FUNCTOR(CR_stk_val(0)) && CR_IS_FUNCTOR(CR_stk_val(1))) {
            CR_GET_FUNCTOR_SYM(CR_stk_val(0),sym1);
            CR_GET_FUNCTOR_SYM(CR_stk_val(1),sym2);
            if(((sym1 == CR_TRUE_SYMBOL()) || (sym1 == CR_FALSE_SYMBOL())) &&
               ((sym2 == CR_TRUE_SYMBOL()) || (sym2 == CR_FALSE_SYMBOL()))) {
                CR_pop_stk(1);
                ch_reg = 1;
                CR_top_stk_val() = ((sym1 == CR_TRUE_SYMBOL()) &&
                                    (sym2 == CR_FALSE_SYMBOL()))?
                                    CR_false_model:CR_true_model;
                CR_continue(0);
            }
        }
        CR_MAKE_FUNCTOR_2(CR_BIMPL_SYMBOL(),CR_stk_val(1),CR_stk_val(0),
            CR_stk_val(1));
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(not_0):
    {
        if(CR_IS_FUNCTOR_SYM(CR_stk_val(0),CR_TRUE_SYMBOL())) {
            CR_top_stk_val() = CR_false_model;
            ch_reg = 1;
        } else if(CR_IS_FUNCTOR_SYM(CR_stk_val(0),CR_FALSE_SYMBOL())) {
            CR_top_stk_val() = CR_true_model;
            ch_reg = 1;
        } else
            CR_MAKE_FUNCTOR_1(CR_BNOT_SYMBOL(),CR_stk_val(0),CR_stk_val(0));
        CR_continue(0);
    }
CR_code(lt_0):
    {
        if(CR_model_compare(CR_stk_val(1),CR_stk_val(0)) == CR_LT)
            CR_stk_val(1) = CR_true_model;
        else
            CR_stk_val(1) = CR_false_model;
        ch_reg = 1;
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(gt_0):
    {
        if(CR_model_compare(CR_stk_val(1),CR_stk_val(0)) == CR_GT)
            CR_stk_val(1) = CR_true_model;
        else
            CR_stk_val(1) = CR_false_model;
        ch_reg = 1;
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(leq_0):
    {
        if(CR_model_compare(CR_stk_val(1),CR_stk_val(0)) != CR_GT)
            CR_stk_val(1) = CR_true_model;
        else
            CR_stk_val(1) = CR_false_model;
        ch_reg = 1;
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(geq_0):
    {
        if(CR_model_compare(CR_stk_val(1),CR_stk_val(0)) != CR_LT)
            CR_stk_val(1) = CR_true_model;
        else
            CR_stk_val(1) = CR_false_model;
        ch_reg = 1;
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(eq_0):
    {
        if(CR_model_compare(CR_stk_val(0),CR_stk_val(1)) == CR_EQ)
            CR_stk_val(1) = CR_true_model;
        else
            CR_stk_val(1) = CR_false_model;
        ch_reg = 1;
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(neq_0):
    {
        if(CR_model_compare(CR_stk_val(0),CR_stk_val(1)) != CR_EQ)
            CR_stk_val(1) = CR_true_model;
        else
            CR_stk_val(1) = CR_false_model;
        ch_reg = 1;
        CR_pop_stk(1);
        CR_continue(0);
    }
CR_code(lookup_is_int_2):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_Integer int_val;

        switch(CR_tag(model)) {
            case CR_FCT_TAG:
                CR_lookup_return(CR_GT);
            case CR_INT_TAG: case CR_UBI_TAG:
                CR_GET_INT_VAL(model,int_val);
                if((MR_Integer)CR_opand(2) < int_val)
                    CR_lookup_return(CR_LT);
                if((MR_Integer)CR_opand(2) > int_val)
                    CR_lookup_return(CR_GT);
                CR_continue(2);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_float_2):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_Float flt_val;

        switch(CR_tag(model)) {
            case CR_FCT_TAG: case CR_INT_TAG: case CR_UBI_TAG:
            case CR_VAR_TAG:
                CR_lookup_return(CR_GT);
            case CR_FLT_TAG:
                CR_GET_FLOAT_VAL(model,flt_val);
                if(MR_word_to_float(CR_opand(2)) < flt_val)
                    CR_lookup_return(CR_LT);
                if(MR_word_to_float(CR_opand(2)) > flt_val)
                    CR_lookup_return(CR_GT);
                CR_continue(2);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_string_2):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_String str_val;
        MR_Integer lcl_cmp;

        switch(CR_tag(model)) {
            case CR_FCT_TAG: case CR_INT_TAG: case CR_UBI_TAG:
            case CR_VAR_TAG: case CR_FLT_TAG:
                CR_lookup_return(CR_GT);
            case CR_STR_TAG:
                CR_GET_STRING_VAL(model,str_val);
                lcl_cmp = strcmp((MR_String)CR_opand(2),str_val);
                if(lcl_cmp < 0)
                    CR_lookup_return(CR_LT);
                if(lcl_cmp > 0)
                    CR_lookup_return(CR_GT);
                CR_continue(2);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_named_var_2):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_Integer var_val;

        switch(CR_tag(model)) {
            case CR_FCT_TAG: case CR_INT_TAG: case CR_UBI_TAG:
                CR_lookup_return(CR_GT);
            case CR_VAR_TAG:
                CR_GET_VAR_VAL(model,var_val);
                if((MR_Integer)CR_opand(2) < var_val)
                    CR_lookup_return(CR_LT);
                if((MR_Integer)CR_opand(2) > var_val)
                    CR_lookup_return(CR_GT);
                CR_continue(2);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_functor_2):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_Word sym;

        switch(CR_tag(model)) {
            case CR_FCT_TAG:
                CR_GET_FUNCTOR_SYM(model,sym);
                if(CR_IS_AC_SYM(sym))
                    CR_lookup_return(CR_GT);
                /*
                if(CR_SYM_ATY(CR_opand(2)) < CR_SYM_ATY(sym))
                    CR_lookup_return(CR_LT);
                if(CR_SYM_ATY(CR_opand(2)) > CR_SYM_ATY(sym))
                    CR_lookup_return(CR_GT);
                */
                if(CR_opand(2) < sym)
                    CR_lookup_return(CR_LT);
                if(CR_opand(2) > sym)
                    CR_lookup_return(CR_GT);
                CR_continue(2);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_ac_functor_3):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_Word sym;
        MR_Integer aty;

        switch(CR_tag(model)) {
            case CR_FCT_TAG:
                CR_GET_FUNCTOR_SYM(model,sym);
                if(!CR_IS_AC_SYM(sym))
                    CR_lookup_return(CR_LT);
                if(CR_opand(2) < sym)
                    CR_lookup_return(CR_LT);
                if(CR_opand(2) > sym)
                    CR_lookup_return(CR_GT);
                CR_GET_AC_FUNCTOR_ATY(model,aty);
                if((MR_Integer)CR_opand(3) < aty)
                    CR_lookup_return(CR_LT);
                if((MR_Integer)CR_opand(3) > aty)
                    CR_lookup_return(CR_GT);
                CR_continue(3);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_geq_ac_functor_3):
    {
        MR_Word model = CR_reg_val(CR_opand(1));
        MR_Word sym;
        MR_Integer aty;

        switch(CR_tag(model)) {
            case CR_FCT_TAG:
                CR_GET_FUNCTOR_SYM(model,sym);
                if(!CR_IS_AC_SYM(sym))
                    CR_lookup_return(CR_LT);
                if(CR_opand(2) < sym)
                    CR_lookup_return(CR_LT);
                if(CR_opand(2) > sym)
                    CR_lookup_return(CR_GT);
                CR_GET_AC_FUNCTOR_ATY(model,aty);
                if((MR_Integer)CR_opand(3) > aty)
                    CR_lookup_return(CR_GT);
                CR_continue(3);
            default:
                CR_lookup_return(CR_LT);
        }
    }
CR_code(lookup_is_type_2):
    {
        MR_Integer type;

        CR_GET_TYPE(CR_reg_val(CR_opand(1)),type);
        if((MR_Integer)CR_opand(2) < type)
            CR_lookup_return(CR_LT);
        if((MR_Integer)CR_opand(2) > type)
            CR_lookup_return(CR_GT);

        CR_continue(2);
    }
CR_code(lookup_compare_2):
    {
        MR_Integer lcl_cmp = CR_model_compare(CR_reg_val(CR_opand(2)),
            CR_reg_val(CR_opand(1)));
        if(lcl_cmp == CR_EQ)
            CR_continue(2);
        CR_lookup_return(lcl_cmp);
    }
CR_code(lookup_accept_0):
    CR_lookup_return(CR_EQ);
CR_code(debug_1):
    CR_debug(CR_opand(1));
    CR_continue(1);
CR_code(call_foreign_2):
    {
        MR_Word sym, err;
        MR_Word result;
        switch(CR_opand(1)) {
            case 0:
                result = CR_FOREIGN_CALL_0();
                break;
            case 1:
                result = CR_FOREIGN_CALL_1(CR_stk_val(0));
                break;
            case 2:
                result = CR_FOREIGN_CALL_2(CR_stk_val(1),CR_stk_val(0));
                break;
            case 3:
                result = CR_FOREIGN_CALL_3(CR_stk_val(2),CR_stk_val(1),
                    CR_stk_val(0));
                break;
            case 4:
                result = CR_FOREIGN_CALL_4(CR_stk_val(3),CR_stk_val(2),
                    CR_stk_val(1),CR_stk_val(0));
                break;
            case 5:
                result = CR_FOREIGN_CALL_5(CR_stk_val(4),CR_stk_val(3),
                    CR_stk_val(2),CR_stk_val(1),CR_stk_val(0));
                break;
            case 6:
                result = CR_FOREIGN_CALL_6(CR_stk_val(5),CR_stk_val(4),
                    CR_stk_val(3),CR_stk_val(2),CR_stk_val(1),CR_stk_val(0));
                break;
            case 7:
                result = CR_FOREIGN_CALL_7(CR_stk_val(6),CR_stk_val(5),
                    CR_stk_val(4),CR_stk_val(3),CR_stk_val(2),CR_stk_val(1),
                    CR_stk_val(0));
                break;
            case 8:
                result = CR_FOREIGN_CALL_8(CR_stk_val(7),CR_stk_val(6),
                    CR_stk_val(5),CR_stk_val(4),CR_stk_val(3),CR_stk_val(2),
                    CR_stk_val(1),CR_stk_val(0));
                break;
            default:
                sym = CR_symbol((MR_String)""runtime_error"",1);
                CR_MAKE_STRING_0(
                    ""foreign function call with too many arguments"",err);
                CR_MAKE_FUNCTOR_1(sym,err,err);
                CR_throw_exception(err);
        }
        CR_pop_stk(CR_opand(1));
        CR_push_stk(result);
        ch_reg = 1;
        CR_continue(2);
    }
CR_code(halt_0):
    CR_halt();
CR_code(interpret_1):
    {
        MR_Word tmp;

        /*
         * Arguments for CR_INTERPRET_CALL* macros cannot access the cstack
         * directly.
         */
        tmp = CR_EVENTS_UNION(CR_reg_val(CR_opand(1)),CR_EVENTS_REDO);
        CR_INTERPRET_CALL(interpret_1_call,tmp,CR_DUMMY_SYMBOL());
        CR_continue(1);
    }
CR_code(interpret_0):
    CR_INTERPRET_CALL(interpret_0_call,CR_EVENTS_EMPTY,CR_DUMMY_SYMBOL());
    CR_continue(0);

    {
    MR_Integer proc_addr;

    /*
     * WARNING: low-level C code madness!
     *
     * - We cannot break this code up into smaller functions, since one of the
     *   aims here is to use constant C stack space.  I.e. NEVER invoke the
     *   top-level CR_execute (even indirectly) anywhere in here!  It has been
     *   established several times that Cd quickly runs out of C stack if the
     *   interpret calls itself.
     * - Since we still want to recursively invoke the interpreter, we use the
     *   special CR_INTERPRET_CALL macro, which behaves like a recursive call,
     *   HOWEVER, it does not understand C variables which may be clobbered.
     * - To have a variable survive a call to CR_INTERPRET_CALL, you must use
     *   space on the cstack.  The CR_VAR_* variables are really cstack
     *   variables.  This means the cstack will grow, but that's OK, because
     *   the cstack can be dynamically reallocated.
     */
interpret_0_entry:

    CR_VAR_MODEL = CR_top_stk_val();
    CR_pop_stk(1);

    CR_GET_ANNOTS(CR_VAR_MODEL,CR_VAR_ANNOTS);
    if(CR_VAR_ANNOTS != CR_empty_annots_model) {
        CR_push_stk(CR_VAR_ANNOTS);
        CR_INTERPRET_CALL(interpret_0_annot_normalise,CR_EVENTS_EMPTY,
            CR_DUMMY_SYMBOL());
        CR_VAR_ANNOTS = CR_make_annotations(CR_top_stk_val());
        CR_pop_stk(1);
    }

    /*
     * Next we interpret the term itself.
     */
    switch(CR_tag(CR_VAR_MODEL)) {
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(CR_VAR_MODEL,CR_VAR_SYM);
            if(CR_IS_AC_SYM(CR_VAR_SYM)) {
                if(!CR_IS_D_SYM(CR_VAR_SYM)) {
                    MR_Word arg, tmp;
                    CR_push_stk(CR_make_atom(CR_VAR_SYM));

                    CR_GET_AC_FUNCTOR_IDX(CR_VAR_MODEL,tmp);
                    CR_VAR_I = (MR_Word)GC_MALLOC(sizeof(CR_iterator_s));
                    CR_ITR_INIT((CR_iterator)CR_VAR_I);
                    CR_ITR_PUSH((CR_iterator)CR_VAR_I,0,tmp);

                    while(CR_iterator_get_least((CR_iterator)CR_VAR_I,&arg)) {
                        CR_push_stk(arg);
                        CR_INTERPRET_CALL(interpret_0_ac_arg_normalise,
                            CR_EVENTS_EMPTY,CR_VAR_SYM);
                        annot_reg = CR_empty_annots_model;
                        CR_stk_val(1) = CR_ac_merge(CR_stk_val(0),
                            CR_VAR_SYM,CR_stk_val(1));
                        CR_pop_stk(1);
                    }
                    CR_SET_ANNOTS(CR_VAR_ANNOTS,CR_top_stk_val(),
                        CR_top_stk_val());
                    annot_reg = CR_VAR_ANNOTS;
                    proc_addr = CR_SYM_PROC(CR_VAR_SYM);
                    if(proc_addr != (MR_Word)0)
                        CR_INTERPRET_PROC_CALL(proc_addr,
                            interpret_0_ac_functor_call);
                } else {

 /***************************************************************************/
 /* ACD MODELS                                                              */
 /***************************************************************************/

    CR_VAR_I             = ch_reg;
    CR_VAR_LCL_EXTRA_REG = extra_reg;

    while(1) {
        MR_Word tmp;
        CR_VAR_NEXT_EVENTS = CR_EVENTS_EMPTY;
        CR_GET_AC_FUNCTOR_IDX(CR_VAR_MODEL,CR_VAR_IDX);
        CR_MAKE_AC_FUNCTOR(CR_VAR_SYM,0,CR_EMPTY_IDX,tmp);
        CR_push_stk(tmp);

        ch_reg     = 0;
        CR_VAR_J   = wakeup_reg;
        wakeup_reg = CR_EVENTS_EMPTY;

        /*
         * Loop that does a single conjunction 'pass', i.e. normalises each
         * conjunct if required.
         */

        while(CR_VAR_IDX != CR_EMPTY_IDX) {
            MR_Word arg;

            CR_VAR_IDX = CR_index_delete_least(CR_VAR_IDX,&arg,(
                MR_Bool *)&tmp);

            /*
             * Order is important: for a first pass from the initial goal,
             * we have that CR_VAR_EVENTS == CR_EVENTS_EMPTY.  Furthermore,
             * calling CR_get_wakeups is invalid, since the conjuncts have
             * not been attached with their wakeup conditions yet.
             */
            if(
#ifdef CR_USE_EVENTS
              (CR_VAR_EVENTS == CR_EVENTS_EMPTY) ||
              CR_EVENTS_INTERSECTION(CR_VAR_EVENTS,CR_get_wakeups(arg))
#else
              1
#endif
               ) {

                extra_reg             = CR_EVENTS_EMPTY;
                CR_VAR_LCL_CH_REG     = ch_reg;
                ch_reg                = 0;
                CR_VAR_LCL_WAKEUP_REG = wakeup_reg;
                wakeup_reg            = CR_EVENTS_EMPTY;

                /*
                 * Set-up the CC stack.  This involves pushing the accumulated
                 * normalised conjunction and the remaining CR_VAR_IDX onto the
                 * CC stack.
                 */
                tmp = CR_top_stk_val();
                if(CR_IS_FUNCTOR_SYM(tmp,CR_VAR_SYM))
                    CR_GET_AC_FUNCTOR_IDX(tmp,tmp);
                else
                    tmp = CR_index_singleton(tmp);
                CR_push_ccstk(tmp);
                CR_push_ccstk(CR_VAR_IDX);

                /*
                 * Now we can normalise the conjunct `arg'.
                 */
                CR_push_stk(arg);
                CR_VAR_OLD_ARG = arg;
                CR_INTERPRET_CALL(interpret_0_cc_arg_normalise,CR_EVENTS_EMPTY,
                    CR_VAR_SYM);

                CR_pop_ccstk(2);

                /*
                 * If the conjunct did not change, use the old value.  The new
                 * value it replaces is a copy of the old value, however, the
                 * order of AC arguments may have changed.  Using the old value
                 * ensures a fairer comparison when CR_USE_EVENTS is disabled.
                 */
                if(!ch_reg)
                    CR_top_stk_val() = CR_VAR_OLD_ARG;

                /*
                 * Global wakeup_reg contains all wakeup conditions generated
                 * during the normalisation of `arg'.  Attach these to the
                 * normalised arg.
                 */
                CR_top_stk_val() =
                    CR_set_wakeups(wakeup_reg,CR_top_stk_val(),CR_VAR_SYM);
                wakeup_reg = CR_EVENTS_UNION(wakeup_reg,CR_VAR_LCL_WAKEUP_REG);

                if(ch_reg) {
                    CR_VAR_I = 1;
                    CR_VAR_NEXT_EVENTS = CR_EVENTS_UNION(extra_reg,
                        CR_EVENTS_UNION(CR_VAR_NEXT_EVENTS,
                        CR_create_events(CR_top_stk_val(),CR_VAR_SYM)));
                    CR_VAR_LCL_EXTRA_REG = CR_EVENTS_UNION(extra_reg,
                        CR_VAR_LCL_EXTRA_REG);
                } else
                    ch_reg = CR_VAR_LCL_CH_REG;
            } else {
                /*
                 * The conjunct does not need to be woken-up.  So we simply
                 * push it onto the stack instead of (re)normalising it.
                 */
                CR_push_stk(arg);
            }
            annot_reg = CR_empty_annots_model;
            CR_stk_val(1) = CR_ac_merge(CR_stk_val(0),CR_VAR_SYM,
                CR_stk_val(1));
            CR_pop_stk(1);
        }

        wakeup_reg = CR_EVENTS_UNION(wakeup_reg,CR_VAR_J);

        /*
         * Implementation of top_level.  We are at the top-level if ccptr==0.
         * Conjuncts added to the top-level are accumulated into
         * CR_ccstk_val(0).  They are merged with the rest of the conjunction
         * here.  Appropriate create events must be generated.
         */
        if(ccptr == 0) {
            if(CR_ccstk_val(0) != CR_EMPTY_IDX) {
                MR_Word tmp_conj;

                CR_MAKE_AC_FUNCTOR(CR_VAR_SYM,
                    CR_index_size(CR_ccstk_val(0)),CR_ccstk_val(0),tmp_conj);
                annot_reg = CR_empty_annots_model;
                CR_stk_val(0) =
                    CR_ac_merge(tmp_conj,CR_VAR_SYM,CR_stk_val(0));

                CR_pop_ccstk(1);
                CR_push_ccstk(CR_EMPTY_IDX);
            }
        }

        CR_VAR_MODEL = CR_top_stk_val();

        if(!ch_reg)
            break;

        CR_GET_FUNCTOR_SYM(CR_VAR_MODEL,tmp);
        if(tmp != CR_VAR_SYM)
            break;

        CR_pop_stk(1);
        CR_VAR_EVENTS = CR_EVENTS_UNION(CR_VAR_NEXT_EVENTS,CR_EVENTS_REDO);
    }

    extra_reg = CR_VAR_LCL_EXTRA_REG;
    ch_reg    = CR_VAR_I;

    /*
     * A fixed-point has been reached.  Call the procedure for conjunction.
     */
    CR_SET_ANNOTS(CR_VAR_ANNOTS,CR_top_stk_val(),CR_top_stk_val());
    annot_reg = CR_VAR_ANNOTS;
    proc_addr = CR_SYM_PROC(CR_VAR_SYM);
    if(proc_addr != (MR_Word)0)
        CR_INTERPRET_PROC_CALL(proc_addr,interpret_0_conj_functor_call);

/*****************************************************************************/

                }
            } else {
                if(CR_IS_D_SYM(CR_VAR_SYM)) {
                    if(CR_SYM_ATY(CR_VAR_SYM) == 2) {
                        MR_Word arg;

                        CR_GET_FUNCTOR_ARG(1,CR_VAR_MODEL,CR_VAR_ARG);
                        CR_GET_SYMBOL(CR_VAR_ARG,CR_VAR_SYMS);

                        /*
                         * If the argument is a list, normalise each list
                         * element E_i with E_j (j < i) in its CC.  Normalised
                         * list elements accumulate on the stack.
                         */
                        for(CR_VAR_DEPTH = 0; CR_VAR_SYMS == CR_CONS_SYMBOL();
                                CR_VAR_DEPTH++) {
                            CR_GET_FUNCTOR_ARG(1,CR_VAR_ARG,CR_VAR_HEAD);
                            CR_push_stk(CR_VAR_HEAD);
                            CR_INTERPRET_CALL(
                                interpret_0_cc_list_head_1_normalise,
                                CR_EVENTS_EMPTY,CR_DUMMY_SYMBOL());
                            if(!CR_IS_FUNCTOR(CR_top_stk_val()))
                                CR_push_ccstk(CR_index_singleton(
                                    CR_top_stk_val()));
                            else {
                                CR_GET_FUNCTOR_SYM(CR_top_stk_val(),
                                    CR_VAR_SYMS);
                                if(CR_IS_ACD_SYM(CR_VAR_SYMS))
                                    CR_push_ccstk(CR_ac_idx(CR_top_stk_val()));
                                else
                                    CR_push_ccstk(CR_index_singleton(
                                        CR_top_stk_val()));
                            }
                            CR_GET_FUNCTOR_ARG(2,CR_VAR_ARG,CR_VAR_ARG);
                            CR_GET_SYMBOL(CR_VAR_ARG,CR_VAR_SYMS);
                        }

                        /*
                         * This is either the tail of the list, or the non-list
                         * argument.  Normalise it here.
                         */
                        CR_push_stk(CR_VAR_ARG);
                        CR_VAR_I = CR_VAR_DEPTH;
                        if(CR_VAR_SYMS != CR_NIL_SYMBOL()) {
                            CR_INTERPRET_CALL(interpret_0_cc_arg_1_normalise,
                                CR_EVENTS_EMPTY,CR_DUMMY_SYMBOL());
                            if(!CR_IS_FUNCTOR(CR_top_stk_val()))
                                CR_push_ccstk(CR_index_singleton(
                                    CR_top_stk_val()));
                            else {
                                CR_GET_FUNCTOR_SYM(CR_top_stk_val(),
                                    CR_VAR_SYMS);
                                if(CR_IS_ACD_SYM(CR_VAR_SYMS))
                                    CR_push_ccstk(CR_ac_idx(CR_top_stk_val()));
                                else
                                    CR_push_ccstk(CR_index_singleton(
                                        CR_top_stk_val()));
                            }
                            CR_VAR_DEPTH++;
                        }

                        /*
                         * Re-accumulate the elements of the list on the stack
                         * into a list.
                         */
                        for(; CR_VAR_I != 0; CR_VAR_I--) {
                            CR_MAKE_FUNCTOR_STACK(CR_CONS_SYMBOL(),
                                (((MR_Word *)stk)+ptr),arg);
                            CR_pop_stk(1);
                            CR_top_stk_val() = arg;
                        }

                        /*
                         * Normalise the second argument.
                         */
                        CR_GET_FUNCTOR_ARG(2,CR_VAR_MODEL,arg);
                        CR_push_stk(arg);
                        CR_INTERPRET_CALL(interpret_0_cc_arg_2_normalise,
                            CR_EVENTS_EMPTY,CR_DUMMY_SYMBOL());
                        CR_pop_ccstk(CR_VAR_DEPTH);

                        annot_reg = CR_VAR_ANNOTS;
                        proc_addr = CR_SYM_PROC(CR_VAR_SYM);
                        if(proc_addr != (MR_Word)0)
                            CR_INTERPRET_PROC_CALL(proc_addr,
                                interpret_0_cc_functor_call);
                        else {
                            CR_MAKE_FUNCTOR_STACK(CR_VAR_SYM,
                                (((MR_Word *)stk)+ptr),CR_VAR_MODEL);
                            CR_pop_stk(2);
                            CR_push_stk(CR_VAR_MODEL);
                        }
                    } else {
                        MR_Word sym, err;
                        sym = CR_symbol((MR_String)""runtime_error"",1);
                        CR_MAKE_STRING_0(""D-only symbol must have arity 2""
                            ,err);
                        CR_MAKE_FUNCTOR_1(sym,err,err);
                        CR_throw_exception(err);
                    }
                } else {
                    CR_VAR_I = CR_SYM_ATY(CR_VAR_SYM);
                    for(CR_VAR_I; CR_VAR_I > (MR_Word)0; CR_VAR_I--) {
                        MR_Word arg;
                        CR_GET_FUNCTOR_ARG(CR_SYM_ATY(CR_VAR_SYM)-CR_VAR_I+1,
                            CR_VAR_MODEL,arg);
                        CR_push_stk(arg);
                        CR_INTERPRET_CALL(interpret_0_arg_normalise,
                            CR_EVENTS_EMPTY,CR_DUMMY_SYMBOL());
                        /* Normalised `arg' stays on the stack. */
                    }
                    annot_reg = CR_VAR_ANNOTS;
                    if(CR_IS_CALL_SYM(CR_VAR_SYM)) {
                        CR_VAR_I = CR_SYM_ATY(CR_VAR_SYM)-1;
                        if((MR_Integer)CR_VAR_I >= 0) {
                            proc_addr = CR_ho_call_setup(CR_VAR_I);
                            CR_INTERPRET_PROC_CALL(proc_addr,
                                interpret_0_ho_functor_call);
                        } else {
                            MR_Word sym, err;
                            sym = CR_symbol((MR_String)""runtime_error"",1);
                            CR_MAKE_STRING_0(""call/N where N<0"",err);
                            CR_MAKE_FUNCTOR_1(sym,err,err);
                            CR_throw_exception(err);
                        }
                    } else {
                        proc_addr = CR_SYM_PROC(CR_VAR_SYM);
                        if(proc_addr != (MR_Word)0)
                            CR_INTERPRET_PROC_CALL(proc_addr,
                                interpret_0_functor_call);
                        else {
                            CR_MAKE_FUNCTOR_STACK(CR_VAR_SYM,
                                (((MR_Word *)stk)+ptr),CR_VAR_MODEL);
                            CR_pop_stk(CR_SYM_ATY(CR_VAR_SYM));
                            CR_push_stk(CR_VAR_MODEL);
                        }
                    }
                }
            }
            break;
        case CR_VAR_TAG:
            {
                MR_Integer var_val;
                CR_GET_VAR_VAL(CR_VAR_MODEL,var_val);
                annot_reg = CR_VAR_ANNOTS;
                CR_MAKE_VAR(var_val,CR_VAR_MODEL);
                CR_push_stk(CR_VAR_MODEL);
                proc_addr = CR_SYM_PROC(CR_VAR_SYMBOL());
                if(proc_addr != (MR_Word)0)
                    CR_INTERPRET_PROC_CALL(proc_addr,interpret_0_var_call);
            }
            break;
        case CR_INT_TAG: case CR_UBI_TAG:
            {
                MR_Integer int_val;
                CR_GET_INT_VAL(CR_VAR_MODEL,int_val);
                annot_reg = CR_VAR_ANNOTS;
                CR_MAKE_INT(int_val,CR_VAR_MODEL);
                CR_push_stk(CR_VAR_MODEL);
            }
            break;
        case CR_FLT_TAG:
            {
                MR_Float flt_val;
                CR_GET_FLOAT_VAL(CR_VAR_MODEL,flt_val);
                annot_reg = CR_VAR_ANNOTS;
                CR_MAKE_FLOAT(flt_val,CR_VAR_MODEL);
                CR_push_stk(CR_VAR_MODEL);
            }
            break;
        case CR_STR_TAG:
            {
                MR_String str_val;
                MR_Integer str_len;
                CR_GET_STRING_VAL(CR_VAR_MODEL,str_val);
                CR_GET_STRING_LENGTH(CR_VAR_MODEL,str_len);
                annot_reg = CR_VAR_ANNOTS;
                CR_MAKE_STRING(str_val,str_len,CR_VAR_MODEL);
                CR_push_stk(CR_VAR_MODEL);
            }
            break;
        case CR_FGN_TAG:
            {
                MR_Word val;
                CR_GET_FOREIGN_VAL(CR_VAR_MODEL,val);
                annot_reg = CR_VAR_ANNOTS;
                CR_MAKE_FOREIGN(val,CR_VAR_MODEL);
                CR_push_stk(CR_VAR_MODEL);
            }
            break;
    }

    CR_INTERPRET_RETURN();
    }
CR_end_code_block()

CR_engine_exit:
    /* Jump here after Cadmium has finished. */

    return (MR_Word)0;
}
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C","
MR_Word CR_get_wakeups(MR_Word model);
MR_Word CR_set_wakeups(MR_Word wakeups,MR_Word model,MR_Word acd_sym);
MR_Word CR_set_wakeups_2(MR_Word wakeups,MR_Word model);
MR_Word CR_create_events(MR_Word model,MR_Word acd_sym);
").

:- pragma foreign_code("C","
MR_Word CR_get_wakeups(MR_Word model)
{
#ifdef CR_USE_EVENTS
    MR_Word wakeups;
    CR_GET_EXTENSION(model,wakeups);
    return wakeups;
#else
    return CR_EVENTS_EMPTY;
#endif /* CR_USE_EVENTS */
}

/*
 * Attach a set of wakeup conditions to a term.  If the term has the same
 * symbol as acd_sym, then attach to each ACD argument of the term.
 */
MR_Word CR_set_wakeups(MR_Word wakeups,MR_Word model,MR_Word acd_sym)
{
#ifdef CR_USE_EVENTS
    MR_Word idx;
    MR_Integer aty;
    MR_Word annots;

    if(CR_IS_FUNCTOR_SYM(model,acd_sym)) {
        CR_GET_AC_FUNCTOR_ATY(model,aty);
        CR_GET_AC_FUNCTOR_IDX(model,idx);
        CR_GET_AC_FUNCTOR_ANNOTS(model,annots);
        idx = CR_index_set_wakeups(idx,wakeups);
        CR_MAKE_AC_FUNCTOR(acd_sym,aty,idx,model);
    } else
        model = CR_set_wakeups_2(wakeups,model);
    return model;
#else
    return model;
#endif /* CR_USE_EVENTS */
}

/*
 * Similar to CR_set_wakeups/3 but the given term is known not to be an ACD
 * term.
 */
MR_Word CR_set_wakeups_2(MR_Word wakeups,MR_Word model)
{
#ifdef CR_USE_EVENTS
    MR_Word sym;
    MR_Word copy;
    MR_Word annots;
    MR_Integer int_val;
    MR_Float float_val;
    MR_String string_val;
    MR_Integer string_len;
    MR_Integer var_val;
    MR_Integer aty;
    MR_Word idx;
    MR_Word arg;
    MR_Integer i;

    switch(CR_tag(model)) {
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(model,sym);
            if(CR_IS_AC_SYM(sym)) {
                CR_GET_AC_FUNCTOR_ATY(model,aty);
                CR_GET_AC_FUNCTOR_IDX(model,idx);
                CR_GET_AC_FUNCTOR_ANNOTS(model,annots);
                CR_MAKE_EXTENDED_AC_FUNCTOR(sym,aty,idx,copy,annots,1);
                CR_SET_AC_FUNCTOR_EXTENSION(wakeups,copy);
            } else {
                aty = CR_SYM_ATY(sym);
                CR_GET_FUNCTOR_ANNOTS(model,annots);
                CR_MAKE_EXTENDED_FUNCTOR(sym,copy,annots,1);
                for(i = 1; i <= aty; i++) {
                    CR_GET_FUNCTOR_ARG(i,model,arg);
                    CR_SET_FUNCTOR_ARG(i,copy,arg);
                }
                CR_SET_FUNCTOR_EXTENSION(wakeups,copy);
            }
            break;
        case CR_INT_TAG: case CR_UBI_TAG:
            CR_GET_INT_VAL(model,int_val);
            CR_GET_INT_ANNOTS(model,annots);
            CR_MAKE_EXTENDED_INT(int_val,copy,annots,1);
            CR_SET_INT_EXTENSION(wakeups,copy);
            break;
        case CR_VAR_TAG:
            CR_GET_VAR_VAL(model,var_val);
            CR_GET_VAR_ANNOTS(model,annots);
            CR_MAKE_EXTENDED_VAR(var_val,copy,annots,1);
            CR_SET_VAR_EXTENSION(wakeups,copy);
            break;
        case CR_FLT_TAG:
            CR_GET_FLOAT_VAL(model,float_val);
            CR_GET_FLOAT_ANNOTS(model,annots);
            CR_MAKE_EXTENDED_FLOAT(float_val,copy,annots,1);
            CR_SET_FLOAT_EXTENSION(wakeups,copy);
            break;
        case CR_STR_TAG:
            CR_GET_STRING_VAL(model,string_val);
            CR_GET_STRING_LENGTH(model,string_len);
            CR_GET_STRING_ANNOTS(model,annots);
            CR_MAKE_EXTENDED_STRING(string_val,string_len,copy,annots,1);
            CR_SET_STRING_EXTENSION(wakeups,copy);
            break;
        case CR_FGN_TAG:
            CR_GET_FOREIGN_VAL(model,arg);
            CR_GET_FOREIGN_ANNOTS(model,annots);
            CR_MAKE_EXTENDED_FOREIGN(arg,copy,annots,1);
            CR_SET_FOREIGN_EXTENSION(wakeups,copy);
            break;
        default:
            MR_assert(0);
    }

    return copy;
#else
    return model;
#endif /* CR_USE_EVENTS */
}

/*
 * Generate a set of events based on the given term.  If the term is an ACD
 * term then generate an event for each argument.
 */
MR_Word CR_create_events(MR_Word model,MR_Word acd_sym)
{
#ifdef CR_USE_EVENTS
    MR_Word sym, idx, event;

    CR_GET_SYMBOL(model,sym);

    if(sym == acd_sym) {
        CR_GET_AC_FUNCTOR_IDX(model,idx);
        return CR_index_create_events(idx);
    } else {
        return CR_SYM_EVENT(sym);
    }
#else
    return CR_EVENTS_EMPTY;
#endif /* CR_USE_EVENTS */
}

").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C","
void CR_grow_stack(MR_Word **ptr_to_stack,MR_Integer *ptr_to_size);
").

:- pragma foreign_code("C","
void CR_grow_stack(MR_Word **ptr_to_stack,MR_Integer *ptr_to_size)
{
    MR_Integer new_size = 2*(*ptr_to_size), i;
    MR_Word *new_stack;

    new_stack = (MR_Word *)GC_MALLOC_UNCOLLECTABLE(new_size*sizeof(MR_Word));

    if(new_stack == NULL) {
        MR_Word err, sym;
        sym = CR_symbol((MR_String)""runtime_error"",1);
        CR_MAKE_STRING_0(""out of memory"",err);
        CR_MAKE_FUNCTOR_1(sym,err,err);
        CR_throw_exception(err);
    }

    for(i = 0; i < *ptr_to_size; i++)
        new_stack[i] = (*ptr_to_stack)[i];
    GC_FREE(*ptr_to_stack);
    *ptr_to_stack = new_stack;
    *ptr_to_size  = new_size;
    return;
}
").

%---------------------------------------------------------------------------%

:- func init_stack_size = int.

:- pragma foreign_decl("C","
#define CR_INIT_STACK_SIZE  0x00008000
").

:- pragma foreign_proc("C",
    init_stack_size = (Size::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_modify_trail],
"
    Size = (MR_Word) CR_INIT_STACK_SIZE;
").

%---------------------------------------------------------------------------%

:- impure func init_stack = stack.
:- pragma foreign_proc("C",
    init_stack = (Stack::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Stack = (MR_Word) MR_GC_malloc(CR_INIT_STACK_SIZE * sizeof(MR_Word));
").

%---------------------------------------------------------------------------%

:- semipure func stack_val(int) = model is det.
:- pragma foreign_proc("C",
    stack_val(Idx::in) = (Val::out),
    [will_not_call_mercury, thread_safe, promise_semipure,
        will_not_modify_trail],
"
    Val = CR_stk_val(Idx);
").

%---------------------------------------------------------------------------%

:- impure pred ccstack_push(ac_index::in) is det.
:- pragma foreign_proc("C",
    ccstack_push(Idx::in),
    [will_not_call_mercury,thread_safe,will_not_modify_trail],
"
    CR_push_ccstk(Idx);
").

:- impure pred ccstack_pop(int::in) is det.
:- pragma foreign_proc("C",
    ccstack_pop(N::in),
    [will_not_call_mercury,thread_safe,will_not_modify_trail],
"
    CR_pop_ccstk(N);
").

%:- semipure func ccstack_val(int) = ac_index is det.
%:- pragma foreign_proc("C",ccstack_val(Idx::in) = (Val::out),
%        [will_not_call_mercury,thread_safe,promise_semipure,
%        will_not_modify_trail],"
%    Val = CR_ccstk_val(ccptr-(MR_Integer)Idx);
%").

%---------------------------------------------------------------------------%

:- func null_memory = c_pointer.
:- pragma foreign_proc("C",
    null_memory = (Null::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Null = (MR_Word) NULL;
").

:- func init_memory(int) = c_pointer.
:- pragma foreign_proc("C",
    init_memory(Size::in) = (Mem::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    MR_incr_hp(Mem, Size);
").

:- semipure pred get_memory(int::in, c_pointer::in, T::out) is det.
:- pragma foreign_proc("C",
    get_memory(I::in, Mem::in, Val::out),
    [will_not_call_mercury,promise_semipure,thread_safe,will_not_modify_trail],
"
    Val = ((MR_Word *) Mem)[I];
").

:- impure pred set_memory(int::in, c_pointer::in, T::in) is det.
:- pragma foreign_proc("C",set_memory(I::in, Mem::in, Val::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    ((MR_Word *) Mem)[I] = Val;
").

:- func offset_memory(int,c_pointer) = c_pointer.
:- pragma foreign_proc("C",
    offset_memory(N::in, Mem0::in) = (Mem1::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Mem1 = (MR_Word) (((MR_Word *) Mem0) + N);
").

:- impure pred zero_memory(int::in,c_pointer::in) is det.
:- pragma foreign_proc("C",
    zero_memory(Size::in, Mem::in),
    [will_not_call_mercury,thread_safe, will_not_modify_trail],
"
    MR_memset((void *) Mem, 0, Size * sizeof(MR_Word));
").

%---------------------------------------------------------------------------%

:- func null_stack = c_pointer.

null_stack = null_memory.

%---------------------------------------------------------------------------%

disassemble(LLProg) :-
    foldl(reset_proc_address,LLProg,unit,_),
    semipure get_event_symbols(ESyms),
    impure set_event_symbols([]),
    foldl(reset_event_symbol,ESyms,unit,_),
    impure ccstack_pop(1),
    impure set_prog(null_memory),
    impure machine.reset.

disassemble(Prog, !IO) :-
    promise_pure (
        impure disassemble(Prog),
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

:- pred reset_event_symbol(symbol::in,unit::in,unit::out) is det.

reset_event_symbol(ESym,!Unit) :-
    promise_pure impure symbol_set_event(ESym,0).

%---------------------------------------------------------------------------%

assemble(Debug, LLProg, !IO) :-
    promise_pure (
            % Set the debug mode.  Affects how control-C is handled.
            %
        set_debug(Debug,!IO),

            % First we determine how big the program is, so we know how much
            % memory to allocate.  We also determine the `LabelInfo' -- i.e.
            % the final address associated to each label.
            %
        ll_prog_fold2(ll_instr_preprocess,LLProg,init,LabelInfo,0,Len),

            % Create the Prog.
            %
        Prog = init_memory(Len),
        ll_prog_fold2(ll_instr_assemble(LabelInfo),LLProg,Prog,_,0,_),

            % Assign procedure addresses to each executable symbol.
            %
        foldl(set_proc_address(LabelInfo,Prog),LLProg,unit,_),

            % Assign procedure addresses to each built-in symbol.
            % NOTE: Order is important.
            %
        AddSym = hl_symbol(builtin_plus_name,2),
        SubSym = hl_symbol(builtin_minus_name,2),
        MulSym = hl_symbol(builtin_multiply_name,2),
        DivSym = hl_symbol(builtin_divide_name,2),
        ModSym = hl_symbol(builtin_mod_name,2),
        NegSym = hl_symbol(builtin_minus_name,1),
        AndSym = hl_symbol(builtin_and_name,2),
        OrSym  = hl_symbol(builtin_or_name,2),
        IffSym = hl_symbol(builtin_iff_name,2),
        XorSym = hl_symbol(builtin_xor_name,2),
        ImpSym = hl_symbol(builtin_impl_name,2),
        NotSym = hl_symbol(builtin_not_name,1),
        LtSym  = hl_symbol(builtin_lt_name,2),
        LeqSym = hl_symbol(builtin_leq_name,2),
        GtSym  = hl_symbol(builtin_gt_name,2),
        GeqSym = hl_symbol(builtin_geq_name,2),
        EqSym  = hl_symbol(builtin_eq_name,2),
        NeqSym = hl_symbol(builtin_neq_name,2),
        impure set_builtin_proc_address(LabelInfo,Prog,0,AddSym),
        impure set_builtin_proc_address(LabelInfo,Prog,1,SubSym),
        impure set_builtin_proc_address(LabelInfo,Prog,2,MulSym),
        impure set_builtin_proc_address(LabelInfo,Prog,3,DivSym),
        impure set_builtin_proc_address(LabelInfo,Prog,4,ModSym),
        impure set_builtin_proc_address(LabelInfo,Prog,5,NegSym),
        impure set_builtin_proc_address(LabelInfo,Prog,6,AndSym),
        impure set_builtin_proc_address(LabelInfo,Prog,7,OrSym),
        impure set_builtin_proc_address(LabelInfo,Prog,8,IffSym),
        impure set_builtin_proc_address(LabelInfo,Prog,9,XorSym),
        impure set_builtin_proc_address(LabelInfo,Prog,10,ImpSym),
        impure set_builtin_proc_address(LabelInfo,Prog,11,NotSym),
        impure set_builtin_proc_address(LabelInfo,Prog,12,LtSym),
        impure set_builtin_proc_address(LabelInfo,Prog,13,LeqSym),
        impure set_builtin_proc_address(LabelInfo,Prog,14,GtSym),
        impure set_builtin_proc_address(LabelInfo,Prog,15,GeqSym),
        impure set_builtin_proc_address(LabelInfo,Prog,16,EqSym),
        impure set_builtin_proc_address(LabelInfo,Prog,17,NeqSym),

            % Ready the Cadmium engine for the initial goal.
            %
        semipure get_engine_initialised(IsInit),
        (
            IsInit = no,
            unexpected($file, $pred, "Cadmium engine not initialised")
        ;
            IsInit = yes,
            impure set_prog(Prog),
            impure setup_singint_handler
        ),
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

:- pred reset_proc_address(hl_symbol::in,ll_proc::in,unit::in,unit::out)
    is det.

reset_proc_address(HLSym,_,_,unit) :-
    Sym = hl_symbol_to_symbol(HLSym),
    promise_pure impure symbol_set_proc_addr(Sym,null_memory).

%---------------------------------------------------------------------------%

:- pred set_proc_address(label_info::in,prog::in,hl_symbol::in,ll_proc::in,
    unit::in,unit::out) is det.

set_proc_address(LabelInfo,Prog,HLSym,Proc,_,unit) :-
    Proc = ll_proc(Label,_,_,_),
    lookup(LabelInfo,Label,Pos),
    Addr = offset_memory(Pos,Prog),
    Sym = hl_symbol_to_symbol(HLSym),
    promise_pure impure symbol_set_proc_addr(Sym,Addr).

%---------------------------------------------------------------------------%

:- impure pred set_builtin_proc_address(label_info::in,prog::in,label::in,
    hl_symbol::in) is det.

set_builtin_proc_address(LabelInfo,Prog,Label,HLSym) :-
    lookup(LabelInfo,Label,Pos),
    Addr = offset_memory(Pos,Prog),
    Sym = hl_symbol_to_symbol(HLSym),
    impure symbol_set_proc_addr(Sym,Addr).

%---------------------------------------------------------------------------%

:- pred ll_instr_assemble(label_info::in,ll_instr::in,prog::in,prog::out,
    address::in,address::out) is det.

:- pragma promise_pure(ll_instr_assemble/6).
ll_instr_assemble(LabelInfo,Instr,P,P,PC,NPC) :-
    ll_instr_to_ll_instr_0(Instr,Instr0),
    ( Instr0 \= label_1 ->
        impure ll_instr_0_to_exec_ll_instr_0(Instr0,Instr1),
        impure set_memory(PC,P,Instr1)
    ;   true
    ),
    ( Instr = is_int(Reg,Int),
        impure set_memory(PC+1,P,Reg),
        impure set_memory(PC+2,P,Int)
    ; Instr = is_float(Reg,Flt),
        impure set_memory(PC+1,P,Reg),
        impure set_memory(PC+2,P,Flt)
    ; Instr = is_string(Reg,Str),
        impure set_memory(PC+1,P,Reg),
        impure set_memory(PC+2,P,Str)
    ; Instr = is_named_var(Reg,NameStr),
        impure MVar = named_mvar(NameStr),
        impure set_memory(PC+1,P,Reg),
        impure set_memory(PC+2,P,MVar)
    ; Instr = is_functor(Reg,Sym),
        impure set_memory(PC+1,P,Reg),
        ESym = hl_symbol_to_symbol(Sym),
        impure set_memory(PC+2,P,ESym)
    ; Instr = is_ac_functor(Reg,Sym),
        impure set_memory(PC+1,P,Reg),
        impure set_memory(PC+2,P,hl_symbol_to_symbol(Sym)),
        impure set_memory(PC+3,P,hl_symbol_arity(Sym))
    ; Instr = is_ac_functor(Sym),
        impure set_memory(PC+1,P,hl_symbol_to_symbol(Sym)),
        impure set_memory(PC+2,P,hl_symbol_arity(Sym))
    ; Instr = is_geq_ac_functor(Reg,Sym),
        impure set_memory(PC+1,P,Reg),
        impure set_memory(PC+2,P,hl_symbol_to_symbol(Sym)),
        impure set_memory(PC+3,P,hl_symbol_arity(Sym))
    ; Instr = is_geq_ac_functor(Sym),
        impure set_memory(PC+1,P,hl_symbol_to_symbol(Sym)),
        impure set_memory(PC+2,P,hl_symbol_arity(Sym))
    ; Instr = is_type(Reg,HLType),
        impure set_memory(PC+1,P,Reg),
        Type = hl_model_type_to_model_type(HLType),
        impure set_memory(PC+2,P,Type)
    ; Instr = get_arg(SrcReg,Idx,DstReg),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,Idx),
        impure set_memory(PC+3,P,DstReg)
    ; Instr = get_arg(Idx,Reg),
        impure set_memory(PC+1,P,Idx-1),
        impure set_memory(PC+2,P,Reg)
    ; Instr = pre_guard(Reg),
        impure set_memory(PC+1,P,Reg)
    ; Instr = post_guard(Reg),
        impure set_memory(PC+1,P,Reg)
    ; Instr = post_match_guard(ResReg,ChReg),
        impure set_memory(PC+1,P,ResReg),
        impure set_memory(PC+2,P,ChReg)
    ; Instr = switch(Reg,Table,TbLen),
        impure set_memory(PC+1,P,Reg),
        impure assemble_switch_table(LabelInfo,P,TbLen,Table,ETable),
        impure set_memory(PC+2,P,ETable),
        impure set_memory(PC+3,P,TbLen)
    ; Instr = pop(N),
        impure set_memory(PC+1,P,N)
    ; Instr = construct(Sym),
        ESym = hl_symbol_to_symbol(Sym),
        impure set_memory(PC+1,P,ESym)
    ; Instr = construct_ac(Sym,Aty),
        ESym = hl_symbol_to_symbol(Sym),
        impure set_memory(PC+1,P,ESym),
        impure set_memory(PC+2,P,Aty)
    ; Instr = construct_acd(DstReg,Sym,Aty),
        impure set_memory(PC+1,P,DstReg),
        ESym = hl_symbol_to_symbol(Sym),
        impure set_memory(PC+2,P,ESym),
        impure set_memory(PC+3,P,Aty)
    ; Instr = construct_acd(DstReg,Sym,Aty,CollReg),
        impure set_memory(PC+1,P,DstReg),
        ESym = hl_symbol_to_symbol(Sym),
        impure set_memory(PC+2,P,ESym),
        impure set_memory(PC+3,P,Aty),
        impure set_memory(PC+4,P,CollReg)
    ; Instr = interpret
    ; Instr = interpret(SrcReg),
        impure set_memory(PC+1,P,SrcReg)
    ; Instr = return
    ; Instr = push_new_var
    ; Instr = push_int(Int),
        impure set_memory(PC+1,P,Int)
    ; Instr = push_float(Flt),
        impure set_memory(PC+1,P,Flt)
    ; Instr = push_string(Str),
        impure set_memory(PC+1,P,Str)
    ; Instr = push_named_var(NameStr),
        impure MVar = named_mvar(NameStr),
        impure set_memory(PC+1,P,MVar)
    ; Instr = ccpush(Reg),
        impure set_memory(PC+1,P,Reg)
    ; Instr = ccpop
    ; Instr = cpush(N),
        impure set_memory(PC+1,P,N)
    ; Instr = cset(Reg),
        impure set_memory(PC+1,P,Reg)
    ; Instr = cget(Reg),
        impure set_memory(PC+1,P,Reg)
    ; Instr = ccopy(SrcReg,DstReg),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,DstReg)
    ; Instr = ceqeq(Reg0,Reg1),
        impure set_memory(PC+1,P,Reg0),
        impure set_memory(PC+2,P,Reg1)
    ; Instr = cpop(N),
        impure set_memory(PC+1,P,N)
    ; Instr = call_ac_arg_label(Sym,Label),
        ESym = hl_symbol_to_symbol(Sym),
        lookup(LabelInfo,Label,Pos),
        Addr = offset_memory(Pos,P),
        impure set_memory(PC+1,P,ESym),
        impure set_memory(PC+2,P,Addr)
    ; Instr = call_ac_label_lco(Label),
        lookup(LabelInfo,Label,Pos),
        Addr = offset_memory(Pos,P),
        impure set_memory(PC+1,P,Addr)
    ; Instr = call_label(Label),
        lookup(LabelInfo,Label,Pos),
        Addr = offset_memory(Pos,P),
        impure set_memory(PC+1,P,Addr)
    ; Instr = call_label_lco(Label),
        lookup(LabelInfo,Label,Pos),
        Addr = offset_memory(Pos,P),
        impure set_memory(PC+1,P,Addr)
    ; Instr = call_ho(N),
        impure set_memory(PC+1,P,N)
    ; Instr = create_cp(Label),
        lookup(LabelInfo,Label,Pos),
        Addr = offset_memory(Pos,P),
        impure set_memory(PC+1,P,Addr)
    ; Instr = commit
    ; Instr = commit(N,S),
        impure set_memory(PC+1,P,N),
        impure set_memory(PC+2,P,S)
    ; Instr = call_foreign(N,Name),
        impure set_memory(PC+1,P,N),
        ( impure lookup_foreign_proc(Name,Proc) ->
            impure set_memory(PC+2,P,Proc)
        ;   Message = message(
                "foreign call symbol '%s' not found",[s(Name)],none),
            impure compiler_error(Message)
        )
    ; Instr = lookup_(SrcReg,LookupLabel,ArgReg,NodeReg,PosReg),
        impure set_memory(PC+1,P,SrcReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+2,P,LookupAddr),
        impure set_memory(PC+3,P,ArgReg),
        impure set_memory(PC+4,P,NodeReg),
        impure set_memory(PC+5,P,PosReg)
    ; Instr = lookup_(LookupLabel,ArgReg,NodeReg,PosReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,LookupAddr),
        impure set_memory(PC+2,P,ArgReg),
        impure set_memory(PC+3,P,NodeReg),
        impure set_memory(PC+4,P,PosReg)
    ; Instr = lookup_first(SrcReg,LookupLabel,ArgReg),
        impure set_memory(PC+1,P,SrcReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+2,P,LookupAddr),
        impure set_memory(PC+3,P,ArgReg)
    ; Instr = lookup_first(LookupLabel,ArgReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,LookupAddr),
        impure set_memory(PC+2,P,ArgReg)
    ; Instr = split(SrcReg,ArgReg),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,ArgReg)
    ; Instr = init_itr(NodeReg,PosReg,ItrReg),
        impure set_memory(PC+1,P,NodeReg),
        impure set_memory(PC+2,P,PosReg),
        impure set_memory(PC+3,P,ItrReg)
    ; Instr = get_next(ItrReg,Label,LookupLabel,ArgReg,IdReg),
        lookup(LabelInfo,Label,Pos),
        lookup(LabelInfo,LookupLabel,LookupPos),
        Addr = offset_memory(Pos,P),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,ItrReg),
        impure set_memory(PC+2,P,Addr),
        impure set_memory(PC+3,P,LookupAddr),
        impure set_memory(PC+4,P,ArgReg),
        impure set_memory(PC+5,P,IdReg)
    ; Instr = lookup_cc(LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,LookupAddr),
        impure set_memory(PC+2,P,ItrNoReg),
        impure set_memory(PC+3,P,ArgReg),
        impure set_memory(PC+4,P,NodeReg),
        impure set_memory(PC+5,P,PosReg)
    ; Instr = lookup_cc(CollReg,LookupLabel,ItrNoReg,ArgReg,NodeReg,PosReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,CollReg),
        impure set_memory(PC+2,P,LookupAddr),
        impure set_memory(PC+3,P,ItrNoReg),
        impure set_memory(PC+4,P,ArgReg),
        impure set_memory(PC+5,P,NodeReg),
        impure set_memory(PC+6,P,PosReg)
    ; Instr = lookup_first_cc(LookupLabel,ArgReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,LookupAddr),
        impure set_memory(PC+2,P,ArgReg)
    ; Instr = lookup_first_cc(CollReg,LookupLabel,ArgReg),
        lookup(LabelInfo,LookupLabel,LookupPos),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,CollReg),
        impure set_memory(PC+2,P,LookupAddr),
        impure set_memory(PC+3,P,ArgReg)
    ; Instr = get_next_cc(ItrNoReg,ItrReg,Label,LookupLabel,ArgReg,IdReg),
        lookup(LabelInfo,Label,Pos),
        lookup(LabelInfo,LookupLabel,LookupPos),
        Addr = offset_memory(Pos,P),
        LookupAddr = offset_memory(LookupPos,P),
        impure set_memory(PC+1,P,ItrNoReg),
        impure set_memory(PC+2,P,ItrReg),
        impure set_memory(PC+3,P,Addr),
        impure set_memory(PC+4,P,LookupAddr),
        impure set_memory(PC+5,P,ArgReg),
        impure set_memory(PC+6,P,IdReg)
    ; Instr = top_level
    ; Instr = is_diff(Reg1,Reg2),
        impure set_memory(PC+1,P,Reg1),
        impure set_memory(PC+2,P,Reg2)
    ; Instr = delete(SrcReg,ArgReg),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,ArgReg)
    ; Instr = flatten(SrcReg),
        impure set_memory(PC+1,P,SrcReg)
    ; Instr = set_annots(SrcReg),
        impure set_memory(PC+1,P,SrcReg)
    ; Instr = get_annots(DstReg),
        impure set_memory(PC+1,P,DstReg)
    ; Instr = get_annots(SrcReg,DstReg),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,DstReg)
    ; Instr = clear_annots
    ; Instr = restore_annots
    ; Instr = setup_annots
    ; Instr = call_annots
    ; Instr = attach_annots
    ; Instr = wakeup_on_redo
    ; Instr = wakeup_on(Syms),
        impure ll_instr_wakeup_on_assemble(Syms,0,Conds),
        impure set_memory(PC+1,P,Conds)
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
    ; Instr = lookup_is_int(SrcReg,Int),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,Int)
    ; Instr = lookup_is_float(SrcReg,Flt),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,Flt)
    ; Instr = lookup_is_string(SrcReg,Str),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,Str)
    ; Instr = lookup_is_named_var(SrcReg,NameStr),
        impure MVar = named_mvar(NameStr),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,MVar)
    ; Instr = lookup_is_functor(SrcReg,Sym),
        ESym = hl_symbol_to_symbol(Sym),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,ESym)
    ; Instr = lookup_is_ac_functor(SrcReg,Sym),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,hl_symbol_to_symbol(Sym)),
        impure set_memory(PC+3,P,hl_symbol_arity(Sym))
    ; Instr = lookup_is_geq_ac_functor(SrcReg,Sym),
        impure set_memory(PC+1,P,SrcReg),
        impure set_memory(PC+2,P,hl_symbol_to_symbol(Sym)),
        impure set_memory(PC+3,P,hl_symbol_arity(Sym))
    ; Instr = lookup_is_type(SrcReg,HLType),
        impure set_memory(PC+1,P,SrcReg),
        Type = hl_model_type_to_model_type(HLType),
        impure set_memory(PC+2,P,Type)
    ; Instr = lookup_compare(Reg0,Reg1),
        impure set_memory(PC+1,P,Reg0),
        impure set_memory(PC+2,P,Reg1)
    ; Instr = lookup_accept
    ; Instr = debug(DI0),
        debug_info_to_exec_debug_info(DI0,DI1),
        impure set_memory(PC+1,P,DI1)
    ; Instr = halt
    ; Instr = label(_)
    ; ( Instr = call_sym(_)
      ; Instr = call_sym_lco(_)
      ; Instr = call_ac_arg_sym(_,_)
      ; Instr = call_ac_sym_lco(_) ),
        unexpected($file, $pred, "call symbol instruction")
    ),
    ( Instr0 = label_1 ->
        NPC = PC
    ;   ll_instr_0_length(Instr0,Len),
        NPC = PC + Len + 1
    ).

%---------------------------------------------------------------------------%

:- mutable(event_symbols,list(symbol),[],ground,[untrailed]).

:- impure pred ll_instr_wakeup_on_assemble(list(hl_symbol)::in,events::in,
    events::out) is det.

ll_instr_wakeup_on_assemble([],!Events).
ll_instr_wakeup_on_assemble([Sym|Syms],!Events) :-
    ESym = hl_symbol_to_symbol(Sym),
    Event0 = symbol_event(ESym),
    ( Event0 = 0 ->
        impure next_event(Event1),
        impure symbol_set_event(ESym,Event1),
        semipure get_event_symbols(ESyms0),
        impure set_event_symbols([ESym|ESyms0])
    ;   Event1 = Event0
    ),
    !:Events = Event1 \/ !.Events,
    impure ll_instr_wakeup_on_assemble(Syms,!Events).

%---------------------------------------------------------------------------%

:- impure pred next_event(int::out) is det.
:- pragma foreign_proc("C",
    next_event(Event::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"
    static MR_Word next_event = CR_EVENT_MIN;
    next_event = CR_EVENT_NEXT(next_event);
    Event = next_event;
").

%---------------------------------------------------------------------------%

:- impure pred lookup_foreign_proc(string::in, c_pointer::out) is semidet.
:- pragma foreign_proc("C",
    lookup_foreign_proc(Name::in, Proc::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],"
/* 
    Proc = (MR_Word)dlsym(NULL,(const char *)Name);
*/
    Proc = (MR_Word) CR_link_table_lookup(Name);

    if (Proc == (MR_Word) NULL) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

%---------------------------------------------------------------------------%

:- func hl_symbol_to_symbol(hl_symbol) = symbol.

hl_symbol_to_symbol(HLSym) = RTSym :-
    Name = hl_symbol_name(HLSym),
    Aty = hl_symbol_arity(HLSym),
    RTSym = symbol(Name,Aty).

%---------------------------------------------------------------------------%

:- func hl_model_type_to_model_type(hl_model_type) = model_type.

hl_model_type_to_model_type(int)     = int_type.
hl_model_type_to_model_type(float)   = float_type.
hl_model_type_to_model_type(string)  = string_type.
hl_model_type_to_model_type(var)     = var_type.
hl_model_type_to_model_type(functor) = functor_type.

%---------------------------------------------------------------------------%

:- pred debug_info_to_exec_debug_info(debug_info(hl_symbol,register)::in,
    debug_info::out) is det.

debug_info_to_exec_debug_info(debug_info(Sym0,PP0,Cxt,VRI),
        debug_info(Sym1,PP1,Cxt,VRI)) :-
    Sym1 = hl_symbol_to_symbol(Sym0),
    ( PP0 = call(HLSym),
        RTSym = hl_symbol_to_symbol(HLSym),
        PP1 = call(RTSym)
    ; PP0 = pass(RId),
        PP1 = pass(RId)
    ; PP0 = fail(RId),
        PP1 = fail(RId)
    ; PP0 = return(HLSym,B),
        RTSym = hl_symbol_to_symbol(HLSym),
        PP1 = return(RTSym,B)
    ).

%---------------------------------------------------------------------------%

:- pred ll_instr_preprocess(ll_instr::in,label_info::in,label_info::out,
    int::in,int::out) is det.

ll_instr_preprocess(LLInstr,!LabelInfo,!Len) :-
    ( LLInstr = label(Label) ->
        map.det_insert(Label,!.Len,!LabelInfo)
    ;   ll_instr_to_ll_instr_0(LLInstr,LLInstr0),
        ll_instr_0_length(LLInstr0,InstrLen),
        !:Len = 1 + InstrLen + !.Len
    ).

%---------------------------------------------------------------------------%

    % Creates a new io state.
    %
:- impure func new_io = (io::uo) is det.
:- pragma foreign_proc("C",
    new_io = (_IO::uo),
    [will_not_call_mercury,thread_safe],
"").

%---------------------------------------------------------------------------%

:- impure pred debug_instruction(int::in,ll_instr_0::in) is det.

:- pragma foreign_export("C",debug_instruction(in,in),"CR_debug_instruction").

debug_instruction(PC,Instr) :-
    impure IO0 = new_io,
    semipure debug_instruction_2(PC,Instr,IO0,_).

%---------------------------------------------------------------------------%

:- semipure pred debug_instruction_2(int::in,ll_instr_0::in,io::di,io::uo)
    is det.

debug_instruction_2(PC,Instr,!IO) :-
    write(Instr,!IO),
    write_string(", stk = ",!IO),
    semipure get_stk_ptr(Ptr),
    write_int(Ptr,!IO),
    write_string(", cstk = ",!IO),
    semipure get_cstk_ptr(CPtr),
    write_int(CPtr,!IO),
    write_string(", dstk = ",!IO),
    semipure get_dstk_ptr(DPtr),
    write_int(DPtr,!IO),
    write_string(", pc = ",!IO),
    write_int(PC,!IO),
    nl(!IO).

%---------------------------------------------------------------------------%

dummy_label = (-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Switch handling.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

    % A switch table is a block of 2*N words, where N is the number of cases
    % in the switch.  The first N words contains the symbols, then the
    % final N words contains the addresses.
    %
:- impure pred assemble_switch_table(label_info::in,prog::in,int::in,
    ll_switch_table::in,switch_table::out) is det.

assemble_switch_table(LabelInfo,P,N,LLTable,Table) :-
    Table = init_memory(2*N),
    LLTable = ll_switch_table(Syms,Labels),
    ll_switch_table_to_sym_addr_list(LabelInfo,P,Syms,Labels,[],Cases),
    sort(Cases,SCases),
    impure switch_table_init(SCases,0,N,Table).

%---------------------------------------------------------------------------%

:- pred ll_switch_table_to_sym_addr_list(label_info::in,prog::in,
    list(hl_symbol)::in,list(label)::in,list({symbol,c_pointer})::in,
    list({symbol,c_pointer})::out) is det.

ll_switch_table_to_sym_addr_list(_,_,[],_,!Cases).
ll_switch_table_to_sym_addr_list(LabelInfo,P,[HLSym|HLSyms],Labels,!Cases) :-
    RTSym = hl_symbol_to_symbol(HLSym),
    lookup(LabelInfo,det_head(Labels),Pos),
    Addr = offset_memory(Pos,P),
    Case = {RTSym,Addr},
    !:Cases = [Case|!.Cases],
    ll_switch_table_to_sym_addr_list(LabelInfo,P,HLSyms,det_tail(Labels),
        !Cases).

%---------------------------------------------------------------------------%

:- impure pred switch_table_init(list({symbol,c_pointer})::in,int::in,int::in,
    switch_table::in) is det.

switch_table_init([],_,_,_).
switch_table_init([{Sym,Addr}|Cases],I,J,Table) :-
    impure set_memory(I,Table,Sym),
    impure set_memory(J,Table,Addr),
    impure switch_table_init(Cases,I+1,J+1,Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SIGINT Handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pragma foreign_decl("C","

#include <signal.h>

void handle_sig(int sig);

extern MR_Code *old_handler;

").

:- pragma foreign_code("C","

MR_Code *old_handler = NULL;

void handle_sig(int sig)
{
    if (sig == SIGINT) {
        handle_sigint();
        signal(SIGINT,handle_sig);
    }
}

").

%---------------------------------------------------------------------------%

:- impure pred setup_singint_handler is det.
:- pragma foreign_proc("C",
    setup_singint_handler,
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    old_handler = signal(SIGINT, handle_sig);
").

%---------------------------------------------------------------------------%

:- impure pred restore_sigint_handler is det.
:- pragma foreign_proc("C",
    restore_sigint_handler,
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    signal(SIGINT, old_handler);
").

%---------------------------------------------------------------------------%

:- pred handle_sigint(io::di,io::uo) is det.
:- pragma foreign_export("C",handle_sigint(di,uo),"handle_sigint").

handle_sigint(!IO) :-
    get_debug(Debug,!IO),
    ( Debug = yes,
        set_debugger_step_state(!IO)
    ; Debug = no,
        Interrupt = construct("interrupt", []),
        machine.throw(Interrupt)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debugger Support
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    lookup_register_value(Reg::in,Model::out),
    [will_not_call_mercury, thread_safe, promise_semipure, will_not_modify_trail],
"
    Model = CR_reg_val(Reg);
").

%---------------------------------------------------------------------------%

lookup_stack_value(N,Val) :-
    semipure Val = stack_val(N).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    lookup_stack_model(Sym::in,Model::out),
    [will_not_call_mercury, thread_safe, promise_semipure, will_not_modify_trail],
"
    CR_MAKE_FUNCTOR_STACK(Sym, (((MR_Word *) stk) + ptr), Model);
").

%---------------------------------------------------------------------------%

lookup_cc(CC) :-
    semipure get_ccstack(CCStk),
    semipure get_ccstk_ptr(CCPtr),
    semipure accumulate_cc(CCPtr, CCStk, [], CC).

%---------------------------------------------------------------------------%

:- semipure pred accumulate_cc(int::in,stack::in,list(model)::in,
    list(model)::out) is det.

accumulate_cc(CCPtr, CCStk, !CC) :-
    ( if CCPtr = -1 then
        true
    else
        semipure get_memory(CCPtr, CCStk, Idx),
        ac_index_to_list(Idx, CC0),
        append(CC0, !CC),
        semipure accumulate_cc(CCPtr - 1, CCStk, !CC)
    ).

%-----------------------------------------------------------------------------%
:- end_module machine.
%-----------------------------------------------------------------------------%
