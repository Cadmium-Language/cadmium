%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Author: Gregory J. Duck.
%
% Defines the "model" data structure that is manipulated by the Cadmium
% execution engine.
%
% Note: The Cd engine and internals are now almost exclusively C.  The Hg
% interface to these internals is mainly just legacy stuff I think, although
% some of it is still used in the Zn/Cd interface.  Almost all of these 
% functions do no sanity checking whatsoever, so use at own risk.
%
%---------------------------------------------------------------------------%

:- module model.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module term.
:- import_module varset.

:- import_module ac_index.

%---------------------------------------------------------------------------%

:- type symbol.
:- type mvar == int.

:- type address == int. 

:- type model.
:- type model_type == int.

    % Model type constants.
    %
:- func int_type     = model_type.
:- func float_type   = model_type.
:- func string_type  = model_type.
:- func var_type     = model_type.
:- func functor_type = model_type.

    % Initialise the symbol table (should only be used by machine.m)
    %
:- impure pred startup is det.

    % Resets the mutables (e.g. the symbol table, etc) in this module.
    %
:- impure pred reset_model is det.
:- pred reset_model(io::di, io::uo) is det.

    % Free up the symbol table (should only be used by machine.m)
    %
:- impure pred shutdown is det.

    % Returns a list of all symbols matching the given prefix.  Useful for
    % TAB completion.
    %
:- func match_symbol_names(string) = list(string).

    % High-level construct function.
    %
:- func construct(string,list(model)) = model.

    % High-level deconstruct predicate.  Fails if given model is not a functor.
    %
:- pred deconstruct(model::in,string::out,list(model)::out) is semidet.

    % Compare two models.
    %
:- pred compare_models(model::in,model::in,comparison_result::out) is det.

    % Return a new (unique) mvar.
    %
:- impure func new_mvar = mvar.

    % Create a new mvar with a given name, or return the existing one if 
    % it is already created.
    %
:- impure func named_mvar(string) = mvar.

    % Return the name of an mvar.
    %
:- semipure func mvar_name(mvar) = string.

    % Return the "id" of an mvar.
    %
:- func mvar_id(mvar) = int.

    % Return the symbol associated with the given function/arity pair.
    %
:- func symbol(string,int) = symbol.

    % Return the symbol associated with the given character.
    %
:- func ascii_symbol(char) = symbol.

    % Assign an procedure address to a symbol.
    %
:- impure pred symbol_set_proc_addr(symbol::in,c_pointer::in) is det.

    % Cast a symbol to an integer.
    %
:- func symbol_to_int(symbol) = int.

    % Return the name of a symbol.
    %
:- func symbol_name(symbol) = string.

    % Return the arity of a symbol.
    % Note: For AC operators arity/1 always returns 0 (use ac_arity instead).
    %
:- func symbol_arity(symbol) = int.

    % Return the event associated to a symbol.  Event 0 represents no event.
    %
:- func symbol_event(symbol) = int.

    % Set the event of a symbol.
    %
:- impure pred symbol_set_event(symbol::in,int::in) is det.

    % Return the arity of an AC model.
    %
:- func ac_arity(model) = int.

    % Sets the arity of a symbol.
    % Note: This is a NOP for AC operators.
    %
:- func set_arity(symbol,int) = symbol.

    % Returns the n-th argument of a model.
    %
:- func arg(int,model) = model.

    % Returns the AC arguments of an AC model (as an ac_index).
    %
:- func ac_args(model) = ac_index.

    % Decides if two models are equal.
    %
:- pred eqeq(model::in,model::in) is semidet.

    % Gets the "symbol" of a model.  Works for ints, floats, strings and
    % vars (see int_symbol, etc.)
    %
:- func get_symbol(model) = symbol.

    % Gets the "arity" of a model.
    %
:- func get_arity(model) = int.

    % Decides if the given model is a particular type and returns its 
    % corresponding value (or variable ID or symbol).
    %
:- pred is_functor(model::in,symbol::out) is semidet.
:- pred is_int(model::in,int::out) is semidet.
:- pred is_float(model::in,float::out) is semidet.
:- pred is_string(model::in,string::out) is semidet.
:- pred is_foreign(model::in,T::out) is semidet.
:- pred is_var(model::in,mvar::out) is semidet.

    % Same as above but do not return anything.
    %
:- pred is_functor(model::in) is semidet.
:- pred is_int(model::in) is semidet.
:- pred is_float(model::in) is semidet.
:- pred is_string(model::in) is semidet.
:- pred is_foreign(model::in) is semidet.
:- pred is_var(model::in) is semidet.

    % Construct a model out from a value.
    %
:- func int(int) = model.
:- func float(float) = model.
:- func string(string) = model.
:- func foreign(T) = model.
:- func var(mvar) = model.

    % Build a functor model.  Assumes the arity of the symbol is correct.
    % NOTE: Do not use for AC operators!
    % Note: apply/2 is a built-in that cannot be overloaded, hence the 
    % extraneous `_'.
    %
:- func apply_(symbol,list(model)) = model.

    % Like apply_/2 but for AC operators.  Assumes the second argument is
    % the number of elements in the ac_index.
    %
:- func apply_ac(symbol,int,ac_index) = model.

    % Build a functor model from a c_pointer (which points to the arguments).
    % NOTE: Do not use for AC operators!
    %
:- func apply_c_pointer(symbol,c_pointer) = model.

    % Special apply function for building list cons.
    %
:- func apply_cons(model,model) = model.

    % Special apply function for building list nil.
    %
:- func apply_nil = model.

    % Print out the model.
    % Does not print annotations.
    %
:- pred write_model(model::in,io::di,io::uo) is det.

    % Print out the model.
    % Does print annotations.
    %
:- pred write_model_with_annotations(model::in,io::di,io::uo) is det.

    % Convert a model into a string.
    %
:- func model_to_string(model) = string.

    % Convert a term into a model.
    %
:- func term_to_model(varset,term) = model.

    % Tests if the given symbol is an AC operator.
    %
:- pred is_ac(symbol::in) is semidet.

    % Tests of the given symbol is a call/N symbol.
    %
:- pred is_call(symbol::in) is semidet.

    % Tests if the given symbol is a procedure.
    %
:- pred is_proc(symbol::in,address::out) is semidet.

    % Pre-defined symbols.  (In no particular order).
    %
:- func cons_symbol      = symbol.
:- func nil_symbol       = symbol.
:- func conj_symbol      = symbol.
:- func var_symbol       = symbol.
:- func true_symbol      = symbol.
:- func false_symbol     = symbol.
:- func call_symbol      = symbol.
:- func int_symbol       = symbol.
:- func float_symbol     = symbol.
:- func string_symbol    = symbol.
:- func annotate_symbol  = symbol.
:- func cc_symbol        = symbol.
:- func dummy_symbol     = symbol.     % To be used as a placeholder only.
:- func foreign_symbol   = symbol.
:- func undefined_symbol = symbol.
:- func bplus_symbol     = symbol.
:- func bminus_symbol    = symbol.
:- func buminus_symbol   = symbol.
:- func bmultiply_symbol = symbol.
:- func bdivide_symbol   = symbol.
:- func bmod_symbol      = symbol.
:- func band_symbol      = symbol.
:- func bor_symbol       = symbol.
:- func biff_symbol      = symbol.
:- func bxor_symbol      = symbol.
:- func bimpl_symbol     = symbol.
:- func bnot_symbol      = symbol.
:- func blt_symbol       = symbol.
:- func bleq_symbol      = symbol.
:- func bgt_symbol       = symbol.
:- func bgeq_symbol      = symbol.
:- func beq_symbol       = symbol.
:- func bneq_symbol      = symbol.
:- func eq_symbol        = symbol.
:- func lt_symbol        = symbol.
:- func gt_symbol        = symbol.

    % Atom `true'
    %
:- func true_model = model.

    % Atom `false'
    %
:- func false_model = model.

    % Atom `undefined'
    %
:- func undefined_model = model.

    % Atom `::'
    %
:- func empty_annotations_model = model.

    % An empty set of annotations.
    % Note: Empty annotations have a special representation that is not 
    % compatible with other `model' operations.
    %
:- func empty_annotations = model.

    % Set the annotation register.
    % This is really part of the abstract machine.  It is included here 
    % because it is (implicitly) used when models are constructed.
    %
:- impure pred set_annotation_reg(model::in) is det.

    % Get the annotation register's contents.
    %
:- semipure pred get_annotation_reg(model::out) is det.

    % Sets the annotations attached to a model.  
    %
:- func annotate(model,model) = model.

    % Gets the annotations attached to a model.
    %
:- pred get_annotations(model::in,model::out) is det.

    % Set the annotations attached to a model.
    %
:- pred set_annotations(model::in,model::in,model::out) is det.

    % Given two models Model1, Model2, computes (Model1 `Op` Model2).
    %
:- pred ac_merge(symbol::in,model::in,model::in,model::out) is det.

    % Apply ac_merge/4 to a list of models.
    %
:- pred ac_merge_list(symbol::in,list(model)::in,model::out) is det.

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.

:- import_module cadmium_common.

%---------------------------------------------------------------------------%

:- type symbol == int.
:- type vector == c_pointer.
:- type model == vector.

:- pragma foreign_decl("C","
/* 
 * Check that the Mercury runtime is suitably configured. 
 */
#ifdef MR_HIGHTAGS
#error ""Cadmium cannot be compiled with MR_HIGHTAGS defined""
#endif
#if MR_TAGBITS < 2
#error ""Cadmium requires at least 2 tag bits, MR_TAGBITS too small""
#endif
#if MR_FIRST_UNRESERVED_RAW_TAG != 0
#error ""Cadmium requires MR_FIRST_UNRESERVED_RAW_TAG to be 0""
#endif

/* 
 * The tags for each of the different term types.  
 */
#define CR_TAGBITS     MR_TAGBITS

/*
 * Tag ordering is important (see the model comparison function).
 */
#define CR_FCT_TAG     MR_mktag(0)
#define CR_INT_TAG     MR_mktag(1)
#define CR_VAR_TAG     MR_mktag(2)

#if CR_TAGBITS < 3

#define CR_OTH_TAG     MR_mktag(3)
#define CR_FLT_TAG     4
#define CR_STR_TAG     5
#define CR_FGN_TAG     6
#define CR_UBI_TAG     7        /* Not used */

#define CR_tag(model)                                                         \\
    ((MR_tag(model) == CR_OTH_TAG)?CR_GET_TAG_FIELD(model):MR_tag(model))
#define CR_GET_TAG_FIELD(model)                                               \\
    ((MR_Integer)MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0))

#else

#define CR_FLT_TAG     MR_mktag(3)
#define CR_STR_TAG     MR_mktag(4)
#define CR_FGN_TAG     MR_mktag(5)
#define CR_UBI_TAG     MR_mktag(6)

#define CR_tag(model)  MR_tag(model)

#define CR_USE_UNBOXED_INT
#define CR_CAN_UNBOX_INT(i)                                                   \\
    (((MR_Integer)(i) >= (MR_Integer)0) &&                                    \\
     (((MR_Integer)(i) <= (~(MR_Unsigned)0x0) >> CR_TAGBITS)) &&              \\
     (annot_reg == CR_empty_annots_model))

#endif  /* CR_TAGBITS < 3 */

/*
 * The `CR_symbol_info' structure contains everything associated to a `symbol'.
 * A `symbol' is a tagged pointer to a `CR_symbol_info'.
 */
typedef struct __CR_symbol_info {
    /*
     * The name of the symbol.
     */
    MR_String name;
    
    /*
     * The arity of the symbol.
     * For AC symbols, `arity' must be 0.
     */
    MR_Integer arity;

    /*
     * If the symbol is also a procedure, then the start address of the 
     * procedure is stored here.
     * Otherwise, `proc_addr' must be 0.
     */
    MR_Word proc_addr;

    /*
     * If the program can generate a create(f/a) event for this symbol, then
     * store the event here.
     * Otherwise, `event' must be 0.
     */
    MR_Word event;

} CR_symbol_info;

#define CR_SYMBOL_INFOS_INIT_SIZE       1024

CR_symbol_info *CR_symbol_infos;
MR_Integer CR_symbol_infos_size;
MR_Integer CR_symbol_infos_next;

void CR_init_builtin_symbols(void);

#define CR_CONS_SYMBOL_INFO_IDX         0
#define CR_NIL_SYMBOL_INFO_IDX          1
#define CR_VAR_SYMBOL_INFO_IDX          2
#define CR_TRUE_SYMBOL_INFO_IDX         3
#define CR_FALSE_SYMBOL_INFO_IDX        4
#define CR_CALL_SYMBOL_INFO_IDX         5
#define CR_INT_SYMBOL_INFO_IDX          6
#define CR_FLOAT_SYMBOL_INFO_IDX        7
#define CR_STRING_SYMBOL_INFO_IDX       8
#define CR_ANNOTATE_SYMBOL_INFO_IDX     9
#define CR_CC_SYMBOL_INFO_IDX           10
#define CR_DUMMY_SYMBOL_INFO_IDX        11
#define CR_CONJ_SYMBOL_INFO_IDX         12
#define CR_FOREIGN_SYMBOL_INFO_IDX      13
#define CR_UNDEFINED_SYMBOL_INFO_IDX    14
#define CR_BPLUS_SYMBOL_INFO_IDX        15
#define CR_BMINUS_SYMBOL_INFO_IDX       16
#define CR_BUMINUS_SYMBOL_INFO_IDX      17
#define CR_BMULTIPLY_SYMBOL_INFO_IDX    18
#define CR_BDIVIDE_SYMBOL_INFO_IDX      19
#define CR_BMOD_SYMBOL_INFO_IDX         20
#define CR_BAND_SYMBOL_INFO_IDX         21
#define CR_BOR_SYMBOL_INFO_IDX          22
#define CR_BIFF_SYMBOL_INFO_IDX         23
#define CR_BXOR_SYMBOL_INFO_IDX         24
#define CR_BIMPL_SYMBOL_INFO_IDX        25
#define CR_BNOT_SYMBOL_INFO_IDX         26
#define CR_BLT_SYMBOL_INFO_IDX          27
#define CR_BLEQ_SYMBOL_INFO_IDX         28
#define CR_BGT_SYMBOL_INFO_IDX          29
#define CR_BGEQ_SYMBOL_INFO_IDX         30
#define CR_BEQ_SYMBOL_INFO_IDX          31
#define CR_BNEQ_SYMBOL_INFO_IDX         32
#define CR_EQ_SYMBOL_INFO_IDX           33
#define CR_LT_SYMBOL_INFO_IDX           34
#define CR_GT_SYMBOL_INFO_IDX           35
#define CR_SYMBOL_INFOS_INIT_0_SIZE     36

#define CR_AC_BIT                       0x00000001
#define CR_D_BIT                        0x00000002
#define CR_CALL_BIT                     0x00000004

#define CR_NONAC_TYPE                   0x00000000
#define CR_AC_TYPE                      CR_AC_BIT
#define CR_ACD_TYPE                     (CR_AC_BIT | CR_D_BIT)
#define CR_D_TYPE                       CR_D_BIT
#define CR_CALL_TYPE                    CR_CALL_BIT

#define CR_SYMBOL_TYPE_MASK             0x00000007
#define CR_SYMBOL_ARITY_MASK            0x00007FF8
#define CR_SYMBOL_INDEX_MASK            0xFFFF8000
#define CR_SYMBOL_TYPE_SHIFT            0
#define CR_SYMBOL_ARITY_SHIFT           3
#define CR_SYMBOL_INDEX_SHIFT           15

#define CR_SYMBOL(index,arity,type)                                 \\
    (((index)<<CR_SYMBOL_INDEX_SHIFT) |                             \\
     ((arity)<<CR_SYMBOL_ARITY_SHIFT) |                             \\
     ((type) <<CR_SYMBOL_TYPE_SHIFT))

#define CR_SYMBOL_INFO(name,arity,proc_addr)   \\
    {(MR_String)(name),(MR_Integer)(arity),(MR_Word)(proc_addr),(MR_Word)0}

#define CR_CONS_SYMBOL()                                            \\
    CR_SYMBOL(CR_CONS_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_NIL_SYMBOL()                                             \\
    CR_SYMBOL(CR_NIL_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_VAR_SYMBOL()                                             \\
    CR_SYMBOL(CR_VAR_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_TRUE_SYMBOL()                                            \\
    CR_SYMBOL(CR_TRUE_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_FALSE_SYMBOL()                                           \\
    CR_SYMBOL(CR_FALSE_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_CALL_SYMBOL()                                            \\
    CR_SYMBOL(CR_CALL_SYMBOL_INFO_IDX,0,CR_CALL_TYPE)
#define CR_INT_SYMBOL()                                             \\
    CR_SYMBOL(CR_INT_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_FLOAT_SYMBOL()                                           \\
    CR_SYMBOL(CR_FLOAT_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_STRING_SYMBOL()                                          \\
    CR_SYMBOL(CR_STRING_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_ANNOTATE_SYMBOL()                                        \\
    CR_SYMBOL(CR_ANNOTATE_SYMBOL_INFO_IDX,0,CR_AC_TYPE)
#define CR_CC_SYMBOL()                                              \\
    CR_SYMBOL(CR_CC_SYMBOL_INFO_IDX,2,CR_D_TYPE)
#define CR_DUMMY_SYMBOL()                                           \\
    CR_SYMBOL(CR_DUMMY_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_CONJ_SYMBOL()                                            \\
    CR_SYMBOL(CR_CONJ_SYMBOL_INFO_IDX,0,CR_ACD_TYPE)
#define CR_FOREIGN_SYMBOL()                                         \\
    CR_SYMBOL(CR_FOREIGN_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_UNDEFINED_SYMBOL()                                       \\
    CR_SYMBOL(CR_UNDEFINED_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_BPLUS_SYMBOL()                                           \\
    CR_SYMBOL(CR_BPLUS_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BMINUS_SYMBOL()                                          \\
    CR_SYMBOL(CR_BMINUS_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BUMINUS_SYMBOL()                                         \\
    CR_SYMBOL(CR_BUMINUS_SYMBOL_INFO_IDX,1,CR_NONAC_TYPE)
#define CR_BMULTIPLY_SYMBOL()                                       \\
    CR_SYMBOL(CR_BMULTIPLY_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BDIVIDE_SYMBOL()                                         \\
    CR_SYMBOL(CR_BDIVIDE_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BMOD_SYMBOL()                                            \\
    CR_SYMBOL(CR_BMOD_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BAND_SYMBOL()                                            \\
    CR_SYMBOL(CR_BAND_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BOR_SYMBOL()                                             \\
    CR_SYMBOL(CR_BOR_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BIFF_SYMBOL()                                            \\
    CR_SYMBOL(CR_BIFF_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BXOR_SYMBOL()                                            \\
    CR_SYMBOL(CR_BXOR_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BIMPL_SYMBOL()                                           \\
    CR_SYMBOL(CR_BIMPL_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BNOT_SYMBOL()                                            \\
    CR_SYMBOL(CR_BNOT_SYMBOL_INFO_IDX,1,CR_NONAC_TYPE)
#define CR_BLT_SYMBOL()                                             \\
    CR_SYMBOL(CR_BLT_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BLEQ_SYMBOL()                                            \\
    CR_SYMBOL(CR_BLEQ_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BGT_SYMBOL()                                             \\
    CR_SYMBOL(CR_BGT_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BGEQ_SYMBOL()                                            \\
    CR_SYMBOL(CR_BGEQ_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BEQ_SYMBOL()                                             \\
    CR_SYMBOL(CR_BEQ_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_BNEQ_SYMBOL()                                            \\
    CR_SYMBOL(CR_BNEQ_SYMBOL_INFO_IDX,2,CR_NONAC_TYPE)
#define CR_EQ_SYMBOL()                                              \\
    CR_SYMBOL(CR_EQ_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_LT_SYMBOL()                                              \\
    CR_SYMBOL(CR_LT_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)
#define CR_GT_SYMBOL()                                              \\
    CR_SYMBOL(CR_GT_SYMBOL_INFO_IDX,0,CR_NONAC_TYPE)

#define CR_SYM_INDEX(symbol)                                                  \\
    (((symbol)&CR_SYMBOL_INDEX_MASK)>>CR_SYMBOL_INDEX_SHIFT)
#define CR_SYM_INFO(symbol)                                                   \\
    (CR_symbol_infos[CR_SYM_INDEX(symbol)])
#define CR_SYM_NAME(symbol)                                                   \\
    (CR_SYM_INFO(symbol).name)
#define CR_SYM_ATY(symbol)                                                    \\
    (((symbol)&CR_SYMBOL_ARITY_MASK)>>CR_SYMBOL_ARITY_SHIFT)
#define CR_SYM_PROC(symbol)                                                   \\
    (CR_SYM_INFO(symbol).proc_addr)
#define CR_SYM_EVENT(symbol)                                                  \\
    (CR_SYM_INFO(symbol).event)
#define CR_SYM_TYPE(symbol)                                                   \\
    (((symbol)&CR_SYMBOL_TYPE_MASK)>>CR_SYMBOL_TYPE_SHIFT)
#define CR_IS_AC_SYM(symbol)                                                  \\
    (CR_SYM_TYPE(symbol) & CR_AC_BIT)
#define CR_IS_D_SYM(symbol)                                                   \\
    (CR_SYM_TYPE(symbol) & CR_D_BIT)
#define CR_IS_ACD_SYM(symbol)                                                 \\
    (CR_SYM_TYPE(symbol) & (CR_AC_BIT | CR_D_BIT))
#define CR_IS_CALL_SYM(symbol)                                                \\
    (CR_SYM_TYPE(symbol) & CR_CALL_BIT)

#define CR_new_object(type,size,msg)                                          \\
    ((type)MR_GC_malloc(size))

/*
** XXX Previously, CR_new_object was defined as follows, but the interface
** to MR_new_object has changed in recent versions of Mercury in order
** to support memory attribution profiling.  The above definition is
** compatible with both old and new Mercury runtimes -- once everything
** is updated then the old definition (suitably modified) should be used.

#ifdef MR_HIGHLEVEL_CODE
#define CR_new_object(type,size,msg)                                          \\
    MR_new_object(type,size,msg)
#else
#define CR_new_object(type,size,msg)                                          \\
    ((type)MR_GC_malloc(size))
#endif

*/

/*
 * Integer representation:
 *      32-bit/64-bit: 
 *      -(CR_INT_TAG)-> {(MR_Integer)Int,(MR_Word)Annotations}
 *
 *      64-bit:
 *      (CR_UBI_TAG) & ((MR_Integer)Int << CR_TAGBITS)
 */

#ifdef CR_USE_UNBOXED_INT

#define CR_MAKE_INT(int_val,model)                                            \\
    do {                                                                      \\
        if(CR_CAN_UNBOX_INT(int_val))                                         \\
            CR_MAKE_UNBOXED_INT(int_val,model);                               \\
        else                                                                  \\
            CR_MAKE_EXTENDED_INT(int_val,model,annot_reg,0);                  \\
    } while(0)
#define CR_MAKE_UNBOXED_INT(int_val,model)                                    \\
    ((model) = MR_mkbody(int_val) + CR_UBI_TAG)

#else   /* CR_USE_UNBOXED_INT */

#define CR_MAKE_INT(int_val,model)                                            \\
    CR_MAKE_EXTENDED_INT(int_val,model,annot_reg,0)

#endif  /* CR_USE_UNBOXED_INT */

#define CR_MAKE_ANNOTATED_INT(int_val,model,annots)                           \\
    CR_MAKE_EXTENDED_INT(int_val,model,annots,0)
#define CR_MAKE_EXTENDED_INT(int_val,model,annots,ext)                        \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_INT_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Integer)+sizeof(MR_Word)+         \\
            (ext)*sizeof(MR_Word),""$int""));                                 \\
        MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(int_val);                                                \\
        MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)1) =              \\
            (MR_Box)(annots);                                                 \\
    } while(0)

#ifdef CR_USE_UNBOXED_INT

#define CR_IS_INT(model)                                                      \\
    ( (MR_tag(model) == CR_UBI_TAG) || (MR_tag(model) == CR_INT_TAG) )
#define CR_IS_INT_VAL(model,int_val)                                          \\
    (                                                                         \\
      ((MR_tag(model) == CR_UBI_TAG) &&                                       \\
      ((MR_Integer)MR_unmkbody(model) == (MR_Integer)(int_val))) ||           \\
      ((MR_tag(model) == CR_INT_TAG) &&                                       \\
      ((MR_Integer)MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)0) ==  \\
        (MR_Integer)(int_val)))                                               \\
    )
#define CR_GET_INT_VAL(model,int_val)                                         \\
    do {                                                                      \\
        if(MR_tag(model) == CR_UBI_TAG)                                       \\
            (int_val) = (MR_Integer)MR_unmkbody(model);                       \\
        else                                                                  \\
            (int_val) =                                                       \\
                (MR_Integer)MR_hl_field(CR_INT_TAG,(MR_Word)(model),          \\
                (MR_Integer)0);                                               \\
    } while(0)
#define CR_GET_INT_ANNOTS(model,annots)                                       \\
    do {                                                                      \\
        if(MR_tag(model) == CR_UBI_TAG)                                       \\
            (annots) = CR_empty_annots_model;                                 \\
        else                                                                  \\
            (annots) =                                                        \\
                (MR_Word)MR_hl_field(CR_INT_TAG,(MR_Word)(model),             \\
                (MR_Integer)1);                                               \\
    } while(0)

#else   /* CR_USE_UNBOXED_INT */

#define CR_IS_INT(model)                                                      \\
    ( MR_tag(model) == CR_INT_TAG )
#define CR_IS_INT_VAL(model,int_val)                                          \\
    (                                                                         \\
      (MR_tag(model) == CR_INT_TAG) &&                                        \\
      ((MR_Integer)MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)0) ==  \\
        (MR_Integer)(int_val))                                                \\
    )
#define CR_GET_INT_VAL(model,int_val)                                         \\
    do {                                                                      \\
        (int_val) =                                                           \\
           (MR_Integer)MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)0);\\
    } while(0)
#define CR_GET_INT_ANNOTS(model,annots)                                       \\
    do {                                                                      \\
        (annots) =                                                            \\
            (MR_Word)MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)1);  \\
    } while(0)

#endif  /* CR_USE_UNBOXED_INT */

#define CR_GET_INT_EXTENSION(model,extension)                                 \\
    do {                                                                      \\
        (extension) =                                                         \\
            (MR_Word)MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)2);  \\
    } while(0)
#define CR_SET_INT_EXTENSION(extension,model)                                 \\
    do {                                                                      \\
        MR_hl_field(CR_INT_TAG,(MR_Word)(model),(MR_Integer)2) =              \\
            (MR_Box)(extension);                                              \\
    } while(0)

/*
 * Floating point representation: 
 *      32-bit:
 *      -(CR_OTH_TAG)-> {(MR_Integer)CR_FLT_TAG,(MR_Word)Annotations,
 *          (MR_Float)Float}
 *      
 *      64-bit:
 *      -(CR_FLT_TAG)-> {(MR_Word)Annotations,(MR_Float)Float}
 *
 * `Float' may be bigger than a single word.
 */

#if CR_TAGBITS < 3

#define CR_FLT_MERCURY_TAG              CR_OTH_TAG
#define CR_FLT_TAG_FIELD_SIZE           sizeof(MR_Integer)
#define CR_FLT_SET_TAG_FIELD(model)                                           \\
    MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0) =                  \\
        (MR_Box)CR_FLT_TAG
#define CR_FLT_IS_TAG_FIELD(model)                                            \\
    ((MR_Integer)MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0) ==    \\
        (MR_Integer)CR_FLT_TAG)
#define CR_FLT_ANNOTS_FIELD             ((MR_Integer)1)
#define CR_FLT_FLOAT_FIELD              ((MR_Integer)2)

#else

#define CR_FLT_MERCURY_TAG              CR_FLT_TAG
#define CR_FLT_TAG_FIELD_SIZE           0
#define CR_FLT_SET_TAG_FIELD(model)     /* NOP */
#define CR_FLT_IS_TAG_FIELD(model)      1
#define CR_FLT_ANNOTS_FIELD             ((MR_Integer)0)
#define CR_FLT_FLOAT_FIELD              ((MR_Integer)1)

#endif /* CR_TAGBITS < 3 */

#define CR_MAKE_FLOAT(flt_val,model)                                          \\
    CR_MAKE_EXTENDED_FLOAT(flt_val,model,annot_reg,0)
#define CR_MAKE_ANNOTATED_FLOAT(flt_val,model,annots)                         \\
    CR_MAKE_EXTENDED_FLOAT(flt_val,model,annots,0)
#define CR_MAKE_EXTENDED_FLOAT(flt_val,model,annots,ext)                      \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_FLT_MERCURY_TAG,                      \\
            CR_new_object(MR_Word,CR_FLT_TAG_FIELD_SIZE+sizeof(MR_Word)+      \\
            sizeof(MR_Float)+(ext)*sizeof(MR_Word),""$float""));              \\
        CR_FLT_SET_TAG_FIELD(model);                                          \\
        MR_hl_field(CR_FLT_MERCURY_TAG,(MR_Word)(model),CR_FLT_ANNOTS_FIELD)  \\
            = (MR_Box)(annots);                                               \\
        *(MR_Float *)(&MR_hl_field(CR_FLT_MERCURY_TAG,(MR_Word)(model),       \\
            CR_FLT_FLOAT_FIELD)) = (MR_Float)(flt_val);                       \\
    } while(0)
#define CR_IS_FLOAT(model)                                                    \\
    (                                                                         \\
      (MR_tag(model) == CR_FLT_MERCURY_TAG) && CR_FLT_IS_TAG_FIELD(model)     \\
    )
#define CR_IS_FLOAT_VAL(model,flt_val)                                        \\
    (                                                                         \\
      (MR_tag(model) == CR_FLT_MERCURY_TAG) && CR_FLT_IS_TAG_FIELD(model) &&  \\
      (*(MR_Float *)(&MR_hl_field(CR_FLT_MERCURY_TAG,(MR_Word)(model),        \\
        CR_FLT_FLOAT_FIELD)) == (MR_Float)(flt_val))                          \\
    )
#define CR_GET_FLOAT_VAL(model,flt_val)                                       \\
    do {                                                                      \\
        (flt_val) =                                                           \\
            *(MR_Float *)(&MR_hl_field(CR_FLT_MERCURY_TAG,(MR_Word)(model),   \\
            CR_FLT_FLOAT_FIELD));                                             \\
    } while(0)
#define CR_GET_FLOAT_ANNOTS(model,flt_annots)                                 \\
    do {                                                                      \\
        (flt_annots) =                                                        \\
            (MR_Word)MR_hl_field(CR_FLT_MERCURY_TAG,(MR_Word)(model),         \\
            CR_FLT_ANNOTS_FIELD);                                             \\
    } while(0)
#define CR_GET_FLOAT_EXTENSION(model,extension)                               \\
    do {                                                                      \\
        (extension) =                                                         \\
            *(MR_Word *)(((MR_Float *)(&MR_hl_field(CR_FLT_MERCURY_TAG,       \\
            (MR_Word)(model),CR_FLT_ANNOTS_FIELD)))+1);                       \\
    } while(0)
#define CR_SET_FLOAT_EXTENSION(extension,model)                               \\
    do {                                                                      \\
        *(MR_Word **)(((MR_Float *)(&MR_hl_field(CR_FLT_MERCURY_TAG,          \\
        (MR_Word)(model),CR_FLT_ANNOTS_FIELD)))+1) = (MR_Box)(extension);     \\
    } while(0)

/*
 * String representation:
 *      32-bit:
 *      -(CR_OTH_TAG)-> {(MR_Integer)CR_STR_TAG,(MR_String)String,
 *          (MR_Integer)Length,(MR_Word)Annotations}
 *
 *      64-bit:
 *      -(CR_STR_TAG)-> {(MR_String)String,(MR_Integer)Length,
 *          (MR_Word)Annotations}
 */

#if CR_TAGBITS < 3

#define CR_STR_MERCURY_TAG      CR_OTH_TAG
#define CR_STR_TAG_FIELD_SIZE   sizeof(MR_Integer)
#define CR_STR_SET_TAG_FIELD(model)                                           \\
    MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0) =                  \\
        (MR_Box)CR_STR_TAG
#define CR_STR_IS_TAG_FIELD(model)                                            \\
    ((MR_Integer)MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0) ==    \\
        (MR_Integer)CR_STR_TAG)
#define CR_STR_STRING_FIELD             ((MR_Integer)1)
#define CR_STR_LENGTH_FIELD             ((MR_Integer)2)
#define CR_STR_ANNOTS_FIELD             ((MR_Integer)3)
#define CR_STR_EXTENSION_FIELD          ((MR_Integer)4)

#else

#define CR_STR_MERCURY_TAG              CR_STR_TAG
#define CR_STR_TAG_FIELD_SIZE           0
#define CR_STR_SET_TAG_FIELD(model)     /* NOP */
#define CR_STR_IS_TAG_FIELD(model)      1
#define CR_STR_STRING_FIELD             ((MR_Integer)0)
#define CR_STR_LENGTH_FIELD             ((MR_Integer)1)
#define CR_STR_ANNOTS_FIELD             ((MR_Integer)2)
#define CR_STR_EXTENSION_FIELD          ((MR_Integer)3)

#endif /* CR_TAGBITS < 3 */

#define CR_MAKE_STRING_0(str_val,model)                                       \\
    CR_MAKE_STRING(str_val,((MR_Word)strlen((MR_String)str_val)),model)
#define CR_MAKE_STRING(str_val,str_len,model)                                 \\
    CR_MAKE_EXTENDED_STRING(str_val,str_len,model,annot_reg,0)
#define CR_MAKE_ANNOTATED_STRING(str_val,str_len,model,annots)                \\
    CR_MAKE_EXTENDED_STRING(str_val,str_len,model,annots,0)
#define CR_MAKE_EXTENDED_STRING(str_val,str_len,model,annots,ext)             \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_STR_MERCURY_TAG,                      \\
            CR_new_object(MR_Word,CR_STR_TAG_FIELD_SIZE+sizeof(MR_String)+    \\
            sizeof(MR_Integer)+sizeof(MR_Word)+(ext)*sizeof(MR_Word),         \\
            ""$string""));                                                    \\
        CR_STR_SET_TAG_FIELD(model);                                          \\
        MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),CR_STR_STRING_FIELD)  \\
            = (MR_Box)(str_val);                                              \\
        MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),CR_STR_LENGTH_FIELD)  \\
            = (MR_Box)(str_len);                                              \\
        MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),CR_STR_ANNOTS_FIELD)  \\
            = (MR_Box)(annots);                                               \\
    } while(0)
#define CR_IS_STRING(model)                                                   \\
    (                                                                         \\
      (MR_tag(model) == CR_STR_MERCURY_TAG) && CR_STR_IS_TAG_FIELD(model)     \\
    )
#define CR_IS_STRING_VAL(model,str_val)                                       \\
    (                                                                         \\
      (MR_tag(model) == CR_STR_MERCURY_TAG) && CR_STR_IS_TAG_FIELD(model) &&  \\
      (strcmp(                                                                \\
        (MR_String)MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),           \\
        CR_STR_STRING_FIELD),(MR_String)str_val ) == 0)                       \\
    )
#define CR_GET_STRING_VAL(model,str_val)                                      \\
    do {                                                                      \\
        (str_val) =                                                           \\
            (MR_String)MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),       \\
            CR_STR_STRING_FIELD);                                             \\
    } while(0)
#define CR_GET_STRING_LENGTH(model,str_len)                                   \\
    do {                                                                      \\
        (str_len) =                                                           \\
            (MR_Integer)MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),      \\
            CR_STR_LENGTH_FIELD);                                             \\
    } while(0)
#define CR_GET_STRING_ANNOTS(model,annots)                                    \\
    do {                                                                      \\
        (annots) =                                                            \\
            (MR_Word)MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),         \\
            CR_STR_ANNOTS_FIELD);                                             \\
    } while(0)
#define CR_GET_STRING_EXTENSION(model,extension)                              \\
    do {                                                                      \\
        (extension) =                                                         \\
            (MR_Word)MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),         \\
            CR_STR_EXTENSION_FIELD);                                          \\
    } while(0)
#define CR_SET_STRING_EXTENSION(extension,model)                              \\
    do {                                                                      \\
        MR_hl_field(CR_STR_MERCURY_TAG,(MR_Word)(model),                      \\
            CR_STR_EXTENSION_FIELD) = (MR_Box)(extension);                    \\
    } while(0)

/*
 * Foreign type representation:
 *      32-bit:   
 *      -(CR_OTH_TAG)-> {(MR_Integer)CR_FGN_TAG,(MR_Word)Val,
 *          (MR_Word)Annotations}
 *
 *      64-bit:
 *      -(CR_FGN_TAG)-> {(MR_Word)Val,(MR_Word)Annotations}
 */

#if CR_TAGBITS < 3

#define CR_FGN_MERCURY_TAG              CR_OTH_TAG
#define CR_FGN_TAG_FIELD_SIZE           sizeof(MR_Integer)
#define CR_FGN_SET_TAG_FIELD(model)                                           \\
    MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0) =                  \\
        (MR_Box)CR_FGN_TAG
#define CR_FGN_IS_TAG_FIELD(model)                                            \\
    ((MR_Integer)MR_hl_field(CR_OTH_TAG,(MR_Word)(model),(MR_Integer)0) ==    \\
        (MR_Integer)CR_FGN_TAG)
#define CR_FGN_FOREIGN_FIELD            ((MR_Integer)1)
#define CR_FGN_ANNOTS_FIELD             ((MR_Integer)2)
#define CR_FGN_EXTENSION_FIELD          ((MR_Integer)3)

#else

#define CR_FGN_MERCURY_TAG              CR_FGN_TAG
#define CR_FGN_TAG_FIELD_SIZE           0
#define CR_FGN_SET_TAG_FIELD(model)     /* NOP */
#define CR_FGN_IS_TAG_FIELD(model)      1
#define CR_FGN_FOREIGN_FIELD            ((MR_Integer)0)
#define CR_FGN_ANNOTS_FIELD             ((MR_Integer)1)
#define CR_FGN_EXTENSION_FIELD          ((MR_Integer)2)

#endif /* CR_TAGBITS < 3 */

#define CR_MAKE_FOREIGN(val,model)                                            \\
    CR_MAKE_EXTENDED_FOREIGN(val,model,annot_reg,0)
#define CR_MAKE_ANNOTATED_FOREIGN(val,model,annots)                           \\
    CR_MAKE_EXTENDED_FOREIGN(val,model,annots,0)
#define CR_MAKE_EXTENDED_FOREIGN(val,model,annots,ext)                        \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_FGN_MERCURY_TAG,                      \\
            CR_new_object(MR_Word,CR_FGN_TAG_FIELD_SIZE+sizeof(MR_String)+    \\
            sizeof(MR_Word)+(ext)*sizeof(MR_Word),""$foreign""));             \\
        CR_FGN_SET_TAG_FIELD(model);                                          \\
        MR_hl_field(CR_FGN_MERCURY_TAG,(MR_Word)(model),CR_FGN_FOREIGN_FIELD) \\
            = (MR_Box)(val);                                                  \\
        MR_hl_field(CR_FGN_MERCURY_TAG,(MR_Word)(model),CR_FGN_ANNOTS_FIELD)  \\
            = (MR_Box)(annots);                                               \\
    } while(0)
#define CR_IS_FOREIGN(model)                                                  \\
    (                                                                         \\
      (MR_tag(model) == CR_FGN_MERCURY_TAG) && CR_FGN_IS_TAG_FIELD(model)     \\
    )
#define CR_GET_FOREIGN_VAL(model,val)                                         \\
    do {                                                                      \\
        (val) =                                                               \\
            (MR_Word)MR_hl_field(CR_FGN_MERCURY_TAG,(MR_Word)(model),         \\
            CR_FGN_FOREIGN_FIELD);                                            \\
    } while(0)
#define CR_GET_FOREIGN_ANNOTS(model,annots)                                   \\
    do {                                                                      \\
        (annots) =                                                            \\
            (MR_Word)MR_hl_field(CR_FGN_MERCURY_TAG,(MR_Word)(model),         \\
            CR_FGN_ANNOTS_FIELD);                                             \\
    } while(0)
#define CR_GET_FOREIGN_EXTENSION(model,extension)                             \\
    do {                                                                      \\
        (extension) =                                                         \\
            (MR_Word)MR_hl_field(CR_FGN_MERCURY_TAG,(MR_Word)(model),         \\
            CR_FGN_EXTENSION_FIELD);                                          \\
    } while(0)
#define CR_SET_FOREIGN_EXTENSION(extension,model)                             \\
    do {                                                                      \\
        MR_hl_field(CR_FGN_MERCURY_TAG,(MR_Word)(model),                      \\
            CR_FGN_EXTENSION_FIELD) = (MR_Box)(extension);                    \\
    } while(0)

/*
 * Variable representation:
 *      -(CR_VAR_TAG)-> {(MR_Integer)Id,(MR_Word)Annotations}
 */
#define CR_MAKE_VAR(var_val,model)                                            \\
    CR_MAKE_EXTENDED_VAR(var_val,model,annot_reg,0)
#define CR_MAKE_ANNOTATED_VAR(var_val,model,annots)                           \\
    CR_MAKE_EXTENDED_VAR(var_val,model,annots,0)
#define CR_MAKE_EXTENDED_VAR(var_val,model,annots,ext)                        \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_VAR_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Integer)+sizeof(MR_Word)+         \\
            (ext)*sizeof(MR_Word),""$var""));                                 \\
        MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(var_val);                                                \\
        MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)1) =              \\
            (MR_Box)(annots);                                                 \\
    } while(0)
#define CR_IS_VAR(model)                                                      \\
      ( MR_tag(model) == CR_VAR_TAG )
#define CR_IS_VAR_VAL(model,var_val)                                          \\
    (                                                                         \\
      (MR_tag(model) == CR_VAR_TAG) &&                                        \\
      ((MR_Integer)MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)0) ==  \\
        (MR_Integer)var_val)                                                  \\
    )
#define CR_GET_VAR_VAL(model,var_val)                                         \\
    do {                                                                      \\
        (var_val) =                                                           \\
           (MR_Integer)MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)0);\\
    } while(0)
#define CR_GET_VAR_ANNOTS(model,annots)                                       \\
    do {                                                                      \\
        (annots) =                                                            \\
            (MR_Word)MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)1);  \\
    } while(0)
#define CR_GET_VAR_EXTENSION(model,extension)                                 \\
    do {                                                                      \\
        (extension) =                                                         \\
            (MR_Word)MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)2);  \\
    } while(0)
#define CR_SET_VAR_EXTENSION(extension,model)                                 \\
    do {                                                                      \\
        MR_hl_field(CR_VAR_TAG,(MR_Word)(model),(MR_Integer)2) =              \\
            (MR_Box)(extension);                                              \\
    } while(0)

/*
 * Functor-term representation:
 *      -(CR_FCT_TAG)-> {(MR_Word)Symbol,(MR_Word)Arg1,...,(MR_Word)ArgN,
 *                       (MR_Word)Annotations}
 *      -(CR_FCT_TAG)-> {(MR_Word)Symbol,(MR_Integer)Arity,(MR_Word)Index,
 *                       (MR_Word)Annotations}
 *
 * The first and second representations are for non-AC and AC models 
 * respectively.
 */
#define CR_MAKE_FUNCTOR_LIST(symbol,args,model)                               \\
    do {                                                                      \\
        MR_Integer tmp_i;                                                     \\
        MR_Word tmp_args;                                                     \\
        (model) = (MR_Word)MR_mkword(CR_FCT_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Word)+                            \\
            CR_SYM_ATY(symbol)*sizeof(MR_Word)+sizeof(MR_Word),""$functor""));\\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(symbol);                                                 \\
        for(tmp_i = 1, tmp_args = args; !MR_list_is_empty(tmp_args);          \\
                tmp_i++, tmp_args = MR_list_tail(tmp_args))                   \\
            MR_hl_field(CR_FCT_TAG,(MR_Word)(model),tmp_i) =                  \\
                (MR_Box)MR_list_head(tmp_args);                               \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),tmp_i) = (MR_Box)annot_reg;   \\
    } while(0)
#define CR_MAKE_FUNCTOR_STACK(symbol,args,model)                              \\
    do {                                                                      \\
        MR_Integer tmp_i, tmp_aty = CR_SYM_ATY(symbol);                       \\
        (model) = (MR_Word)MR_mkword(CR_FCT_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Word)+tmp_aty*sizeof(MR_Word)+    \\
            sizeof(MR_Word),""$functor""));                                   \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(symbol);                                                 \\
        for(tmp_i = 1; tmp_i <= tmp_aty; tmp_i++)                             \\
            MR_hl_field(CR_FCT_TAG,(MR_Word)(model),tmp_i) =                  \\
                (MR_Box)(((MR_Word *)args)[tmp_i-tmp_aty]);                   \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),tmp_aty+(MR_Integer)1) =      \\
            (MR_Box)annot_reg;                                                \\
    } while(0)
#define CR_MAKE_FUNCTOR(symbol,model)                                         \\
    CR_MAKE_EXTENDED_FUNCTOR(symbol,model,annot_reg,0)
#define CR_MAKE_ANNOTATED_FUNCTOR(symbol,model,annots)                        \\
    CR_MAKE_EXTENDED_FUNCTOR(symbol,model,annots,0)
#define CR_MAKE_EXTENDED_FUNCTOR(symbol,model,annots,ext)                     \\
    do {                                                                      \\
        MR_Integer tmp_aty = CR_SYM_ATY(symbol);                              \\
        (model) = (MR_Word)MR_mkword(CR_FCT_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Word)+tmp_aty*sizeof(MR_Word)+    \\
            sizeof(MR_Word)+(ext)*sizeof(MR_Word),""$functor""));             \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(symbol);                                                 \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),tmp_aty+(MR_Integer)1) =      \\
            (MR_Box)(annots);                                                 \\
    } while(0)
#define CR_MAKE_FUNCTOR_0(symbol,model)                                       \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_FCT_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Word)+sizeof(MR_Word),            \\
            ""$functor""));                                                   \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(symbol);                                                 \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)1) =              \\
            (MR_Box)annot_reg;                                                \\
    } while(0)
#define CR_MAKE_FUNCTOR_1(symbol,arg1,model)                                  \\
    do {                                                                      \\
        MR_Word tmp;                                                          \\
        tmp = (MR_Word)MR_mkword(CR_FCT_TAG,                                  \\
            CR_new_object(MR_Word,sizeof(MR_Word)+sizeof(MR_Word)+            \\
            sizeof(MR_Word),""$functor""));                                   \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)0) = (MR_Box)(symbol);         \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)1) = (MR_Box)(arg1);           \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)2) = (MR_Box)annot_reg;        \\
        (model) = tmp;                                                        \\
    } while(0)
#define CR_MAKE_FUNCTOR_2(symbol,arg1,arg2,model)                             \\
    do {                                                                      \\
        MR_Word tmp;                                                          \\
        tmp = (MR_Word)MR_mkword(CR_FCT_TAG,                                  \\
            CR_new_object(MR_Word,sizeof(MR_Word)+2*sizeof(MR_Word)+          \\
            sizeof(MR_Word),""$functor""));                                   \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)0) = (MR_Box)(symbol);         \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)1) = (MR_Box)(arg1);           \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)2) = (MR_Box)(arg2);           \\
        MR_hl_field(CR_FCT_TAG,tmp,(MR_Integer)3) = (MR_Box)annot_reg;        \\
        (model) = tmp;                                                        \\
    } while(0)
#define CR_MAKE_AC_FUNCTOR(symbol,arity,index,model)                          \\
    CR_MAKE_EXTENDED_AC_FUNCTOR(symbol,arity,index,model,annot_reg,0)
#define CR_MAKE_ANNOTATED_AC_FUNCTOR(symbol,arity,index,model,annots)         \\
    CR_MAKE_EXTENDED_AC_FUNCTOR(symbol,arity,index,model,annots,0)
#define CR_MAKE_EXTENDED_AC_FUNCTOR(symbol,arity,index,model,annots,ext)      \\
    do {                                                                      \\
        (model) = (MR_Word)MR_mkword(CR_FCT_TAG,                              \\
            CR_new_object(MR_Word,sizeof(MR_Word)+sizeof(MR_Integer)+         \\
                2*sizeof(MR_Word)+(ext)*sizeof(MR_Word),""$functor""));       \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0) =              \\
            (MR_Box)(symbol);                                                 \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)1) =              \\
            (MR_Box)(arity);                                                  \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)2) =              \\
            (MR_Box)(index);                                                  \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)3) =              \\
            (MR_Box)(annots);                                                 \\
    } while(0)
#define CR_IS_FUNCTOR(model)                                                  \\
    (                                                                         \\
      MR_tag(model) == CR_FCT_TAG                                             \\
    )
#define CR_IS_FUNCTOR_SYM(model,symbol)                                       \\
    (                                                                         \\
      (MR_tag(model) == CR_FCT_TAG) &&                                        \\
      ((MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0) ==     \\
        (MR_Word)(symbol))                                                    \\
    )
#define CR_GET_FUNCTOR_SYM(model,symbol)                                      \\
    do {                                                                      \\
        (symbol) =                                                            \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)0);  \\
    } while(0)
#define CR_GET_FUNCTOR_ARG(i,model,arg)                                       \\
    do {                                                                      \\
        (arg) =                                                               \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)(i));\\
    } while(0)
#define CR_SET_FUNCTOR_ARG(i,model,arg)                                       \\
    do {                                                                      \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)(i)) =            \\
            (MR_Box)(arg);                                                    \\
    } while(0)
#define CR_GET_AC_FUNCTOR_ATY(model,arity)                                    \\
    do {                                                                      \\
        (arity) =                                                             \\
            (MR_Integer)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),              \\
            (MR_Integer)1);                                                   \\
    } while(0)
#define CR_HAS_AC_FUNCTOR_ATY(model,arity)                                    \\
    ( (arity) == (MR_Integer)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),         \\
        (MR_Integer)1) )
#define CR_HAS_GEQ_AC_FUNCTOR_ATY(model,arity)                                \\
    ( (arity) <= (MR_Integer)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),         \\
        (MR_Integer)1) )
#define CR_GET_AC_FUNCTOR_IDX(model,index)                                    \\
    do {                                                                      \\
        (index) =                                                             \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)2);  \\
    } while(0)
#define CR_GET_FUNCTOR_ANNOTS(model,annots)                                   \\
    do {                                                                      \\
        (annots) =                                                            \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),                 \\
            CR_SYM_ATY((MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),      \\
            (MR_Integer)0))+(MR_Integer)1);                                   \\
    } while(0)
#define CR_GET_AC_FUNCTOR_ANNOTS(model,annots)                                \\
    do {                                                                      \\
        (annots) =                                                            \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)3);  \\
    } while(0)
#define CR_SET_AC_FUNCTOR_ANNOTS(annots,model)                                \\
    do {                                                                      \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)3) =              \\
            (MR_Box)(annots);                                                 \\
    } while(0)
#define CR_GET_FUNCTOR_EXTENSION(model,extension)                             \\
    do {                                                                      \\
        (extension) =                                                         \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),                 \\
            CR_SYM_ATY((MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),      \\
            (MR_Integer)0))+(MR_Integer)2);                                   \\
    } while(0)
#define CR_GET_AC_FUNCTOR_EXTENSION(model,extension)                          \\
    do {                                                                      \\
        (extension) =                                                         \\
            (MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)4);  \\
    } while(0)
#define CR_SET_FUNCTOR_EXTENSION(extension,model)                             \\
    do {                                                                      \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),                              \\
            CR_SYM_ATY((MR_Word)MR_hl_field(CR_FCT_TAG,(MR_Word)(model),      \\
            (MR_Integer)0))+(MR_Integer)2) =                                  \\
            (MR_Box)(extension);                                              \\
    } while(0)
#define CR_SET_AC_FUNCTOR_EXTENSION(extension,model)                          \\
    do {                                                                      \\
        MR_hl_field(CR_FCT_TAG,(MR_Word)(model),(MR_Integer)4) =              \\
            (MR_Box)(extension);                                              \\
    } while(0)

/*
 * Generic representation macros.
 */
#define CR_GET_ANNOTS(model,annots)                                           \\
    do {                                                                      \\
        MR_Word tmp_sym;                                                      \\
        switch(CR_tag(model)) {                                               \\
            case CR_FCT_TAG:                                                  \\
                CR_GET_FUNCTOR_SYM(model,tmp_sym);                            \\
                if(CR_IS_AC_SYM(tmp_sym))                                     \\
                    CR_GET_AC_FUNCTOR_ANNOTS(model,annots);                   \\
                else                                                          \\
                    CR_GET_FUNCTOR_ANNOTS(model,annots);                      \\
                break;                                                        \\
            case CR_INT_TAG: case CR_UBI_TAG:                                 \\
                CR_GET_INT_ANNOTS(model,annots);                              \\
                break;                                                        \\
            case CR_VAR_TAG:                                                  \\
                CR_GET_VAR_ANNOTS(model,annots);                              \\
                break;                                                        \\
            case CR_FLT_TAG:                                                  \\
                CR_GET_FLOAT_ANNOTS(model,annots);                            \\
                break;                                                        \\
            case CR_STR_TAG:                                                  \\
                CR_GET_STRING_ANNOTS(model,annots);                           \\
                break;                                                        \\
            case CR_FGN_TAG:                                                  \\
                CR_GET_FOREIGN_ANNOTS(model,annots);                          \\
                break;                                                        \\
        }                                                                     \\
    } while(0)
#define CR_GET_EXTENSION(model,extension)                                     \\
    do {                                                                      \\
        MR_Word tmp_sym;                                                      \\
        switch(CR_tag(model)) {                                               \\
            case CR_FCT_TAG:                                                  \\
                CR_GET_FUNCTOR_SYM(model,tmp_sym);                            \\
                if(CR_IS_AC_SYM(tmp_sym))                                     \\
                    CR_GET_AC_FUNCTOR_EXTENSION(model,extension);             \\
                else                                                          \\
                    CR_GET_FUNCTOR_EXTENSION(model,extension);                \\
                break;                                                        \\
            case CR_INT_TAG: case CR_UBI_TAG:                                 \\
                CR_GET_INT_EXTENSION(model,extension);                        \\
                break;                                                        \\
            case CR_VAR_TAG:                                                  \\
                CR_GET_VAR_EXTENSION(model,extension);                        \\
                break;                                                        \\
            case CR_FLT_TAG:                                                  \\
                CR_GET_FLOAT_EXTENSION(model,extension);                      \\
                break;                                                        \\
            case CR_STR_TAG:                                                  \\
                CR_GET_STRING_EXTENSION(model,extension);                     \\
                break;                                                        \\
            case CR_FGN_TAG:                                                  \\
                CR_GET_FOREIGN_EXTENSION(model,extension);                    \\
                break;                                                        \\
        }                                                                     \\
    } while(0)
#define CR_SET_ANNOTS(annots,model,new_model)                                 \\
    ((new_model) = CR_set_annots(annots,model))
#define CR_GET_SYMBOL(model,symbol)                                           \\
    do {                                                                      \\
        switch(CR_tag(model)) {                                               \\
            case CR_FCT_TAG:                                                  \\
                CR_GET_FUNCTOR_SYM(model,symbol);                             \\
                break;                                                        \\
            case CR_INT_TAG: case CR_UBI_TAG:                                 \\
                (symbol) = CR_INT_SYMBOL();                                   \\
                break;                                                        \\
            case CR_VAR_TAG:                                                  \\
                (symbol) = CR_VAR_SYMBOL();                                   \\
                break;                                                        \\
            case CR_FLT_TAG:                                                  \\
                (symbol) = CR_FLOAT_SYMBOL();                                 \\
                break;                                                        \\
            case CR_STR_TAG:                                                  \\
                (symbol) = CR_STRING_SYMBOL();                                \\
                break;                                                        \\
            case CR_FGN_TAG:                                                  \\
                (symbol) = CR_FOREIGN_SYMBOL();                               \\
                break;                                                        \\
        }                                                                     \\
    } while(0)
#define CR_GET_ARITY(model,arity)                                             \\
    do {                                                                      \\
        MR_Word tmp_sym;                                                      \\
        if(MR_tag(model) == CR_FCT_TAG) {                                     \\
            CR_GET_FUNCTOR_SYM(model,tmp_sym);                                \\
            if(CR_IS_AC_SYM(tmp_sym))                                         \\
                CR_GET_AC_FUNCTOR_ATY(model,arity);                           \\
            else                                                              \\
                arity = CR_SYM_ATY(tmp_sym);                                  \\
        } else                                                                \\
            arity = (MR_Integer)0;                                            \\
    } while(0)

#ifdef CR_USE_UNBOXED_INT

#define CR_GET_TYPE(model,type)                                               \\
    do {                                                                      \\
        type = CR_tag(model);                                                 \\
        if(type == CR_UBI_TAG)                                                \\
            type = CR_INT_TAG;                                                \\
    } while(0)

#else   /* CR_USE_UNBOXED_INT */

#define CR_GET_TYPE(model,type)                                               \\
    do {                                                                      \\
        type = CR_tag(model);                                                 \\
    } while(0)

#endif  /* CR_USE_UNBOXED_INT */

/*
 * Function prototypes.
 */
extern MR_Word CR_set_annots(MR_Word annots,MR_Word model);
extern MR_Word CR_ac_merge(MR_Word model1,MR_Word sym,MR_Word model2);
extern MR_Word CR_ac_merge_annotations(MR_Word model1,MR_Word model2);
extern MR_bool CR_eqeq(MR_Word model1,MR_Word model2);
extern MR_Word CR_ac_idx(MR_Word model);
extern MR_Word CR_make_atom(MR_Word sym);
extern MR_Word CR_ac_flatten(MR_Word model);
extern MR_Word CR_ac_unflatten(MR_Word symbol,MR_Word model);
extern MR_Word CR_make_annotations(MR_Word model);

").

:- pragma foreign_code("C","

CR_symbol_info *CR_symbol_infos = NULL;
MR_Integer CR_symbol_infos_size = 0;
MR_Integer CR_symbol_infos_next = 0;

#define CR_SYMBOL_INFO_INIT(index,name0,arity0)                     \\
    do {                                                            \\
        CR_symbol_infos[index].name      = (MR_String)(name0);      \\
        CR_symbol_infos[index].arity     = (MR_Integer)(arity0);    \\
        CR_symbol_infos[index].proc_addr = (MR_Word)0;              \\
        CR_symbol_infos[index].event     = (MR_Word)0;              \\
        CR_symbol_infos_next++;                                     \\
    } while(0)

void CR_init_builtin_symbols(void)
{
    CR_SYMBOL_INFO_INIT(CR_CONS_SYMBOL_INFO_IDX,""[|]"",2);
    CR_SYMBOL_INFO_INIT(CR_NIL_SYMBOL_INFO_IDX,""[]"",0);
    CR_SYMBOL_INFO_INIT(CR_VAR_SYMBOL_INFO_IDX,""$var"",0);
    CR_SYMBOL_INFO_INIT(CR_TRUE_SYMBOL_INFO_IDX,""true"",0);
    CR_SYMBOL_INFO_INIT(CR_FALSE_SYMBOL_INFO_IDX,""false"",0);
    CR_SYMBOL_INFO_INIT(CR_CALL_SYMBOL_INFO_IDX,""call"",0);
    CR_SYMBOL_INFO_INIT(CR_INT_SYMBOL_INFO_IDX,""$int"",0);
    CR_SYMBOL_INFO_INIT(CR_FLOAT_SYMBOL_INFO_IDX,""$float"",0);
    CR_SYMBOL_INFO_INIT(CR_STRING_SYMBOL_INFO_IDX,""$string"",0);
    CR_SYMBOL_INFO_INIT(CR_ANNOTATE_SYMBOL_INFO_IDX,""::"",0);
    CR_SYMBOL_INFO_INIT(CR_CC_SYMBOL_INFO_IDX,""$cc"",2);
    CR_SYMBOL_INFO_INIT(CR_DUMMY_SYMBOL_INFO_IDX,""$dummy"",0);
    CR_SYMBOL_INFO_INIT(CR_CONJ_SYMBOL_INFO_IDX,""/\\\\"",0);
    CR_SYMBOL_INFO_INIT(CR_FOREIGN_SYMBOL_INFO_IDX,""$foreign"",0);
    CR_SYMBOL_INFO_INIT(CR_UNDEFINED_SYMBOL_INFO_IDX,""undefined"",0);
    CR_SYMBOL_INFO_INIT(CR_BPLUS_SYMBOL_INFO_IDX,""$+"",2);
    CR_SYMBOL_INFO_INIT(CR_BMINUS_SYMBOL_INFO_IDX,""$-"",2);
    CR_SYMBOL_INFO_INIT(CR_BUMINUS_SYMBOL_INFO_IDX,""$-"",1);
    CR_SYMBOL_INFO_INIT(CR_BMULTIPLY_SYMBOL_INFO_IDX,""$*"",2);
    CR_SYMBOL_INFO_INIT(CR_BDIVIDE_SYMBOL_INFO_IDX,""$/"",2);
    CR_SYMBOL_INFO_INIT(CR_BMOD_SYMBOL_INFO_IDX,""$mod"",2);
    CR_SYMBOL_INFO_INIT(CR_BAND_SYMBOL_INFO_IDX,""$/\\\\"",2);
    CR_SYMBOL_INFO_INIT(CR_BOR_SYMBOL_INFO_IDX,""$\\\\/"",2);
    CR_SYMBOL_INFO_INIT(CR_BIFF_SYMBOL_INFO_IDX,""$<->"",2);
    CR_SYMBOL_INFO_INIT(CR_BXOR_SYMBOL_INFO_IDX,""$xor"",2);
    CR_SYMBOL_INFO_INIT(CR_BIMPL_SYMBOL_INFO_IDX,""$->"",2);
    CR_SYMBOL_INFO_INIT(CR_BNOT_SYMBOL_INFO_IDX,""$not"",2);
    CR_SYMBOL_INFO_INIT(CR_BLT_SYMBOL_INFO_IDX,""$<"",2);
    CR_SYMBOL_INFO_INIT(CR_BLEQ_SYMBOL_INFO_IDX,""$<="",2);
    CR_SYMBOL_INFO_INIT(CR_BGT_SYMBOL_INFO_IDX,""$>"",2);
    CR_SYMBOL_INFO_INIT(CR_BGEQ_SYMBOL_INFO_IDX,""$>="",2);
    CR_SYMBOL_INFO_INIT(CR_BEQ_SYMBOL_INFO_IDX,""$="",2);
    CR_SYMBOL_INFO_INIT(CR_BNEQ_SYMBOL_INFO_IDX,""$!="",2);
    CR_SYMBOL_INFO_INIT(CR_EQ_SYMBOL_INFO_IDX,""="",0);
    CR_SYMBOL_INFO_INIT(CR_LT_SYMBOL_INFO_IDX,""<"",0);
    CR_SYMBOL_INFO_INIT(CR_GT_SYMBOL_INFO_IDX,"">"",0);
    return;
}

MR_Word CR_set_annots(MR_Word annots,MR_Word model)
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

    switch(MR_tag(model)) {
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(model,sym);
            if(CR_IS_AC_SYM(sym)) {
                CR_GET_AC_FUNCTOR_ATY(model,aty);
                CR_GET_AC_FUNCTOR_IDX(model,idx);
                CR_MAKE_ANNOTATED_AC_FUNCTOR(sym,aty,idx,copy,annots);
            } else {
                aty = CR_SYM_ATY(sym);
                CR_MAKE_ANNOTATED_FUNCTOR(sym,copy,annots);
                for(i = 1; i <= aty; i++) {
                    CR_GET_FUNCTOR_ARG(i,model,arg);
                    CR_SET_FUNCTOR_ARG(i,copy,arg);
                }
            }
            break;
        case CR_INT_TAG: case CR_UBI_TAG:
            CR_GET_INT_VAL(model,int_val);
            CR_MAKE_ANNOTATED_INT(int_val,copy,annots);
            break;
        case CR_VAR_TAG:
            CR_GET_VAR_VAL(model,var_val);
            CR_MAKE_ANNOTATED_VAR(var_val,copy,annots);
            break;
        case CR_FLT_TAG:
            CR_GET_FLOAT_VAL(model,float_val);
            CR_MAKE_ANNOTATED_FLOAT(float_val,copy,annots);
            break;
        case CR_STR_TAG:
            CR_GET_STRING_VAL(model,string_val);
            CR_GET_STRING_LENGTH(model,string_len);
            CR_MAKE_ANNOTATED_STRING(string_val,string_len,copy,annots);
            break;
        case CR_FGN_TAG:
            CR_GET_FOREIGN_VAL(model,arg);
            CR_MAKE_ANNOTATED_FOREIGN(arg,copy,annots);
            break;
        default:
            MR_assert(0);
    }

    return copy;
}

MR_Word CR_ac_merge(MR_Word model1,MR_Word sym,MR_Word model2)
{
    MR_Word model3;
    MR_Word idx1, idx2, idx3;
    MR_Integer aty1, aty2, aty3;

    if(CR_IS_FUNCTOR_SYM(model1,sym)) {
        CR_GET_AC_FUNCTOR_ATY(model1,aty1);
        if(aty1 == 0)
            return model2;
        CR_GET_AC_FUNCTOR_IDX(model1,idx1);
        if(CR_IS_FUNCTOR_SYM(model2,sym)) {
            CR_GET_AC_FUNCTOR_IDX(model2,idx2);
            CR_GET_AC_FUNCTOR_ATY(model2,aty2);
            if(aty1 < aty2)
                idx3 = CR_index_merge(idx1,idx2);
            else
                idx3 = CR_index_merge(idx2,idx1);
            aty3 = aty1 + aty2;
        } else {
            idx3 = CR_index_insert(idx1,model2);
            aty3 = aty1 + 1;
        }
    } else if(CR_IS_FUNCTOR_SYM(model2,sym)) {
        CR_GET_AC_FUNCTOR_ATY(model2,aty2);
        if(aty2 == 0)
            return model1;
        CR_GET_AC_FUNCTOR_IDX(model2,idx2);
        idx3 = CR_index_insert(idx2,model1);
        aty3 = aty2 + 1;
    } else {
        idx3 = CR_index_insert(CR_EMPTY_IDX,model1);
        idx3 = CR_index_insert(idx3,model2);
        aty3 = 2;
    }
    CR_MAKE_AC_FUNCTOR(sym,aty3,idx3,model3);

    return model3;
}

MR_Word CR_ac_merge_annotations(MR_Word model1,MR_Word model2)
{
    MR_Word sym;
    MR_Integer aty;
    MR_Bool empty1, empty;
   
    if(CR_IS_FUNCTOR(model1)) {
        CR_GET_FUNCTOR_SYM(model1,sym);
        CR_GET_AC_FUNCTOR_ATY(model1,aty);
        if((sym == CR_ANNOTATE_SYMBOL()) && (aty == 0)) {
            if(CR_IS_FUNCTOR(model2)) {
                CR_GET_FUNCTOR_SYM(model2,sym);
                CR_GET_AC_FUNCTOR_ATY(model2,aty);
                if((sym == CR_ANNOTATE_SYMBOL()) && (aty == 0))
                    return CR_empty_annots_model;
                else
                    return CR_ac_unflatten(CR_ANNOTATE_SYMBOL(),model2);
            } else
                return CR_ac_unflatten(CR_ANNOTATE_SYMBOL(),model2);
        }
    }
    if(CR_IS_FUNCTOR(model2)) {
        CR_GET_FUNCTOR_SYM(model2,sym);
        CR_GET_AC_FUNCTOR_ATY(model2,aty);
        if((sym == CR_ANNOTATE_SYMBOL()) && (aty == 0))
            return CR_ac_unflatten(CR_ANNOTATE_SYMBOL(),model1);
    }
    
    return CR_ac_merge(model1,CR_ANNOTATE_SYMBOL(),model2);
}

MR_Word CR_ac_idx(MR_Word model)
{
    MR_Word sym, idx;

    if(CR_IS_FUNCTOR(model)) {
        CR_GET_FUNCTOR_SYM(model,sym);
        if(CR_IS_AC_SYM(sym)) {
            CR_GET_AC_FUNCTOR_IDX(model,idx);
            return idx;
        } else
            return CR_index_singleton(model);
    } else
        return CR_index_singleton(model);
}

MR_Word CR_ac_flatten(MR_Word model)
{
    MR_Word sym, idx;
    MR_Integer aty;

    if(CR_IS_FUNCTOR(model)) {
        CR_GET_FUNCTOR_SYM(model,sym);
        if(CR_IS_AC_SYM(sym)) {
            CR_GET_AC_FUNCTOR_ATY(model,aty);
            if(aty == 1) {
                CR_GET_AC_FUNCTOR_IDX(model,idx);
                return CR_index_singleton_flatten(idx);
            }
        }
    }
    return model;
}

MR_Word CR_ac_unflatten(MR_Word sym,MR_Word model)
{
    MR_Word idx;
    
    if(!CR_IS_FUNCTOR_SYM(model,sym)) {
        idx = CR_index_singleton(model);
        CR_MAKE_AC_FUNCTOR(sym,1,idx,model);
    }
    return model;
}

MR_Word CR_make_annotations(MR_Word model)
{
    MR_Integer aty;
    MR_Word idx;

    if(CR_IS_FUNCTOR_SYM(model,CR_ANNOTATE_SYMBOL())) {
        CR_GET_AC_FUNCTOR_ATY(model,aty);
        if(aty == 0)
            return CR_empty_annots_model;
        CR_SET_ANNOTS(CR_empty_annots_model,model,model);
    } else {
        idx = CR_index_singleton(model);
        CR_MAKE_AC_FUNCTOR(CR_ANNOTATE_SYMBOL(),1,idx,model);
        CR_SET_ANNOTS(CR_empty_annots_model,model,model);
    }
    return model;
}

MR_bool CR_eqeq(MR_Word model1,MR_Word model2)
{
    MR_Word tag = CR_tag(model1);
    MR_Word val, val_2, val_3;
    MR_Float float_val;
    MR_String string_val;
    MR_Integer string_len1, string_len2;
    int i;

    if(CR_tag(model2) != tag)
        return MR_FALSE;

    switch(tag) {
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(model1,val);
            if(!CR_IS_FUNCTOR_SYM(model2,val))
                return MR_FALSE;
            if(CR_IS_AC_SYM(val)) {
                CR_GET_AC_FUNCTOR_ATY(model1,val);
                if(!CR_HAS_AC_FUNCTOR_ATY(model2,val))
                    return MR_FALSE;
                CR_GET_AC_FUNCTOR_IDX(model1,val);
                CR_GET_AC_FUNCTOR_IDX(model2,val_2);
                return CR_index_eqeq(val,val_2);
            } else {
                val = CR_SYM_ATY(val);
                for(i = 1; i <= val; i++) {
                    CR_GET_FUNCTOR_ARG(i,model1,val_2);
                    CR_GET_FUNCTOR_ARG(i,model2,val_3);
                    if(!CR_eqeq(val_2,val_3))
                        return MR_FALSE;
                }
                return MR_TRUE;
            }
        case CR_INT_TAG: case CR_UBI_TAG:
            CR_GET_INT_VAL(model1,val);
            return CR_IS_INT_VAL(model2,val);
        case CR_VAR_TAG:
            CR_GET_VAR_VAL(model1,val);
            return CR_IS_VAR_VAL(model2,val);
        case CR_FLT_TAG:
            CR_GET_FLOAT_VAL(model1,float_val);
            return CR_IS_FLOAT_VAL(model2,float_val);
        case CR_STR_TAG:
            CR_GET_STRING_LENGTH(model1,string_len1);
            CR_GET_STRING_LENGTH(model2,string_len2);
            if(string_len1 != string_len2)
                return MR_FALSE;
            CR_GET_STRING_VAL(model1,string_val);
            return CR_IS_STRING_VAL(model2,string_val);
        case CR_FGN_TAG:
            CR_GET_FOREIGN_VAL(model1,val);
            CR_GET_FOREIGN_VAL(model2,val_2);
            return (val == val_2);
        default:
            MR_assert(0);
    }
    return MR_FALSE;
}

MR_Word CR_make_atom(MR_Word sym)
{
    MR_Word model;

    sym = CR_set_arity(sym,0);
    if(CR_IS_AC_SYM(sym))
       CR_MAKE_AC_FUNCTOR(sym,0,CR_EMPTY_IDX,model);
    else
       CR_MAKE_FUNCTOR_0(sym,model);
    return model;
}

").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",int_type = (Type::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Type = (MR_Word)CR_INT_TAG;
").
:- pragma foreign_proc("C",float_type = (Type::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Type = (MR_Word)CR_FLT_TAG;
").
:- pragma foreign_proc("C",string_type = (Type::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Type = (MR_Word)CR_STR_TAG;
").
:- pragma foreign_proc("C",var_type = (Type::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Type = (MR_Word)CR_VAR_TAG;
").
:- pragma foreign_proc("C",functor_type = (Type::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Type = (MR_Word)CR_FCT_TAG;
").

:- type symbol_info == int.

    % The symbol table maps functor/arity pairs (represented as hl_symbols)
    % to the low-level `symbol' representation.
    %
:- type symbol_table == map(string,map(int,symbol)).
:- mutable(symbol_table,symbol_table,init,ground,
    [untrailed,attach_to_io_state]).

    % The next available mvar value.
    %
:- mutable(next_mvar,mvar,0,ground,[untrailed]).

    % The table of run-time variable names.
    %
:- mutable(mvar_names,bimap(string,mvar),init,ground,[untrailed]).

%---------------------------------------------------------------------------%

:- impure pred init_symbol_infos is det.

:- pragma foreign_proc("C",init_symbol_infos,
    [will_not_call_mercury,thread_safe,will_not_modify_trail],"
    CR_symbol_infos_size = CR_SYMBOL_INFOS_INIT_0_SIZE;
    CR_symbol_infos =
        GC_malloc(CR_SYMBOL_INFOS_INIT_0_SIZE*sizeof(CR_symbol_info));
    CR_init_builtin_symbols();
").

%---------------------------------------------------------------------------%

:- func make_symbol_info(string,int) = symbol_info.

:- pragma foreign_proc("C",
    make_symbol_info(Name::in,Aty::in) = (SymInfo::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_symbol_infos_next == CR_symbol_infos_size) {
        if(CR_symbol_infos_size == CR_SYMBOL_INFOS_INIT_0_SIZE)
            CR_symbol_infos_size = CR_SYMBOL_INFOS_INIT_SIZE;
        else
            CR_symbol_infos_size = 2*CR_symbol_infos_size;
            
        CR_symbol_infos = GC_realloc(CR_symbol_infos,
            CR_symbol_infos_size*sizeof(CR_symbol_info));
    }
   
    CR_symbol_infos[CR_symbol_infos_next].name      = Name;
    CR_symbol_infos[CR_symbol_infos_next].arity     = Aty;
    CR_symbol_infos[CR_symbol_infos_next].proc_addr = (MR_Word)0;
    CR_symbol_infos[CR_symbol_infos_next].event     = (MR_Word)0;

    SymInfo = (MR_Word)CR_symbol_infos_next;
    CR_symbol_infos_next++;
").

%---------------------------------------------------------------------------%

:- func make_symbol(string,int) = symbol.

make_symbol(Name,Aty) = Sym :-
    SymInfo = make_symbol_info(Name,Aty),
    ( is_acd(Name) ->
        Sym = make_acd_symbol_from_symbol_info(SymInfo)
    ; is_ac(Name) ->
        Sym = make_ac_symbol_from_symbol_info(SymInfo)
    ; is_d_only(Name) ->
        Sym = make_d_symbol_from_symbol_info(SymInfo)
    ; Name = call_name ->
        Sym = make_call_symbol_from_symbol_info(SymInfo)
    ;   Sym = make_symbol_from_symbol_info(SymInfo)
    ).

%---------------------------------------------------------------------------%

:- func make_symbol_from_symbol_info(symbol_info) = symbol.

:- pragma foreign_proc("C",
    make_symbol_from_symbol_info(SymInfo::in) = (Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Sym = CR_SYMBOL((MR_Integer)SymInfo,
            CR_symbol_infos[(MR_Integer)SymInfo].arity,
            CR_NONAC_TYPE);
").

%---------------------------------------------------------------------------%

:- func make_ac_symbol_from_symbol_info(symbol_info) = symbol.

:- pragma foreign_proc("C",
    make_ac_symbol_from_symbol_info(SymInfo::in) = (Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Sym = CR_SYMBOL((MR_Integer)SymInfo,
            CR_symbol_infos[(MR_Integer)SymInfo].arity,
            CR_AC_TYPE);
").

%---------------------------------------------------------------------------%

:- func make_acd_symbol_from_symbol_info(symbol_info) = symbol.

:- pragma foreign_proc("C",
    make_acd_symbol_from_symbol_info(SymInfo::in) = (Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Sym = CR_SYMBOL((MR_Integer)SymInfo,
            CR_symbol_infos[(MR_Integer)SymInfo].arity,
            CR_ACD_TYPE);
").

%---------------------------------------------------------------------------%

:- func make_d_symbol_from_symbol_info(symbol_info) = symbol.

:- pragma foreign_proc("C",
    make_d_symbol_from_symbol_info(SymInfo::in) = (Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Sym = CR_SYMBOL((MR_Integer)SymInfo,
            CR_symbol_infos[(MR_Integer)SymInfo].arity,
            CR_D_TYPE);
").

%---------------------------------------------------------------------------%

:- func make_call_symbol_from_symbol_info(symbol_info) = symbol.

:- pragma foreign_proc("C",
    make_call_symbol_from_symbol_info(SymInfo::in) = (Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Sym = CR_SYMBOL((MR_Integer)SymInfo,
            CR_symbol_infos[(MR_Integer)SymInfo].arity,
            CR_CALL_TYPE);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    symbol_name(Sym::in) = (Name::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Name = CR_SYM_NAME(Sym);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    symbol_arity(Sym::in) = (Arity::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Arity = CR_SYM_ATY(Sym);
").

%---------------------------------------------------------------------------%

:- func symbol_proc_addr(symbol) = int.

:- pragma foreign_proc("C",
    symbol_proc_addr(Sym::in) = (Addr::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Addr = CR_SYM_PROC(Sym);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    symbol_set_proc_addr(Sym::in,Addr::in),
        [will_not_call_mercury,thread_safe,will_not_modify_trail],"
    CR_SYM_PROC(Sym) = Addr;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    symbol_event(Sym::in) = (Event::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Event = CR_SYM_EVENT(Sym);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    symbol_set_event(Sym::in,Event::in),
        [will_not_call_mercury,thread_safe,will_not_modify_trail],"
    CR_SYM_EVENT(Sym) = Event;
").

%---------------------------------------------------------------------------%

:- pragma promise_pure(match_symbol_names/1).
match_symbol_names(Name) = Names :-
    semipure get_symbol_table(Table),
    Names0 = keys(Table),
    Names = filter(is_matching_name(Name),Names0).

%---------------------------------------------------------------------------%

:- pred is_matching_name(string::in,string::in) is semidet.

is_matching_name(Prefix,Name) :-
    prefix(Name,Prefix).

%---------------------------------------------------------------------------%

startup :-
    impure set_symbol_table(init_symbol_table),
    impure set_annot_reg(empty_annotations).

%---------------------------------------------------------------------------%

shutdown :-
    impure set_symbol_table(init),
    impure set_annot_reg(empty_annotations).

%---------------------------------------------------------------------------%

:- func init_symbol_table = symbol_table.

init_symbol_table = Table :-
    some [!Tb] (
        init(!:Tb),
        init_symbol(cons_name,2,cons_symbol,!Tb),
        init_symbol(nil_name,0,nil_symbol,!Tb),
        init_symbol(conj_name,0,conj_symbol,!Tb),
        init_symbol(var_name,0,var_symbol,!Tb),
        init_symbol(true_name,0,true_symbol,!Tb),
        init_symbol(false_name,0,false_symbol,!Tb),
        init_symbol(call_name,0,call_symbol,!Tb),
        init_symbol(int_name,0,int_symbol,!Tb),
        init_symbol(float_name,0,float_symbol,!Tb),
        init_symbol(string_name,0,string_symbol,!Tb),
        init_symbol(annotate_name,0,annotate_symbol,!Tb),
        init_symbol(cc_name,2,cc_symbol,!Tb),
        init_symbol(foreign_name,0,foreign_symbol,!Tb),
        init_symbol(undefined_name,0,undefined_symbol,!Tb),
        init_symbol(builtin_plus_name,2,bplus_symbol,!Tb),
        init_symbol(builtin_minus_name,2,bminus_symbol,!Tb),
        init_symbol(builtin_minus_name,1,buminus_symbol,!Tb),
        init_symbol(builtin_multiply_name,2,bmultiply_symbol,!Tb),
        init_symbol(builtin_divide_name,2,bdivide_symbol,!Tb),
        init_symbol(builtin_mod_name,2,bmod_symbol,!Tb),
        init_symbol(builtin_and_name,2,band_symbol,!Tb),
        init_symbol(builtin_or_name,2,bor_symbol,!Tb),
        init_symbol(builtin_iff_name,2,biff_symbol,!Tb),
        init_symbol(builtin_xor_name,2,bxor_symbol,!Tb),
        init_symbol(builtin_impl_name,2,bimpl_symbol,!Tb),
        init_symbol(builtin_not_name,2,bnot_symbol,!Tb),
        init_symbol(builtin_lt_name,2,blt_symbol,!Tb),
        init_symbol(builtin_leq_name,2,bleq_symbol,!Tb),
        init_symbol(builtin_gt_name,2,bgt_symbol,!Tb),
        init_symbol(builtin_geq_name,2,bgeq_symbol,!Tb),
        init_symbol(builtin_eq_name,2,beq_symbol,!Tb),
        init_symbol(builtin_neq_name,2,bneq_symbol,!Tb),
        init_symbol(eq_name,0,eq_symbol,!Tb),
        init_symbol(lt_name,0,lt_symbol,!Tb),
        init_symbol(gt_name,0,gt_symbol,!Tb),
        init_ascii_symbols(min_char_value+1,!Tb),
        Table = !.Tb
    ).

%---------------------------------------------------------------------------%

:- pred init_symbol(string::in,int::in,symbol::in,symbol_table::in,
    symbol_table::out) is det.

init_symbol(Name,Arity,Sym,!Table) :-
    ( search(!.Table,Name,AtyTable0) ->
        AtyTable1 = AtyTable0
    ;   AtyTable1 = init
    ),
    ( map.insert(Arity,Sym,AtyTable1,AtyTable) ->
        map.set(Name,AtyTable,!Table)
    ;       % Symbol already exists -- ignore new(er) symbol.
            %
        true
    ).

%---------------------------------------------------------------------------%

:- pred init_ascii_symbols(int::in,symbol_table::in,symbol_table::out) is det.

init_ascii_symbols(Code,!Tb) :-
    Char = det_from_int(Code),
    Name = char_to_string(Char),
    Sym = make_symbol(Name,0),
    init_symbol(Name,0,Sym,!Tb),
    ( Code >= max_unsigned_char ->
        true
    ;   init_ascii_symbols(Code+1,!Tb)
    ).

:- func max_unsigned_char = int.

max_unsigned_char = 255.

%---------------------------------------------------------------------------%

:- pred search_symbol_table(symbol_table::in,string::in,int::in,symbol::out) 
    is semidet.

search_symbol_table(Table,Name,Aty,Sym) :-
    search(Table,Name,AtyTable),
    search(AtyTable,Aty,Sym).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",new_mvar = out,"CR_new_mvar").

new_mvar = Var :-
    semipure get_next_mvar(Var),
    impure set_next_mvar(Var+1).

%---------------------------------------------------------------------------%

named_mvar(Name) = Var :-
    semipure get_mvar_names(VarNames),
    ( search(VarNames,Name,Var0) ->
        Var = Var0
    ;   impure Var = new_mvar,
        bimap.det_insert(Name,Var,VarNames,NVarNames),
        impure set_mvar_names(NVarNames)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",mvar_name(in) = out,"CR_get_var_name").

mvar_name(Var) = Name :-
    semipure get_mvar_names(VarNames),
    ( reverse_search(VarNames,Name0,Var) ->
        Name = Name0
    ;   Name = "V_" ++ int_to_string(Var)
    ).

%---------------------------------------------------------------------------%

mvar_id(Var) = Var.

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",ascii_symbol(in) = out,"CR_ascii_symbol").

ascii_symbol(Ch) = symbol(char_to_string(Ch),0).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",symbol(in,in) = out,"CR_symbol").

symbol(Name,Aty0) = Sym :-
    promise_pure (
        semipure get_symbol_table(Table0),
        ( is_ac(Name) ->
            Aty = 0
        ;   Aty = Aty0
        ),
        ( search_symbol_table(Table0,Name,Aty,Sym0) ->
            Sym = Sym0
        ;   Sym = make_symbol(Name,Aty),
            init_symbol(Name,Aty,Sym,Table0,Table1),
            impure set_symbol_table(Table1)
        )
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    symbol_to_int(Sym::in) = (Int::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Int = Sym;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cons_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_CONS_SYMBOL();
").

:- pragma foreign_proc("C",
    nil_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_NIL_SYMBOL();
").

:- pragma foreign_proc("C",
    true_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_TRUE_SYMBOL();
").

:- pragma foreign_proc("C",
    false_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_FALSE_SYMBOL();
").

:- pragma foreign_proc("C",
    call_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_CALL_SYMBOL();
").

:- pragma foreign_proc("C",
    int_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_INT_SYMBOL();
").

:- pragma foreign_proc("C",
    float_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_FLOAT_SYMBOL();
").

:- pragma foreign_proc("C",
    string_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_STRING_SYMBOL();
").

:- pragma foreign_proc("C",
    annotate_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_ANNOTATE_SYMBOL();
").

:- pragma foreign_proc("C",
    cc_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_CC_SYMBOL();
").

:- pragma foreign_proc("C",
    dummy_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_DUMMY_SYMBOL();
").

:- pragma foreign_proc("C",
    conj_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_CONJ_SYMBOL();
").

:- pragma foreign_proc("C",
    var_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_VAR_SYMBOL();
").

:- pragma foreign_proc("C",
    foreign_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_FOREIGN_SYMBOL();
").

:- pragma foreign_proc("C",
    undefined_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_UNDEFINED_SYMBOL();
").

:- pragma foreign_proc("C",
    bplus_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BPLUS_SYMBOL();
").

:- pragma foreign_proc("C",
    bminus_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BMINUS_SYMBOL();
").

:- pragma foreign_proc("C",
    buminus_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BUMINUS_SYMBOL();
").

:- pragma foreign_proc("C",
    bmultiply_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BMULTIPLY_SYMBOL();
").

:- pragma foreign_proc("C",
    bdivide_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BDIVIDE_SYMBOL();
").

:- pragma foreign_proc("C",
    bmod_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BMOD_SYMBOL();
").

:- pragma foreign_proc("C",
    band_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BAND_SYMBOL();
").

:- pragma foreign_proc("C",
    bor_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BOR_SYMBOL();
").

:- pragma foreign_proc("C",
    biff_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BIFF_SYMBOL();
").

:- pragma foreign_proc("C",
    bxor_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BXOR_SYMBOL();
").

:- pragma foreign_proc("C",
    bimpl_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BIMPL_SYMBOL();
").

:- pragma foreign_proc("C",
    bnot_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BNOT_SYMBOL();
").

:- pragma foreign_proc("C",
    blt_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BLT_SYMBOL();
").

:- pragma foreign_proc("C",
    bleq_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BLEQ_SYMBOL();
").

:- pragma foreign_proc("C",
    bgt_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BGT_SYMBOL();
").

:- pragma foreign_proc("C",
    bgeq_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BGEQ_SYMBOL();
").

:- pragma foreign_proc("C",
    beq_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BEQ_SYMBOL();
").

:- pragma foreign_proc("C",
    bneq_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_BNEQ_SYMBOL();
").

:- pragma foreign_proc("C",
    eq_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_EQ_SYMBOL();
").

:- pragma foreign_proc("C",
    lt_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_LT_SYMBOL();
").

:- pragma foreign_proc("C",
    gt_symbol = (V::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    V = CR_GT_SYMBOL();
").

%---------------------------------------------------------------------------%

:- func null_model = model.

:- pragma foreign_proc("C",null_model = (Null::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Null = (MR_Word)NULL;
").

%---------------------------------------------------------------------------%

:- mutable(empty_annotations_model,model,null_model,ground,
        [foreign_name("C","CR_empty_annots_model"),constant]).

empty_annotations = empty_annotations_model.

empty_annotations_model = EmptyAnnots :-
    get_empty_annotations_model(EmptyAnnots).

%---------------------------------------------------------------------------%

:- mutable(true_model,model,null_model,ground,
        [foreign_name("C","CR_true_model"),constant]).

true_model = True :-
    get_true_model(True).

%---------------------------------------------------------------------------%

:- mutable(false_model,model,null_model,ground,
        [foreign_name("C","CR_false_model"),constant]).

false_model = False :-
    get_false_model(False).

%---------------------------------------------------------------------------%

:- mutable(undefined_model,model,null_model,ground,
        [foreign_name("C","CR_undefined_model"),constant]).

undefined_model = Undefined :-
    get_undefined_model(Undefined).

%---------------------------------------------------------------------------%

    % Note: model constants depend on empty_annotations, which therefore must
    %       be initialised first.
    %
:- initialise init_constants/0.

:- impure pred init_constants is det.

init_constants :-
    impure init_symbol_infos,
    True0 = apply_(true_symbol,[]),
    False0 = apply_(false_symbol,[]),
    Undefined0 = apply_(undefined_symbol,[]),
    EmptyAnnots = apply_ac(annotate_symbol,0,ac_index_empty),
    set_annotations(True0,EmptyAnnots,True),
    set_annotations(False0,EmptyAnnots,False),
    set_annotations(Undefined0,EmptyAnnots,Undefined),
    impure set_constants(True,False,Undefined,EmptyAnnots).

%---------------------------------------------------------------------------%

:- impure pred set_constants(model::in,model::in,model::in,model::in) is det.

:- pragma foreign_proc("C",set_constants(True::in,False::in,Undefined::in,
        EmptyAnnots::in),
        [will_not_call_mercury,thread_safe,will_not_modify_trail],"
    CR_SET_AC_FUNCTOR_ANNOTS(EmptyAnnots,EmptyAnnots);
    CR_true_model         = True;
    CR_false_model        = False;
    CR_undefined_model    = Undefined;
    CR_empty_annots_model = EmptyAnnots;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_ac(Sym::in),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_AC_SYM(Sym);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_call(Sym::in),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_CALL_SYM(Sym);
").

%---------------------------------------------------------------------------%

is_proc(Sym,Addr) :-
    Addr = symbol_proc_addr(Sym),
    Addr \= 0.

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",set_arity(in,in) = out,"CR_set_arity").

set_arity(Sym,Aty) = Sym1 :-
    ( is_ac(Sym) ->
        Sym1 = Sym
    ; symbol_arity(Sym) = Aty ->
        Sym1 = Sym
    ;   Sym1 = symbol(symbol_name(Sym),Aty)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",eqeq(Model1::in,Model2::in),
        [may_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_eqeq(Model1,Model2);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",get_symbol(Model::in) = (Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_GET_SYMBOL(Model,Sym);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",get_arity(Model::in) = (Aty::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_GET_ARITY(Model,Aty);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_functor(Model::in,Sym::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_IS_FUNCTOR(Model)) {
        CR_GET_FUNCTOR_SYM(Model,Sym);
        SUCCESS_INDICATOR = MR_TRUE;
    } else
        SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_functor(Model::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_FUNCTOR(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_int(Model::in,Int::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_IS_INT(Model)) {
        CR_GET_INT_VAL(Model,Int);
        SUCCESS_INDICATOR = MR_TRUE;
    } else
        SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_int(Model::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_INT(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_float(Model::in,Flt::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_IS_FLOAT(Model)) {
        CR_GET_FLOAT_VAL(Model,Flt);
        SUCCESS_INDICATOR = MR_TRUE;
    } else
        SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_float(Model::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_FLOAT(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_string(Model::in,Str::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_IS_STRING(Model)) {
        CR_GET_STRING_VAL(Model,Str);
        SUCCESS_INDICATOR = MR_TRUE;
    } else
        SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_string(Model::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_STRING(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_foreign(Model::in,Ptr::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_IS_FOREIGN(Model)) {
        CR_GET_FOREIGN_VAL(Model,Ptr);
        SUCCESS_INDICATOR = MR_TRUE;
    } else
        SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_foreign(Model::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_FOREIGN(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_var(Model::in,Var::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    if(CR_IS_VAR(Model)) {
        CR_GET_VAR_VAL(Model,Var);
        SUCCESS_INDICATOR = MR_TRUE;
    } else
        SUCCESS_INDICATOR = MR_FALSE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",is_var(Model::in),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    SUCCESS_INDICATOR = CR_IS_VAR(Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",arg(N::in,Model::in) = (Arg::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_GET_FUNCTOR_ARG(N,Model,Arg);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",ac_args(Model::in) = (Idx::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_GET_AC_FUNCTOR_IDX(Model,Idx);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",ac_arity(Model::in) = (Aty::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_GET_AC_FUNCTOR_ATY(Model,Aty);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",apply_(Sym::in,Args::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_FUNCTOR_LIST(Sym,Args,Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",apply_ac(Sym::in,Aty::in,Idx::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_AC_FUNCTOR(Sym,Aty,Idx,Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",apply_c_pointer(Sym::in,Args::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_FUNCTOR_STACK(Sym,Args,Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",apply_cons(Hd::in,Tl::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_FUNCTOR_2(CR_CONS_SYMBOL(),Hd,Tl,Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",apply_nil = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_FUNCTOR_0(CR_NIL_SYMBOL(),Model);
").

%---------------------------------------------------------------------------%

    % Gets the `model_type' or a model.
    %
:- func get_type(model) = model_type.

:- pragma foreign_proc("C",get_type(Model::in) = (Type::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"    CR_GET_TYPE(Model,Type);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",int(Int::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_INT(Int,Model);
").

:- pragma foreign_proc("C",float(Flt::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_FLOAT(Flt,Model);
").

:- pragma foreign_proc("C",string(Str::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_STRING_0(Str,Model);
").

:- pragma foreign_proc("C",foreign(Ptr::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_FOREIGN(Ptr,Model);
").

:- pragma foreign_proc("C",var(Var::in) = (Model::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_MAKE_VAR(Var,Model);
").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",write_model(Model::in,_IO0::di,_IO1::uo),
    [may_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_stream stream;
    CR_MAKE_FILE_STREAM(stream,stdout);
    CR_stream_put_model(stream,MR_FALSE,Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    write_model_with_annotations(Model::in,_IO0::di,_IO1::uo),
    [may_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_stream stream;
    CR_MAKE_FILE_STREAM(stream,stdout);
    CR_stream_put_model(stream,MR_TRUE,Model);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    model_to_string(Model::in) = (Str::out),
    [may_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_stream stream;
    CR_OPEN_STRSTREAM(stream);
    CR_stream_put_model(stream,MR_FALSE,Model);
    CR_CLOSE_STRSTREAM(stream);
    Str = CR_GET_STRSTREAM_STR(stream);
").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term_to_model(VarSet,Term) = Model :-
    promise_pure impure Model = term_to_model_2(VarSet,Term).

%---------------------------------------------------------------------------%

:- impure func term_to_model_2(varset,term) = model.

term_to_model_2(VarSet,variable(Var,_)) = Model :-
    ( search_name(VarSet,Var,Name) ->
        impure MVar = named_mvar(Name)
    ;   impure MVar = new_mvar
    ),
    Model = var(MVar).
term_to_model_2(VarSet,functor(Cnst,Args,_)) = Model :-
    ( Cnst = integer(Int),
        Model = int(Int)
    ; Cnst = float(Flt),
        Model = float(Flt)
    ; Cnst = string(Str),
        Model = string(Str)
    ; Cnst = implementation_defined(_),
        unexpected($file, $pred, "implementation_defined/1 constant")
    ; Cnst = atom(F),
        ( F = annotate_name,
          Args = [Arg1,Arg2] ->
            impure Model0 = term_to_model_2(VarSet,Arg1),
            term_to_flattened_ac_arg(VarSet,Cnst,Arg2,[],MArgs),
            length(MArgs,Aty),
            Sym = symbol(annotate_name,Aty),
            foldl(ac_index_insert,MArgs,ac_index_empty,Idx),
            Annots = apply_ac(Sym,Aty,Idx),
            set_annotations(Model0,Annots,Model)
        ;   ( is_ac(F) ->
                foldl(term_to_flattened_ac_arg(VarSet,Cnst),Args,[],MArgs)
            ;   MArgs = map(term_to_model(VarSet),Args)
            ),
            length(MArgs,Aty),
            Sym = symbol(F,Aty),
            ( is_ac(Sym) ->
                foldl(ac_index_insert,MArgs,ac_index_empty,Idx),
                Model = apply_ac(Sym,Aty,Idx)
            ;   Model = apply_(Sym,MArgs)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred term_to_flattened_ac_arg(varset::in,const::in,term::in,list(model)::in,
    list(model)::out) is det.

term_to_flattened_ac_arg(VarSet,ACOp,Arg,!ACArgs) :-
    ( Arg = functor(ACOp,Args0,_) ->
        foldl(term_to_flattened_ac_arg(VarSet,ACOp),Args0,!ACArgs)
    ;   !:ACArgs = [term_to_model(VarSet,Arg)|!.ACArgs]
    ).

%---------------------------------------------------------------------------%

construct(F,Args) = Model :-
    length(Args,Aty),
    Sym = symbol(F,Aty),
    ( is_ac(Sym) ->
        ac_merge_list(Sym,Args,Model)
    ;   Model = apply_(Sym,Args)
    ).

%---------------------------------------------------------------------------%

deconstruct(Model,Fct,Args) :-
    is_functor(Model,Sym),
    Fct = symbol_name(Sym),
    ( is_ac(Sym) ->
            % Convert n-ary AC term back into a binary operator again.
            %
        Aty = ac_arity(Model),
        ( Aty = 0 ->
            Args = []
        ;   Idx = ac_args(Model),
            ac_index_delete_least(Arg,Idx,NIdx),
            Rest0 = apply_ac(Sym,Aty-1,NIdx),
            ac_flatten(Rest0,Rest),
            Args = [Arg,Rest]
        )
    ;   get_model_args_list(Model,symbol_arity(Sym),[],Args)
    ).

%---------------------------------------------------------------------------%

:- pred get_model_args_list(model::in,int::in,list(model)::in,list(model)::out)
    is det.

get_model_args_list(Model,N,!Args) :-
    ( N = 0 ->
        true
    ;   Arg = arg(N,Model),
        !:Args = [Arg|!.Args],
        get_model_args_list(Model,N-1,!Args)
    ).

%---------------------------------------------------------------------------%

compare_models(Model0,Model1,Result) :-
    compare_models_0(Model0,Model1,Result0),
    ( Result0 < 0 ->
        Result = (<)
    ; Result0 > 0 ->
        Result = (>)
    ;   Result = (=)
    ).

%---------------------------------------------------------------------------%

:- pred compare_models_0(model::in,model::in,int::out) is det.

:- pragma foreign_proc("C",compare_models_0(Model1::in,Model2::in,Result::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    MR_Integer lcl_cmp = CR_model_compare(Model1,Model2);
    switch(lcl_cmp) {
        case CR_LT:
            Result = (-1);
            break;
        case CR_GT:
            Result = 1;
            break;
        default:
            Result = 0;
            break;
    }
").

%---------------------------------------------------------------------------%

    % The annotation register. 
    %
:- mutable(annot_reg,model,empty_annotations,ground,
    [foreign_name("C","annot_reg"),untrailed]).

%---------------------------------------------------------------------------%

get_annotation_reg(Annots) :-
    semipure get_annot_reg(Annots).

%---------------------------------------------------------------------------%

set_annotation_reg(Annots) :-
    impure set_annot_reg(Annots).

%---------------------------------------------------------------------------%

:- pragma no_inline(get_annotations/2).
:- pragma foreign_proc("C",get_annotations(Model::in,Annots::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_GET_ANNOTS(Model,Annots);
").

%---------------------------------------------------------------------------%

:- pragma no_inline(set_annotations/3).
:- pragma foreign_proc("C",set_annotations(Model::in,Annots::in,NewModel::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    CR_SET_ANNOTS(Annots,Model,NewModel);
").

:- pragma foreign_proc("C",annotate(Model::in,Annots::in) = (NewModel::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    MR_Word annots;
    
    CR_GET_ANNOTS(Model,annots);
    annots = CR_ac_merge_annotations(annots,Annots);
    CR_SET_ANNOTS(annots,Model,NewModel);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",ac_merge(Sym::in,Model1::in,Model2::in,Model3::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Model3 = CR_ac_merge(Model1,Sym,Model2);
").

%---------------------------------------------------------------------------%

ac_merge_list(Sym,[],Model) :-
    Model = apply_ac(Sym,0,ac_index_empty).
ac_merge_list(Sym,[Arg0|Args],Model) :-
    ac_merge_list(Sym,Args,Arg0,Model).

%---------------------------------------------------------------------------%

:- pred ac_merge_list(symbol::in,list(model)::in,model::in,model::out) is det.

ac_merge_list(_,[],!Model).
ac_merge_list(Sym,[Arg|Args],!Model) :-
    ac_merge(Sym,Arg,!Model),
    ac_merge_list(Sym,Args,!Model).

%---------------------------------------------------------------------------%

:- pred ac_flatten(model::in,model::out) is det.

:- pragma foreign_proc("C",ac_flatten(Model0::in,Model1::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Model1 = CR_ac_flatten(Model0);
").

%---------------------------------------------------------------------------%

reset_model :-
    impure set_symbol_table(init_symbol_table),
    impure set_next_mvar(0),
    impure set_mvar_names(init),
    impure set_annot_reg(empty_annotations).

reset_model(!IO) :-
    promise_pure (
        impure reset_model,
        !:IO = !.IO
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_import_module("C",lib).
:- pragma foreign_import_module("C",ac_index).
:- pragma foreign_import_module("C",parse).

:- pragma foreign_decl("C","

typedef struct {
    MR_Word data;
    void (*put)(MR_Word,const char *,MR_Word);
} CR_stream_s;

typedef CR_stream_s *CR_stream;

typedef struct {
    MR_String  str;
    MR_Integer pos;
    MR_Integer len;
} CR_strstream_data_s;

typedef CR_strstream_data_s *CR_strstream_data;

#define CR_STRSTREAM_INIT_LEN           64
#define CR_OPEN_STRSTREAM(stream)                                           \\
    do {                                                                    \\
        CR_strstream_data tmp_data;                                         \\
        (stream) = (CR_stream)GC_MALLOC(sizeof(CR_stream_s));               \\
        (stream)->put = CR_strstream_put;                                   \\
        tmp_data =                                                          \\
            (CR_strstream_data)GC_MALLOC(sizeof(CR_strstream_data_s));      \\
        tmp_data->str = (MR_String)GC_MALLOC(CR_STRSTREAM_INIT_LEN);        \\
        tmp_data->pos = 0;                                                  \\
        tmp_data->len = CR_STRSTREAM_INIT_LEN;                              \\
        (stream)->data = (MR_Word)tmp_data;                                 \\
    } while(0)
#define CR_CLOSE_STRSTREAM(stream)                                          \\
    do {                                                                    \\
        CR_strstream_data tmp_data;                                         \\
        tmp_data = (CR_strstream_data)((stream)->data);                     \\
        tmp_data->str = (MR_String)GC_REALLOC(tmp_data->str,                \\
            tmp_data->pos+1);                                               \\
        tmp_data->str[tmp_data->pos] = '\\0';                               \\
        tmp_data->len = tmp_data->pos;                                      \\
    } while(0)
#define CR_GET_STRSTREAM_STR(stream)                                        \\
    (((CR_strstream_data)((stream)->data))->str)
#define CR_GET_STRSTREAM_LEN(stream)                                        \\
    (((CR_strstream_data)((stream)->data))->len)

#define CR_MAKE_FILE_STREAM(stream,file)                                    \\
    do {                                                                    \\
        (stream) = (CR_stream)GC_MALLOC(sizeof(CR_stream_s));               \\
        (stream)->put  = CR_filestream_put;                                 \\
        (stream)->data = (MR_Word)(file);                                   \\
    } while(0);

#define CR_MAX_PRIORITY                 99999999

#define CR_ANYTHING                     2
#define CR_ALPHA                        1
#define CR_OP                           0

void CR_stream_put_model(CR_stream stream,MR_Bool annots,MR_Word model);
void CR_strstream_put(MR_Word data0,const char *format,MR_Word arg);
void CR_filestream_put(MR_Word file,const char *format,MR_Word arg);

").

:- pragma foreign_code("C","

#define CR_stream_put(stream,format,arg)                                    \\
    (*(stream)->put)((stream)->data,(format),(MR_Word)(arg))

#define CR_functor_kind(func)                                               \\
    (func[0] == '\\0'? CR_OP :                                              \\
    (islower(func[0])? CR_ALPHA : CR_OP))

static void CR_stream_put_priority_model(CR_stream stream,MR_Integer priority,
    MR_Integer start,MR_Bool annots,MR_Word model);
static void CR_stream_put_ac_model(CR_stream stream,MR_Integer priority,
    MR_Integer start,MR_Bool have_annots,MR_Bool annots,MR_Word model);
static void CR_stream_put_list_model(CR_stream stream,MR_Bool annots,
    MR_Word model);
static void CR_stream_put_functor(CR_stream stream,MR_String func);
static void CR_stream_put_string(CR_stream stream,MR_String str);

    /*
     * For some reason I thought implementing the following in C was a good
     * idea -- gjd
     */
void CR_stream_put_model(CR_stream stream,MR_Bool annots,MR_Word model)
{
    CR_stream_put_priority_model(stream,CR_MAX_PRIORITY,CR_ANYTHING,annots,
        model);
    return;
}

static void CR_stream_put_priority_model(CR_stream stream,MR_Integer priority,
    MR_Integer start,MR_Bool annots,MR_Word model)
{
    MR_Integer int_val, var_val, aty, i, new_priority;
    MR_Float flt_val;
    MR_String str_val;
    MR_Word arg, sym, as, idx;
    MR_Bool have_annots, parens;
    static char flt_buf[MR_SPRINTF_FLOAT_BUF_SIZE];

    if(annots) {
        CR_GET_ANNOTS(model,as);
        if(as == CR_empty_annots_model)
            have_annots = MR_FALSE;
        else
            have_annots = MR_TRUE;
    } else
        have_annots = MR_FALSE;

    switch(CR_tag(model)) {
        case CR_INT_TAG: case CR_UBI_TAG:
            CR_GET_INT_VAL(model,int_val);
            parens = (start != CR_ANYTHING) && 
                (((int_val < 0) && (start != CR_OP)) ||
                (((int_val >= 0) && (start != CR_ALPHA))));
            if(parens)
                CR_stream_put(stream,""%c"",'(');
            CR_stream_put(stream,""%ld"",int_val);
            if(parens)
                CR_stream_put(stream,""%c"",')');
            break;
        case CR_FLT_TAG:
            CR_GET_FLOAT_VAL(model,flt_val);
            MR_sprintf_float(flt_buf,flt_val);
            parens = (start != CR_ANYTHING) && 
                (((flt_val < 0.0) && (start != CR_OP)) ||
                (((flt_val >= 0.0) && (start != CR_ALPHA))));
            if(parens)
                CR_stream_put(stream,""%c"",'(');
            CR_stream_put(stream,""%s"",flt_buf);
            if(parens)
                CR_stream_put(stream,""%c"",')');
            break;
        case CR_STR_TAG:
            CR_GET_STRING_VAL(model,str_val);
            CR_stream_put(stream,""%c"",'\\""');
            CR_stream_put_string(stream,str_val);
            CR_stream_put(stream,""%c"",'\\""');
            break;
        case CR_VAR_TAG:
            CR_GET_VAR_VAL(model,var_val);
            str_val = CR_get_var_name(var_val);
            parens = ((start != CR_ANYTHING) && (start != CR_ALPHA));
            if(parens)
                CR_stream_put(stream,""%c"",'(');
            CR_stream_put(stream,""%s"",str_val);
            if(parens)
                CR_stream_put(stream,""%c"",')');
            break;
        case CR_FGN_TAG:
            CR_stream_put(stream,""%s"",""($foreign)"");
            break;
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(model,sym);
            if(CR_IS_AC_SYM(sym)) {
                CR_stream_put_ac_model(stream,priority,start,have_annots,
                    annots,model);
                break;
            }
            switch(sym) {
                case CR_CONS_SYMBOL():
                    CR_stream_put(stream,""%c"",'[');
                    CR_GET_FUNCTOR_ARG(1,model,arg);
                    CR_stream_put_model(stream,annots,arg);
                    CR_GET_FUNCTOR_ARG(2,model,model);
                    CR_stream_put_list_model(stream,annots,model);
                    CR_stream_put(stream,""%c"",']');
                    goto CR_put_annots;
                case CR_NIL_SYMBOL():
                    CR_stream_put(stream,""%s"",""[]"");
                    goto CR_put_annots;
                default:
                    break;
            }
            aty = CR_SYM_ATY(sym);
            if((aty == 1) && 
               (new_priority = CR_is_unary_operator(CR_SYM_NAME(sym)))) {
                parens = (have_annots || (new_priority >= priority) ||
                    ((start != CR_ANYTHING) && 
                     (start != CR_functor_kind(CR_SYM_NAME(sym)))));
                if(parens)
                    CR_stream_put(stream,""%c"",'(');
                CR_stream_put_functor(stream,CR_SYM_NAME(sym));
                CR_GET_FUNCTOR_ARG(1,model,arg);
                start = !CR_functor_kind(CR_SYM_NAME(sym));
                CR_stream_put_priority_model(stream,new_priority,start,
                    annots,arg);
                if(parens)
                    CR_stream_put(stream,""%c"",')');
                break;
            }
            if((aty == 2) && 
               (new_priority = CR_is_binary_operator(CR_SYM_NAME(sym)))) {
                parens = (have_annots || (new_priority >= priority) ||
                    (start != CR_ANYTHING));
                if(parens) {
                    CR_stream_put(stream,""%c"",'(');
                    start = CR_ANYTHING;
                }
                CR_GET_FUNCTOR_ARG(1,model,arg);
                CR_stream_put_priority_model(stream,new_priority,start,
                    annots,arg);
                CR_stream_put_functor(stream,CR_SYM_NAME(sym));
                CR_GET_FUNCTOR_ARG(2,model,arg);
                start = !CR_functor_kind(CR_SYM_NAME(sym));
                CR_stream_put_priority_model(stream,new_priority,start,
                    annots,arg);
                if(parens)
                    CR_stream_put(stream,""%c"",')');
                break;
            }
            parens = ((start != CR_ANYTHING) && 
                      (start != CR_functor_kind(CR_SYM_NAME(sym))));
            if(parens)
                CR_stream_put(stream,""%c"",'(');
            CR_stream_put_functor(stream,CR_SYM_NAME(sym));
            if(aty == 0) {
                if(parens)
                    CR_stream_put(stream,""%c"",')');
                break;
            }
            CR_stream_put(stream,""%c"",'(');
            for(i = 1; i < aty; i++) {
                CR_GET_FUNCTOR_ARG(i,model,arg);
                CR_stream_put_model(stream,annots,arg);
                CR_stream_put(stream,""%c"",',');
            }
            CR_GET_FUNCTOR_ARG(i,model,arg);
            CR_stream_put_model(stream,annots,arg);
            CR_stream_put(stream,""%c"",')');
            if(parens)
                CR_stream_put(stream,""%c"",')');
            break;
        default:
            MR_assert(0);
    }

CR_put_annots:
    if(have_annots) {
        CR_stream_put(stream,""%s"",""::"");
        CR_stream_put_ac_model(stream,CR_MAX_PRIORITY,CR_ANYTHING,MR_FALSE,
            annots,as);
    }

    return;
}

static void CR_stream_put_ac_model(CR_stream stream,MR_Integer priority,
    MR_Integer start,MR_Bool have_annots,MR_Bool annots,MR_Word model)
{
    MR_Integer aty, new_priority, is_op;
    MR_Word sym, idx, arg;
    MR_Bool parens;
    CR_iterator itr;

    CR_GET_FUNCTOR_SYM(model,sym);

    is_op = new_priority = CR_is_binary_operator(CR_SYM_NAME(sym));
    if(!new_priority)
        new_priority = CR_MAX_PRIORITY;

    CR_GET_AC_FUNCTOR_ATY(model,aty);

    if(aty == 0) {
        parens = (have_annots || (new_priority >= priority) ||
            ((start != CR_ANYTHING) && 
             (start != CR_functor_kind(CR_SYM_NAME(sym)))));
        if(parens)
            CR_stream_put(stream,""%c"",'(');
        CR_stream_put_functor(stream,CR_SYM_NAME(sym));
        if(parens)
            CR_stream_put(stream,""%c"",')');
        return;
    }
    
    CR_GET_AC_FUNCTOR_IDX(model,idx);

    itr = (CR_iterator)GC_MALLOC(sizeof(CR_iterator_s));

    CR_ITR_INIT(itr);
    CR_ITR_PUSH(itr,0,idx);

    parens = (have_annots || (new_priority >= priority));

    if(parens) {
        CR_stream_put(stream,""%c"",'(');
        start = CR_ANYTHING;
    }

    CR_iterator_get_least(itr,&arg);
    CR_stream_put_priority_model(stream,new_priority,start,annots,arg);
    if(!is_op)
        start = CR_ANYTHING;
    else
        start = !CR_functor_kind(CR_SYM_NAME(sym));
    while(CR_iterator_get_least(itr,&arg)) {
        if(!is_op)
            CR_stream_put(stream,""%c"",'`');
        CR_stream_put_functor(stream,CR_SYM_NAME(sym));
        if(!is_op)
            CR_stream_put(stream,""%c"",'`');
        CR_stream_put_priority_model(stream,new_priority,start,annots,arg);
    }

    if(parens)
        CR_stream_put(stream,""%c"",')');

    return;
}

static void CR_stream_put_list_model(CR_stream stream,MR_Bool annots,
    MR_Word model)
{
    MR_Word sym, arg;

    while(1) {
        if(CR_tag(model) == CR_FCT_TAG) {
            CR_GET_FUNCTOR_SYM(model,sym);
            switch(sym) {
                case CR_CONS_SYMBOL():
                    CR_GET_FUNCTOR_ARG(1,model,arg);
                    CR_stream_put(stream,""%c"",',');
                    CR_stream_put_model(stream,annots,arg);
                    CR_GET_FUNCTOR_ARG(2,model,model);
                    CR_stream_put_list_model(stream,annots,model);
                case CR_NIL_SYMBOL():
                    return;
                default:
                    CR_stream_put(stream,""%c"",'|');
                    CR_stream_put_model(stream,annots,model);
                    return;
            }
        } else {
            CR_stream_put(stream,""%c"",'|');
            CR_stream_put_model(stream,annots,model);
            return;
        }
    }
}

static void CR_stream_put_functor(CR_stream stream,MR_String func)
{
    MR_Integer i;
    MR_Bool quote = MR_FALSE;

    /*
     * Decide if the atom needs to be quoted.
     */
    if(!func[0])
        quote = MR_TRUE;
    else if(func[0] && islower(func[0])) {
        for(i = 1; func[i]; i++) {
            if(!isalnum(func[i]) && (func[i] != '_')) {
                quote = MR_TRUE;
                break;
            }
        }
    } else {
        for(i = 0; func[i] && (quote != MR_TRUE); i++) {
            switch(func[i]) {
                case '!': case '#': case '$': case '&': case '*': case '+':
                case '-': case '.': case '/': case ':': case '<': case '=':
                case '>': case '?': case '@': case '^': case '~': case '\\\\':
                    break;
                default:
                    quote = MR_TRUE;
                    break;
            }
        }
    }

    if(quote) {
        CR_stream_put(stream,""%c"",'\\'');
        CR_stream_put_string(stream,func);
        CR_stream_put(stream,""%c"",'\\'');
    } else
        CR_stream_put(stream,""%s"",func);

    return;
}


static void CR_stream_put_string(CR_stream stream,MR_String str)
{
    MR_Integer i;
    
    for(i = 0; str[i]; i++) {
        switch(str[i]) {
            case '\\n':
                CR_stream_put(stream,""%c"",'\\\\');
                CR_stream_put(stream,""%c"",'n');
                break;
            case '\\t':
                CR_stream_put(stream,""%c"",'\\\\');
                CR_stream_put(stream,""%c"",'t');
                break;
            case '\\b':
                CR_stream_put(stream,""%c"",'\\\\');
                CR_stream_put(stream,""%c"",'b');
                break;
            case '\\\\':
                CR_stream_put(stream,""%c"",'\\\\');
                CR_stream_put(stream,""%c"",'\\\\');
                break;
            case '\\""':
                CR_stream_put(stream,""%c"",'\\\\');
                CR_stream_put(stream,""%c"",'\\""');
            case '\\'':
                CR_stream_put(stream,""%c"",'\\\\');
                CR_stream_put(stream,""%c"",'\\'');
                break;
            default:
                CR_stream_put(stream,""%c"",str[i]);
                break;
        }
    }
    return;
}

void CR_strstream_put(MR_Word data0,const char *format,MR_Word arg)
{
    MR_Integer n;
    CR_strstream_data data = (CR_strstream_data)data0;

    n = snprintf(data->str+data->pos,data->len-data->pos,format,arg);

    if(n+data->pos >= data->len) {
        data->len = data->len*2;
        data->str = (MR_String)MR_GC_realloc(data->str,data->len);
        CR_strstream_put(data0,format,arg);
    } else
        data->pos += n;

    return;
}

void CR_filestream_put(MR_Word file,const char *format,MR_Word arg)
{
    if(fprintf((FILE *)file,format,arg) < 0)
        CR_io_error();
    return;
}

").

%----------------------------------------------------------------------------%
:- end_module model.
%----------------------------------------------------------------------------%
