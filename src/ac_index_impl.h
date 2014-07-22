/*
 * ac_index_impl.h
 * Gregory J. Duck
 *
 * Implementation of AC index structures.  The index structure is a
 * 2-3-4-tree, where each node contains AC arguments.
 *
 * We can't use Mercury's 2-3-4 implementations for several reasons.  Mainly
 * because we need to implement specialised lookups that do not fit into
 * Mercury's standard tree operations efficiently.
 *
 * The entire implementation is C not Mercury.  C was chosen because it is
 * easier to do low-level hacks and easier to integrate with the rest of 
 * the Cadmium virtual machine.  OTOH, C is less readable and harder to debug;
 * however, once the tree implementation is stable there should be little
 * or no need to change it further.
 */
#ifndef __AC_INDEX_H
#define __AC_INDEX_H

#define CR_2_TAG        MR_mktag(0)
#define CR_3_TAG        MR_mktag(1)
#define CR_4_TAG        MR_mktag(2)
#define CR_EMPTY_TAG    MR_mktag(3)

#define CR_EMPTY_NODE   ((MR_Word)MR_mkword(CR_EMPTY_TAG,((MR_Word)0x0)))
#define CR_EMPTY_IDX    CR_EMPTY_NODE

typedef struct {
    MR_Word t[2];
    MR_Word k[1];
} CR_two_node_s;

typedef struct {
    MR_Word t[3];
    MR_Word k[2];
} CR_three_node_s;

typedef struct {
    MR_Word t[4];
    MR_Word k[3];
} CR_four_node_s;

#define CR_2_NODE(node)                                                     \
    ((CR_two_node_s *)(MR_body((node),CR_2_TAG)))
#define CR_3_NODE(node)                                                     \
    ((CR_three_node_s *)(MR_body((node),CR_3_TAG)))
#define CR_4_NODE(node)                                                     \
    ((CR_four_node_s *)(MR_body((node),CR_4_TAG)))

#define CR_MAKE_2_NODE(t0,t1,k0,node)                                       \
    do {                                                                    \
        MR_Word tmp_node;                                                   \
        tmp_node = (MR_Word)MR_mkword(CR_2_TAG,                             \
            CR_new_object(MR_Word,sizeof(CR_two_node_s),"CR_2_NODE"));      \
        CR_2_NODE(tmp_node)->t[0] = (t0);                                   \
        CR_2_NODE(tmp_node)->t[1] = (t1);                                   \
        CR_2_NODE(tmp_node)->k[0] = (k0);                                   \
        (node) = tmp_node;                                                  \
    } while(0)
#define CR_MAKE_3_NODE(t0,t1,t2,k0,k1,node)                                 \
    do {                                                                    \
        MR_Word tmp_node;                                                   \
        tmp_node = (MR_Word)MR_mkword(CR_3_TAG,                             \
            CR_new_object(MR_Word,sizeof(CR_three_node_s),"CR_3_NODE"));    \
        CR_3_NODE(tmp_node)->t[0] = (t0);                                   \
        CR_3_NODE(tmp_node)->t[1] = (t1);                                   \
        CR_3_NODE(tmp_node)->t[2] = (t2);                                   \
        CR_3_NODE(tmp_node)->k[0] = (k0);                                   \
        CR_3_NODE(tmp_node)->k[1] = (k1);                                   \
        (node) = tmp_node;                                                  \
    } while(0)
#define CR_MAKE_4_NODE(t0,t1,t2,t3,k0,k1,k2,node)                           \
    do {                                                                    \
        MR_Word tmp_node;                                                   \
        tmp_node = (MR_Word)MR_mkword(CR_4_TAG,                             \
            CR_new_object(MR_Word,sizeof(CR_four_node_s),"CR_4_NODE"));     \
        CR_4_NODE(tmp_node)->t[0] = (t0);                                   \
        CR_4_NODE(tmp_node)->t[1] = (t1);                                   \
        CR_4_NODE(tmp_node)->t[2] = (t2);                                   \
        CR_4_NODE(tmp_node)->t[3] = (t3);                                   \
        CR_4_NODE(tmp_node)->k[0] = (k0);                                   \
        CR_4_NODE(tmp_node)->k[1] = (k1);                                   \
        CR_4_NODE(tmp_node)->k[2] = (k2);                                   \
        (node) = tmp_node;                                                  \
    } while(0)
#define CR_SPLIT_4_NODE(node,lnode,rnode)                                   \
    do {                                                                    \
        CR_MAKE_2_NODE(CR_4_NODE(node)->t[0],CR_4_NODE(node)->t[1],         \
            CR_4_NODE(node)->k[0],lnode);                                   \
        CR_MAKE_2_NODE(CR_4_NODE(node)->t[2],CR_4_NODE(node)->t[3],         \
            CR_4_NODE(node)->k[2],rnode);                                   \
    } while(0)

#define CR_ITR_STK_SIZE             64

typedef struct {
    MR_Integer ptr;
    MR_Word stack[CR_ITR_STK_SIZE];
} CR_iterator_s;

typedef CR_iterator_s *CR_iterator;

#define CR_ID(pos,node)                                                     \
    ((MR_Integer)MR_mkword(pos,MR_strip_tag(node)))

#define CR_ITR_INIT(itr)                                                    \
    ((itr)->ptr = (-1))
#define CR_ITR_IS_EMPTY(itr)                                                \
    ((itr)->ptr == (-1))
#define CR_ITR_PUSH(itr,pos,node)                                           \
    do {                                                                    \
        (itr)->stack[((itr)->ptr)+1] = (node);                              \
        (itr)->stack[((itr)->ptr)+2] = (pos);                               \
        (itr)->ptr += 2;                                                    \
    } while(0)
#define CR_ITR_POP(itr,pos,node)                                            \
    do {                                                                    \
        (pos)  = (itr)->stack[(itr)->ptr];                                  \
        (node) = (itr)->stack[(itr)->ptr-1];                                \
        (itr)->ptr -= 2;                                                    \
    } while(0)

#define CR_EQ                           0x0
#define CR_GT                           0x1
#define CR_LT                           0x2

#define CR_lookup_execute(lookup,arg,model)                                 \
    ((*(arg) = (model)),                                                    \
    CR_execute(CR_LOOKUP,(lookup),(MR_Word)NULL))

#define CR_index_abort()                                                    \
    exit(EXIT_FAILURE);

MR_Integer CR_index_compare(MR_Word idx1,MR_Word idx2);
MR_Bool CR_index_eqeq(MR_Word idx1,MR_Word idx2);
MR_Integer CR_model_compare(MR_Word model1,MR_Word model2);
MR_Integer CR_model_strict_compare(MR_Word model1,MR_Word model2);
MR_Bool CR_iterator_get_least(CR_iterator itr,MR_Word *least);
MR_Bool CR_index_find_first_node(MR_Word idx,MR_Word lookup,MR_Word *next,
    MR_Word *node,MR_Integer *pos);
MR_Bool CR_iterator_get_next(CR_iterator itr,MR_Word lookup,MR_Word *next,
    MR_Integer *id);
MR_Word CR_index_singleton(MR_Word model);
MR_Word CR_index_insert(MR_Word idx,MR_Word model);
MR_Word CR_index_2_insert(MR_Word idx,MR_Word model);
MR_Word CR_index_3_insert(MR_Word idx,MR_Word model);
MR_Word CR_index_delete(MR_Word idx,MR_Word model,MR_Bool *red);
MR_Word CR_index_delete_least(MR_Word idx,MR_Word *least,MR_Bool *red);
MR_Word CR_fix_2_t0(MR_Word k0,MR_Word t0,MR_Word t1,MR_Bool *red);
MR_Word CR_fix_2_t1(MR_Word k0,MR_Word t0,MR_Word t1,MR_Bool *red);
MR_Word CR_fix_3_t0(MR_Word k0,MR_Word k1,MR_Word t0,MR_Word t1,MR_Word t2,
    MR_Bool *red);
MR_Word CR_fix_3_t1(MR_Word k0,MR_Word k1,MR_Word t0,MR_Word t1,MR_Word t2,
    MR_Bool *red);
MR_Word CR_fix_3_t2(MR_Word k0,MR_Word k1,MR_Word t0,MR_Word t1,MR_Word t2,
    MR_Bool *red);
MR_Word CR_fix_4_t0(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red);
MR_Word CR_fix_4_t1(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red);
MR_Word CR_fix_4_t2(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red);
MR_Word CR_fix_4_t3(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red);
MR_Word CR_index_singleton_flatten(MR_Word idx);
MR_Word CR_index_merge(MR_Word idx1,MR_Word idx2);
MR_Word CR_index_set_wakeups(MR_Word idx,MR_Word wakeups);
MR_Word CR_index_create_events(MR_Word idx);
MR_Word CR_index_to_list(MR_Word idx);
MR_Integer CR_index_size(MR_Word idx);
MR_Integer CR_is_balanced(MR_Word idx,MR_Integer curr);
void CR_index_print_node(MR_Word node);
void CR_index_print_index(MR_Word idx);

#endif

