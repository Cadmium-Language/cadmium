/*
 * ac_index_impl.c
 * Gregory J. Duck
 *
 * Implementation of the AC index structure.  See ac_index_impl.h
 */

#include "lib.mh"

void CR_index_to_list_2(MR_Word idx,MR_Word lsin,MR_Word *lsout);

MR_Integer CR_index_compare(MR_Word idx1,MR_Word idx2)
{
    CR_iterator_s itr_s1, itr_s2;
    CR_iterator itr1 = &itr_s1, itr2 = &itr_s2;
    MR_Word model1, model2;
    MR_Integer cmp;

    CR_ITR_INIT(itr1);
    CR_ITR_INIT(itr2);

    CR_ITR_PUSH(itr1,0,idx1);
    CR_ITR_PUSH(itr2,0,idx2);

    while(CR_iterator_get_least(itr1,&model1)) {
        CR_iterator_get_least(itr2,&model2);
        cmp = CR_model_compare(model1,model2);
        if(cmp != CR_EQ)
            return cmp;
    } 

    return CR_EQ;
}

MR_Bool CR_index_eqeq(MR_Word idx1,MR_Word idx2)
{
    CR_iterator_s itr_s1, itr_s2;
    CR_iterator itr1 = &itr_s1, itr2 = &itr_s2;
    MR_Word model1, model2;

    CR_ITR_INIT(itr1);
    CR_ITR_INIT(itr2);

    CR_ITR_PUSH(itr1,0,idx1);
    CR_ITR_PUSH(itr2,0,idx2);

    while(CR_iterator_get_least(itr1,&model1)) {
        CR_iterator_get_least(itr2,&model2);
        if(!CR_eqeq(model1,model2))
            return MR_FALSE;
    }

    return MR_TRUE;
}

MR_Integer CR_model_compare(MR_Word model1,MR_Word model2)
{
    MR_Integer tag1, tag2;
    MR_Integer int_val1, int_val2;
    MR_Float flt_val1, flt_val2;
    MR_Integer var_val1, var_val2;
    MR_String str_val1, str_val2;
    MR_Integer sym1, sym2;
    MR_Word idx1, idx2;
    MR_Word fgn_val1, fgn_val2;
    MR_Word arg1, arg2;
    MR_Integer aty1, aty2;
    MR_Integer cmp, i;

    CR_GET_TYPE(model1,tag1);
    CR_GET_TYPE(model2,tag2);
    if(tag1 < tag2)
        return CR_LT;
    if(tag1 > tag2)
        return CR_GT;

    switch(tag1) {
        case CR_FCT_TAG:
            CR_GET_FUNCTOR_SYM(model1,sym1);
            CR_GET_FUNCTOR_SYM(model2,sym2);
            if(CR_IS_AC_SYM(sym1)) {
                if(CR_IS_AC_SYM(sym2)) {
                    if(sym1 < sym2)
                        return CR_LT;
                    if(sym1 > sym2)
                        return CR_GT;
                    CR_GET_AC_FUNCTOR_ATY(model1,aty1);
                    CR_GET_AC_FUNCTOR_ATY(model2,aty2);
                    if(aty1 < aty2)
                        return CR_LT;
                    if(aty1 > aty2)
                        return CR_GT;
                    CR_GET_AC_FUNCTOR_IDX(model1,idx1);
                    CR_GET_AC_FUNCTOR_IDX(model2,idx2);
                    return CR_index_compare(idx1,idx2);
                } else
                    return CR_LT;
            } else if(CR_IS_AC_SYM(sym2))
                return CR_GT;
            aty1 = CR_SYM_ATY(sym1);
            /*
            aty2 = CR_SYM_ATY(sym2);
            if(aty1 < aty2)
                return CR_LT;
            if(aty1 > aty2)
                return CR_GT;
            */
            if(sym1 < sym2)
                return CR_LT;
            if(sym1 > sym2)
                return CR_GT;
            
            for(i = 1; i <= aty1; i++) {
                CR_GET_FUNCTOR_ARG(i,model1,arg1);
                CR_GET_FUNCTOR_ARG(i,model2,arg2);
                cmp = CR_model_compare(arg1,arg2);
                if(cmp != CR_EQ)
                    return cmp;
            }
            return CR_EQ;
        case CR_INT_TAG: case CR_UBI_TAG:
            CR_GET_INT_VAL(model1,int_val1);
            CR_GET_INT_VAL(model2,int_val2);
            if(int_val1 < int_val2)
                return CR_LT;
            if(int_val1 > int_val2)
                return CR_GT;
            return CR_EQ;
        case CR_FLT_TAG:
            CR_GET_FLOAT_VAL(model1,flt_val1);
            CR_GET_FLOAT_VAL(model2,flt_val2);
            if(flt_val1 < flt_val2)
                return CR_LT;
            if(flt_val1 > flt_val2)
                return CR_GT;
            return CR_EQ;
        case CR_STR_TAG:
            CR_GET_STRING_VAL(model1,str_val1);
            CR_GET_STRING_VAL(model2,str_val2);
            cmp = strcmp(str_val1,str_val2);
            if(cmp < 0)
                return CR_LT;
            if(cmp > 0)
                return CR_GT;
            return CR_EQ;
        case CR_VAR_TAG:
            CR_GET_VAR_VAL(model1,var_val1);
            CR_GET_VAR_VAL(model2,var_val2);
            if(var_val1 < var_val2)
                return CR_LT;
            if(var_val1 > var_val2)
                return CR_GT;
            return CR_EQ;
        case CR_FGN_TAG:
            CR_GET_FOREIGN_VAL(model1,fgn_val1);
            CR_GET_FOREIGN_VAL(model2,fgn_val2);
            if(fgn_val1 < fgn_val2)
                return CR_LT;
            if(fgn_val1 > fgn_val2)
                return CR_GT;
            return CR_EQ;
        default:
            CR_index_abort();
    }
}

MR_Integer CR_model_strict_compare(MR_Word model1,MR_Word model2)
{
    MR_Integer cmp;

    if((MR_Unsigned)model1 == (MR_Unsigned)model2)
        return CR_EQ;
    
    cmp = CR_model_compare(model1,model2);
    if(cmp != CR_EQ)
        return cmp;

    if((MR_Unsigned)model1 < (MR_Unsigned)model2)
        return CR_LT;
    return CR_GT;
}

MR_Bool CR_index_find_first_node(MR_Word idx,MR_Word lookup,MR_Word *next,
    MR_Word *node,MR_Integer *pos)
{
    MR_Integer cmp;

    while(1) {
        switch(MR_tag(idx)) {
            case CR_EMPTY_TAG:
                return MR_FALSE;
            case CR_2_TAG:
                cmp = CR_lookup_execute(lookup,next,CR_2_NODE(idx)->k[0]);
                switch(cmp) {
                    case CR_LT:
                        idx = CR_2_NODE(idx)->t[0];
                        break;
                    case CR_GT:
                        idx = CR_2_NODE(idx)->t[1];
                        break;
                    default:
                        *node = idx;
                        *pos  = 0;
                        return MR_TRUE;
                }
                break;
            case CR_3_TAG:
                cmp = CR_lookup_execute(lookup,next,CR_3_NODE(idx)->k[0]);
                switch(cmp) {
                    case CR_LT:
                        idx = CR_3_NODE(idx)->t[0];
                        break;
                    case CR_GT:
                        cmp = CR_lookup_execute(lookup,next,
                            CR_3_NODE(idx)->k[1]);
                        switch(cmp) {
                            case CR_LT:
                                idx = CR_3_NODE(idx)->t[1];
                                break;
                            case CR_GT:
                                idx = CR_3_NODE(idx)->t[2];
                                break;
                            default:
                                *node = idx;
                                *pos  = 1;
                                return MR_TRUE;
                        }
                        break;
                    default:
                        *node = idx;
                        *pos  = 0;
                        return MR_TRUE;
                }
                break;
            case CR_4_TAG:
                cmp = CR_lookup_execute(lookup,next,CR_4_NODE(idx)->k[0]);
                switch(cmp) {
                    case CR_LT:
                        idx = CR_4_NODE(idx)->t[0];
                        break;
                    case CR_GT:
                        cmp = CR_lookup_execute(lookup,next,
                            CR_4_NODE(idx)->k[1]);
                        switch(cmp) {
                            case CR_LT:
                                idx = CR_4_NODE(idx)->t[1];
                                break;
                            case CR_GT:
                                cmp = CR_lookup_execute(lookup,next,
                                    CR_4_NODE(idx)->k[2]);
                                switch(cmp) {
                                    case CR_LT:
                                        idx = CR_4_NODE(idx)->t[2];
                                        break;
                                    case CR_GT:
                                        idx = CR_4_NODE(idx)->t[3];
                                        break;
                                    default:
                                        *node = idx;
                                        *pos  = 2;
                                        return MR_TRUE;
                                }
                                break;
                            default:
                                *node = idx;
                                *pos  = 1;
                                return MR_TRUE;
                        }
                        break;
                    default:
                        *node = idx;
                        *pos  = 0;
                        return MR_TRUE;
                }
                break;
        }
    }
}

MR_Bool CR_iterator_get_least(CR_iterator itr,MR_Word *least)
{
    MR_Word node;
    MR_Integer pos;

    CR_ITR_POP(itr,pos,node);
    while(1) {
        switch(MR_tag(node)) {
            case CR_EMPTY_TAG:
                if(CR_ITR_IS_EMPTY(itr))
                    return MR_FALSE;
                CR_ITR_POP(itr,pos,node);
                break;
            case CR_2_TAG:
                if(pos) {
                    CR_ITR_PUSH(itr,0,CR_2_NODE(node)->t[1]);
                    *least = CR_2_NODE(node)->k[0];
                    return MR_TRUE;
                } else {
                    CR_ITR_PUSH(itr,1,node);
                    node = CR_2_NODE(node)->t[0];
                    break;
                }
            case CR_3_TAG:
                switch(pos) {
                    case 0:
                        CR_ITR_PUSH(itr,1,node);
                        node = CR_3_NODE(node)->t[0];
                        break;
                    case 1:
                        CR_ITR_PUSH(itr,2,node);
                        CR_ITR_PUSH(itr,0,CR_3_NODE(node)->t[1]);
                        *least = CR_3_NODE(node)->k[0];
                        return MR_TRUE;
                    case 2:
                        CR_ITR_PUSH(itr,0,CR_3_NODE(node)->t[2]);
                        *least = CR_3_NODE(node)->k[1];
                        return MR_TRUE;
                }
                break;
            case CR_4_TAG:
                switch(pos) {
                    case 0:
                        CR_ITR_PUSH(itr,1,node);
                        node = CR_4_NODE(node)->t[0];
                        break;
                    case 1: case 2:
                        CR_ITR_PUSH(itr,pos+1,node);
                        CR_ITR_PUSH(itr,0,CR_4_NODE(node)->t[pos]);
                        *least = CR_4_NODE(node)->k[pos-1];
                        return MR_TRUE;
                    case 3:
                        CR_ITR_PUSH(itr,0,CR_4_NODE(node)->t[3]);
                        *least = CR_4_NODE(node)->k[2];
                        return MR_TRUE;
                }
                break;
        }
    }
}

MR_Bool CR_iterator_get_next(CR_iterator itr,MR_Word lookup,MR_Word *next,
    MR_Integer *id)
{
    MR_Word node;
    MR_Integer cmp, pos;

    CR_ITR_POP(itr,pos,node);
    while(1) {
        switch(MR_tag(node)) {
            case CR_EMPTY_TAG:
                if(CR_ITR_IS_EMPTY(itr))
                    return MR_FALSE;
                CR_ITR_POP(itr,pos,node);
                break;
            case CR_2_TAG:
                cmp = CR_lookup_execute(lookup,next,CR_2_NODE(node)->k[0]);
                switch(cmp) {
                    case CR_EQ:
                        *next = CR_2_NODE(node)->k[0];
                        *id   = CR_ID(pos,node);
                        CR_ITR_PUSH(itr,0,CR_2_NODE(node)->t[0]);
                        CR_ITR_PUSH(itr,0,CR_2_NODE(node)->t[1]);
                        return MR_TRUE;
                    case CR_LT:
                        node = CR_2_NODE(node)->t[0];
                        break;
                    case CR_GT:
                        node = CR_2_NODE(node)->t[1];
                        break;
                }
                break;
            case CR_3_TAG:
                cmp = CR_lookup_execute(lookup,next,CR_3_NODE(node)->k[pos]);
                switch(cmp) {
                    case CR_EQ:
                        *next = CR_3_NODE(node)->k[pos];
                        *id   = CR_ID(pos,node);
                        CR_ITR_PUSH(itr,0,CR_3_NODE(node)->t[pos]);
                        if(pos != 1)
                            CR_ITR_PUSH(itr,1,node);
                        else
                            CR_ITR_PUSH(itr,0,CR_3_NODE(node)->t[2]);
                        return MR_TRUE;
                    case CR_LT:
                        node = CR_3_NODE(node)->t[pos];
                        pos = 0;
                        break;
                    case CR_GT:
                        if(pos != 1) 
                            pos++;
                        else {
                            node = CR_3_NODE(node)->t[2];
                            pos = 0;
                        }
                        break;
                }
                break;
            case CR_4_TAG:
                cmp = CR_lookup_execute(lookup,next,CR_4_NODE(node)->k[pos]);
                switch(cmp) {
                    case CR_EQ:
                        *next = CR_4_NODE(node)->k[pos];
                        *id   = CR_ID(pos,node);
                        CR_ITR_PUSH(itr,0,CR_4_NODE(node)->t[pos]);
                        if(pos != 2)
                            CR_ITR_PUSH(itr,pos+1,node);
                        else
                            CR_ITR_PUSH(itr,0,CR_4_NODE(node)->t[3]);
                        return MR_TRUE;
                    case CR_LT:
                        node = CR_4_NODE(node)->t[pos];
                        pos = 0;
                        break;
                    case CR_GT:
                        if(pos != 2)
                            pos++;
                        else {
                            node = CR_4_NODE(node)->t[3];
                            pos = 0;
                        }
                        break;
                }
                break;
        }
    }
}

MR_Word CR_index_singleton(MR_Word model)
{
    MR_Word idx;

    CR_MAKE_2_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,model,idx);
    return idx;
}

void CR_index_print_node(MR_Word node) {

    switch(MR_tag(node)) {
        case CR_EMPTY_TAG:
            printf("empty");
            return;
        case CR_2_TAG:
            printf("two(");
            CR_write(CR_2_NODE(node)->k[0]);
            printf(")");
            return;
        case CR_3_TAG:
            printf("three(");
            CR_write(CR_3_NODE(node)->k[0]);
            printf(",");
            CR_write(CR_3_NODE(node)->k[1]);
            printf(")");
            return;
        case CR_4_TAG:
            printf("four(");
            CR_write(CR_4_NODE(node)->k[0]);
            printf(",");
            CR_write(CR_4_NODE(node)->k[1]);
            printf(",");
            CR_write(CR_4_NODE(node)->k[2]);
            printf(")");
            return;
        default:
            printf("error");
            return;
    }
}

void CR_index_print_index(MR_Word idx) {
    
    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            printf("empty");
            return;
        case CR_2_TAG:
            printf("two(");
            CR_index_print_index(CR_2_NODE(idx)->t[0]);
            putchar(',');
            CR_write(CR_2_NODE(idx)->k[0]);
            /*putchar('#');
            printf("%lx",CR_2_NODE(idx)->k[0]);*/
            putchar(',');
            CR_index_print_index(CR_2_NODE(idx)->t[1]);
            putchar(')');
            return;
        case CR_3_TAG:
            printf("three(");
            CR_index_print_index(CR_3_NODE(idx)->t[0]);
            putchar(',');
            CR_write(CR_3_NODE(idx)->k[0]);
            /*putchar('#');
            printf("%lx",CR_3_NODE(idx)->k[0]);*/
            putchar(',');
            CR_index_print_index(CR_3_NODE(idx)->t[1]);
            putchar(',');
            CR_write(CR_3_NODE(idx)->k[1]);
            /*putchar('#');
            printf("%lx",CR_3_NODE(idx)->k[1]);*/
            putchar(',');
            CR_index_print_index(CR_3_NODE(idx)->t[2]);
            putchar(')');
            return;
        case CR_4_TAG:
            printf("four(");
            CR_index_print_index(CR_4_NODE(idx)->t[0]);
            putchar(',');
            CR_write(CR_4_NODE(idx)->k[0]);
            /*putchar('#');
            printf("%lx",CR_4_NODE(idx)->k[0]);*/
            putchar(',');
            CR_index_print_index(CR_4_NODE(idx)->t[1]);
            putchar(',');
            CR_write(CR_4_NODE(idx)->k[1]);
            /*putchar('#');
            printf("%lx",CR_4_NODE(idx)->k[1]);*/
            putchar(',');
            CR_index_print_index(CR_4_NODE(idx)->t[2]);
            putchar(',');
            CR_write(CR_4_NODE(idx)->k[2]);
            /*putchar('#');
            printf("%lx",CR_4_NODE(idx)->k[2]);*/
            putchar(',');
            CR_index_print_index(CR_4_NODE(idx)->t[3]);
            putchar(')');
            return;
        default:
            printf("error");
            return;
    }
}

MR_Word CR_index_insert(MR_Word idx,MR_Word model)
{
    MR_Integer cmp;
    MR_Word lnode, rnode;

    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            CR_MAKE_2_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,model,idx);
            return idx;
        case CR_2_TAG:
            return CR_index_2_insert(idx,model);
        case CR_3_TAG:
            return CR_index_3_insert(idx,model);
        case CR_4_TAG:
            cmp = CR_model_strict_compare(model,CR_4_NODE(idx)->k[1]);
            CR_SPLIT_4_NODE(idx,lnode,rnode);
            switch(cmp) {
                case CR_LT:
                    lnode = CR_index_2_insert(lnode,model);
                    break;
                default:
                    rnode = CR_index_2_insert(rnode,model);
                    break;
            }
            CR_MAKE_2_NODE(lnode,rnode,CR_4_NODE(idx)->k[1],idx);
            return idx;
    }
    CR_index_abort();
}

MR_Word CR_index_2_insert(MR_Word idx,MR_Word model)
{
    MR_Integer cmp, pos;
    MR_Word node, lnode, rnode;

    cmp = CR_model_strict_compare(model,CR_2_NODE(idx)->k[0]);
    if(cmp == CR_LT)
        pos = 0;
    else
        pos = 1;
    node = CR_2_NODE(idx)->t[pos];
    switch(MR_tag(node)) {
        case CR_4_TAG:
            cmp = CR_model_strict_compare(model,CR_4_NODE(node)->k[1]);
            CR_SPLIT_4_NODE(node,lnode,rnode);
            switch(cmp) {
                case CR_LT:
                    lnode = CR_index_2_insert(lnode,model);
                    break;
                default:
                    rnode = CR_index_2_insert(rnode,model);
                    break;
            }
            if(pos)
                CR_MAKE_3_NODE(CR_2_NODE(idx)->t[0],lnode,rnode,
                    CR_2_NODE(idx)->k[0],CR_4_NODE(node)->k[1],node);
            else
                CR_MAKE_3_NODE(lnode,rnode,CR_2_NODE(idx)->t[1],
                    CR_4_NODE(node)->k[1],CR_2_NODE(idx)->k[0],node);
            break;
        case CR_3_TAG:
            node = CR_index_3_insert(node,model);
            if(pos)
                CR_MAKE_2_NODE(CR_2_NODE(idx)->t[0],node,CR_2_NODE(idx)->k[0],
                    node);
            else
                CR_MAKE_2_NODE(node,CR_2_NODE(idx)->t[1],CR_2_NODE(idx)->k[0],
                    node);
            break;
        case CR_2_TAG:
            node = CR_index_2_insert(node,model);
            if(pos)
                CR_MAKE_2_NODE(CR_2_NODE(idx)->t[0],node,CR_2_NODE(idx)->k[0],
                    node);
            else
                CR_MAKE_2_NODE(node,CR_2_NODE(idx)->t[1],CR_2_NODE(idx)->k[0],
                    node);
            break;
        case CR_EMPTY_TAG:
            if(pos)
                CR_MAKE_3_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,CR_EMPTY_NODE,
                    CR_2_NODE(idx)->k[0],model,node);
            else
                CR_MAKE_3_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,CR_EMPTY_NODE,
                    model,CR_2_NODE(idx)->k[0],node);
            break;
    }
    return node;
}

MR_Word CR_index_3_insert(MR_Word idx,MR_Word model)
{
    MR_Integer cmp, pos;
    MR_Word node, lnode, rnode;

    cmp = CR_model_strict_compare(model,CR_3_NODE(idx)->k[0]);
    switch(cmp) {
        case CR_LT:
            pos = 0;
            break;
        case CR_EQ:
            pos = 1;
            break;
        default:
            cmp = CR_model_strict_compare(model,CR_3_NODE(idx)->k[1]);
            if(cmp == CR_LT)
                pos = 1;
            else
                pos = 2;
            break;
    }
    node = CR_3_NODE(idx)->t[pos];
    switch(MR_tag(node)) {
        case CR_4_TAG:
            cmp = CR_model_strict_compare(model,CR_4_NODE(node)->k[1]);
            CR_SPLIT_4_NODE(node,lnode,rnode);
            switch(cmp) {
                case CR_LT:
                    lnode = CR_index_2_insert(lnode,model);
                    break;
                default:
                    rnode = CR_index_2_insert(rnode,model);
                    break;
            }
            switch(pos) {
                case 0:
                    CR_MAKE_4_NODE(lnode,rnode,CR_3_NODE(idx)->t[1],
                        CR_3_NODE(idx)->t[2],CR_4_NODE(node)->k[1],
                        CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],node);
                    break;
                case 1:
                    CR_MAKE_4_NODE(CR_3_NODE(idx)->t[0],lnode,rnode,
                        CR_3_NODE(idx)->t[2],CR_3_NODE(idx)->k[0],
                        CR_4_NODE(node)->k[1],CR_3_NODE(idx)->k[1],node);
                    break;
                case 2:
                    CR_MAKE_4_NODE(CR_3_NODE(idx)->t[0],CR_3_NODE(idx)->t[1],
                        lnode,rnode,CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],
                        CR_4_NODE(node)->k[1],node);
                    break;
            }
            return node;
        case CR_3_TAG:
            node = CR_index_3_insert(node,model);
            CR_MAKE_3_NODE(CR_3_NODE(idx)->t[0],CR_3_NODE(idx)->t[1],
                CR_3_NODE(idx)->t[2],CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],
                idx);
            CR_3_NODE(idx)->t[pos] = node;
            return idx;
        case CR_2_TAG:
            node = CR_index_2_insert(node,model);
            CR_MAKE_3_NODE(CR_3_NODE(idx)->t[0],CR_3_NODE(idx)->t[1],
                CR_3_NODE(idx)->t[2],CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],
                idx);
            CR_3_NODE(idx)->t[pos] = node;
            return idx;
        case CR_EMPTY_TAG:
            switch(pos) {
                case 0:
                    CR_MAKE_4_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,CR_EMPTY_NODE,
                        CR_EMPTY_NODE,model,CR_3_NODE(idx)->k[0],
                        CR_3_NODE(idx)->k[1],node);
                    break;
                case 1:
                    CR_MAKE_4_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,CR_EMPTY_NODE,
                        CR_EMPTY_NODE,CR_3_NODE(idx)->k[0],model,
                        CR_3_NODE(idx)->k[1],node);
                    break;
                case 2:
                    CR_MAKE_4_NODE(CR_EMPTY_NODE,CR_EMPTY_NODE,CR_EMPTY_NODE,
                        CR_EMPTY_NODE,CR_3_NODE(idx)->k[0],
                        CR_3_NODE(idx)->k[1],model,node);
                    break;
            }
            return node;
    }
    CR_index_abort();
}

MR_Word CR_index_delete(MR_Word idx,MR_Word model,MR_Bool *red)
{
    MR_Integer cmp;
    MR_Word node, least;
    MR_Bool red0;

    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            CR_index_abort();
        case CR_2_TAG:
            cmp = CR_model_strict_compare(model,CR_2_NODE(idx)->k[0]);
            switch(cmp) {
                case CR_LT: 
                    node = CR_index_delete(CR_2_NODE(idx)->t[0],model,&red0);
                    if(red0)
                        node = CR_fix_2_t0(CR_2_NODE(idx)->k[0],node,
                            CR_2_NODE(idx)->t[1],red);
                    else {
                        CR_MAKE_2_NODE(node,CR_2_NODE(idx)->t[1],
                            CR_2_NODE(idx)->k[0],node);
                        *red = MR_FALSE;
                    }
                    return node;
                case CR_EQ:
                    if(CR_2_NODE(idx)->t[1] == CR_EMPTY_NODE) {
                        *red = MR_TRUE;
                        return CR_2_NODE(idx)->t[0];
                    }
                    node = CR_index_delete_least(CR_2_NODE(idx)->t[1],
                        &least,&red0);
                    if(red0)
                        node = CR_fix_2_t1(least,CR_2_NODE(idx)->t[0],node,
                            red);
                    else {
                        CR_MAKE_2_NODE(CR_2_NODE(idx)->t[0],node,least,node);
                        *red = MR_FALSE;
                    }
                    return node;
                case CR_GT:
                    node = CR_index_delete(CR_2_NODE(idx)->t[1],model,&red0);
                    if(red0)
                        node = CR_fix_2_t1(CR_2_NODE(idx)->k[0],
                            CR_2_NODE(idx)->t[0],node,red);
                    else {
                        CR_MAKE_2_NODE(CR_2_NODE(idx)->t[0],node,
                            CR_2_NODE(idx)->k[0],node);
                        *red = MR_FALSE;
                    }
                    return node;
            }
        case CR_3_TAG:
            cmp = CR_model_strict_compare(model,CR_3_NODE(idx)->k[0]);
            switch(cmp) {
                case CR_LT:
                    node = CR_index_delete(CR_3_NODE(idx)->t[0],model,&red0);
                    if(red0)
                        node = CR_fix_3_t0(CR_3_NODE(idx)->k[0],
                            CR_3_NODE(idx)->k[1],node,CR_3_NODE(idx)->t[1],
                            CR_3_NODE(idx)->t[2],red);
                    else {
                        CR_MAKE_3_NODE(node,CR_3_NODE(idx)->t[1],
                            CR_3_NODE(idx)->t[2],CR_3_NODE(idx)->k[0],
                            CR_3_NODE(idx)->k[1],node);
                        *red = MR_FALSE;
                    }
                    return node;
                case CR_EQ:
                    if(CR_3_NODE(idx)->t[1] == CR_EMPTY_NODE) {
                        *red = MR_FALSE;
                        CR_MAKE_2_NODE(CR_3_NODE(idx)->t[0],
                            CR_3_NODE(idx)->t[2],CR_3_NODE(idx)->k[1],node);
                        return node;
                    }
                    node = CR_index_delete_least(CR_3_NODE(idx)->t[1],
                        &least,&red0);
                    if(red0)
                        node = CR_fix_3_t1(least,CR_3_NODE(idx)->k[1],
                            CR_3_NODE(idx)->t[0],node,CR_3_NODE(idx)->t[2],
                            red);
                    else {
                        CR_MAKE_3_NODE(CR_3_NODE(idx)->t[0],node,
                            CR_3_NODE(idx)->t[2],least,CR_3_NODE(idx)->k[1],
                            node);
                        *red = MR_FALSE;
                    }
                    return node;
                case CR_GT:
                    cmp = CR_model_strict_compare(model,CR_3_NODE(idx)->k[1]);
                    switch(cmp) {
                        case CR_LT:
                            node = CR_index_delete(CR_3_NODE(idx)->t[1],model,
                                &red0);
                            if(red0)
                                node = CR_fix_3_t1(CR_3_NODE(idx)->k[0],
                                    CR_3_NODE(idx)->k[1],CR_3_NODE(idx)->t[0],
                                    node,CR_3_NODE(idx)->t[2],red);
                            else {
                                CR_MAKE_3_NODE(CR_3_NODE(idx)->t[0],
                                    node,CR_3_NODE(idx)->t[2],
                                    CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],
                                    node);
                                *red = MR_FALSE;
                            }
                            return node;
                        case CR_EQ:
                            if(CR_3_NODE(idx)->t[2] == CR_EMPTY_NODE) {
                                *red = MR_FALSE;
                                CR_MAKE_2_NODE(CR_3_NODE(idx)->t[0],
                                    CR_3_NODE(idx)->t[1],CR_3_NODE(idx)->k[0],
                                    node);
                                return node;
                            }
                            node = CR_index_delete_least(CR_3_NODE(idx)->t[2],
                                &least,&red0);
                            if(red0)
                                node = CR_fix_3_t2(CR_3_NODE(idx)->k[0],
                                    least,CR_3_NODE(idx)->t[0],
                                    CR_3_NODE(idx)->t[1],node,red);
                            else {
                                CR_MAKE_3_NODE(CR_3_NODE(idx)->t[0],
                                    CR_3_NODE(idx)->t[1],node,
                                    CR_3_NODE(idx)->k[0],least,node);
                                *red = MR_FALSE;
                            }
                            return node;
                        case CR_GT:
                            node = CR_index_delete(CR_3_NODE(idx)->t[2],model,
                                &red0);
                            if(red0)
                                node = CR_fix_3_t2(CR_3_NODE(idx)->k[0],
                                    CR_3_NODE(idx)->k[1],CR_3_NODE(idx)->t[0],
                                    CR_3_NODE(idx)->t[1],node,red);
                            else {
                                CR_MAKE_3_NODE(CR_3_NODE(idx)->t[0],
                                    CR_3_NODE(idx)->t[1],node,
                                    CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],
                                    node);
                                *red = MR_FALSE;
                            }
                            return node;
                    }
            }
        case CR_4_TAG:
            cmp = CR_model_strict_compare(model,CR_4_NODE(idx)->k[0]);
            switch(cmp) {
                case CR_LT:
                    node = CR_index_delete(CR_4_NODE(idx)->t[0],model,&red0);
                    if(red0)
                        node = CR_fix_4_t0(CR_4_NODE(idx)->k[0],
                            CR_4_NODE(idx)->k[1],CR_4_NODE(idx)->k[2],
                            node,CR_4_NODE(idx)->t[1],CR_4_NODE(idx)->t[2],
                            CR_4_NODE(idx)->t[3],red);
                    else {
                        CR_MAKE_4_NODE(node,CR_4_NODE(idx)->t[1],
                            CR_4_NODE(idx)->t[2],CR_4_NODE(idx)->t[3],
                            CR_4_NODE(idx)->k[0],CR_4_NODE(idx)->k[1],
                            CR_4_NODE(idx)->k[2],node);
                        *red = MR_FALSE;
                    }
                    return node;
                case CR_EQ:
                    if(CR_4_NODE(idx)->t[1] == CR_EMPTY_NODE) {
                        CR_MAKE_3_NODE(CR_4_NODE(idx)->t[0],
                            CR_4_NODE(idx)->t[2],CR_4_NODE(idx)->t[3],
                            CR_4_NODE(idx)->k[1],CR_4_NODE(idx)->k[2],
                            node);
                        *red = MR_FALSE;
                        return node;
                    }
                    node = CR_index_delete_least(CR_4_NODE(idx)->t[1],&least,
                        &red0);
                    if(red0)
                        node = CR_fix_4_t1(least,CR_4_NODE(idx)->k[1],
                            CR_4_NODE(idx)->k[2],CR_4_NODE(idx)->t[0],node,
                            CR_4_NODE(idx)->t[2],CR_4_NODE(idx)->t[3],red);
                    else {
                        CR_MAKE_4_NODE(CR_4_NODE(idx)->t[0],node,
                            CR_4_NODE(idx)->t[2],CR_4_NODE(idx)->t[3],least,
                            CR_4_NODE(idx)->k[1],CR_4_NODE(idx)->k[2],node);
                        *red = MR_FALSE;
                    }
                    return node;
                case CR_GT:
                    cmp = CR_model_strict_compare(model,CR_4_NODE(idx)->k[1]);
                    switch(cmp) {
                        case CR_LT:
                            node = CR_index_delete(CR_4_NODE(idx)->t[1],model,
                                &red0);
                            if(red0)
                                node = CR_fix_4_t1(CR_4_NODE(idx)->k[0],
                                    CR_4_NODE(idx)->k[1],CR_4_NODE(idx)->k[2],
                                    CR_4_NODE(idx)->t[0],node,
                                    CR_4_NODE(idx)->t[2],CR_4_NODE(idx)->t[3],
                                    red);
                            else {
                                CR_MAKE_4_NODE(CR_4_NODE(idx)->t[0],
                                    node,CR_4_NODE(idx)->t[2],
                                    CR_4_NODE(idx)->t[3],CR_4_NODE(idx)->k[0],
                                    CR_4_NODE(idx)->k[1],CR_4_NODE(idx)->k[2],
                                    node);
                                *red = MR_FALSE;
                            }
                            return node;
                        case CR_EQ:
                            if(CR_4_NODE(idx)->t[2] == CR_EMPTY_NODE) {
                                CR_MAKE_3_NODE(CR_4_NODE(idx)->t[0],
                                    CR_4_NODE(idx)->t[1],CR_4_NODE(idx)->t[3],
                                    CR_4_NODE(idx)->k[0],CR_4_NODE(idx)->k[2],
                                    node);
                                *red = MR_FALSE;
                                return node;
                            }
                            node = CR_index_delete_least(CR_4_NODE(idx)->t[2],
                                &least,&red0);
                            if(red0)
                                node = CR_fix_4_t2(CR_4_NODE(idx)->k[0],
                                    least,CR_4_NODE(idx)->k[2],
                                    CR_4_NODE(idx)->t[0],CR_4_NODE(idx)->t[1],
                                    node,CR_4_NODE(idx)->t[3],red);
                            else {
                                CR_MAKE_4_NODE(CR_4_NODE(idx)->t[0],
                                    CR_4_NODE(idx)->t[1],node,
                                    CR_4_NODE(idx)->t[3],CR_4_NODE(idx)->k[0],
                                    least,CR_4_NODE(idx)->k[2],node);
                                *red = MR_FALSE;
                            }
                            return node;
                        case CR_GT:
                            cmp = CR_model_strict_compare(model,
                                CR_4_NODE(idx)->k[2]);
                            switch(cmp) {
                                case CR_LT:
                                    node = CR_index_delete(CR_4_NODE(idx)->t[2],
                                        model,&red0);
                                    if(red0)
                                        node = CR_fix_4_t2(CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],
                                            CR_4_NODE(idx)->k[2],
                                            CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],node,
                                            CR_4_NODE(idx)->t[3],red);
                                    else {
                                        CR_MAKE_4_NODE(CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],node,
                                            CR_4_NODE(idx)->t[3],
                                            CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],
                                            CR_4_NODE(idx)->k[2],node);
                                        *red = MR_FALSE;
                                    }
                                    return node;
                                case CR_EQ:
                                    if(CR_4_NODE(idx)->t[3] == CR_EMPTY_NODE) {
                                        CR_MAKE_3_NODE(CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],
                                            CR_4_NODE(idx)->t[2],
                                            CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],node);
                                        *red = MR_FALSE;
                                        return node;
                                    }
                                    node = CR_index_delete_least(
                                        CR_4_NODE(idx)->t[3],&least,&red0);
                                    if(red0)
                                        node = CR_fix_4_t3(CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],least,
                                            CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],
                                            CR_4_NODE(idx)->t[2],node,red);
                                    else {
                                        CR_MAKE_4_NODE(CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],
                                            CR_4_NODE(idx)->t[2],node,
                                            CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],least,node);
                                        *red = MR_FALSE;
                                    }
                                    return node;
                                case CR_GT:
                                    node = CR_index_delete(CR_4_NODE(idx)->t[3],
                                        model,&red0);
                                    if(red0)
                                        node = CR_fix_4_t3(CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],
                                            CR_4_NODE(idx)->k[2],
                                            CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],
                                            CR_4_NODE(idx)->t[2],node,red);
                                    else {
                                        CR_MAKE_4_NODE(CR_4_NODE(idx)->t[0],
                                            CR_4_NODE(idx)->t[1],
                                            CR_4_NODE(idx)->t[2],node,
                                            CR_4_NODE(idx)->k[0],
                                            CR_4_NODE(idx)->k[1],
                                            CR_4_NODE(idx)->k[2],node);
                                        *red = MR_FALSE;
                                    }
                                    return node;
                            }

                    }
            }
    }
    CR_index_abort();
}

MR_Word CR_index_delete_least(MR_Word idx,MR_Word *least,MR_Bool *red)
{
    MR_Word node;
    MR_Bool red0;

    switch(MR_tag(idx)) {
        case CR_2_TAG:
            if(CR_2_NODE(idx)->t[0] == CR_EMPTY_NODE) {
                *least = CR_2_NODE(idx)->k[0];
                *red = MR_TRUE;
                return CR_2_NODE(idx)->t[1];
            }
            node = CR_index_delete_least(CR_2_NODE(idx)->t[0],least,&red0);
            if(red0)
                node = CR_fix_2_t0(CR_2_NODE(idx)->k[0],node,
                    CR_2_NODE(idx)->t[1],red);
            else {
                CR_MAKE_2_NODE(node,CR_2_NODE(idx)->t[1],CR_2_NODE(idx)->k[0],
                    node);
                *red = MR_FALSE;
            }
            return node;
        case CR_3_TAG:
            if(CR_3_NODE(idx)->t[0] == CR_EMPTY_NODE) {
                *least = CR_3_NODE(idx)->k[0];
                *red = MR_FALSE;
                CR_MAKE_2_NODE(CR_3_NODE(idx)->t[1],CR_3_NODE(idx)->t[2],
                    CR_3_NODE(idx)->k[1],node);
                return node;
            }
            node = CR_index_delete_least(CR_3_NODE(idx)->t[0],least,&red0);
            if(red0)
                node = CR_fix_3_t0(CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],
                    node,CR_3_NODE(idx)->t[1],CR_3_NODE(idx)->t[2],red);
            else {
                CR_MAKE_3_NODE(node,CR_3_NODE(idx)->t[1],CR_3_NODE(idx)->t[2],
                    CR_3_NODE(idx)->k[0],CR_3_NODE(idx)->k[1],node);
                *red = MR_FALSE;
            }
            return node;
        case CR_4_TAG:
            if(CR_4_NODE(idx)->t[0] == CR_EMPTY_NODE) {
                *least = CR_4_NODE(idx)->k[0];
                *red = MR_FALSE;
                CR_MAKE_3_NODE(CR_4_NODE(idx)->t[1],CR_4_NODE(idx)->t[2],
                    CR_4_NODE(idx)->t[3],CR_4_NODE(idx)->k[1],
                    CR_4_NODE(idx)->k[2],node);
                return node;
            }
            node = CR_index_delete_least(CR_4_NODE(idx)->t[0],least,&red0);
            if(red0)
                node = CR_fix_4_t0(CR_4_NODE(idx)->k[0],CR_4_NODE(idx)->k[1],
                    CR_4_NODE(idx)->k[2],node,CR_4_NODE(idx)->t[1],
                    CR_4_NODE(idx)->t[2],CR_4_NODE(idx)->t[3],red);
            else {
                CR_MAKE_4_NODE(node,CR_4_NODE(idx)->t[1],CR_4_NODE(idx)->t[2],
                    CR_4_NODE(idx)->t[3],CR_4_NODE(idx)->k[0],
                    CR_4_NODE(idx)->k[1],CR_4_NODE(idx)->k[2],node);
                *red = MR_FALSE;
            }
            return node;
    }
    CR_index_abort();
}

MR_Word CR_fix_2_t0(MR_Word k0,MR_Word t0,MR_Word t1,MR_Bool *red)
{
    MR_Word new_t1, node;
    
    switch(MR_tag(t1)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t1)->t[1],CR_4_NODE(t1)->t[2],
                CR_4_NODE(t1)->t[3],CR_4_NODE(t1)->k[1],CR_4_NODE(t1)->k[2],
                new_t1);
            CR_MAKE_2_NODE(t0,CR_4_NODE(t1)->t[0],k0,node);
            CR_MAKE_2_NODE(node,new_t1,CR_4_NODE(t1)->k[0],node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t1)->t[1],CR_3_NODE(t1)->t[2],
                CR_3_NODE(t1)->k[1],new_t1);
            CR_MAKE_2_NODE(t0,CR_3_NODE(t1)->t[0],k0,node);
            CR_MAKE_2_NODE(node,new_t1,CR_3_NODE(t1)->k[0],node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(t0,CR_2_NODE(t1)->t[0],CR_2_NODE(t1)->t[1],
                k0,CR_2_NODE(t1)->k[0],node);
            *red = MR_TRUE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_2_t0: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_2_t1(MR_Word k0,MR_Word t0,MR_Word t1,MR_Bool *red)
{
    MR_Word new_t0, node;

    switch(MR_tag(t0)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t0)->t[0],CR_4_NODE(t0)->t[1],
                CR_4_NODE(t0)->t[2],CR_4_NODE(t0)->k[0],CR_4_NODE(t0)->k[1],
                new_t0);
            CR_MAKE_2_NODE(CR_4_NODE(t0)->t[3],t1,k0,node);
            CR_MAKE_2_NODE(new_t0,node,CR_4_NODE(t0)->k[2],node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t0)->t[0],CR_3_NODE(t0)->t[1],
                CR_3_NODE(t0)->k[0],new_t0);
            CR_MAKE_2_NODE(CR_3_NODE(t0)->t[2],t1,k0,node);
            CR_MAKE_2_NODE(new_t0,node,CR_3_NODE(t0)->k[1],node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(CR_2_NODE(t0)->t[0],CR_2_NODE(t0)->t[1],t1,
                CR_2_NODE(t0)->k[0],k0,node);
            *red = MR_TRUE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_2_t1: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_3_t0(MR_Word k0,MR_Word k1,MR_Word t0,MR_Word t1,MR_Word t2,
    MR_Bool *red)
{
    MR_Word new_t1, node;

    switch(MR_tag(t1)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t1)->t[1],CR_4_NODE(t1)->t[2],
                CR_4_NODE(t1)->t[3],CR_4_NODE(t1)->k[1],CR_4_NODE(t1)->k[2],
                new_t1);
            CR_MAKE_2_NODE(t0,CR_4_NODE(t1)->t[0],k0,node);
            CR_MAKE_3_NODE(node,new_t1,t2,CR_4_NODE(t1)->k[0],k1,node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t1)->t[1],CR_3_NODE(t1)->t[2],
                CR_3_NODE(t1)->k[1],new_t1);
            CR_MAKE_2_NODE(t0,CR_3_NODE(t1)->t[0],k0,node);
            CR_MAKE_3_NODE(node,new_t1,t2,CR_3_NODE(t1)->k[0],k1,node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(t0,CR_2_NODE(t1)->t[0],CR_2_NODE(t1)->t[1],
                k0,CR_2_NODE(t1)->k[0],new_t1);
            CR_MAKE_2_NODE(new_t1,t2,k1,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_3_t0: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_3_t1(MR_Word k0,MR_Word k1,MR_Word t0,MR_Word t1,MR_Word t2,
    MR_Bool *red)
{
    MR_Word new_t0, node;

    switch(MR_tag(t0)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t0)->t[0],CR_4_NODE(t0)->t[1],
                CR_4_NODE(t0)->t[2],CR_4_NODE(t0)->k[0],CR_4_NODE(t0)->k[1],
                new_t0);
            CR_MAKE_2_NODE(CR_4_NODE(t0)->t[3],t1,k0,node);
            CR_MAKE_3_NODE(new_t0,node,t2,CR_4_NODE(t0)->k[2],k1,node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t0)->t[0],CR_3_NODE(t0)->t[1],
                CR_3_NODE(t0)->k[0],new_t0);
            CR_MAKE_2_NODE(CR_3_NODE(t0)->t[2],t1,k0,node);
            CR_MAKE_3_NODE(new_t0,node,t2,CR_3_NODE(t0)->k[1],k1,node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(CR_2_NODE(t0)->t[0],CR_2_NODE(t0)->t[1],t1,
                CR_2_NODE(t0)->k[0],k0,new_t0);
            CR_MAKE_2_NODE(new_t0,t2,k1,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_3_t1: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_3_t2(MR_Word k0,MR_Word k1,MR_Word t0,MR_Word t1,MR_Word t2,
    MR_Bool *red)
{
    MR_Word new_t1, node;

    switch(MR_tag(t1)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t1)->t[0],CR_4_NODE(t1)->t[1],
                CR_4_NODE(t1)->t[2],CR_4_NODE(t1)->k[0],CR_4_NODE(t1)->k[1],
                new_t1);
            CR_MAKE_2_NODE(CR_4_NODE(t1)->t[3],t2,k1,node);
            CR_MAKE_3_NODE(t0,new_t1,node,k0,CR_4_NODE(t1)->k[2],node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t1)->t[0],CR_3_NODE(t1)->t[1],
                CR_3_NODE(t1)->k[0],new_t1);
            CR_MAKE_2_NODE(CR_3_NODE(t1)->t[2],t2,k1,node);
            CR_MAKE_3_NODE(t0,new_t1,node,k0,CR_3_NODE(t1)->k[1],node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(CR_2_NODE(t1)->t[0],CR_2_NODE(t1)->t[1],t2,
                CR_2_NODE(t1)->k[0],k1,new_t1);
            CR_MAKE_2_NODE(t0,new_t1,k0,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_3_t2: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }

    return node;
}

MR_Word CR_fix_4_t0(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red)
{
    MR_Word new_t1, node;

    switch(MR_tag(t1)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t1)->t[1],CR_4_NODE(t1)->t[2],
                CR_4_NODE(t1)->t[3],CR_4_NODE(t1)->k[1],CR_4_NODE(t1)->k[2],
                new_t1);
            CR_MAKE_2_NODE(t0,CR_4_NODE(t1)->t[0],k0,node);
            CR_MAKE_4_NODE(node,new_t1,t2,t3,CR_4_NODE(t1)->k[0],k1,k2,node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t1)->t[1],CR_3_NODE(t1)->t[2],
                CR_3_NODE(t1)->k[1],new_t1);
            CR_MAKE_2_NODE(t0,CR_3_NODE(t1)->t[0],k0,node);
            CR_MAKE_4_NODE(node,new_t1,t2,t3,CR_3_NODE(t1)->k[0],k1,k2,node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(t0,CR_2_NODE(t1)->t[0],CR_2_NODE(t1)->t[1],
                k0,CR_2_NODE(t1)->k[0],new_t1);
            CR_MAKE_3_NODE(new_t1,t2,t3,k1,k2,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_4_t0: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_4_t1(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red)
{
    MR_Word new_t2, node;

    switch(MR_tag(t2)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t2)->t[1],CR_4_NODE(t2)->t[2],
                CR_4_NODE(t2)->t[3],CR_4_NODE(t2)->k[1],CR_4_NODE(t2)->k[2],
                new_t2);
            CR_MAKE_2_NODE(t1,CR_4_NODE(t2)->t[0],k1,node);
            CR_MAKE_4_NODE(t0,node,new_t2,t3,k0,CR_4_NODE(t2)->k[0],k2,node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t2)->t[1],CR_3_NODE(t2)->t[2],
                CR_3_NODE(t2)->k[1],new_t2);
            CR_MAKE_2_NODE(t1,CR_3_NODE(t2)->t[0],k1,node);
            CR_MAKE_4_NODE(t0,node,new_t2,t3,k0,CR_3_NODE(t2)->k[0],k2,node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(t1,CR_2_NODE(t2)->t[0],CR_2_NODE(t2)->t[1],
                k1,CR_2_NODE(t2)->k[0],new_t2);
            CR_MAKE_3_NODE(t0,new_t2,t3,k0,k2,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_4_t1: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_4_t2(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red)
{
    MR_Word new_t3, node;

    switch(MR_tag(t3)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t3)->t[1],CR_4_NODE(t3)->t[2],
                CR_4_NODE(t3)->t[3],CR_4_NODE(t3)->k[1],CR_4_NODE(t3)->k[2],
                new_t3);
            CR_MAKE_2_NODE(t2,CR_4_NODE(t3)->t[0],k2,node);
            CR_MAKE_4_NODE(t0,t1,node,new_t3,k0,k1,CR_4_NODE(t3)->k[0],node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t3)->t[1],CR_3_NODE(t3)->t[2],
                CR_3_NODE(t3)->k[1],new_t3);
            CR_MAKE_2_NODE(t2,CR_3_NODE(t3)->t[0],k2,node);
            CR_MAKE_4_NODE(t0,t1,node,new_t3,k0,k1,CR_3_NODE(t3)->k[0],node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(t2,CR_2_NODE(t3)->t[0],CR_2_NODE(t3)->t[1],
                k2,CR_2_NODE(t3)->k[0],new_t3);
            CR_MAKE_3_NODE(t0,t1,new_t3,k0,k1,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_4_t2: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_fix_4_t3(MR_Word k0,MR_Word k1,MR_Word k2,MR_Word t0,MR_Word t1,
    MR_Word t2,MR_Word t3,MR_Bool *red)
{
    MR_Word new_t2, node;

    switch(MR_tag(t2)) {
        case CR_4_TAG:
            CR_MAKE_3_NODE(CR_4_NODE(t2)->t[0],CR_4_NODE(t2)->t[1],
                CR_4_NODE(t2)->t[2],CR_4_NODE(t2)->k[0],CR_4_NODE(t2)->k[1],
                new_t2);
            CR_MAKE_2_NODE(CR_4_NODE(t2)->t[3],t3,k2,node);
            CR_MAKE_4_NODE(t0,t1,new_t2,node,k0,k1,CR_4_NODE(t2)->k[2],node);
            *red = MR_FALSE;
            break;
        case CR_3_TAG:
            CR_MAKE_2_NODE(CR_3_NODE(t2)->t[0],CR_3_NODE(t2)->t[1],
                CR_3_NODE(t2)->k[0],new_t2);
            CR_MAKE_2_NODE(CR_3_NODE(t2)->t[2],t3,k2,node);
            CR_MAKE_4_NODE(t0,t1,new_t2,node,k0,k1,CR_3_NODE(t2)->k[1],node);
            *red = MR_FALSE;
            break;
        case CR_2_TAG:
            CR_MAKE_3_NODE(CR_2_NODE(t2)->t[0],CR_2_NODE(t2)->t[1],t3,
                CR_2_NODE(t2)->k[0],k2,new_t2);
            CR_MAKE_3_NODE(t0,t1,new_t2,k0,k1,node);
            *red = MR_FALSE;
            break;
        case CR_EMPTY_TAG:
            fprintf(stderr,"CR_fix_4_t3: unbalanced tree\n");
            exit(EXIT_FAILURE);
    }
    return node;
}

MR_Word CR_index_singleton_flatten(MR_Word idx)
{
    return CR_2_NODE(idx)->k[0];
}

MR_Word CR_index_merge(MR_Word idx1,MR_Word idx2)
{
    CR_iterator_s itr_s;
    CR_iterator itr = &itr_s;
    MR_Word model;

    CR_ITR_INIT(itr);
    CR_ITR_PUSH(itr,0,idx1);

    while(CR_iterator_get_least(itr,&model))
        idx2 = CR_index_insert(idx2,model);
    return idx2;
}

MR_Word CR_index_set_wakeups(MR_Word idx,MR_Word wakeups)
{
    MR_Word k0, k1, k2;
    MR_Word t0, t1, t2, t3;

    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            return idx;
        case CR_2_TAG:
            k0 = CR_set_wakeups_2(wakeups,CR_2_NODE(idx)->k[0]);
            t0 = CR_index_set_wakeups(CR_2_NODE(idx)->t[0],wakeups);
            t1 = CR_index_set_wakeups(CR_2_NODE(idx)->t[1],wakeups);
            CR_MAKE_2_NODE(t0,t1,k0,idx);
            return idx;
        case CR_3_TAG:
            k0 = CR_set_wakeups_2(wakeups,CR_3_NODE(idx)->k[0]);
            k1 = CR_set_wakeups_2(wakeups,CR_3_NODE(idx)->k[1]);
            t0 = CR_index_set_wakeups(CR_3_NODE(idx)->t[0],wakeups);
            t1 = CR_index_set_wakeups(CR_3_NODE(idx)->t[1],wakeups);
            t2 = CR_index_set_wakeups(CR_3_NODE(idx)->t[2],wakeups);
            CR_MAKE_3_NODE(t0,t1,t2,k0,k1,idx);
            return idx;
        case CR_4_TAG:
            k0 = CR_set_wakeups_2(wakeups,CR_4_NODE(idx)->k[0]);
            k1 = CR_set_wakeups_2(wakeups,CR_4_NODE(idx)->k[1]);
            k2 = CR_set_wakeups_2(wakeups,CR_4_NODE(idx)->k[2]);
            t0 = CR_index_set_wakeups(CR_4_NODE(idx)->t[0],wakeups);
            t1 = CR_index_set_wakeups(CR_4_NODE(idx)->t[1],wakeups);
            t2 = CR_index_set_wakeups(CR_4_NODE(idx)->t[2],wakeups);
            t3 = CR_index_set_wakeups(CR_4_NODE(idx)->t[3],wakeups);
            CR_MAKE_4_NODE(t0,t1,t2,t3,k0,k1,k2,idx);
            return idx;
    }
    CR_index_abort();
}

MR_Word CR_index_create_events(MR_Word idx)
{
    MR_Word es, es1, es2;

    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            return CR_EVENTS_EMPTY;
        case CR_2_TAG:
            es = CR_index_create_events(CR_2_NODE(idx)->t[0]);
            es = CR_EVENTS_UNION(es,
                CR_index_create_events(CR_2_NODE(idx)->t[1]));
            es = CR_EVENTS_UNION(es,
                CR_create_events(CR_2_NODE(idx)->k[0],CR_DUMMY_SYMBOL()));
            return es;
        case CR_3_TAG:
            es  = CR_create_events(CR_3_NODE(idx)->k[0],CR_DUMMY_SYMBOL());
            es1 = CR_create_events(CR_3_NODE(idx)->k[1],CR_DUMMY_SYMBOL());
            if(es != es1) {
                es = CR_EVENTS_UNION(es,es1);
                es = CR_EVENTS_UNION(es,
                    CR_index_create_events(CR_3_NODE(idx)->t[1]));
            }
            es = CR_EVENTS_UNION(es,
                CR_index_create_events(CR_3_NODE(idx)->t[0]));
            es = CR_EVENTS_UNION(es,
                CR_index_create_events(CR_3_NODE(idx)->t[2]));
            return es;
        case CR_4_TAG:
            es1 = CR_create_events(CR_4_NODE(idx)->k[0],CR_DUMMY_SYMBOL());
            es2 = CR_create_events(CR_4_NODE(idx)->k[1],CR_DUMMY_SYMBOL());
            if(es1 != es2) {
                es = CR_EVENTS_UNION(es1,es2);
                es = CR_EVENTS_UNION(es,
                    CR_index_create_events(CR_4_NODE(idx)->t[1]));
            } else
                es = es1;
            es1 = CR_create_events(CR_4_NODE(idx)->k[2],CR_DUMMY_SYMBOL());
            if(es1 != es2) {
                es = CR_EVENTS_UNION(es,es1);
                es = CR_EVENTS_UNION(es,
                    CR_index_create_events(CR_4_NODE(idx)->t[2]));
            }
            es = CR_EVENTS_UNION(es,
                CR_index_create_events(CR_4_NODE(idx)->t[0]));
            es = CR_EVENTS_UNION(es,
                CR_index_create_events(CR_4_NODE(idx)->t[3]));
            return es;
    }
    CR_index_abort();
}

MR_Word CR_index_to_list(MR_Word idx)
{
    MR_Word ls;

    CR_index_to_list_2(idx,MR_list_empty(),&ls);
    return ls;
}

void CR_index_to_list_2(MR_Word idx,MR_Word lsin,MR_Word *lsout)
{
    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            *lsout = lsin;
            return;
        case CR_2_TAG:
            CR_index_to_list_2(CR_2_NODE(idx)->t[1],lsin,&lsin);
            lsin = MR_list_cons(CR_2_NODE(idx)->k[0],lsin);
            CR_index_to_list_2(CR_2_NODE(idx)->t[0],lsin,lsout);
            return;
        case CR_3_TAG:
            CR_index_to_list_2(CR_3_NODE(idx)->t[2],lsin,&lsin);
            lsin = MR_list_cons(CR_3_NODE(idx)->k[1],lsin);
            CR_index_to_list_2(CR_3_NODE(idx)->t[1],lsin,&lsin);
            lsin = MR_list_cons(CR_3_NODE(idx)->k[0],lsin);
            CR_index_to_list_2(CR_3_NODE(idx)->t[0],lsin,lsout);
            return;
        case CR_4_TAG:
            CR_index_to_list_2(CR_4_NODE(idx)->t[3],lsin,&lsin);
            lsin = MR_list_cons(CR_4_NODE(idx)->k[2],lsin);
            CR_index_to_list_2(CR_4_NODE(idx)->t[2],lsin,&lsin);
            lsin = MR_list_cons(CR_4_NODE(idx)->k[1],lsin);
            CR_index_to_list_2(CR_4_NODE(idx)->t[1],lsin,&lsin);
            lsin = MR_list_cons(CR_4_NODE(idx)->k[0],lsin);
            CR_index_to_list_2(CR_4_NODE(idx)->t[0],lsin,lsout);
            return;
    }
    CR_index_abort();
}

MR_Integer CR_index_size(MR_Word idx)
{
    MR_Integer size;

    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            size = 0;
            break;
        case CR_2_TAG:
            size = 1 + CR_index_size(CR_2_NODE(idx)->t[0]) +
                CR_index_size(CR_2_NODE(idx)->t[1]);
            break;
        case CR_3_TAG:
            size = 2 + CR_index_size(CR_3_NODE(idx)->t[0]) +
                CR_index_size(CR_3_NODE(idx)->t[1]) +
                CR_index_size(CR_3_NODE(idx)->t[2]);
            break;
        case CR_4_TAG:
            size = 3 + CR_index_size(CR_4_NODE(idx)->t[0]) +
                CR_index_size(CR_4_NODE(idx)->t[1]) +
                CR_index_size(CR_4_NODE(idx)->t[2]) +
                CR_index_size(CR_4_NODE(idx)->t[3]);
            break;
    }
    return size;
}

/* DEBUGGING */

MR_Integer CR_is_balanced(MR_Word idx,MR_Integer curr)
{
    MR_Integer dmin, dmax, d1, d2, d3, d4;

#define CR_min(x,y)     ((x)<(y)?(x):(y))
#define CR_max(x,y)     ((x)>(y)?(y):(x))

    switch(MR_tag(idx)) {
        case CR_EMPTY_TAG:
            return curr;
        case CR_2_TAG:
            d1 = CR_is_balanced(CR_2_NODE(idx)->t[0],curr+1);
            d2 = CR_is_balanced(CR_2_NODE(idx)->t[1],curr+1);
            if(abs(d1-d2) > 1) {
                fprintf(stderr,
                    "CR_is_balanced: (2-node) unbalanced tree %"
                    MR_INTEGER_LENGTH_MODIFIER "d vs. %"
                    MR_INTEGER_LENGTH_MODIFIER "d\n",
                    d1,d2);
                exit(EXIT_FAILURE);
            }
            if(d1 > d2)
                return d1;
            return d2;
        case CR_3_TAG:
            d1 = CR_is_balanced(CR_3_NODE(idx)->t[0],curr+1);
            d2 = CR_is_balanced(CR_3_NODE(idx)->t[1],curr+1);
            d3 = CR_is_balanced(CR_3_NODE(idx)->t[2],curr+1);
            dmin = CR_min(d1,d2);
            dmin = CR_min(d3,dmin);
            dmax = CR_max(d1,d2);
            dmax = CR_max(d3,dmax);
            if(dmax-dmin > 1) {
                fprintf(stderr,
                    "CR_is_balanced: (3-node) unbalanced tree %"
                    MR_INTEGER_LENGTH_MODIFIER "d vs. %"
                    MR_INTEGER_LENGTH_MODIFIER "d\n",
                     dmin,dmax);
                exit(EXIT_FAILURE);
            }
            return dmax;
        case CR_4_TAG:
            d1 = CR_is_balanced(CR_4_NODE(idx)->t[0],curr+1);
            d2 = CR_is_balanced(CR_4_NODE(idx)->t[1],curr+1);
            d3 = CR_is_balanced(CR_4_NODE(idx)->t[2],curr+1);
            d4 = CR_is_balanced(CR_4_NODE(idx)->t[3],curr+1);
            dmin = CR_min(d1,d2);
            dmin = CR_min(d3,dmin);
            dmin = CR_min(d4,dmin);
            dmax = CR_max(d1,d2);
            dmax = CR_max(d3,dmax);
            dmax = CR_max(d4,dmax);
            if(dmax-dmin > 1) {
                fprintf(stderr,
                    "CR_is_balanced: (4-node) unbalanced tree %"
                    MR_INTEGER_LENGTH_MODIFIER "d vs. %"
                    MR_INTEGER_LENGTH_MODIFIER "d\n",
                    dmin,dmax);
                exit(EXIT_FAILURE);
            }
            return dmax;
        default:
            CR_index_abort();
    }
}

