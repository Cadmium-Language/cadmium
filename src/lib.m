%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Opturion Pty Ltd.
% Copyright (C) 2006-2009 NICTA.
%---------------------------------------------------------------------------%
%
% Gregory J. Duck
%
% Library of built-in functions for Cadmium.
% Ad hoc extensions belond here.
%---------------------------------------------------------------------------%

:- module lib.
:- interface.

:- pred lib_dummy is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_import_module("C", machine).
:- pragma foreign_import_module("C", ac_index).

:- pragma foreign_decl("C","

/* I/O functions */
extern MR_Word CR_write(MR_Word model);             /* XXX: Deprecated */
extern MR_Word CR_write_string(MR_Word model);      /* XXX: Deprecated */
extern MR_Word CR_unsafe_io(void);
extern MR_Word CR_put_string(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put_int(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put_float(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put_var(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put_char(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put_with_annots(MR_Word stream,MR_Word model,MR_Word io);
extern MR_Word CR_put_format(MR_Word stream,MR_Word format,MR_Word args,
    MR_Word io);
extern MR_Word CR_get_line(MR_Word stream,MR_Word io);
extern MR_Word CR_get_contents(MR_Word stream,MR_Word io);
extern MR_Word CR_get_char(MR_Word stream,MR_Word io);
extern MR_Word CR_open_input(MR_Word model,MR_Word io);
extern MR_Word CR_open_output(MR_Word model,MR_Word io);
extern MR_Word CR_close_input(MR_Word model,MR_Word io);
extern MR_Word CR_close_output(MR_Word model,MR_Word io);
extern MR_Word CR_stdout(void);
extern MR_Word CR_stdin(void);
extern MR_Word CR_stderr(void);
extern MR_Word CR_get(MR_Word io);
extern MR_Word CR_set(MR_Word val,MR_Word io);

/* Math functions */
extern MR_Word CR_sin(MR_Word model);
extern MR_Word CR_cos(MR_Word model);
extern MR_Word CR_tan(MR_Word model);
extern MR_Word CR_asin(MR_Word model);
extern MR_Word CR_acos(MR_Word model);
extern MR_Word CR_atan(MR_Word model);
extern MR_Word CR_sinh(MR_Word model);
extern MR_Word CR_cosh(MR_Word model);
extern MR_Word CR_tanh(MR_Word model);
/* extern MR_Word CR_asinh(MR_Word model); */
/* extern MR_Word CR_acosh(MR_Word model); */
/* extern MR_Word CR_atanh(MR_Word model); */
extern MR_Word CR_exp(MR_Word model);
extern MR_Word CR_log(MR_Word model);
extern MR_Word CR_log10(MR_Word model);
extern MR_Word CR_pow(MR_Word model1,MR_Word model2);
extern MR_Word CR_sqrt(MR_Word model);
extern MR_Word CR_floor(MR_Word model);
extern MR_Word CR_ceil(MR_Word model);
/* extern MR_Word CR_round(MR_Word model); */
extern MR_Word CR_int_to_float(MR_Word model);
extern MR_Word CR_float_to_int(MR_Word model);

/* String functions */
extern MR_Word CR_strlen(MR_Word model);
extern MR_Word CR_strlookup(MR_Word model1,MR_Word model2);
extern MR_Word CR_strcat(MR_Word model1,MR_Word model2);
extern MR_Word CR_strshow(MR_Word model);
extern MR_Word CR_strshow_with_annots(MR_Word model);
extern MR_Word CR_strfrom_list(MR_Word model);
extern MR_Word CR_strsubstring(MR_Word model,MR_Word model1,MR_Word model2);
extern MR_Word CR_strto_int(MR_Word model);
extern MR_Word CR_strto_float(MR_Word model);
extern MR_Word CR_strfrom_char(MR_Word model);

/* Term functions */
extern MR_Word CR_compare(MR_Word model1,MR_Word model2);
extern MR_Word CR_functor(MR_Word model);
extern MR_Word CR_arity(MR_Word);
extern MR_Word CR_arg(MR_Word,MR_Word);
extern MR_Word CR_wrap(MR_Word,MR_Word);

/* Mutable type support. */
extern MR_Word CR_mutable_normalise(MR_Word);
extern MR_Word CR_mutable_init(MR_Word);
extern MR_Word CR_mutable_get_val(MR_Word);
extern MR_Word CR_mutable_set_val(MR_Word,MR_Word);

/* Cd/C linking. */
extern void *CR_link_table_lookup(MR_String);

/* Misc. */
void CR_io_error(void) __attribute__((noreturn));

").

:- pragma foreign_code("C","

#include <math.h>
#include ""model.mh""

MR_Word CR_io_result     = (MR_Word)NULL;
MR_Word CR_stdout_stream = (MR_Word)NULL;
MR_Word CR_stdin_stream  = (MR_Word)NULL;
MR_Word CR_stderr_stream = (MR_Word)NULL;

#define CR_make_string(str)                                     \\
    CR_make_string_2(str)
#define CR_make_string_2(str)                                   \\
    #str

#define CR_FLOAT_FUNCTION_1(F,model)                            \\
    MR_Float f;                                                 \\
    if(CR_IS_FLOAT(model)) {                                    \\
        CR_GET_FLOAT_VAL(model,f);                              \\
        f = F(f);                                               \\
        CR_MAKE_FLOAT(f,model);                                 \\
    } else                                                      \\
        model = CR_undefined_model;                             \\
    return model;

#define CR_FLOAT_FUNCTION_2(F,model1,model2)                    \\
    MR_Float f1, f2;                                            \\
    if(CR_IS_FLOAT(model1) && CR_IS_FLOAT(model2)) {            \\
        CR_GET_FLOAT_VAL(model1,f1);                            \\
        CR_GET_FLOAT_VAL(model2,f2);                            \\
        f1 = F(f1,f2);                                          \\
        CR_MAKE_FLOAT(f1,model1);                               \\
    } else                                                      \\
        model1 = CR_undefined_model;                            \\
    return model1;


/*****************************************************************************/
/* Linking */

    /*
     * Ideally we would like to use runtime linking to resolve the 
     * string->function mapping.  This can be achieved with dlsym and 
     * appropriate linker options under Linux, but is not portable to other
     * systems.
     *
     * To work around this, we explictly maintain a string->function mapping
     * in the form of the cd_link_table array.  The disadvantage is that every
     * new foreign function must be registered with this array.
     */
typedef struct {
    MR_String  c_name;
    void      *f_ptr;
} CR_link_entry_s;

int CR_link_entry_compare(const void *entry1,const void *entry2);

#define CR_LINK_ENTRY(name)                                     \\
    {(MR_String)CR_make_string(name),(void *)(name)}

static MR_Bool CR_link_table_sorted    = MR_FALSE;
static MR_Integer CR_link_table_length = 0;

static CR_link_entry_s CR_link_table[] = {
    CR_LINK_ENTRY(CR_deconstruct),
    CR_LINK_ENTRY(CR_construct),
    CR_LINK_ENTRY(CR_error),
    CR_LINK_ENTRY(CR_wrap),
    CR_LINK_ENTRY(CR_throw_exception),

    CR_LINK_ENTRY(CR_write),
    CR_LINK_ENTRY(CR_write_string),
    CR_LINK_ENTRY(CR_unsafe_io),
    CR_LINK_ENTRY(CR_put_string),
    CR_LINK_ENTRY(CR_put_int),
    CR_LINK_ENTRY(CR_put_float),
    CR_LINK_ENTRY(CR_put_var),
    CR_LINK_ENTRY(CR_put_char),
    CR_LINK_ENTRY(CR_put),
    CR_LINK_ENTRY(CR_put_with_annots),
    CR_LINK_ENTRY(CR_put_format),
    CR_LINK_ENTRY(CR_get_line),
    CR_LINK_ENTRY(CR_get_contents),
    CR_LINK_ENTRY(CR_get_char),
    CR_LINK_ENTRY(CR_open_input),
    CR_LINK_ENTRY(CR_open_output),
    CR_LINK_ENTRY(CR_close_input),
    CR_LINK_ENTRY(CR_close_output),
    CR_LINK_ENTRY(CR_stdout),
    CR_LINK_ENTRY(CR_stdin),
    CR_LINK_ENTRY(CR_stderr),
    CR_LINK_ENTRY(CR_get),
    CR_LINK_ENTRY(CR_set),
   
    CR_LINK_ENTRY(CR_compare),
    CR_LINK_ENTRY(CR_functor),
    CR_LINK_ENTRY(CR_arity),
    CR_LINK_ENTRY(CR_arg),
    CR_LINK_ENTRY(CR_strlen),
    CR_LINK_ENTRY(CR_strlookup),
    CR_LINK_ENTRY(CR_strcat),
    CR_LINK_ENTRY(CR_strshow),
    CR_LINK_ENTRY(CR_strshow_with_annots),
    CR_LINK_ENTRY(CR_strfrom_list),
    CR_LINK_ENTRY(CR_strsubstring),
    CR_LINK_ENTRY(CR_strto_int),
    CR_LINK_ENTRY(CR_strto_float),
    CR_LINK_ENTRY(CR_strfrom_char),
    CR_LINK_ENTRY(CR_strto_var),
    CR_LINK_ENTRY(CR_strsub),

    CR_LINK_ENTRY(CR_sin),
    CR_LINK_ENTRY(CR_cos),
    CR_LINK_ENTRY(CR_tan),
    CR_LINK_ENTRY(CR_asin),
    CR_LINK_ENTRY(CR_acos),
    CR_LINK_ENTRY(CR_atan),
    CR_LINK_ENTRY(CR_sinh),
    CR_LINK_ENTRY(CR_cosh),
    CR_LINK_ENTRY(CR_tanh),
    CR_LINK_ENTRY(CR_exp),
    CR_LINK_ENTRY(CR_log),
    CR_LINK_ENTRY(CR_log10),
    CR_LINK_ENTRY(CR_pow),
    CR_LINK_ENTRY(CR_sqrt),
    CR_LINK_ENTRY(CR_floor),
    CR_LINK_ENTRY(CR_ceil),
    CR_LINK_ENTRY(CR_int_to_float),
    CR_LINK_ENTRY(CR_float_to_int),

    CR_LINK_ENTRY(CR_map_normalise),
    CR_LINK_ENTRY(CR_map_init),
    CR_LINK_ENTRY(CR_map_insert),
    CR_LINK_ENTRY(CR_map_delete),
    CR_LINK_ENTRY(CR_map_lookup),

    CR_LINK_ENTRY(CR_mutable_normalise),
    CR_LINK_ENTRY(CR_mutable_init),
    CR_LINK_ENTRY(CR_mutable_get_val),
    CR_LINK_ENTRY(CR_mutable_set_val),

    {NULL,NULL}
};

void *CR_link_table_lookup(MR_String name) 
{
    CR_link_entry_s *table = CR_link_table;
    MR_Integer length;
    MR_Integer min, max, mid, mid_res;

    if(!CR_link_table_sorted) {
        /*
         * Sort the elements in CR_link_table to make future lookups faster.
         */
        for(length = 0; table[length].c_name != NULL; length++)
            ;
        qsort(table,length,sizeof(CR_link_entry_s),CR_link_entry_compare);
        CR_link_table_sorted = MR_TRUE;
        CR_link_table_length = length;
    }
    
    min = 0;
    max = CR_link_table_length;
    while(min <= max) {
        mid = (min + max)/2;
        mid_res = strcmp(name,table[mid].c_name);
        if(mid_res < 0)
            max = mid-1;
        else if(mid_res > 0)
            min = mid+1;
        else
            return table[mid].f_ptr;
    }
    return NULL;
}

int CR_link_entry_compare(const void *entry1,const void *entry2)
{
    return strcmp(((CR_link_entry_s *)entry1)->c_name,
                  ((CR_link_entry_s *)entry2)->c_name);
}

/*****************************************************************************/
/* Code */

extern MR_Word CR_write(MR_Word model)
{
    CR_stream stream;
    CR_MAKE_FILE_STREAM(stream,stdout);
    CR_stream_put_model(stream,MR_TRUE,model);
    return model;
}

extern MR_Word CR_write_string(MR_Word model)
{
    MR_String s;
    if(CR_IS_STRING(model)) {
        CR_GET_STRING_VAL(model,s);
        fputs(s,stdout);
    } else
        model = CR_undefined_model;
    return model;
}

extern MR_Word CR_unsafe_io(void)
{
    MR_Word model;
    CR_MAKE_FOREIGN((MR_Word)NULL,model);
    return model;
}

void CR_io_string_error(MR_String message) __attribute__((noreturn));
static MR_Word CR_file_put_string(FILE *file,MR_Word model,MR_Word io);

void CR_io_error(void)
{
    MR_Word model, sym;

    CR_MAKE_STRING_0(strerror(errno),model);
    sym = CR_symbol((MR_String)""io_error"",1);
    CR_MAKE_FUNCTOR_1(sym,model,model);
    CR_throw_exception(model);
}

void CR_io_string_error(MR_String message)
{
    MR_Word model, sym;
    CR_MAKE_STRING_0(message,model);
    sym = CR_symbol((MR_String)""io_error"",1);
    CR_MAKE_FUNCTOR_1(sym,model,model);
    CR_throw_exception(model);
}

extern MR_Word CR_put_string(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_String s;
    MR_Word file;

    if(CR_IS_FOREIGN(stream) && CR_IS_STRING(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(stream,file);
        CR_GET_STRING_VAL(model,s);
        if(fputs(s,(FILE *)file) == EOF)
            CR_io_error();
    } else
        CR_io_string_error((MR_String)
            ""put_str type error: expected output stream, string, and IO ""
            ""state"");

    return io;
}

extern MR_Word CR_put_int(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_Integer i;
    MR_Word file;

    if(CR_IS_FOREIGN(stream) && CR_IS_INT(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(stream,file);
        CR_GET_INT_VAL(model,i);
        if(fprintf((FILE *)file,""%ld"",i) < 0)
            CR_io_error();
    } else
        CR_io_string_error((MR_String)
            ""put_int type error: expected output stream, integer, and IO ""
            ""state"");

    return io;
}

extern MR_Word CR_put_float(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_Float f;
    MR_Word file;
    static char flt_buf[MR_SPRINTF_FLOAT_BUF_SIZE];

    if(CR_IS_FOREIGN(stream) && CR_IS_FLOAT(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(stream,file);
        CR_GET_FLOAT_VAL(model,f);
        MR_sprintf_float(flt_buf,f);
        if(fputs(flt_buf,(FILE *)file) == EOF)
            CR_io_error();
    } else
        CR_io_string_error((MR_String)""put_float type error: ""
            ""expected output stream, floating point number, and IO state"");

    return io;
}

extern MR_Word CR_put_var(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_Integer v;
    MR_String name;
    MR_Word file;

    if(CR_IS_FOREIGN(stream) && CR_IS_VAR(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(stream,file);
        CR_GET_VAR_VAL(model,v);
        name = CR_get_var_name(v);
        if(fputs(name,(FILE *)file) == EOF)
            CR_io_error();
    } else
        CR_io_string_error((MR_String)
            ""put_var type error: expected output stream, variable, and IO ""
            ""state"");

    return io;
}

extern MR_Word CR_put_char(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_Word sym;
    MR_String name;
    MR_Char c;
    MR_Word file;

    if(CR_IS_FOREIGN(stream) && CR_IS_FUNCTOR(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(stream,file);
        CR_GET_FUNCTOR_SYM(model,sym);
        if(CR_SYM_ATY(sym) == 0) {
            name = CR_SYM_NAME(sym);
            if((name[0] != '\\0') && (name[1] == '\\0')) {
                if(fputc(name[0],(FILE *)file) == EOF)
                    CR_io_error();
                return io;
            } 
        } 
    } 

    CR_io_string_error((MR_String)
        ""put_char type error: expected output stream, character, and IO ""
        ""state"");
}

extern MR_Word CR_put(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_Word file;
    CR_stream file_stream;

    if(!CR_IS_FOREIGN(stream) || !CR_IS_FOREIGN(io)) 
        CR_io_string_error((MR_String)""put type error: expected output ""
            ""stream and IO state"");
    
    CR_GET_FOREIGN_VAL(stream,file);
    CR_MAKE_FILE_STREAM(file_stream,(FILE *)file);
    CR_stream_put_model(file_stream,MR_FALSE,model);
    return io;
}

extern MR_Word CR_put_with_annots(MR_Word stream,MR_Word model,MR_Word io)
{
    MR_Word file;
    CR_stream file_stream;

    if(!CR_IS_FOREIGN(stream) || !CR_IS_FOREIGN(io))
        CR_io_string_error((MR_String)""put_with_annots type error: ""
            ""expected output stream and IO state"");
    
    CR_GET_FOREIGN_VAL(stream,file);
    CR_MAKE_FILE_STREAM(file_stream,(FILE *)file);
    CR_stream_put_model(file_stream,MR_TRUE,model);
    return io;
}

extern MR_Word CR_put_format(MR_Word stream,MR_Word format,MR_Word args,
    MR_Word io)
{
    MR_String fmtstr;
    MR_Word arg, file0;
    MR_Integer i, int_val;
    FILE *file;

#define CR_args_deconstruct(arg,args)                       \\
    do {                                                    \\
        if(!CR_IS_FUNCTOR_SYM((args),CR_CONS_SYMBOL()))     \\
            goto CR_put_format_error;                       \\
        CR_GET_FUNCTOR_ARG(1,(args),(arg));                 \\
        CR_GET_FUNCTOR_ARG(2,(args),(args));                \\
    } while(0)

    if(!CR_IS_FOREIGN(stream))
        CR_io_string_error((MR_String)""put_format type error: ""
            ""expected output stream"");

    CR_GET_FOREIGN_VAL(stream,file0);
    file = (FILE *)file0;

    if(CR_IS_STRING(format) && CR_IS_FOREIGN(io)) {
        CR_GET_STRING_VAL(format,fmtstr);
        for(i = 0; fmtstr[i]; i++) {
            if(fmtstr[i] == '%') {
                i++;
                switch(fmtstr[i]) {
                    case 'i': case 'd':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_INT(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected integer for %i"");
                        io = CR_put_int(stream,arg,io);
                        break;
                    case 'u':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_INT(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected integer for %u"");
                        CR_GET_INT_VAL(arg,int_val);
                        if(fprintf(file,""%lu"",int_val) < 0)
                            CR_io_error();
                        break;
                    case 'o':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_INT(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected integer for %o"");
                        CR_GET_INT_VAL(arg,int_val);
                        if(fprintf(file,""%lo"",int_val) < 0)
                            CR_io_error();
                        break;
                    case 'x':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_INT(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected integer for %x"");
                        CR_GET_INT_VAL(arg,int_val);
                        if(fprintf(file,""0x%lx"",int_val) < 0)
                            CR_io_error();
                        break;
                    case 'X':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_INT(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected integer for %X"");
                        CR_GET_INT_VAL(arg,int_val);
                        if(fprintf(file,""0x%lX"",int_val) < 0)
                            CR_io_error();
                        break;
                    case 'f':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_FLOAT(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected floating point ""
                                ""number for %f"");
                        io = CR_put_float(stream,arg,io);
                        break;
                    case 's':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_STRING(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected string for %s"");
                        io = CR_put_string(stream,arg,io);
                        break;
                    case 'c':
                        CR_args_deconstruct(arg,args);
                        io = CR_put_char(stream,arg,io);
                        break;
                    case 'p':
                        CR_args_deconstruct(arg,args);
                        io = CR_put(stream,arg,io);
                        break;
                    case 'P':
                        CR_args_deconstruct(arg,args);
                        io = CR_put_with_annots(stream,arg,io);
                        break;
                    case 'v':
                        CR_args_deconstruct(arg,args);
                        if(!CR_IS_VAR(arg))
                            CR_io_string_error((MR_String)
                                ""put_format error: expected variable for ""
                                ""%v"");
                        io = CR_put_var(stream,arg,io);
                        break;
                    case '\\0':
                        if((fputc('%',file) == EOF))
                            CR_io_error();
                        goto CR_put_format_done;
                    case '%':
                        if((fputc('%',file) == EOF))
                            CR_io_error();
                        break;
                    default:
                        if((fputc('%',file) == EOF) ||
                           (fputc(fmtstr[i],file) == EOF))
                            CR_io_error();
                        break;
                }
            } else {
                if(fputc(fmtstr[i],file) == EOF)
                    CR_io_error();
            }
        }
    
CR_put_format_done:
        if(CR_IS_FUNCTOR_SYM(args,CR_NIL_SYMBOL()))
            return io;
    }

CR_put_format_error:
    CR_io_string_error((MR_String)
        ""put_format error: expected format string, list of arguments, and ""
        ""an IO state"");
}

#define CR_GET_LINE_INIT_BUF_SIZE       85

extern MR_Word CR_get_line(MR_Word stream,MR_Word io)
{
    MR_String buf;
    MR_Integer buf_size = CR_GET_LINE_INIT_BUF_SIZE, i;
    MR_Word file;
    MR_Char c;

    if(!CR_IS_FOREIGN(stream) || !CR_IS_FOREIGN(io))
        CR_io_string_error((MR_String)
            ""get_line type error: expected input stream and IO state"");

    CR_GET_FOREIGN_VAL(stream,file);

    buf = (MR_String)GC_MALLOC(buf_size);

    for(i = 0; 1; i++) {
        c = getc((FILE *)file);
        if(ferror((FILE *)file))
            CR_io_error();
        if((c == '\\n') || (c == '\\0') || (c == EOF)) {
            buf[i] = '\\0';
            buf = (MR_String)GC_REALLOC(buf,i+1);
            CR_MAKE_STRING(buf,i,CR_io_result);
            return io;
        }
        buf[i] = c;
        if(i == (buf_size-1)) {
            buf_size = buf_size*2;
            buf = (MR_String)GC_REALLOC(buf,buf_size);
        }
    }
}

#define CR_GET_CONTENTS_INIT_BUF_SIZE   1000

extern MR_Word CR_get_contents(MR_Word stream,MR_Word io)
{
    /*
     * XXX: We could use fstat to get the length of the file, however, this
     *      is not portable?
     */
    MR_Integer buf_size = CR_GET_CONTENTS_INIT_BUF_SIZE, i;
    MR_String buf;
    MR_Word file;
    MR_Char c;

    if(!CR_IS_FOREIGN(stream) || !CR_IS_FOREIGN(io))
        CR_io_string_error((MR_String)
            ""get_contents type error: expected input stream and IO state"");

    buf = (MR_String)GC_malloc(buf_size);
    CR_GET_FOREIGN_VAL(stream,file);

    for(i = 0; 1; i++) {
        c = getc((FILE *)file);
        if(ferror((FILE *)file))
            CR_io_error();
        if((c == '\\0') || (c == EOF)) {
            buf[i] = '\\0';
            buf = (MR_String)GC_REALLOC(buf,i+1);
            CR_MAKE_STRING(buf,i,CR_io_result);
            return io;
        }
        buf[i] = c;
        if(i == (buf_size-1)) {
            buf_size = buf_size*2;
            buf = (MR_String)GC_REALLOC(buf,buf_size);
        }
    }
}

extern MR_Word CR_get_char(MR_Word stream,MR_Word io)
{
    MR_Char c;
    MR_Word sym;
    MR_Word file;

    if(!CR_IS_FOREIGN(stream) || !CR_IS_FOREIGN(io))
        CR_io_string_error((MR_String)
            ""get_char type error: expected input stream and IO state"");

    CR_GET_FOREIGN_VAL(stream,file);
    c = getc((FILE *)file);
    sym = CR_ascii_symbol(c);
    CR_MAKE_FUNCTOR_0(sym,CR_io_result);
    return io;
}

extern MR_Word CR_open_input(MR_Word model,MR_Word io)
{
    MR_String filename;
    FILE *file;

    if(CR_IS_STRING(model) && CR_IS_FOREIGN(io)) {
        CR_GET_STRING_VAL(model,filename);
        file = fopen(filename,""r"");
        if(file == NULL)
            CR_io_error();
        else
            CR_MAKE_FOREIGN((MR_Word)file,CR_io_result);
    } else
        CR_io_string_error((MR_String)""open_input type error: expected ""
            ""string filename and IO state"");
    
    return io;
}

extern MR_Word CR_open_output(MR_Word model,MR_Word io)
{
    MR_String filename;
    FILE *file;

    if(CR_IS_STRING(model) && CR_IS_FOREIGN(io)) {
        CR_GET_STRING_VAL(model,filename);
        file = fopen(filename,""w"");
        if(file == NULL)
            CR_io_error();
        else
            CR_MAKE_FOREIGN((MR_Word)file,CR_io_result);
    } else
        CR_io_string_error((MR_String)""open_output type error: expected ""
            ""string filename and IO state"");
    
    return io;
}

extern MR_Word CR_close_input(MR_Word model,MR_Word io)
{
    MR_Word file;

    if(CR_IS_FOREIGN(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(model,file);
        if(fclose((FILE *)file) == EOF)
            CR_io_error();
    } else
        CR_io_string_error((MR_String)""close_input type error: expected ""
            ""input stream and IO state"");

    return io;
}

extern MR_Word CR_close_output(MR_Word model,MR_Word io)
{
    MR_Word file;

    if(CR_IS_FOREIGN(model) && CR_IS_FOREIGN(io)) {
        CR_GET_FOREIGN_VAL(model,file);
        if(fclose((FILE *)file) == EOF)
            CR_io_error();
    } else
        CR_io_string_error((MR_String)""close_output type error: expected ""
            ""output stream and IO state"");

    return io;
}

extern MR_Word CR_stdout(void)
{
    if(CR_stdout_stream == (MR_Word)NULL)
        CR_MAKE_FOREIGN((MR_Word)stdout,CR_stdout_stream);
    return CR_stdout_stream;
}

extern MR_Word CR_stdin(void)
{
    if(CR_stdin_stream == (MR_Word)NULL)
        CR_MAKE_FOREIGN((MR_Word)stdin,CR_stdin_stream);
    return CR_stdin_stream;
}

extern MR_Word CR_stderr(void)
{
    if(CR_stderr_stream == (MR_Word)NULL)
        CR_MAKE_FOREIGN((MR_Word)stderr,CR_stderr_stream);
    return CR_stderr_stream;
}

extern MR_Word CR_get(MR_Word io)
{
    if(!CR_IS_FOREIGN(io))
        CR_io_string_error((MR_String)""get type error: expected IO state"");

    if(CR_io_result != (MR_Word)NULL)
        return CR_io_result;
    else
        return CR_undefined_model;
}

extern MR_Word CR_set(MR_Word val,MR_Word io)
{
    if(CR_IS_FOREIGN(io)) {
        CR_io_result = val;
        return io;
    } else
        return CR_undefined_model;
}

extern MR_Word CR_sin(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(sin,model);
}

extern MR_Word CR_cos(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(cos,model);
}

extern MR_Word CR_tan(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(tan,model);
}

extern MR_Word CR_asin(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(asin,model);
}

extern MR_Word CR_acos(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(acos,model);
}

extern MR_Word CR_atan(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(atan,model);
}

extern MR_Word CR_sinh(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(sinh,model);
}

extern MR_Word CR_cosh(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(cosh,model);
}

extern MR_Word CR_tanh(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(tanh,model);
}

/*
extern MR_Word CR_asinh(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(asinh,model);
}

extern MR_Word CR_acosh(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(acosh,model);
}

extern MR_Word CR_atanh(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(atanh,model);
}
*/

extern MR_Word CR_exp(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(exp,model);
}

extern MR_Word CR_log(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(log,model);
}

extern MR_Word CR_log10(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(log10,model);
}

extern MR_Word CR_pow(MR_Word model1,MR_Word model2)
{
    CR_FLOAT_FUNCTION_2(pow,model1,model2);
}

extern MR_Word CR_sqrt(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(sqrt,model);
}

extern MR_Word CR_floor(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(floor,model);
}

extern MR_Word CR_ceil(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(ceil,model);
}

/*
extern MR_Word CR_round(MR_Word model)
{
    CR_FLOAT_FUNCTION_1(round,model);
}
*/

extern MR_Word CR_int_to_float(MR_Word model)
{
    MR_Integer i;
    if(CR_IS_INT(model)) {
        CR_GET_INT_VAL(model,i);
        CR_MAKE_FLOAT((MR_Float)i,model);
    } else
        model = CR_undefined_model;
    return model;
}

extern MR_Word CR_float_to_int(MR_Word model)
{
    MR_Float f;
    if(CR_IS_FLOAT(model)) {
        CR_GET_FLOAT_VAL(model,f);
        CR_MAKE_INT((MR_Integer)f,model);
    } else
        model = CR_undefined_model;
    return model;
}

extern MR_Word CR_strlen(MR_Word model)
{
    MR_Integer len;
    if(CR_IS_STRING(model)) {
        CR_GET_STRING_LENGTH(model,len);
        CR_MAKE_INT(len,model);
    } else
        model = CR_undefined_model;
    return model;
}

extern MR_Word CR_strlookup(MR_Word model1,MR_Word model2)
{
    MR_String str;
    MR_Integer len, i;
    MR_Word sym;
    if(CR_IS_INT(model2) && CR_IS_STRING(model1)) {
        CR_GET_STRING_VAL(model1,str);
        CR_GET_STRING_LENGTH(model1,len);
        CR_GET_INT_VAL(model2,i);
        if(i < 0 || i >= len)
            return CR_undefined_model;
        sym = CR_ascii_symbol(str[i]);
        if(CR_IS_AC_SYM(sym))
            CR_MAKE_AC_FUNCTOR(sym,0,CR_EMPTY_IDX,model1);
        else
            CR_MAKE_FUNCTOR_LIST(sym,MR_list_empty(),model1);
    } else
        model1 = CR_undefined_model;
    return model1;
}

extern MR_Word CR_strcat(MR_Word model1,MR_Word model2)
{
    MR_Integer len1, len2;
    MR_String str1, str2, str3;
    if(CR_IS_STRING(model1) && CR_IS_STRING(model2)) {
        CR_GET_STRING_VAL(model1,str1);
        CR_GET_STRING_VAL(model2,str2);
        CR_GET_STRING_LENGTH(model1,len1);
        CR_GET_STRING_LENGTH(model2,len2);
        str3 = MR_GC_malloc(len1+len2+sizeof(MR_Word));
        strcpy(str3,str1);
        strcpy(str3+len1,str2);
        CR_MAKE_STRING(str3,len1+len2,model1);
    } else
        model1 = CR_undefined_model;
    return model1;
}

extern MR_Word CR_strshow(MR_Word model)
{
    CR_stream stream;

    CR_OPEN_STRSTREAM(stream);
    CR_stream_put_model(stream,MR_FALSE,model);
    CR_CLOSE_STRSTREAM(stream);
    CR_MAKE_STRING(CR_GET_STRSTREAM_STR(stream),CR_GET_STRSTREAM_LEN(stream),
        model);
    return model;
}

extern MR_Word CR_strshow_with_annots(MR_Word model)
{
    CR_stream stream;

    CR_OPEN_STRSTREAM(stream);
    CR_stream_put_model(stream,MR_TRUE,model);
    CR_CLOSE_STRSTREAM(stream);
    CR_MAKE_STRING(CR_GET_STRSTREAM_STR(stream),CR_GET_STRSTREAM_LEN(stream),
        model);
    return model;
}

extern MR_Word CR_strfrom_list(MR_Word model)
{
    MR_Integer len = 0, i;
    MR_Word copy = model, arg, sym;
    MR_String str, name;
    MR_Char c;

    /*
     * Calculate the list length (and also check that we have a list).
     */
    while(1) {
        if(CR_IS_FUNCTOR(copy)) {
            CR_GET_FUNCTOR_SYM(copy,sym);
            if(sym == CR_CONS_SYMBOL()) {
                len++;
                CR_GET_FUNCTOR_ARG(2,copy,copy);
            } else {
                if(sym == CR_NIL_SYMBOL())
                    break;
                return CR_undefined_model;
            }
        } else
            return CR_undefined_model;
    }
                    
    str = (MR_String)GC_MALLOC(len+1);
    for(i = 0; ; i++) {
        CR_GET_FUNCTOR_SYM(model,sym);
        if(sym == CR_CONS_SYMBOL()) {
            CR_GET_FUNCTOR_ARG(1,model,arg);
            if(CR_IS_FUNCTOR(arg)) {
                CR_GET_FUNCTOR_SYM(arg,sym);
                if(CR_SYM_ATY(sym) == 0) {
                    name = CR_SYM_NAME(sym);
                    if((name[0] != '\\0') && (name[1] == '\\0'))
                        str[i] = name[0];
                    else
                        return CR_undefined_model;
                } else
                    return CR_undefined_model;
            } else
                return CR_undefined_model;
            CR_GET_FUNCTOR_ARG(2,model,model);
        } else
            break;
    }
            
    str[i] = '\\0';
    CR_MAKE_STRING(str,len,model);
    return model;
}

extern MR_Word CR_strsubstring(MR_Word model,MR_Word model1,MR_Word model2)
{
    MR_String str, substr;
    MR_Integer len, i, j;
    
    if(CR_IS_STRING(model) && CR_IS_INT(model1) && CR_IS_INT(model2)) {
        CR_GET_STRING_VAL(model,str);
        CR_GET_STRING_LENGTH(model,len);
        CR_GET_INT_VAL(model1,i);
        CR_GET_INT_VAL(model2,j);
        if(i < 0)
            i = 0;
        else if(i >= len)
            i = len-1;
        if(j < 0)
            j = 0;
        else if(j >= len)
            j = len-1;
        if(i > j) {
            CR_MAKE_STRING("""",0,model);
            return model;
        } else if((i == 0) && (j == (len-1)))
            return model;
        len = j-i+1;
        substr = (MR_String)GC_MALLOC(len+1);
        strncpy(substr,str+i,len);
        CR_MAKE_STRING(substr,len,model);
        return model;
    } else
        return CR_undefined_model;
}

extern MR_Word CR_strto_int(MR_Word model)
{
    MR_String str, end;
    MR_Integer i;

    if(CR_IS_STRING(model)) {
        CR_GET_STRING_VAL(model,str);
        i = strtol(str,&end,10);
        if((i == (MR_Integer)0) && (errno == EINVAL)) {
            errno = 0;
            return CR_undefined_model;
        }
        if(end[0] != '\\0')
            return CR_undefined_model;
        CR_MAKE_INT(i,model);
        return model;
    } else
        return CR_undefined_model;
}

extern MR_Word CR_strto_float(MR_Word model)
{
    MR_String str, end;
    MR_Float f;

    if(CR_IS_STRING(model)) {
        CR_GET_STRING_VAL(model,str);
        f = strtod(str,&end);
        if((f == (MR_Float)0.0) && (errno = EINVAL)) {
            errno = 0;
            return CR_undefined_model;
        }
        if(end[0] != '\\0')
            return CR_undefined_model;
        CR_MAKE_FLOAT(f,model);
        return model;
    } else
        return CR_undefined_model;
}

extern MR_Word CR_strfrom_char(MR_Word model)
{
    MR_String name;
    MR_Word sym;

    if(CR_IS_FUNCTOR(model)) {
        CR_GET_FUNCTOR_SYM(model,sym);
        if(CR_SYM_ATY(sym) == 0) {
            name = CR_SYM_NAME(sym);
            if((name[0] != '\\0') && (name[1] == '\\0')) {
                CR_MAKE_STRING(name,1,model);
                return model;
            } 
        } 
    } 

    return CR_undefined_model;
}

extern MR_Word CR_compare(MR_Word model1,MR_Word model2)
{
    MR_Integer cmp;

    cmp = CR_model_compare(model1,model2);
    switch(cmp) {
        case CR_LT:
            CR_MAKE_FUNCTOR_0(CR_LT_SYMBOL(),model1);
            return model1;
        case CR_GT:
            CR_MAKE_FUNCTOR_0(CR_GT_SYMBOL(),model1);
            return model1;
        default:
            CR_MAKE_FUNCTOR_0(CR_EQ_SYMBOL(),model1);
            return model1;
    }
}

extern MR_Word CR_functor(MR_Word model)
{
    MR_Word sym;
    CR_GET_SYMBOL(model,sym);
    sym = CR_set_arity(sym,0);
    if(CR_IS_AC_SYM(sym))
        CR_MAKE_AC_FUNCTOR(sym,0,CR_EMPTY_IDX,model);
    else
        CR_MAKE_FUNCTOR_LIST(sym,MR_list_empty(),model);
    return model;
}

extern MR_Word CR_arity(MR_Word model)
{
    MR_Integer aty;
    CR_GET_ARITY(model,aty);
    CR_MAKE_INT(aty,model);
    return model;
}

extern MR_Word CR_arg(MR_Word model1,MR_Word model2)
{
    MR_Integer aty, idx;
    MR_Word sym;
    if(CR_IS_INT(model1) && CR_IS_FUNCTOR(model2)) {
        CR_GET_ARITY(model2,aty);
        CR_GET_INT_VAL(model1,idx);
        if(idx <= 0 || idx > aty)
            return CR_undefined_model;
        CR_GET_SYMBOL(model2,sym);
        if(CR_IS_AC_SYM(sym))
            model1 = CR_undefined_model;
        else
            CR_GET_FUNCTOR_ARG(idx,model2,model1);
    } else
        model1 = CR_undefined_model;
    return model1;
}

extern MR_Word CR_wrap(MR_Word fmodel,MR_Word model)
{
    MR_Word sym, nmodel;
    if(CR_IS_FUNCTOR(fmodel)) {
        CR_GET_FUNCTOR_SYM(fmodel,sym);
        if(CR_IS_AC_SYM(sym) || (CR_SYM_ATY(sym) != 0))
            return CR_undefined_model;
        sym = CR_set_arity(sym,1);
        CR_MAKE_FUNCTOR_1(sym,model,nmodel);
        return nmodel;
    } else
        model = CR_undefined_model;
    return model;
}

extern MR_Word CR_mutable_normalise(MR_Word mut)
{
    MR_Word ref;
    if(CR_IS_FOREIGN(mut)) {
        CR_GET_FOREIGN_VAL(mut,ref);
        *((MR_Word *)ref) = CR_interpret(*(MR_Word *)ref);
    } else
        return CR_undefined_model;
    return mut;
}

extern MR_Word CR_mutable_init(MR_Word val)
{
    MR_Word model;
    MR_Word *ref;
    ref = GC_malloc(sizeof(MR_Word));
    *ref = val;
    CR_MAKE_FOREIGN(ref,model);
    return model;
}

extern MR_Word CR_mutable_get_val(MR_Word mut)
{
    MR_Word ref;
    if(CR_IS_FOREIGN(mut)) {
        CR_GET_FOREIGN_VAL(mut,ref);
        return *(MR_Word *)ref;
    } else
        return CR_undefined_model;
}

extern MR_Word CR_mutable_set_val(MR_Word mut,MR_Word val)
{
    MR_Word ref;
    if(CR_IS_FOREIGN(mut)) {
        CR_GET_FOREIGN_VAL(mut,ref);
        *(MR_Word *)ref = val;
        return CR_true_model;
    } else
        return CR_undefined_model;
}

").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module ac_index.
:- import_module machine.
:- import_module model.

%---------------------------------------------------------------------------%

:- impure func deconstruct(model) = model.

:- pragma foreign_export("C",deconstruct(in) = out,"CR_deconstruct").

deconstruct(Model0) = Model1 :-
    impure may_call_mercury,
    semipure get_annotation_reg(Annots),
    impure set_annotation_reg(empty_annotations),
    ( is_functor(Model0,Sym) ->
        Fct = functor(Model0),
        ( is_ac(Sym) ->
            Idx = ac_args(Model0),
            ac_index_to_list(Idx,Args),
            Args1 = list_to_model(Args)
        ;   Aty = symbol_arity(Sym),
            Args1 = get_arg_list(1,Aty,Model0)
        ),
        Model1 = apply_cons(Fct,Args1)
    ;   Model1 = undefined_model
    ),
    impure set_annotation_reg(Annots).

%---------------------------------------------------------------------------%

    % XXX: for some reason this is needed in asm_fast.gc.tr.debug grades, 
    % otherwise deconstruct seg. faults.
    %
:- impure pred may_call_mercury is det.

:- pragma foreign_proc("C",may_call_mercury,
    [may_call_mercury,thread_safe,will_not_modify_trail],"").

%---------------------------------------------------------------------------%

:- func construct(model) = model.

:- pragma foreign_export("C",construct(in) = out,"CR_construct").

construct(Model0) = Model1 :-
    ( model_to_list(Model0,Ls,0,Len),
      Ls = [F|Args],
      is_functor(F,Sym),
      symbol_arity(Sym) = 0 ->
        NSym = set_arity(Sym,Len-1),
        ( is_ac(NSym) ->
            ac_merge_list(NSym,Args,Model1)
        ;   Model1 = apply_(NSym,Args)
        )
    ;   Model1 = undefined_model
    ).

%---------------------------------------------------------------------------%

:- func list_to_model(list(model)) = model.

list_to_model([]) =
    apply_nil.
list_to_model([X|Xs]) =
    apply_cons(X,list_to_model(Xs)).

%---------------------------------------------------------------------------%

:- pred model_to_list(model::in,list(model)::out,int::in,int::out) is semidet.

model_to_list(Model,Ls,!Len) :-
    Sym = get_symbol(Model),
    ( Sym = nil_symbol ->
        Ls = []
    ; Sym = cons_symbol ->
        Head = arg(1,Model),
        Tail = arg(2,Model),
        !:Len = 1 + !.Len,
        model_to_list(Tail,Ls0,!Len),
        Ls = [Head|Ls0]
    ;   fail
    ).

%---------------------------------------------------------------------------%

:- func get_arg_list(int,int,model) = model.

get_arg_list(N,M,Model0) = Model1 :-
    ( N > M ->
        Model1 = apply_nil
    ;   Arg = arg(N,Model0),
        Ls = get_arg_list(N+1,M,Model0),
        Model1 = apply_cons(Arg,Ls)
    ).

%---------------------------------------------------------------------------%

:- impure func string_to_var(model) = model.

:- pragma foreign_export("C",string_to_var(in) = out,"CR_strto_var").

string_to_var(Model0) = Model1 :-
    ( is_string(Model0,Name) ->
        impure Var = named_mvar(Name),
        Model1 = var(Var)
    ;   Model1 = undefined_model
    ).

%---------------------------------------------------------------------------%

:- func string_substring(model,model,model) = model.

:- pragma foreign_export("C",string_substring(in,in,in) = out,"CR_strsub").

string_substring(ModelStr,ModelI,ModelJ) = ModelSub :-
    ( is_string(ModelStr,Str),
      is_int(ModelI,I),
      is_int(ModelJ,J),
      I >= 0,
      J =< length(Str) ->
        Sub = unsafe_substring(Str,I,J),
        ModelSub = string(Sub)
    ;   ModelSub = undefined_model
    ).

%---------------------------------------------------------------------------%

:- func error(model) = model.

:- pragma foreign_export("C",error(in) = out,"CR_error").

error(Model) = Model :-
    ( semidet_true ->
        machine.throw(Model)
    ;   true
    ).

%---------------------------------------------------------------------------%

:- import_module map.

    % By default, model.model equality/comparison is just c_pointer 
    % equality/comparison.  We need to define a wrapper type model_key/1 to
    % use model.eqeq and model.compare_models.
    %
:- type model_key.
:- pragma foreign_type("C",model_key,"MR_Word")
    where equality is model_key_equals, comparison is model_key_compare.

:- type model_map == map(model_key,model).

%---------------------------------------------------------------------------%

:- func model_to_key(model) = model_key.
:- func key_to_model(model_key) = model.

:- pragma foreign_proc("C",model_to_key(Model::in) = (Key::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Key = Model;
").

:- pragma foreign_proc("C",key_to_model(Key::in) = (Model::out),
    [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Model = Key;
").

%---------------------------------------------------------------------------%

:- pred model_key_equals(model_key::in,model_key::in) is semidet.

model_key_equals(Key1,Key2) :-
    eqeq(key_to_model(Key1),key_to_model(Key2)).

%---------------------------------------------------------------------------%

:- pred model_key_compare(comparison_result::uo,model_key::in,model_key::in)
    is det.

model_key_compare(Result,Key1,Key2) :-
    compare_models(key_to_model(Key1),key_to_model(Key2),Result0),
    unsafe_promise_unique(Result0,Result).

%---------------------------------------------------------------------------%

:- func map_normalise(model) = model.

:- pragma foreign_export("C",map_normalise(in) = out,"CR_map_normalise").

map_normalise(M0) = M1 :-
    ( is_foreign(M0,Map0) ->
        foldl(map_normalise_key_val,Map0,init,Map1),
        M1 = foreign(Map1)
    ;   M1 = undefined_model
    ).

%---------------------------------------------------------------------------%

:- pred map_normalise_key_val(model_key::in,model::in,model_map::in,
    model_map::out) is det.

map_normalise_key_val(Key0,Val0,!Map) :-
    promise_pure impure interpret(key_to_model(Key0),MKey1),
    promise_pure impure interpret(Val0,Val1),
    Key1 = model_to_key(MKey1),
    ( map.insert(Key1,Val1,!Map) ->
        true
    ;   error("map: two distinct keys rewritten to same key")
    ).

%---------------------------------------------------------------------------%

:- func map_init = model.

:- pragma foreign_export("C",map_init = out,"CR_map_init").

map_init = foreign(init:model_map).

%---------------------------------------------------------------------------%

:- func map_insert(model,model,model) = model.

:- pragma foreign_export("C",map_insert(in,in,in) = out,"CR_map_insert").

map_insert(K,V,M0) = M1 :-
    ( is_foreign(M0,Map0) ->
        map.set(model_to_key(K),V,Map0,Map1),
        M1 = foreign(Map1)
    ;   M1 = undefined_model
    ).

%---------------------------------------------------------------------------%

:- func map_delete(model,model) = model.

:- pragma foreign_export("C",map_delete(in,in) = out,"CR_map_delete").

map_delete(K,M0) = M1 :-
    ( is_foreign(M0,Map0:model_map) ->
        map.delete(model_to_key(K),Map0,Map1),
        M1 = foreign(Map1)
    ;   M1 = undefined_model
    ).

%---------------------------------------------------------------------------%

:- func map_lookup(model,model) = model.

:- pragma foreign_export("C",map_lookup(in,in) = out,"CR_map_lookup").

map_lookup(M,K) = V :-
    ( is_foreign(M,Map) ->
        ( search(Map,model_to_key(K),V0) ->
            V = V0
        ;   V = undefined_model
        )
    ;   V = undefined_model
    ).

%---------------------------------------------------------------------------%

:- func functor(model) = model.

:- pragma foreign_proc("C",functor(Model::in) = (Func::out),
        [will_not_call_mercury,thread_safe,promise_pure,will_not_modify_trail],"
    Func = CR_functor(Model);
").

%---------------------------------------------------------------------------%

lib_dummy :-
    true.

%---------------------------------------------------------------------------%
:- end_module lib.
%---------------------------------------------------------------------------%
