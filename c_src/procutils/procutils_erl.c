#include <stdint.h>
#include "erl_nif.h"

extern int32_t OS_getppid();
extern int32_t OS_pingpid(int32_t pid);
extern int32_t OS_waitpid(int32_t pid, int32_t *statusp, int32_t flags);
extern int32_t OS_launch_process(char *args[]);
extern void randombytes(unsigned char *x,int xlen);


static ERL_NIF_TERM os_get_ppid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret = OS_getppid();
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM os_pingpid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = OS_pingpid(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM randombytes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x;
    ERL_NIF_TERM term;
    unsigned char* binary = 0;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    
    binary = enif_make_new_binary(env, x, &term);
    randombytes(binary, x);
    return term;
}

//name, arity, function
static ErlNifFunc nif_funcs[] = {
    {"os_get_ppid", 0, os_get_ppid_nif},
    {"os_pingpid", 1, os_pingpid_nif},
    {"randombytes", 1, randombytes_nif}
};

ERL_NIF_INIT(procutils, nif_funcs, NULL, NULL, NULL, NULL)