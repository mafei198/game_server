#include "erl_nif.h"
#include "mtwist.c"
#include <stdint.h>

static ERL_NIF_TERM seed_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    int number;
    if (!enif_get_int(env, argv[0], &number)) {
	    return enif_make_badarg(env);
    }
    mt_seed32new(number);
    return enif_make_int(env, 1);
}

static ERL_NIF_TERM rand_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    double rand_value = mt_drand();
    return enif_make_double(env, rand_value);
}

static ERL_NIF_TERM seed_state_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    int number;
    if (!enif_get_int(env, argv[0], &number)) {
	    return enif_make_badarg(env);
    }
    mt_state *my_state = (mt_state*)malloc(sizeof(mt_state));
    mts_seed32new(my_state, number);
    return enif_make_ulong(env, (uintptr_t)(my_state));
}

static ERL_NIF_TERM rand_state_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    uintptr_t number;
    if (!enif_get_ulong(env, argv[0], &number)) {
	    return enif_make_badarg(env);
    }
    mt_state *my_state = (mt_state *)number;
    double rand_value = mts_drand(my_state);
    return enif_make_double(env, rand_value);
}

static ERL_NIF_TERM free_state_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    uintptr_t number;
    if (!enif_get_ulong(env, argv[0], &number)) {
	    return enif_make_badarg(env);
    }
    mt_state *my_state = (mt_state *)number;
    free(my_state);
    return enif_make_int(env, 1);
}

static ErlNifFunc nif_funcs[] = {
    {"seed", 1, seed_nif},
    {"uniform", 0, rand_nif},

    {"new", 1, seed_state_nif},
    {"rand", 1, rand_state_nif},
    {"free", 1, free_state_nif}
};

ERL_NIF_INIT(mtwist, nif_funcs, NULL, NULL, NULL, NULL)
