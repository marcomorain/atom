#pragma once
#include <string.h>

struct Continuation;

struct Continuation* atom_api_open();
void atom_api_close(struct Continuation* env);
void atom_api_loadfile(struct Continuation* env, const char* filename);
void atom_api_load(struct Continuation* cont, const char* data, size_t length);
void atom_api_repl(struct Continuation* env);