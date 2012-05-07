#pragma once
#include <string.h>
#include <stdbool.h>

struct Continuation;

struct Continuation* atom_api_open();
void atom_api_close(struct Continuation* env);
void atom_api_loadfile(struct Continuation* env, const char* filename);
void atom_api_load(struct Continuation* cont, const char* data, size_t length);
void atom_api_repl(struct Continuation* env);

double atom_api_to_number(struct Continuation* cont, int n);
bool atom_api_to_boolean(struct Continuation* cont, int n);
const char* atom_api_to_string(struct Continuation* cont, int n);
size_t atom_api_get_top(struct Continuation* cont);
void atom_api_clear(struct Continuation* cont);

#define atom_api_loads(cont, data) (atom_api_load((cont), (data), strlen(data)))