#pragma once
struct Continuation;

Continuation* atom_api_open();
void atom_api_close(Continuation* env);
void atom_api_loadfile(Continuation* env, const char* filename);
void atom_api_repl(Continuation* env);

// Return the type of the object at the given stack index.
int atom_api_gettop(Continuation* env);
int atom_api_type(Continuation* env, int index);
double atom_api_tonumber(Continuation* env, int index);

int atom_aux_tointeger(Continuation* env, int index);
