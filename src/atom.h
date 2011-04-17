#pragma once
struct Continuation;

Continuation* atom_api_open();
void atom_api_close(Continuation* env);
void atom_api_loadfile(Continuation* env, const char* filename);
void atom_api_repl(Continuation* env);