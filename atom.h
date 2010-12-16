#pragma once
struct Environment;

Environment* atom_api_open();
void atom_api_close(Environment* env);
void atom_api_loadfile(Environment* env, const char* filename);
