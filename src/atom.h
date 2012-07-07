#pragma once
#include <string.h>
#include <stdbool.h>

// Create a new atom state.
struct atom_state*  atom_state_new();
void                atom_state_free(struct atom_state* state);
void                atom_state_load(struct atom_state* state,
                                           const char* data);
void                atom_state_load_file(struct atom_state* state,
                                         const char* filename);
void                atom_state_repl(struct atom_state* state);

double atom_api_to_number(struct atom_state* cont, int n);
bool atom_api_to_boolean(struct atom_state* cont, int n);
const char* atom_api_to_string(struct atom_state* cont, int n);
size_t atom_api_get_top(struct atom_state* cont);
void atom_api_clear(struct atom_state* cont);