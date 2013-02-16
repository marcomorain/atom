#pragma once
#include <string.h>
#include <stdbool.h>

typedef struct atom_state atom_state;

// Create a new atom state.
struct atom_state*  atom_state_new();
void                atom_state_free(atom_state* state);
void                atom_state_load(atom_state* state, const char* data);
void                atom_state_load_file(atom_state* state, const char* filename);

double atom_state_pop_number(atom_state* state);

bool atom_api_to_boolean(atom_state* cont, int n);
const char* atom_api_to_string(atom_state* cont, int n);
size_t atom_api_get_top(atom_state* cont);
void atom_api_clear(atom_state* cont);