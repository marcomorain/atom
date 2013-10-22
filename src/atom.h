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


// Libraries
void atom_load_libraries(atom_state* state);

// Internal
struct Environment;
typedef struct Environment Environment;

// Calling convention:
// Pop params off the stack
// Push (1) the result.
typedef void (*atom_builtin) (Environment* env, int params);

struct Library
{
    const char*  name;
    atom_builtin func;
};

double atom_pop_number(Environment* env);
void atom_push_number(Environment* env, double x);
void atom_add_builtin(atom_state* state, const char* name, atom_builtin function);

int atom_type(atom_state* state, int n);
