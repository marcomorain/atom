#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "linenoise.h"
#include "atom.h"

int test(int argc, char** argv);


static bool match(const char* input, const char* a, const char* b)
{
    return strcmp(input, a) == 0 ||
    strcmp(input, b) == 0;
}

static void do_repl(atom_state* cont)
{
    for (;;)
    {
        char* line = linenoise("> ");
        
        if (!line) // eof/ctrl+d
        {
            break;
        }
        
        if (*line)
        {
            linenoiseHistoryAdd(line);
            atom_state_load(cont, line);
        }
        
        free(line);
    }
}

static const char* history = ".atom_history";


int main (int argc, char** argv)
{
    atom_state* atom = atom_state_new();

    for (int i=1; i<argc; i++)
    {
        atom_state_load_file(atom, argv[i]);
    }
    
    linenoiseHistoryLoad((char*)history);
    do_repl(atom);
    linenoiseHistorySave((char*)history);
    
    atom_state_free(atom);
    return EXIT_SUCCESS;
}