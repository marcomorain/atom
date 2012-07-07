#include <stdlib.h>
#include <stdio.h>
#include <string.h>


// For REPL
extern "C" {
#include "linenoise.h"
#include "atom.h"
}

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
    if (test(argc, argv) != 0)
    {
        return EXIT_FAILURE;
    } else
    {
//        return EXIT_SUCCESS;
    }
    
    atom_state* atom = atom_state_new();
    
    bool repl = false;
    bool file = false;
    const char* filename = NULL;
    
    for (int i=1; i<argc; i++)
    {
        if (match(argv[i], "-i", "--interactive"))
        {
            repl = true;
        }
        else if (match(argv[i], "-f", "--file"))
        {
            i++;
            if (i == argc)
            {
                fprintf(stderr, "filename expected");
                return EXIT_FAILURE;
            }
            file = true;
            filename = argv[i];
        }
    }
    
    if (!file)
    {
        filename = "/Users/marcomorain/dev/scheme/test/the_little_schemer.scm";
    }
    
    printf("Loading input from %s\n", filename);
    
    atom_state_load_file(atom, filename);
    
    if (repl)
    {
        linenoiseHistoryLoad((char*)history);
        printf("Now doing the REPL\n");
        do_repl(atom);
        linenoiseHistorySave((char*)history);
    }
    else
    {
        printf("No REPL.\n");
    }
    
    atom_state_free(atom);
    
    printf("atom shutdwn ok\n");
    
    return EXIT_SUCCESS;
}