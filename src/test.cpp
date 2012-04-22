#include <stdio.h>
#include "../src/atom.h"

/* file: minunit.h */
static int tests_run = 0;
#define mu_assert(message, test) do { if (!(test)) return (char*)message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; if (message) return message; } while (0)
#define mu_str_expand(token) #token
#define mu_str(token)        mu_str_expand(token)
#define mu_assert_msg(test)  mu_assert(mu_str(test), (test))

int foo = 7;
int bar = 5;

static char * test_open_close() {
    
    struct Continuation* c = atom_api_open();
    mu_assert_msg(c != 0);
    atom_api_close(c);
    
    c = atom_api_open();
    mu_assert_msg(c != 0);
    atom_api_close(c);
    
    return 0;
}

static char* test_comile() {
    struct Continuation* atom = atom_api_open();
    atom_api_loads(atom, "1");
    double number = atom_api_to_number(atom, 1);
    mu_assert_msg(number == 1.0);
    mu_assert_msg(atom_api_get_top(atom) == 1);
    
    atom_api_clear(atom);
    mu_assert_msg(atom_api_get_top(atom) == 0);
    
    atom_api_loads(atom, "\"foo\"");
    mu_assert_msg(atom_api_get_top(atom) == 1);

    const char* str = atom_api_to_string(atom, 1);
    mu_assert_msg(strcmp(str, "foo") == 0);
    
    atom_api_close(atom);

    return 0;
}

static char* test_plus() {
    
    struct Continuation* atom = atom_api_open();
    atom_api_loads(atom, "(+ 1 2)");
    double number = atom_api_to_number(atom, 1);
    mu_assert_msg(number == 3.0);
    mu_assert_msg(atom_api_get_top(atom) == 1);
    atom_api_close(atom);    
    return 0;
}

static char * all_tests() {
    mu_run_test(test_open_close);
    mu_run_test(test_comile);
    mu_run_test(test_plus);
    return 0;
}

int test(int argc, char* *argv)
{
    char *result = all_tests();
    if (result != 0) {
        printf("TEST FAILED: %s\n", result);
    }
    else {
        printf("ALL TESTS PASSED\n");
    }
    printf("Tests run: %d\n", tests_run);
    
    return result != 0;
}
