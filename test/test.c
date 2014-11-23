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


static atom_state* atom_with_code(const char* code)
{
    struct atom_state* atom = atom_state_new();
    atom_load_libraries(atom);
    atom_state_load(atom, code);
    return atom;
}

static char * test_open_close() {
    
    atom_state* c = atom_state_new();
    mu_assert_msg(c != 0);
    atom_state_free(c);
    
    c = atom_state_new();
    mu_assert_msg(c != 0);
    atom_state_free(c);
    
    return 0;
}

static char* test_comile() {
    struct atom_state* atom = atom_state_new();
    atom_state_load(atom, "1");
    double number = atom_state_pop_number(atom);
    mu_assert_msg(number == 1.0);
    mu_assert_msg(atom_api_get_top(atom) == 0);
    
    atom_state_load(atom, "\"foo\"");
    mu_assert_msg(atom_api_get_top(atom) == 1);

    const char* str = atom_api_to_string(atom, 1);
    mu_assert_msg(strcmp(str, "foo") == 0);
    
    atom_state_free(atom);

    return 0;
}


static double do_numeric_operation(struct atom_state* atom, const char* op)
{
    atom_state_load(atom, op);
    return atom_state_pop_number(atom);
}

static char* test_plus() {
    
    struct atom_state* atom = atom_state_new();

    atom_load_libraries(atom);

    mu_assert_msg(do_numeric_operation(atom, "(+)") == 0);
    mu_assert_msg(do_numeric_operation(atom, "(+ 7)") == 7);
    mu_assert_msg(do_numeric_operation(atom, "(+ 1 2 3 4)") == 10);
    mu_assert_msg(do_numeric_operation(atom, "(*)") == 1);
    mu_assert_msg(do_numeric_operation(atom, "(* 1 2 )") == 2);
    mu_assert_msg(do_numeric_operation(atom, "(* 10 9 1)") == 90);
    mu_assert_msg(do_numeric_operation(atom, "(* 2 3 4)") == 24);
    
    mu_assert_msg(do_numeric_operation(atom, "(- 1)") == -1);
    mu_assert_msg(do_numeric_operation(atom, "(- 101 2)") == 99);
    mu_assert_msg(do_numeric_operation(atom, "(- 2 5)") == -3);
    mu_assert_msg(do_numeric_operation(atom, "(abs 5)") == 5);

    atom_state_free(atom);    
    return 0;
}

static char* test_state() {
    struct atom_state* atom = atom_state_new();
    atom_state_load(atom, "(define x 17)");
    atom_api_clear(atom);
    mu_assert_msg(do_numeric_operation(atom, "x") == 17);
    
    atom_state_load(atom, "(set! x 9)");
    atom_api_clear(atom);

    mu_assert_msg(do_numeric_operation(atom, "x") == 9);
    
    atom_state_free(atom);    
    return 0;
}

static bool do_boolean_operation(struct atom_state* atom, const char* op)
{
    atom_state_load(atom, op);
    bool result = atom_api_to_boolean(atom, 1);
    atom_api_clear(atom);
    return result;
}

static char* test_equality() {
    struct atom_state* atom = atom_state_new();
    mu_assert_msg( do_boolean_operation(atom, "(eq? 5 5)"));
    mu_assert_msg(!do_boolean_operation(atom, "(eq? 1 2)"));
    mu_assert_msg(!do_boolean_operation(atom, "(eq? \"foo\" \"bar\")"));
    atom_state_free(atom);    
    return 0;
}

static char* test_quote()
{
    struct atom_state* atom = atom_state_new();
    atom_state_load(atom, "(quote a)");
    atom_api_clear(atom);
    atom_state_load(atom, "'a");
    atom_api_clear(atom);
    atom_state_free(atom);    
    return 0;    
}

static char* test_if()
{
    struct atom_state* atom = atom_state_new();
    mu_assert_msg( do_boolean_operation(atom, "(if 1 2 3)"));
    atom_state_free(atom);    
    return 0;    
}

static char* test_list()
{
    struct atom_state* atom = atom_with_code("(list 1 2 3)");
    int type = atom_type(atom, 0);
    mu_assert_msg(type == 5);
    return 0;
}

static char* test_lambda()
{
    struct atom_state* atom = atom_with_code("(define plus (lambda (x) (+ x 1)))");
    atom_api_clear(atom);
    atom_state_load(atom, "(plus 4)");
    mu_assert_msg(atom_state_pop_number(atom) == 5);
    atom_state_free(atom);
    return 0;
}

static char* test_define_short() {
    struct atom_state* atom = atom_state_new();
    atom_load_libraries(atom);
    atom_state_load(atom, "(define (square x) (* x x))");
    atom_api_clear(atom);
    atom_state_load(atom, "(square 2)");
    mu_assert_msg(atom_state_pop_number(atom) == 4);
    atom_state_load(atom, "(square 3)");
    mu_assert_msg(atom_state_pop_number(atom) == 9);
    atom_state_free(atom);
    return 0;
}

static char* test_macros()
{
    struct atom_state* atom = atom_state_new();
    atom_state_load(atom,
        "     (define-syntax and "
        "         (syntax-rules () "
        "            ((and) #t) "
        "            ((and test) test) "
        "            ((and test1 test2 ...) "
        "             (if test1 (and test2 ...) #f)))) ");
    
    atom_state_load(atom,    
    " (define-syntax or "
    " (syntax-rules () "
    "  ((or) #f) "
    "  ((or test) test) "
    "  ((or test1 test2 ...) "
    "   (let ((x test1)) "
    "    (if x x (or test2 ...))))))");
    
    atom_state_free(atom);
    return 0;
}

static char * all_tests() {
    //mu_run_test(test_macros);
    mu_run_test(test_define_short);
    mu_run_test(test_list);
    mu_run_test(test_quote);
    mu_run_test(test_comile);
    mu_run_test(test_lambda);
    mu_run_test(test_if);
    mu_run_test(test_equality);
    mu_run_test(test_state);
    mu_run_test(test_open_close);
    mu_run_test(test_plus);
    return 0;
}

int main(int argc, char* *argv)
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
