// gcc -std=c99 -Wall -Werror atom.c
#include "atom.h"

#define _CRT_SECURE_NO_WARNINGS
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <math.h>
#include <ctype.h>
#include "kvec.h"

#define DEBUG_LEXER (0)

#if (DEBUG_LEXER)
#define LEXER_TRACE(format, ...) printf(format, __VA_ARGS__)
#else
#define LEXER_TRACE(format, ...)
#endif

static unsigned int MurmurHash2 (const void * key, int len);

typedef enum {
    TYPE_BOOLEAN,
    TYPE_CHARACTER,
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_EMPTY_LIST,
    TYPE_PAIR,
    TYPE_VECTOR,
    TYPE_SYMBOL,
    TYPE_BUILT_IN,
    TYPE_PROCEDURE,
    TYPE_SYNTAX_RULES,
    TYPE_INPUT_PORT,
    TYPE_OUTPUT_PORT,
    TYPE_ENVIRONMENT,
    MAX_TYPES
} cell_type;

const static char* typenames [] = {
    [TYPE_BOOLEAN]      = "boolean",
    [TYPE_CHARACTER]    = "character",
	[TYPE_NUMBER]       = "number",
	[TYPE_STRING]       = "string",
    [TYPE_EMPTY_LIST]   = "empty list",
	[TYPE_PAIR]         = "pair",
	[TYPE_VECTOR]       = "vector",
	[TYPE_SYMBOL]       = "symbol",
	[TYPE_BUILT_IN]     = "procedure",
    [TYPE_PROCEDURE]    = "procedure",
    [TYPE_SYNTAX_RULES] = "syntax rules",
    [TYPE_INPUT_PORT]   = "input post",
    [TYPE_OUTPUT_PORT]  = "output port",
    [TYPE_ENVIRONMENT]  = "environment"
};

struct Environment;
struct atom_state;
struct Cell;
struct Symbol;

typedef struct Cell Cell;
typedef struct String String;
typedef struct Pair Pair;
typedef struct Symbol Symbol;
typedef struct Procedure Procedure;
typedef struct Token Token;
typedef kvec_t(Cell*) Vector;

static struct Cell* vector_get(const Vector* vector, int n)
{
    assert(n >= 0);
    assert(n < kv_size(*vector));
    return kv_A(*vector, n);
}

static void vector_set(Vector* vector, int n, Cell* cell)
{
    assert(n >= 0);
    assert(n < kv_size(*vector));
    kv_A(*vector, n) = cell;
}

static int vector_length(Vector* vector)
{
    return (int)kv_size(*vector);
}

static void vector_fill(Vector* vector, Cell* fill)
{
	for (int i=0; i<vector_length(vector); i++)
	{
        vector_set(vector, i, fill);
	}
}

static void vector_init(Vector* vector, int length, Cell* fill)
{
    kv_init(*vector);
    for (int i=0; i<length; i++) {
        kv_push(Cell*, *vector, fill);
    }
    assert(kv_size(*vector) == length);
}

// todo: remove
static void vector_init_empty(Vector* vector)
{
    kv_init(*vector);
}

// todo: remove?
static Cell* vector_pop(Vector* vector)
{
    assert(vector_length(vector) > 0);
    return kv_pop(*vector);
}

static void vector_push(Vector* vector, Cell* cell)
{
    kv_push(Cell*, *vector, cell);
}

struct Pair
{
    Cell* car;
    Cell* cdr;
};

// todo: add a const string type? or a flag?
struct String
{
    char* data;
    int   length;
    bool  mutable;
};

union cell_data
{
    bool            boolean;
    char            character;
    double          number;
    String          string;
    Pair            pair;
    const Symbol*   symbol;
    Vector          vector;
    atom_builtin    built_in;
    struct Procedure* closure;
    FILE*           port;
    Environment*    env;
};

struct Cell
{
	cell_type       type;
	union cell_data	data;
	Cell*           next;
	bool            mark;
};

// TODO: The garbage collector marks this special objects.
static Cell cell_empty_list = { TYPE_EMPTY_LIST, {NULL}, NULL, false };
static Cell cell_true       = { TYPE_BOOLEAN,   {true }, NULL, false };
static Cell cell_false      = { TYPE_BOOLEAN,   {false}, NULL, false };

static Cell* car(const Cell* cell)
{
	assert(cell->type == TYPE_PAIR);
	return cell->data.pair.car;
}

static Cell* cdr(const Cell* cell)
{
	assert(cell->type == TYPE_PAIR);
	return cell->data.pair.cdr;
}

static bool is_pair(const Cell* cell)
{
    assert(cell);
    return cell->type == TYPE_PAIR;
}

enum TokenType
{
    TOKEN_NONE,
	TOKEN_IDENTIFIER,
	TOKEN_BOOLEAN,
	TOKEN_NUMBER,
	TOKEN_CHARACTER,
	TOKEN_STRING,
	TOKEN_LIST_START,
	TOKEN_LIST_END,
    TOKEN_VECTOR_START,
	TOKEN_QUOTE,
	TOKEN_BACKTICK,
	TOKEN_COMMA,
	TOKEN_COMMA_AT,
	TOKEN_DOT,
    TOKEN_ELLIPSIS
};

struct Symbol
{
    // TODO: allocate the string data right on the end of this struct.
    // This will reduce memory, fragments, cache misses, etc.
    size_t  hash;
    Symbol* next;
    char*   name;
};

static void print_external_rep(FILE* file, char c)
{
    switch(c)
    {
        case ' ':
            fprintf(file, "#\\space");
            break;

        case '\n':
            fprintf(file, "#\\newline");
            break;

        default:
            fprintf(file, "#\\%c", c);
            break;
    }
}

static void print_rec_impl(FILE* output, const Cell* cell, bool human, int is_car)
{
    assert(cell);

	switch(cell->type)
	{
        case TYPE_EMPTY_LIST:
            fprintf(output, "()");
            break;

        case TYPE_BOOLEAN:
            fprintf(output, "#%c", (cell->data.boolean ? 't' : 'f'));
            break;

        case TYPE_NUMBER:
            fprintf(output, "%lg", cell->data.number);
            break;

        case TYPE_CHARACTER:
		{
			char c = cell->data.character;
		    if (human)
                fputc(c, output);
		    else
                print_external_rep(output, c);
			break;
		}

        case TYPE_STRING:
            fprintf(output, human ? "%s" : "\"%s\"", cell->data.string.data);
            break;

        case TYPE_SYMBOL:
            fprintf(output, "%s", cell->data.symbol->name);
            break;

        case TYPE_PAIR:
        {
            if (is_car) fprintf(output, "(");
            print_rec_impl(output, cell->data.pair.car, human, 1);

            Cell* c = cdr(cell);

            if (c->type != TYPE_EMPTY_LIST)
            {
                fprintf(output, " ");
                print_rec_impl(output, c, human, 0);
            }

            if (is_car) fprintf(output, ")");
        }
        break;

        case TYPE_VECTOR:
            fprintf(output, "#(");
            for (int i=0; i < cell->data.vector.n; i++)
            {
                if (i>0) fprintf(output, " ");
                print_rec_impl(output, vector_get(&cell->data.vector, i), human, 0);
            }
            fprintf(output, ")");
            break;


        default:
            fprintf(output, "#<%s %p>", typenames[cell->type], cell->data.port);
            break;
    }
}

static void print_rec(FILE* output, const Cell* cell, bool human) {
    print_rec_impl(output, cell, human, 1);
}

static void print(FILE* output, const Cell* cell, bool human)
{
	print_rec(output, cell, human);
	fprintf(output, "\n");
}

static bool power_of_two(int v)
{
	return v && !(v & (v - 1));
}

static bool is_integer(double d)
{
	return d == (int)d;
}

struct symbol_node
{
    const Symbol*       symbol;
    Cell*               value;
    struct symbol_node* next;
};

typedef struct symbol_node symbol_node;

struct Environment
{
	atom_state*     cont;
	Environment*    parent;
	symbol_node**	data;
	unsigned        mask;
};

void environment_init(Environment* env, atom_state* c, int size, Environment* parent_env)
{
    assert(power_of_two(size));
    env->mask = size-1;
    env->data = (symbol_node**)calloc(size, sizeof(symbol_node*));
    env->parent = parent_env;
    env->cont = c;
}


struct jump_buffer
{
	jmp_buf             buffer;
	struct jump_buffer* prev;
};
typedef struct jump_buffer jump_buffer;

struct atom_state
{
	Environment*        env;
	Cell*               cells;
	struct jump_buffer* escape;
	int                 allocated;
	FILE*               input;
    FILE*               output;
    FILE*               log; // not really used right now
    Vector              stack;

    // The symbol table
    Symbol**        symbols;

    // The size of the symbol table, minus one for masking
    size_t          symbol_mask;

    // The number of symbols that exist in the table
    // This is used to know when to grow.
    size_t          symbol_count;

    // TODO:
    // 1. count every byte allocated
    // 2. free all memory on close
};

struct Instruction
{
    int op_code;
    int operand;
};

typedef struct Instruction Instruction;


// Buffers
typedef kvec_t(Instruction) instruction_buffer;
typedef kvec_t(char)        character_buffer;


struct Procedure
{
    // The number of parameters that this function expects.
    int nparams;
    Instruction* instructions;
    int num_instructions;
    Vector constants;
    Cell* source;
};

// Maybe insert a new symbol into the Cont's symbol table.
// Or return an existing one if the name is already in the table
static Symbol* find_or_insert_symbol(atom_state* cont, const char* name)
{
    // TODO: This function assumes that there are no embedded nulls in name

    const size_t hash = cont->symbol_mask & MurmurHash2(name, (int)strlen(name));

    for (Symbol* symbol = cont->symbols[hash]; symbol; symbol = symbol->next)
    {
        if (strcmp(name, symbol->name) == 0)
        {
            return symbol;
        }
    }

    if (cont->symbol_count > (cont->symbol_mask / 2))
    {
        // TODO: grow
    }

    Symbol* new_symbol = (Symbol*)malloc(sizeof(Symbol));

    // Insert front of linked list.
    new_symbol->name = strdup(name);
    new_symbol->next = cont->symbols[hash];
    cont->symbols[hash] = new_symbol;
    return new_symbol;
}

void signal_error(atom_state* cont, const char* message, ...)
{
	va_list args;
	va_start(args, message);
	fprintf(stderr, "Error: ");
	vfprintf(stderr, message, args);
	fprintf(stderr, "\n");
	va_end(args);
	longjmp(cont->escape->buffer, 1);
}

static void type_check(atom_state* cont, int expected, int actual)
{
	if (actual != expected)
	{
		signal_error(cont, "%s expected, got %s",
                     typenames[expected],
                     typenames[actual]);
	}
}

static Cell* make_cell(Environment* env, int type)
{
	Cell* result = (Cell*)calloc(1, sizeof(Cell));
	result->type = type;

	// stick on the first item in the linked list
	result->next = env->cont->cells;
	env->cont->cells = result;
	env->cont->allocated++;

	return result;
}

static Cell* make_symbol(Environment* env, const char* name)
{
    Cell* symbol        = make_cell(env, TYPE_SYMBOL);
    symbol->data.symbol = find_or_insert_symbol(env->cont, name);
    return symbol;
}

static Cell* make_io_port(Environment* env, int type, FILE* port)
{
	Cell* cell = make_cell(env, type);
	cell->data.port = port;
	return cell;
}

static Cell* make_closure(Environment* env, Procedure* closure)
{
    Cell* cell = make_cell(env, TYPE_PROCEDURE);
    cell->data.closure = closure;
    return cell;
}


static Cell* make_input_port(Environment* env, FILE* port)
{
    return make_io_port(env, TYPE_INPUT_PORT, port);
}

static Cell* make_output_port(Environment* env, FILE* port)
{
    return make_io_port(env, TYPE_OUTPUT_PORT, port);
}

static void mark(Cell* cell, size_t marked[]);

static void mark_environment(Environment* env, size_t marked[])
{
	for (unsigned i = 0; i <= env->mask; i++)
	{
		for (symbol_node* node = env->data[i]; node; node = node->next)
		{
			mark(node->value, marked);
		}
	}
}

static void mark_closure(Procedure* closure, size_t marked[])
{
    for (int i=0; i < vector_length(&closure->constants); i++)
        mark(vector_get(&closure->constants, i), marked);
}

static void mark(Cell* cell, size_t marked[])
{
	if (!cell || cell->mark) return;

	cell->mark = true;

    marked[cell->type]++;

	switch(cell->type)
	{
		case TYPE_PAIR:
			mark(cell->data.pair.car, marked);
			mark(cell->data.pair.cdr, marked);
			break;

		case TYPE_VECTOR:
            for(int i=0; i<cell->data.vector.n; i++)
                mark(vector_get(&cell->data.vector, i), marked);
			break;

		case TYPE_PROCEDURE:
            mark_closure(cell->data.closure, marked);
			break;

        case TYPE_ENVIRONMENT:
            mark_environment(cell->data.env, marked);
            break;

        default:
			break;
	}
}

static void print_type_table(FILE* log, size_t marked[], size_t kept[], size_t freed[])
{
    size_t total_marked = 0;
    size_t total_kept   = 0;
    size_t total_freed  = 0;
    fputs("|=================================|", log);
    fprintf(log, "|%-12s|%-6s|%-6s|%-6s|\n", "TYPE", "MARKED", "KEPT", "FREED");
    for (int i=0; i<MAX_TYPES; i++)
    {
        fprintf(log, "|%-12s|%6ld|%6ld|%6ld|\n", typenames[i], marked[i], kept[i], freed[i]);
        total_marked += marked[i];
        total_kept   += kept[i];
        total_freed  += freed[i];
    }
    fprintf(log, "|%-12s|%6ld|%6ld|%6ld|\n", "Total", total_marked, total_kept, total_freed);
    fputs("|=================================|\n", log);
}

static void mark_atom_state(atom_state* cont, size_t marked[])
{
	mark_environment(cont->env, marked);
    for (int i=0; i<vector_length(&cont->stack); i++)
        mark(vector_get(&cont->stack, i), marked);
}

static void sweep(atom_state* cont, size_t kept[], size_t freed[])
{
	Cell* remaining = NULL;
	Cell* next = NULL;

	for (Cell* cell = cont->cells; cell; cell = next)
	{
		next = cell->next;

		if (cell->mark)
		{
            kept[cell->type]++;
			cell->mark = false;
			cell->next = remaining;
			remaining = cell;
		}
		else
		{
            freed[cell->type]++;
		    switch(cell->type)
		    {
		        case TYPE_INPUT_PORT:
                    if (cell->data.port != stdin)
                    {
                        fclose(cell->data.port);
                    }
                    break;

                case TYPE_OUTPUT_PORT:
                    if (cell->data.port != stdout)
                    {
                        fclose(cell->data.port);
                    }
                    break;

                case TYPE_STRING:
                    free(cell->data.string.data);
                    break;

                case TYPE_VECTOR:
                    kv_destroy(cell->data.vector);
                    break;

                default:
                    break;
		    }
			cont->allocated--;
			free(cell);
		}
	}

	cont->cells = remaining;
}

static void collect_garbage(atom_state* cont)
{
    size_t marked[MAX_TYPES] = {};
    size_t kept  [MAX_TYPES] = {};
    size_t freed [MAX_TYPES] = {};

    mark_atom_state(cont, marked);
    sweep(cont, kept, freed);

    //print_type_table(marked, kept, freed);
}

static Cell* make_boolean(bool value)
{
	return value ? &cell_true : &cell_false;
}

static Cell* make_number(Environment* env, double value)
{
	Cell* number = make_cell(env, TYPE_NUMBER);
	number->data.number = value;
	return number;
}

static Cell* make_character(Environment* env, char c)
{
	Cell* character = make_cell(env, TYPE_CHARACTER);
	character->data.character = c;
	return character;
}

static Cell* make_vector(Environment* env, int length, Cell* fill)
{
	Cell* vec = make_cell(env, TYPE_VECTOR);
    vector_init(&vec->data.vector, length, fill);
	return vec;
}

static Cell* make_empty_string(Environment* env, int length)
{
    Cell* string = make_cell(env, TYPE_STRING);
    string->data.string.mutable = true;
    string->data.string.length  = length;
    string->data.string.data    = (char*)calloc(length+1, sizeof(char));

    // Assert if the allocation fails.
    // TODO: handle this.
    assert(string->data.string.data);

    return string;
}

static Cell* fill_string(Cell* str, int length, const char* data)
{
    assert(str->type == TYPE_STRING);
    strncpy(str->data.string.data, data, length);
    return str;
}

static Cell* make_string(Environment* env, int length, const char* data)
{
    // Fail early if the length is wrong.
    assert(length == (int)strlen(data));
    return fill_string(make_empty_string(env, length), length, data);
}

static Cell* make_string_filled(Environment* env, int length, char fill)
{
    Cell* string = make_empty_string(env, length);
	memset(string->data.string.data, fill, length);
    assert((int)strlen(string->data.string.data) == length);
	return string;
}

static void set_car(Cell* list, Cell* car)
{
	assert(list->type == TYPE_PAIR);
    assert(car);
	list->data.pair.car = car;
}

static void set_cdr(Cell* list, Cell* cdr)
{
	assert(list->type == TYPE_PAIR);
    assert(cdr);
	list->data.pair.cdr = cdr;
}

static Cell* cons(Environment* env, Cell* car, Cell* cdr)
{
    assert(car);
    assert(cdr);
	Cell* cell = make_cell(env, TYPE_PAIR);
	set_car(cell, car);
	set_cdr(cell, cdr);
	return cell;
}

struct Token
{
	union Data
	{
		double		number;
		bool		boolean;
		char*		string;
		const char*	identifier;
		char		character;
	} data;
	enum TokenType type;
};

static void token_print(Token* token)
{
    switch(token->type)
    {
        case TOKEN_NUMBER:
            LEXER_TRACE("Token TOKEN_NUMBER %lg\n", token->data.number);
            break;

        case TOKEN_IDENTIFIER:
            LEXER_TRACE("Token TOKEN_IDENTIFIER %s\n", token->data.identifier);
            break;

        case TOKEN_STRING:
            LEXER_TRACE("Token TOKEN_STRING \"%s\"\n", token->data.string);
            break;

#define PRINT_CASE(id) case id: LEXER_TRACE("Token %s\n", #id); break
            PRINT_CASE(TOKEN_NONE);
            PRINT_CASE(TOKEN_BOOLEAN);
            PRINT_CASE(TOKEN_CHARACTER);
            PRINT_CASE(TOKEN_LIST_START);
            PRINT_CASE(TOKEN_LIST_END);
            PRINT_CASE(TOKEN_VECTOR_START);
            PRINT_CASE(TOKEN_QUOTE);
            PRINT_CASE(TOKEN_BACKTICK);
            PRINT_CASE(TOKEN_COMMA);
            PRINT_CASE(TOKEN_COMMA_AT);
            PRINT_CASE(TOKEN_DOT);
            PRINT_CASE(TOKEN_ELLIPSIS);
#undef PRINT_CASE
    }
}

static void token_init(Token* token, enum TokenType type)
{
    token->type = type;
    token_print(token);
}

static void token_backtick(Token* token)
{
    token_init(token, TOKEN_BACKTICK);
}

static void token_list_start(Token* token)
{
    token_init(token, TOKEN_LIST_START);
}

static void token_list_end(Token* token)
{
    token_init(token, TOKEN_LIST_END);
}

static void token_vector_start(Token* token)
{
    token_init(token, TOKEN_VECTOR_START);
}

static void token_quote(Token* token)
{
    token_init(token, TOKEN_QUOTE);
}

static void token_dot(Token* token)
{
    token_init(token, TOKEN_DOT);
}

static void token_comma_at(Token* token)
{
    token_init(token, TOKEN_COMMA_AT);
}

static void token_comma(Token* token)
{
    token_init(token, TOKEN_COMMA);
}

static void token_number(Token* token, double number)
{
    token->type = TOKEN_NUMBER;
    token->data.number = number;
    token_print(token);
}

static void token_character(Token* token, char value)
{
    token->type = TOKEN_CHARACTER;
    token->data.character = value;
    token_print(token);
}

static void token_boolean(Token* token, bool value)
{
    token->type = TOKEN_BOOLEAN;
    token->data.boolean = value;
    token_print(token);
}

#define kv_last(type, v) kv_a(type, v, (kv_size(v) - 1))

static void token_identifier(Token* token, character_buffer* buffer)
{
    // ensure valid, null terminated
    assert(kv_a(char, *buffer, kv_size(*buffer)-1) == 0);
    assert(strlen(buffer->a) == buffer->n-1);

    token->type = TOKEN_IDENTIFIER;
    token->data.identifier = strdup(buffer->a);
    token_print(token);

    // todo: single reset function
    kv_destroy(*buffer);
    kv_init(*buffer);
}

static void token_string(Token* token, character_buffer* buffer)
{
    // ensure valid, null terminated
    assert(kv_last(char, *buffer) == 0);
    assert(strlen(buffer->a) == buffer->n-1);

    token->type = TOKEN_STRING;
    token->data.identifier = strdup(buffer->a);
    token_print(token);

    // todo: single reset function
    kv_destroy(*buffer);
    kv_init(*buffer);

}

struct Input
{
	unsigned	  line;
	unsigned	  column;
	const char*   data;
};

typedef struct Input Input;

static char input_get(const Input* input)
{
    return *input->data;
}

static void input_init(Input* input, const char* d)
{
    input->line     = 1;
    input->column	= 1;
    input->data     = d;
}

static char input_next(Input* input)
{
    assert(*input->data);

    input->column++;

    if (*input->data == '\n')
    {
        input->column = 1;
        input->line++;
    }

    input->data++;
    return input_get(input);
};


void syntax_error(Input* input, const char* message)
{
    // TODO: pass in a cont.
    signal_error(NULL, "Syntax error line %d column %d: %s",
                 input->line,
                 input->column,
                 message);
}

void skip_whitespace(Input* input)
{
	for(char c = input_get(input); c; c = input_next(input))
	{
		switch(c)
		{
			case '\n':
			case ' ':
			case '\t':
                continue;

			case ';': // skip comment to end of line
                for (char d = input_next(input); d != '\n'; d = input_next(input))
                {
                    if (!d) return;
                }
                break;

			default: return;
		}
	}
}
// TODO: Make a table of 8 bit values for each of the ASCII characters.
// Use a single bit to indicate whitespace, special initialise, etc.
// return (table[c] & SP_INIT_MASK);
bool is_special_initial(char c)
{
	switch (c)
	{
		case '!':
		case '$':
		case '%':
		case '&':
		case '*':
		case '/':
		case ':':
		case '<':
		case '=':
		case '>':
		case '?':
		case '^':
		case '_':
		case '~':
            return true;
	}

	return false;
}

static bool is_letter(char c)
{
	return !!isalpha(c);
}

static bool is_initial(char c)
{
	return is_letter(c) || is_special_initial(c);
}

bool is_delimeter(char c)
{
	switch (c)
	{
		case 0: // @todo: having null here is a bit of a hack - not in the spec.
		case ' ':
		case '\n':
		case '\t':
		case '"':
		case '(':
		case ')':
		case ';':
			return true;
	}
	return false;
}

bool is_special_subsequent(char c)
{
	switch(c)
	{
		case '+':
		case '-':
		case '.':
		case '@':
			return true;
	}

	return false;
}


static bool is_subsequent(char c)
{
	return is_initial(c) || isdigit(c) || is_special_subsequent(c);
}

static void read_character(Input* input, Token* token)
{
	char c = input_get(input);
	switch(c)
	{
		case 's':
            if (input_next(input) == 'p'){
                if (input_next(input) != 'a') syntax_error(input, "space expected");
                if (input_next(input) != 'c') syntax_error(input, "space expected");
                if (input_next(input) != 'e') syntax_error(input, "space expected");
                if (!is_delimeter(input_next(input))) syntax_error(input, "space expected");
                token_character(token, ' ');
                return;
            }
            else goto success;

		case 'n':
			if (input_next(input) == 'e'){
				if (input_next(input) != 'w') syntax_error(input, "newline expected");
				if (input_next(input) != 'l') syntax_error(input, "newline expected");
				if (input_next(input) != 'i') syntax_error(input, "newline expected");
				if (input_next(input) != 'n') syntax_error(input, "newline expected");
				if (input_next(input) != 'e') syntax_error(input, "newline expected");
				if (!is_delimeter(input_next(input))) syntax_error(input, "newline expected");
                token_character(token, '\n');
                return;
			}
			else goto success;

		default: goto success;
	}

success:

	if (is_delimeter(input_next(input)))
	{
        token_character(token, c);
		return;
	}

	syntax_error(input, "delimeter expected");
}

// Convert an ascii digit, '0' to '9' into
// a double 0.0 to 9.0
static double char_to_double(char c)
{
	assert(c >= '0' && c <= '9');
	return c - '0';
}

void read_number(Input* input, Token* token)
{
	char c = input_get(input);
    
    switch (c) {
            
    }

	double accum = char_to_double(c);

	for (;;)
	{
		c = input_next(input);

		if (!isdigit(c))
		{
			break;
		}

		accum *= 10;
		accum += char_to_double(c);
	}
    
    double magnitude = 0.1;
    
    if (c != '.') {
        token_number(token, accum);
        return;
    } else {
        for (;;) {
            c = input_next(input);
            
            if (!isdigit(c)) {
                token_number(token, accum);
                return;
            }
            
            accum += char_to_double(c) * magnitude;
            magnitude *= 0.1;
        }
    }
}

void read_string(Input* input, Token* token)
{
    character_buffer buffer;
    kv_init(buffer);

	assert(input_get(input) == '"');

	for (;;)
	{
		char c = input_next(input);

		if (c == '"'){
			input_next(input);
            kv_push(char, buffer, 0); // null terminate
            token_string(token, &buffer);
            kv_destroy(buffer);
			return;
		}

		if (c == '\\'){
			c = input_next(input);
			if (c == '"' || c == '\\')
			{
                kv_push(char, buffer, c);
				continue;
			}
			syntax_error(input, "malformed string");
		}

		if (isprint(c))
		{
            kv_push(char, buffer, c);
			continue;
		}

		syntax_error(input, "unexpected character in string");
	}
}

bool is_peculiar_identifier(char c)
{
	// @todo: ... can be accepted here.
	return c == '+' || c == '-';
}

void read_identifier(Input* input, Token* token)
{
    character_buffer buffer;
    kv_init(buffer);

	char c = input_get(input);
	if (is_initial(c))
	{
        kv_push(char, buffer, c);

		for (;;)
		{
			c = input_next(input);
			if (is_delimeter(c)) break;
			if (!is_subsequent(c))
			{
				syntax_error(input, "malformed identifier");
			}
            kv_push(char, buffer, c);
		}
	}
	else if (is_peculiar_identifier(c))
	{
        kv_push(char, buffer, c);
		input_next(input);
	}
	else
	{
		syntax_error(input, "malformed identifier");
	}

    kv_push(char, buffer, 0);
    token_identifier(token, &buffer);
    kv_destroy(buffer);
}

void read_token(Input* input, Token* token)
{
	skip_whitespace(input);
    token->type = TOKEN_NONE;

	char c = input_get(input);

	switch(c)
	{
		case '(':  input_next(input); token_list_start(token); return;
		case ')':  input_next(input); token_list_end(token);   return;
		case '\'': input_next(input); token_quote(token);      return;
		case '`':  input_next(input); token_backtick(token);   return;
		case '.':  input_next(input); token_dot(token);        return;

        case ',':
        {
            input_next(input);
            if(input_get(input) == '@')
            {
                input_next(input);
                token_comma_at(token);
            }
            else
            {
                token_comma(token);
            }

            break;
        }

		case '#':
		{
			c = input_next(input);
			switch(c)
			{
                    // @todo: check for next character here (should be a delimiter)
				case 't':  input_next(input); token_boolean(token, true);   return;
				case 'f':  input_next(input); token_boolean(token, false);  return;
				case '\\': input_next(input); read_character(input, token); return;
                case '(':  input_next(input); token_vector_start(token);    return;
                default:   syntax_error(input, "malformed identifier after #");
			}

			break;
		}

		case '"': read_string(input, token); break;

		case 0: break;

		default:
		{
			if (isdigit(c))
			{
				read_number(input, token);
			}
			else
			{
				read_identifier(input, token);
			}

			break;
		}
	}
}

Cell* parse_datum(Environment* env, Input* input, Token* token);

Cell* parse_abreviation(Environment* env, Input* input, Token* token)
{
    const char* symbol = NULL;
    switch(token->type)
    {
        case TOKEN_QUOTE:
            symbol = "quote";
            break;

        case TOKEN_BACKTICK:
            symbol = "quasiquote";
            break;

        case TOKEN_COMMA:
            symbol = "unquote";
            break;

        case TOKEN_COMMA_AT:
            symbol = "unquote-splicing";
            break;

            // If the token is not one of the above abreviations, then we early out
        default:
            return NULL;
    }

    Cell* abreviation = make_symbol(env, symbol);

    Token body;
    read_token(input, &body);

    return cons(env, abreviation, cons(env, parse_datum(env, input, &body), &cell_empty_list));
}

Cell* parse_list_tail(Environment* env, Input* input, Token* token)
{
    switch(token->type)
    {
        case TOKEN_LIST_END:
            // Empty list
            return &cell_empty_list;

        case TOKEN_DOT:
        {
            Token next;
            read_token(input, &next);
            Cell* cdr_cell = parse_datum(env, input, &next);

            if (!cdr_cell)
            {
                signal_error(env->cont, "expecting a datum after a dot");
            }

            Token end;
            read_token(input, &end);

            if (end.type != TOKEN_LIST_END)
            {
                signal_error(env->cont, "expecting )");
            }

            return cdr_cell;
        }

        default:
        {
            Cell* data = parse_datum(env, input, token);
            assert(data);
            Token rest;
            read_token(input, &rest);
            Cell* rest_cell = parse_list_tail(env, input, &rest);
            assert(rest_cell);
            return cons(env, data, rest_cell);
        }
    }
}

Cell* parse_list(Environment* env, Input* input, Token* token)
{
    assert(token->type == TOKEN_LIST_START);

    // Token was '(', skip it and move on.
	Token next;
    read_token(input, &next);
    return parse_list_tail(env, input, &next);
}

Cell* parse_vector(Environment* env, Input* input, Token* token)
{
    assert(token->type == TOKEN_VECTOR_START);

    // Token was '(', skip it and move on.
	Token next;
    read_token(input, &next);
    Cell* list = parse_list_tail(env, input, &next);

    if (list == NULL)
    {
        return NULL;
    }

    unsigned length = 0;
    for (Cell* c = list; is_pair(c); c = cdr(c))
    {
        length++;
    }

    // TODO: Fill with empty list or null?
    Cell* vector = make_vector(env, length, NULL);

    unsigned i = 0;
    for (Cell* c = list; is_pair(c); c = cdr(c))
    {
        vector_set(&vector->data.vector, i++, car(c));
        //printf("Vector [%d] = ", i);
        //print(stdout, car(c), true);
    }
	return vector;
}

Cell* parse_datum(Environment* env, Input* input, Token* token)
{
	switch (token->type)
	{
		case TOKEN_BOOLEAN:
            return make_boolean(token->data.boolean);

        case TOKEN_NUMBER:
			return make_number(env, token->data.number);

		case TOKEN_CHARACTER:
            return make_character(env, token->data.character);

        case TOKEN_STRING:
			return make_string(env, (int)strlen(token->data.string), token->data.string);

		case TOKEN_IDENTIFIER:
			return make_symbol(env, token->data.identifier);

        case TOKEN_LIST_START:
            return parse_list(env, input, token);

        case TOKEN_VECTOR_START:
            return parse_vector(env, input, token);

        case TOKEN_QUOTE:
        case TOKEN_BACKTICK:
        case TOKEN_COMMA:
        case TOKEN_COMMA_AT:
            return parse_abreviation(env, input, token);

        default:
            return NULL;
	}
}


//-----------------------------------------------------------------------------
// MurmurHash2, by Austin Appleby

// Note - This code makes a few assumptions about how your machine behaves -

// 1. We can read a 4-byte value from any address without crashing
// 2. sizeof(int) == 4

// And it has a few limitations -

// 1. It will not work incrementally.
// 2. It will not produce the same results on little-endian and big-endian
//    machines.

static unsigned int MurmurHash2 ( const void * key, int len)
{
	const unsigned int seed = 0xdbc;

	assert(sizeof(int) == 4);

	// 'm' and 'r' are mixing constants generated offline.
	// They're not really 'magic', they just happen to work well.

	const unsigned int m = 0x5bd1e995;
	const int r = 24;

	// Initialize the hash to a 'random' value

	unsigned int h = seed ^ len;

	// Mix 4 bytes at a time into the hash

	const unsigned char * data = (const unsigned char *)key;

	while(len >= 4)
	{
		unsigned int k = *(unsigned int *)data;

		k *= m;
		k ^= k >> r;
		k *= m;

		h *= m;
		h ^= k;

		data += 4;
		len -= 4;
	}

	// Handle the last few bytes of the input array

	switch(len)
	{
        case 3: h ^= data[2] << 16;
        case 2: h ^= data[1] << 8;
        case 1: h ^= data[0];
            h *= m;
	};

	// Do a few final mixes of the hash to ensure the last few
	// bytes are well-incorporated.

	h ^= h >> 13;
	h *= m;
	h ^= h >> 15;

	return h;
}


Cell* environment_get(Environment* env, const Cell* symbol)
{
	assert(symbol->type == TYPE_SYMBOL);

	unsigned hash = env->mask & symbol->data.symbol->hash;

	for (symbol_node* node = env->data[hash]; node; node = node->next)
	{
		if (symbol->data.symbol == node->symbol)
		{
			return node->value;
		}
	}

	if (env->parent)
	{
		return environment_get(env->parent, symbol);
	}

    signal_error(env->cont, "reference to undefined identifier: %s", symbol->data.symbol->name);
    return NULL;
}

void environment_define(Environment* env, const Symbol* symbol, Cell* value)
{
	unsigned index = env->mask & symbol->hash;

	for (symbol_node* node = env->data[index]; node; node = node->next)
	{
		if (symbol == node->symbol)
		{
			node->value = value;
			return;
		}
	}

	symbol_node* node   = malloc(sizeof(symbol_node));
	node->symbol		= symbol;
	node->value			= value;
	node->next			= env->data[index];
	env->data[index]	= node;
}

static void environment_set(const Environment* env, const Symbol* symbol, Cell* value)
{
	const size_t hash = symbol->hash;

    for (const Environment* e = env; e; e = e->parent)
	{
		unsigned index = hash & env->mask;

        symbol_node* node;
		for (node = e->data[index]; node; node = node->next)
		{
			if (symbol == node->symbol)
			{
				node->value = value;
				return;
			}
		}
	}

	signal_error(env->cont, "No binding for %s in any scope.", symbol->name);
}

static void eval(atom_state* env, struct Procedure* closure);

static Cell* atom_pop_cell(Environment* env)
{
    return vector_pop(&env->cont->stack);
}

static Cell* atom_pop_a(Environment* env, int type)
{
    Cell* cell = atom_pop_cell(env);
    type_check(env->cont, type, cell->type);
    return cell;
}

static Vector* atom_pop_vector(Environment* env)
{
    return &atom_pop_a(env, TYPE_VECTOR)->data.vector;
}

Cell* atom_pop_list(Environment* env)
{
    return atom_pop_a(env, TYPE_PAIR);
}

double atom_pop_number(Environment* env)
{
    // TODO: GC safety here
    return atom_pop_a(env, TYPE_NUMBER)->data.number;
}

int atom_pop_integer(Environment* env)
{
    double value = atom_pop_number(env);
    int int_val = value;
    if (int_val != value) signal_error(env->cont, "Expected an integer, got %g", value);
    return int_val;
}

static char atom_pop_character(Environment* env)
{
    return atom_pop_a(env, TYPE_CHARACTER)->data.character;
}

static char atom_pop_character_lower(Environment* env)
{
    return tolower(atom_pop_character(env));
}

void atom_push_cell(Environment* env, Cell* cell)
{
    vector_push(&env->cont->stack, cell);
}

void atom_push_boolean(Environment* env, bool boolean)
{
    atom_push_cell(env, make_boolean(boolean));
}

void atom_push_number(Environment* env, double x)
{
    atom_push_cell(env, make_number(env, x));
}

void atom_push_character(Environment* env, char c)
{
    atom_push_cell(env, make_character(env, c));
}

static void atom_push_undefined(Environment* env)
{
    atom_push_boolean(env, false);
}

static void type_q_helper(Environment* env, int params, int type)
{
    atom_push_boolean(env, atom_pop_cell(env)->type == type);
}

static Environment* create_environment(atom_state* cont, Environment* parent)
{
	Environment* env = (Environment*)malloc(sizeof(Environment));
    environment_init(env, cont, 1, parent);
	return env;
}

// (let <bindings> <body>) library syntax
// Syntax: <Bindings> should have the form
// ((<variable1> <init1>) ...), where each <init> is an expression, and <body>
// should be a sequence of one or more expressions. It is an error for a <variable>
// to appear more than once in the list of variables being bound.
//
// Semantics: The <init>s are evaluated in the current environment (in some
// unspecified order), the <variable>s are bound to fresh locations holding the
// results, the <body> is evaluated in the extended environment, and the value(s)
// of the last expression of <body> is(are) returned. Each binding of a <variable>
// has <body> as its region.

// (let* <bindings> <body>) Library syntax
// Syntax: <Bindings> should have the form
// ((<variable1> <init1>) ...), and <body> should be a sequence of one or more
// expressions.
//
// Semantics: Let* is similar to let, but the bindings are performed sequentially
// from left to right, and the region of a binding indicated by (<variable> <init>)
// is that part of the let* expression to the right of the binding. Thus the
// second binding is done in an environment in which the first binding is visible,
// and so on.

static Cell* duplicate(Environment* env, Cell* list)
{
    if (list == NULL) return NULL;
    assert(list->type == TYPE_PAIR);
    return cons(env, car(list), cdr(list));
}

static Cell* append_destructive(Cell* a, Cell* b)
{
    if (!a) return b;

    Cell* current = a;

    for(;;)
    {
        if (cdr(current) == NULL)
        {
            set_cdr(current, b);
            return a;
        }
        current = cdr(current);
    }
    assert(false);
    return NULL;
}


// TODO: Remove this function, replace with a pointer comparison.
static bool symbol_is(const Cell* symbol, const char* name)
{
    assert(symbol && symbol->type == TYPE_SYMBOL);
    return 0 == strcmp(name, symbol->data.symbol->name);
}

// TODO: Handle literal vectors in quasiquote
static Cell* quasiquote_helper(Environment* env, Cell* list)
{
    // break recursion
    if (!list) return NULL;

    // If the object is not a list, then there is nothing to do
    // TODO: vector literals
    if (list->type != TYPE_PAIR) return list;

    // At the end of the function we are going to
    // cons new_head onto recurse(rest)
    // The function modifies new_head
    Cell* head     = car(list);
    Cell* rest     = cdr(list);
    Cell* new_head = head;

    // TODO: make a proper empty list type, remove this line
    if (!head) return NULL;

    if (head->type == TYPE_PAIR)
    {
        Cell* operation = car(head);

        if (symbol_is(operation, "unquote"))
        {
            assert(0);
            new_head = 0; //eval(env, car(cdr(head)));
        }
        else if (symbol_is(operation, "unquote-splicing"))
        {
            assert(0);
            new_head = 0;//eval(env, car(cdr(head)));
            assert(new_head == NULL || new_head->type == TYPE_PAIR);
            return append_destructive(new_head, quasiquote_helper(env, rest));
        }
    }

    return cons(env, new_head, quasiquote_helper(env, rest));
}

static void atom_env(Environment* env, int params) {

    const char* root = "";

    if (params > 0) {
        root = atom_pop_a(env, TYPE_STRING)->data.string.data;
    }

    for (; env; env = env->parent)
    {
        for (unsigned i = 0; i <= env->mask; i++)
        {
            for (symbol_node* node = env->data[i]; node; node = node->next)
            {
                if (strspn(node->symbol->name, root) == strlen(root)) {
                    puts(node->symbol->name);
                }
            }
        }
    }
}

// not built in
// custom function
static void atom_error(Environment* env, int params)
{
	const char* str = "Error";

    // TODO: define the symantics of error
    if (params > 0) {
        Cell* message = atom_pop_cell(env);

        if (message->type == TYPE_STRING)
        {
            str = message->data.string.data;
        }
    }
	
	signal_error(env->cont, "%s", str);
}

void atom_null_function(Environment* env, int params){
    atom_push_boolean(env, &cell_false);
}

// These numerical predicates provide tests for the exactness of a quantity.
// For any Scheme number, precisely one of these predicates is true.
static void atom_exact_q(Environment* env, int params)
{
    atom_pop_number(env);
    atom_push_cell(env, make_boolean(false));
}

static void atom_inexact_q(Environment* env, int params)
{
    assert(params == 1);
    atom_pop_number(env);
	atom_push_cell(env, make_boolean(true));
}

static bool eq_helper(const Cell* obj1, const Cell* obj2, bool recurse_strings,
                      bool recurse_compound);

static bool pair_equal(const Cell* obj1, const Cell* obj2, bool recursive)
{
	if (obj1 == obj2)   return true;
	if (!obj1 || !obj2) return false; // TODO: Test for nulls / empty lists
	if (!recursive)     return false;

	if (!eq_helper(car(obj1), car(obj2), true, true)) return false;

	return pair_equal(cdr(obj1), cdr(obj2), true);
}

static bool vector_equal(const Cell* obj1, const Cell* obj2, bool recursive)
{
	assert(obj1->type == TYPE_VECTOR);
	assert(obj2->type == TYPE_VECTOR);

	if (obj1 == obj2) return true;
	if (!recursive)   return false;

	const int length = obj1->data.vector.n;

	// if different lengths, return false
	if (obj2->data.vector.n != length) return false;

    const Vector* a = &obj1->data.vector;
    const Vector* b = &obj2->data.vector;

	for (int i=0; i<length; i++)
	{
		if (!eq_helper(vector_get(a, i), vector_get(b, i), true, true))
		{
			return false;
		}
	}
	return true;
}

static void atom_syntax_rules(Environment* env, int params)
{
    assert(params > 1);
    //Cell* keywords = atom_pop_list(env);
}

static bool eq_helper(const Cell* obj1, const Cell* obj2, bool recurse_strings, bool recurse_compound)
{
	const cell_type type = obj1->type;

	if (type != obj2->type)
	{
		return false;
	}

	switch(type)
	{
        case TYPE_EMPTY_LIST:
            return true;
            
        case TYPE_BUILT_IN:
            return obj1->data.built_in == obj2->data.built_in;
            
        case TYPE_PROCEDURE:
            return obj1->data.closure == obj2->data.closure;

        case TYPE_INPUT_PORT:
        case TYPE_OUTPUT_PORT:
            return obj1->data.port == obj2->data.port;

        case TYPE_SYNTAX_RULES:
        case MAX_TYPES:
            assert(false);
            break;
            
		case TYPE_BOOLEAN:
            return obj1->data.boolean == obj2->data.boolean;

		case TYPE_CHARACTER:
            return obj1->data.character == obj2->data.character;

		case TYPE_SYMBOL:
            return obj1->data.symbol == obj2->data.symbol;

        case TYPE_ENVIRONMENT:
            return obj1->data.env == obj2->data.env;

		case TYPE_NUMBER:
            return obj1->data.number == obj2->data.number;

		case TYPE_PAIR:
            return pair_equal(obj1, obj2, recurse_compound);

		case TYPE_VECTOR:
            return vector_equal(obj1, obj2, recurse_compound);

		case TYPE_STRING:
            return (obj1 == obj2) ||
            (recurse_strings && (0 == strcmp(obj1->data.string.data, obj2->data.string.data)));
	}

    assert(false);
	return false;
}

// (eqv? obj1 obj2) procedure
// The eqv? procedure defines a useful equivalence relation on objects.
// Briefly, it returns #t if obj1 and obj2 should normally be regarded as the same object.
static void atom_eqv_q(Environment* env, int params)
{
    Cell* obj1 = atom_pop_cell(env);
    Cell* obj2 = atom_pop_cell(env);
    atom_push_boolean(env, eq_helper(obj1, obj2, true, false));
}

// (eq? obj1 obj2)	procedure
// Eq? is similar to eqv? except that in some cases it is capable of discerning
// distinctions finer than those detectable by eqv?.
// Eq? and eqv? are guaranteed to have the same behavior on symbols, booleans,
// the empty list, pairs, procedures, and non-empty strings and vectors.
// Eq?’s behavior on numbers and characters is implementation-dependent, but it
// will always return either true or false, and will return true only when eqv?
// would also return true. Eq? may also behave differently from eqv? on empty
// vectors and empty strings.
static void atom_eq_q(Environment* env, int params)
{
	Cell* obj1 = atom_pop_cell(env);
	Cell* obj2 = atom_pop_cell(env);
	atom_push_boolean(env, eq_helper(obj1, obj2, false, false));
}

// (equal? obj1 obj2)	library procedure
// Equal? recursively compares the contents of pairs, vectors, and strings,
// applying eqv? on other objects such as numbers and symbols. A rule of thumb is
// that objects are generally equal? if they print the same. Equal? may fail to
// terminate if its arguments are circular data structures.
static void atom_equal_q(Environment* env, int params)
{
	Cell* obj1 = atom_pop_cell(env);
	Cell* obj2 = atom_pop_cell(env);
	atom_push_boolean(env, eq_helper(obj1, obj2, true, true));
}

static void atom_number_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_NUMBER);
}

static void atom_integer_q(Environment* env, int params)
{
    assert(params == 1);
    atom_push_boolean(env, is_integer(atom_pop_number(env)));
}

// (sin z)
// (cos z)
// (tan z)
// (asin z)
// (acos z)
// (atan z)
// (atan y x)
// These procedures are part of every implementation that supports general real
// numbers; they compute the usual transcendental functions. Log computes the
// natural logarithm of z (not the base ten logarithm). Asin, acos, and atan
// compute arcsine (sin−1), arccosine (cos−1), and arctangent (tan−1),
// respectively. The two-argument variant of atan computes
// (angle (make-rectangular x y)) (see below), even in implementations that
// don’t support general complex numbers.
//
// In general, the mathematical functions log, arcsine, arccosine, and arctangent
// are multiply defined. The value of log z is defined to be the one whose
// imaginary part lies in the range from −π (exclusive) to π (inclusive). log 0
// is undefined. With log defined this way, the values of sin−1 z, cos−1 z, and
// tan−1 z are according to the following formulae
//
// The above specification follows [27], which in turn cites [19]; refer to these
// sources for more detailed discussion of branch cuts, boundary conditions, and
// implementation of these functions. When it is possible these procedures
// produce a real result from a real argument.

static void atom_sin(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, sin(atom_pop_number(env)));
}

static void atom_cos(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, cos(atom_pop_number(env)));
}

static void atom_tan(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, tan(atom_pop_number(env)));
}

static void atom_asin(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, asin(atom_pop_number(env)));
}

static void atom_acos(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, acos(atom_pop_number(env)));
}

static void atom_atan(Environment* env, int params)
{
    assert(params > 0 && params < 3);
    double y = atom_pop_number(env);

    double result;

    if (params > 1)
    {
        result = atan2(y, atom_pop_number(env));
    }
    else
    {
        result = atan(y);
    }

    atom_push_number(env, result);
}


typedef bool(*number_comparison)(double, double);

static void comparison_helper(Environment* env, int params, number_comparison compare)
{
	int n = 2;

    double a = atom_pop_number(env);
    params--;

	for (;;)
	{
        double b = atom_pop_number(env);
        params--;

		if (!compare(a, b))
		{
            atom_push_boolean(env, false);
            return;
		}

		a = b;
		n++;

        if (params < 1) break;
	}

	atom_push_boolean(env, true);
};

static bool number_equal     (double a, double b) { return a == b; }
static bool number_less      (double a, double b) { return a <  b; }
static bool number_greater   (double a, double b) { return a >  b; }
static bool number_less_eq   (double a, double b) { return a <= b; }
static bool number_greater_eq(double a, double b) { return a >= b; }

static void atom_comapre_equal(Environment* env, int params)
{
	comparison_helper(env, params, number_equal);
}

static void atom_compare_less(Environment* env, int params)
{
	comparison_helper(env, params, number_less);
}

static void atom_compare_greater(Environment* env, int params)
{
    comparison_helper(env, params, number_greater);
}

static void atom_compare_less_equal(Environment* env, int params)
{
	comparison_helper(env, params, number_less_eq);
}

static void atom_compare_greater_equal(Environment* env, int params)
{
    comparison_helper(env, params, number_greater_eq);
}

// (zero? z)
// (positive? x)
// (negative? x)
// (odd? n)
// (even? n)
// These numerical predicates test a number for a particular property, returning
// #t or #f.
static void atom_zero_q(Environment* env, int params)
{
	double result = atom_pop_number(env);
    atom_push_boolean(env, result == 0.0);
}

static void atom_positive_q(Environment* env, int params)
{
    double result = atom_pop_number(env);
    // TODO: > 0 or >= 0?
    atom_push_boolean(env, result >= 0.0);
}

static void atom_negative_q(Environment* env, int params)
{
    double result = atom_pop_number(env);
    // TODO: < 0 or <= 0?
    atom_push_boolean(env, result < 0.0);
}

static bool is_odd(Environment* env, int params)
{
    assert(params == 1);
    int value = atom_pop_integer(env);
    return value & 1;
}

static void atom_odd_q(Environment* env, int params)
{
    atom_push_boolean(env, is_odd(env, params));
}

static void atom_even_q(Environment* env, int params)
{
    atom_push_boolean(env, !is_odd(env, params));
}

// (max x1 x2 ...) library procedure
// (min x1 x2 ...) library procedure
// These procedures return the maximum or minimum of their arguments.

typedef double (*number_select)(double a, double b);

static void min_max_helper(Environment* env, int params, number_select select)
{
    assert(params > 1);
	double result = atom_pop_number(env);

    for (int i=1; i<params; i++)
	{
        result = select(result, atom_pop_number(env));
	}
	atom_push_number(env, result);
}

static double number_min(double a, double b)
{
    return a < b ? a : b;
}

static double number_max(double a, double b)
{
    return a > b ? a : b;
}

static void atom_min(Environment* env, int params)
{
    min_max_helper(env, params, number_min);
}

static void atom_max(Environment* env, int params)
{
	min_max_helper(env, params, number_max);
}

// 6.3
// 6.3.1: Booleans

static void atom_boolean_q(Environment* env, int params)
{
	type_q_helper(env, params, TYPE_BOOLEAN);
}

static bool is_truthy(const Cell* cell)
{
    return cell->type != TYPE_BOOLEAN || cell->data.boolean;
}

static void atom_not(Environment* env, int params)
{
    assert(params == 1);
	atom_push_boolean(env, !is_truthy(atom_pop_cell(env)));
}

// 6.3.2 Pairs and lists
static void atom_pair_q(Environment* env, int params)
{
	type_q_helper(env, params, TYPE_PAIR);
}

static void atom_cons(Environment* env, int params)
{
    assert(params == 2);
	Cell* first  = atom_pop_cell(env);
	Cell* second = atom_pop_cell(env);
	atom_push_cell(env, cons(env, first, second));
}

static void atom_car(Environment* env, int params)
{
    assert(params == 1);
	Cell* list = atom_pop_cell(env);
	atom_push_cell(env, car(list));
}

static void atom_cdr(Environment* env, int params)
{
    assert(params == 1);
	Cell* list = atom_pop_cell(env);
	atom_push_cell(env, cdr(list));
}

static void set_car_cdr_helper(Environment* env, int params, int is_car)
{
    assert(params == 2);
	// @todo: make an error here for constant lists
    Cell* pair = atom_pop_list(env);
	Cell* obj  = atom_pop_cell(env);

	if (is_car)
	{
		pair->data.pair.car = obj;
	}
	else
	{
		pair->data.pair.cdr = obj;
	}

	// return value here is unspecified
	atom_push_cell(env, pair);
}

static void atom_set_car_b(Environment* env, int params)
{
	set_car_cdr_helper(env, params, 1);
}

static void atom_set_cdr_b(Environment* env, int params)
{
    set_car_cdr_helper(env, params, 0);
}

// Returns #t if obj is the empty list, otherwise returns #f.
static void atom_null_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_EMPTY_LIST);
}

static bool atom_list_q_helper(Environment* env, int params)
{
    assert(params == 1);
    for (Cell* obj = atom_pop_cell(env);; obj = cdr(obj))
    {
        switch(obj->type)
        {
            case TYPE_EMPTY_LIST: return true;
            case TYPE_PAIR:       continue;
            default:              return false;
        }
    }
    return false;
}

// (list? obj)
// Returns #t if obj is a list, otherwise returns #f. By definition, all
// lists have finite length and are terminated by the empty list.
static void atom_list_q(Environment* env, int params)
{
    atom_push_boolean(env, atom_list_q_helper(env, params));
}

// (list obj ...)
// Returns a newly allocated list of its arguments.
static void atom_list(Environment* env, int params)
{
    // TODO: test this
    assert(params > 0);
	Cell* result = cons(env, atom_pop_cell(env), &cell_empty_list);

	for (int i = 1; i<params; i++)
	{
        Cell* next = cons(env, atom_pop_cell(env), &cell_empty_list);
        set_cdr(result, next);
        result = next;
	}

    atom_push_cell(env, result);
}

// (length list) Returns the length of list.
static void atom_length(Environment* env, int params)
{
    assert(params == 1);
	int length = 0;
    for (Cell* list = atom_pop_list(env); is_pair(list); list = cdr(list))
    {
        length++;
    }
    atom_push_number(env, length);
}




// (append list ...)
// Returns a list consisting of the elements of the first list followed by
// the elements of the other lists.
static void atom_append(Environment* env, int params)
{
    assert(params > 0);
    Cell* result = NULL;

    for (int i=1; i<params; i++)
    {
        result = append_destructive(result, duplicate(env, atom_pop_list(env)));
    }
    atom_push_cell(env, result);
}


// library procedure: list-tail list K
// Returns the sublist of LIST obtained by omitting the first K
// elements.  It is an error if LIST has fewer than K elements.
// `List-tail' could be defined by
// (define list-tail
//   (lambda (x k)
//     (if (zero? k)
//        x
//        (list-tail (cdr x) (- k 1)))))
static Cell* list_tail_helper(Environment* env, int params)
{
    assert(params == 2);
	Cell* list = atom_pop_list(env);


	for (int k = atom_pop_integer(env); k>0; k--)
	{
		list = cdr(list);
		if (is_pair(list)) signal_error(env->cont, "The given list must have at least K elements");
	}
	return list;
}

static void atom_list_tail(Environment* env, int params)
{
	atom_push_cell(env, list_tail_helper(env, params));
}

// library procedure: list-ref list K
// Returns the Kth element of LIST.  (This is the same as the car of
// (list-tail LIST K).)  It is an error if LIST has fewer than K
// elements.
static void atom_list_ref(Environment* env, int params)
{
	atom_push_cell(env, car(list_tail_helper(env, params)));
}

// a bunh of functions are missing here....

// 6.3.3. Symbols

// (symbol? obj)
// Returns #t if obj is a symbol, otherwise returns #f.
static void atom_symbol_q(Environment* env, int params)
{
	type_q_helper(env, params, TYPE_SYMBOL);
}

// (symbol->string symbol) procedure
// Returns the name of symbol as a string.
// If the symbol was part of an object returned as the value of a literal
// expression (section 4.1.2) or by a call to the read procedure, and its
// name contains alphabetic characters, then the string returned will contain
// characters in the implementation’s preferred standard case—some
// implementations will prefer upper case, others lower case. If the symbol
// was returned by string->symbol, the case of characters in the string
// returned will be the same as the case in the string that was passed to
// string->symbol.
// It is an error to apply mutation procedures like string-set! to strings
// returned by this procedure.
static void atom_symbol_to_string(Environment* env, int params)
{
	const char* symbol = atom_pop_a(env, TYPE_SYMBOL)->data.symbol->name;
    // TODO: bad type conversion here (size_t, int).
    atom_push_cell(env, make_string(env, (int)strlen(symbol), symbol));
}

// (string char ...) library procedure
// Returns a newly allocated string composed of the arguments.
static void atom_string(Environment* env, int params)
{
    assert(params > 0);

    kvec_t(char) buffer;
    kv_init(buffer);

    for (int i=0; i<params; i++)
        kv_push(char, buffer, atom_pop_character(env));
    kv_push(char, buffer, 0); // null terminate

    atom_push_cell(env, make_string(env, params, buffer.a));

    kv_destroy(buffer);
}

static int compare_strings(Environment* env, int params)
{
    assert(params == 2);
    Cell* a = atom_pop_a(env, TYPE_STRING);
    Cell* b = atom_pop_a(env, TYPE_STRING);
    return strcmp(a->data.string.data,
                  b->data.string.data);
}

static int compare_case_strings(Environment* env, int params)
{
    assert(params == 2);
    Cell* a = atom_pop_a(env, TYPE_STRING);
    Cell* b = atom_pop_a(env, TYPE_STRING);
    return strcasecmp(a->data.string.data,
                      b->data.string.data);
}

// (string=? string1 string2) library procedure
// (string-ci=? string1 string2)	library procedure
// Returns #t if the two strings are the same length and contain the same
// characters in the same positions, otherwise returns #f. Stringci=? treats
// upper and lower case letters as though they were the same character, but
// string=? treats upper and lower case as distinct characters.
static void atom_string_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, 0 == compare_strings(env, params));
}

static void atom_string_ci_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, 0 == compare_case_strings(env, params));
}

// (string<?	string1	string2 ) library procedure
// (string>?	string1	string2 ) library procedure
// (string<=?	string1	string2 ) library procedure
// (string>=?	string1	string2 ) library procedure
// (string-ci<?	string1	string2 ) library procedure
// (string-ci>?	string1	string2 ) library procedure
// (string-ci<=?	string1	string2 ) library procedure
// (string-ci>=?	string1	string2 ) library procedure
//
// These procedures are the lexicographic extensions to strings of the
// corresponding orderings on characters. For example, string<? is the lexicographic
// ordering on strings induced by the ordering char<? on characters. If two
// strings differ in length but are the same up to the length of the shorter
// string, the shorter string is considered to be lexicographically less than the
// longer string.
// Implementations may generalize these and the string=? and string-ci=? procedures
// to take more than two arguments, as with the corresponding numerical predicates.

static void atom_string_less_than_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_strings(env, params) < 0);
}

static void atom_string_greater_than_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_strings(env, params) > 0);
}

static void atom_string_less_than_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_strings(env, params) <= 0);
}

static void atom_string_greater_than_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_strings(env, params) >= 0);
}

static void atom_string_ci_less_than_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_case_strings(env, params) < 0);
}

static void atom_string_ci_greater_than_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_case_strings(env, params) > 0);
}

static void atom_string_ci_less_than_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_case_strings(env, params) <= 0);
}

static void atom_string_ci_greater_than_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, compare_case_strings(env, params) >= 0);
}

// (substring string start end) library procedure
// String must be a string, and start and end must be exact integers satisfying
// 0 ≤ start ≤ end ≤ (string-length string).
// Substring returns a newly allocated string formed from the characters of
// string beginning with index start (inclusive) and ending with index end
// (exclusive).
static void atom_substring(Environment* env, int params)
{
    assert(params == 3);

    const Cell* cell = atom_pop_a(env, TYPE_STRING);
    const int start  = atom_pop_integer(env);
    const int end    = atom_pop_integer(env);

    const int length = end - start;

    if (start < 0 || start >= end || end > cell->data.string.length){
        signal_error(env->cont,
                     "Invalid indices (%d, %d) passed to substring. String has length %d",
                     start, end, cell->data.string.length);
    }

    Cell* substring = make_empty_string(env, length);

    strncpy(substring->data.string.data,
            cell->data.string.data + start,
            length);

    atom_push_cell(env, substring);
}

// (string-append string ...) library procedure
// Returns a newly allocated string whose characters form the concatenation of
// the given strings.
static void atom_string_append(Environment* env, int params)
{
    kvec_t(char) buffer;
    kv_init(buffer);

    for (int i=0; i<params; i++)
    {
        Cell* s = atom_pop_a(env, TYPE_STRING);
        for (int j=0; j<s->data.string.length; j++)
            kv_push(char, buffer, s->data.string.data[j]);
    }

    int length = kv_size(buffer);

    Cell* result = make_empty_string(env, length);

    if (length > 0)
    {
        strncpy(result->data.string.data, buffer.a, length);
    }

    atom_push_cell(env, result);

    kv_destroy(buffer);
}

// (string->list string) library procedure
// (list->string list)   library procedure
// String->list returns a newly allocated list of the characters that make up
// the given string. List->string returns a newly allocated string formed from
// the characters in the list list, which must be a list of characters.
// String->list and list->string are inverses so far as equal? is concerned.
static void atom_string_to_list(Environment* env, int params)
{
    assert(params == 1);
    Cell* string = atom_pop_a(env, TYPE_STRING);

    const int length = string->data.string.length;

    Cell* result = NULL;

    for (int i=length-1; i >= 0; i--){
        result = cons(env, make_character(env, string->data.string.data[i]), result);
    }

    atom_push_cell(env, result);
}

static void atom_list_to_string(Environment* env, int params)
{
    assert(params == 1);

    Cell* list = atom_pop_a(env, TYPE_PAIR);

    int length = 0;

    for (Cell* c = list; is_pair(c); c = cdr(c))
    {
        type_check(env->cont, TYPE_CHARACTER, car(c)->type);
        length = length + 1;
    }

    Cell* string = make_empty_string(env, length);

    int i = 0;
    for (Cell* c = list; is_pair(c); c = cdr(c))
    {
        assert(TYPE_CHARACTER == car(c)->type);
        string->data.string.data[i] = car(c)->data.character;
        i++;
    }

    assert((int)strlen(string->data.string.data) == length);

    atom_push_cell(env, string);
}


// (string-copy string)	library procedure
// Returns a newly allocated copy of the given string.
static void atom_string_copy(Environment* env, int params)
{
    assert(params == 1);
    Cell* string = atom_pop_a(env, TYPE_STRING);
    const int length = string->data.string.length;
    atom_push_cell(env, fill_string(make_empty_string(env, length),
                                    length,
                                    string->data.string.data));
}

// (string-fill! string char) library procedure
// Stores char in every element of the given string and returns an unspecified
// value.
static void atom_string_fill_b(Environment* env, int params)
{
    Cell* string = atom_pop_a(env, TYPE_STRING);
    char c       = atom_pop_character(env);

    for (int i=0; i<string->data.string.length; i++)
    {
        string->data.string.data[i] = c;
    }

    atom_push_cell(env, string);
}

// (string->symbol string) procedure
// Returns the symbol whose name is string. This procedure can create symbols
// with names containing special characters or letters in the non-standard
// case, but it is usually a bad idea to create such symbols because in some
// implementations of Scheme they cannot be read as themselves.
static void atom_string_to_symbol(Environment* env, int params)
{
    assert(params == 1);
	atom_push_cell(env, make_symbol(env, atom_pop_a(env, TYPE_STRING)->data.string.data));
}

// 6.3.4 Characters

// (char?	obj )	procedure
// Returns #t if obj is a character, otherwise returns #f.
static void atom_char_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_CHARACTER);
}

static int character_compare(Environment* env, int params)
{
    assert(params == 2);
    char a = atom_pop_character(env);
    char b = atom_pop_character(env);
    return a - b;
}

static int character_compare_lower(Environment* env, int params)
{
    assert(params == 2);
    char a = atom_pop_character_lower(env);
    char b = atom_pop_character_lower(env);
    return a - b;
}

// (char=?	char1	char2 ) procedure
// (char<?	char1	char2 ) procedure
// (char>?	char1	char2 ) procedure
// (char<=?	char1	char2 ) procedure
// (char>=?	char1	char2 ) procedure

// These procedures impose a total ordering on the set of characters.
// It is guaranteed that under this ordering:
// - The upper case characters are in order. For example, (char<? #\A #\B) returns #t.
// - The lower case characters are in order. For example, (char<? #\a #\b) returns #t.
// - The digits are in order. For example, (char<? #\0 #\9) returns #t.
// - Either all the digits precede all the upper case letters, or vice versa.
// - Either all the digits precede all the lower case letters, or vice versa.
// Some implementations may generalize these procedures to take more than two
// arguments, as with the corresponding numerical predicates.
static void atom_char_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare(env, params) == 0);
}

static void atom_char_less_than_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare(env, params) < 0);
}

static void atom_char_greater_than_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare(env, params) > 0);
}

static void atom_char_less_than_or_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare(env, params) <= 0);
}

static void atom_char_greater_than_or_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare(env, params) >= 0);
}

// (char-ci=?	char1	char2 ) library procedure
// (char-ci<?	char1	char2 ) library procedure
// (char-ci>?	char1	char2 ) library procedure
// (char-ci<=?	char1	char2 ) library procedure
// (char-ci>=?	char1	char2 ) library procedure
// These procedures are similar to char=? et cetera, but they treat upper case
// and lower case letters as the same. For example, (char-ci=? #\A #\a) returns #t.
// Some implementations may generalize these procedures to take more than two
// arguments, as with the corresponding numerical predicates.
static void atom_char_ci_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare_lower(env, params) == 0);
}

static void atom_char_ci_less_than_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare_lower(env, params) < 0);
}

static void atom_char_ci_greater_than_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare_lower(env, params) > 0);
}

static void atom_char_ci_less_than_or_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare_lower(env, params) < 0);
}

static void atom_char_ci_greater_than_or_equal_q(Environment* env, int params)
{
    atom_push_boolean(env, character_compare_lower(env, params) >= 0);
}


// (char-alphabetic? char)
// (char-numeric?	 char)
// (char-whitespace? char)
// (char-upper-case? char)
// (char-lower-case? char)
// These procedures return #t if their arguments are alphabetic, numeric,
// whitespace, upper case, or lower case characters, respectively, otherwise
// they return #f. The following remarks, which are specific to the ASCII
// character set, are intended only as a guide: The alphabetic characters are the
// 52 upper and lower case letters. The numeric characters are the ten decimal
// digits.
// The whitespace characters are space, tab, line feed, form feed, and carriage
// return.
static void atom_char_alphabetic_q(Environment* env, int params)
{
    assert(params == 1);
    atom_push_boolean(env, isalpha(atom_pop_character(env)));
}

static void atom_char_numeric_q(Environment* env, int params)
{
    assert(params == 1);
    atom_push_boolean(env, isdigit(atom_pop_character(env)));
}

static void atom_char_whitespace_q(Environment* env, int params)
{
    assert(params == 1);
    atom_push_boolean(env, isspace(atom_pop_character(env)));
}

static void atom_char_upper_case_q(Environment* env, int params)
{
    assert(params == 1);
    atom_push_boolean(env, isupper(atom_pop_character(env)));
}

static void atom_char_lower_case_q(Environment* env, int params)
{
    assert(params == 1);
    atom_push_boolean(env, islower(atom_pop_character(env)));
}

// (char-upcase char)	library procedure
// (char-downcase	char )	library	procedure
// These procedures return a character char2 such that (char-ci=? char char2). In
// addition, if char is alphabetic, then the result of char-upcase is upper case
// and the result of char-downcase is lower case.
static void atom_char_upcase(Environment* env, int params)
{
    assert(params == 1);
    atom_push_character(env, toupper(atom_pop_character(env)));
}

static void atom_char_downcase(Environment* env, int params)
{
    assert(params == 1);
    atom_push_character(env, tolower(atom_pop_character(env)));
}

// (char->integer char)	procedure
// (integer->char n)	procedure
// Given a character, char->integer returns an exact integer representation
// of the character. Given an exact integer that is the image of a character
// under char->integer, integer->char returns that character.
// These procedures implement order-preserving isomorphisms between the set
// of characters under the char<=? ordering and some subset of the integers
// under the <= ordering.
static void atom_char_to_integer(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, atom_pop_character(env));
}

static void atom_integer_to_char(Environment* env, int params)
{
    // TODO: overflow?
    assert(params == 1);
    atom_push_character(env, atom_pop_integer(env));
}

// 6.3.5 Strings

// (string? obj)	procedure
// Returns #t if obj is a string, otherwise returns #f.
static void atom_string_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_STRING);
}

// (make-string k)      procedure
// (make-string k char) procedure
// Make-string returns a newly allocated string of length k. If char is
// given, then all elements of the string are initialized to char,
// otherwise the contents of the string are unspecified.
// ATOM: The contents are zero.
static void atom_make_string(Environment* env, int params)
{
    assert(params < 3 && params > 0);
	int k = atom_pop_integer(env);

	char fill = 0;

    if (params == 2)
    {
        fill = atom_pop_character(env);
    }

	if (k < 0)
	{
        signal_error(env->cont, "positive integer length required");
	}

    atom_push_cell(env, make_string_filled(env, k, fill));
}



// (string char ...) library procedure
// Returns a newly allocated string composed of the arguments.
// todo

// (string-length string)	procedure
// Returns the number of characters in the given string.
static void atom_string_length(Environment* env, int params)
{
    assert(params == 1);
	atom_push_number(env, atom_pop_a(env, TYPE_STRING)->data.string.length);
}

// (string-ref string k)	procedure
// k must be a valid index of string. String-ref returns character k of
// string using zero-origin indexing.
static void atom_string_ref(Environment* env, int params)
{
	Cell* string = atom_pop_a(env, TYPE_STRING);
	int k        = atom_pop_integer(env);

	// todo: watch this cast.
	if (k < 0 || k < string->data.string.length)
	{
		signal_error(env->cont, "k is not a valid index of the given string");
	}

    atom_push_character(env, string->data.string.data[k]);
}


// Set string[k] = c
// Asserts that string is a string
// Asserts that c is a character
// Raises an error if k is an invalid index.
static void string_set_char(Environment* env, Cell* string, int k, char c)
{
    assert(string->type == TYPE_STRING);

	if (k < 0 || k >= string->data.string.length)
	{
		signal_error(env->cont, "invalid string index");
	}
	string->data.string.data[k] = c;
}

// (string-set! string k char)	procedure
// k must be a valid index of string.
// String-set! stores char in element k of string and returns an
// unspecified value.
static void atom_string_set(Environment* env, int params)
{
    Cell* string = atom_pop_a(env, TYPE_STRING);
    int k = atom_pop_integer(env);
    char c = atom_pop_character(env);

    string_set_char(env, string, k, c);

    atom_push_undefined(env);
}

// (vector? obj)
// Returns #t if obj is a vector, otherwise returns #f.
static void atom_vector_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_VECTOR);
}

// (make-vector k)	procedure
// (make-vector k fill)	procedure
// Returns a newly allocated vector of k elements. If a second argument is given,
// then each element is initialized to fill. Otherwise the initial contents of
// each element is unspecified.
static void atom_make_vector(Environment* env, int params)
{
    assert(params > 0);
    assert(params <= 2);
    int k = atom_pop_integer(env);
    assert(k > 0);

    Cell* fill = make_boolean(false);
    if (params > 1) fill = atom_pop_cell(env);

    atom_push_cell(env, make_vector(env, k, fill));
}

// (vector obj ...)	library procedure
// Returns a newly allocated vector whose elements contain the given arguments.
// Analogous to list.
static void atom_vector(Environment* env, int params)
{
    assert(params > 0);
    Cell* v = make_vector(env, params, NULL);

    for (int i=0; i<params; i++)
    {
        vector_set(&v->data.vector, i, atom_pop_cell(env));
    }

    atom_push_cell(env, v);
}

// (vector-length vector)
// Returns the number of elements in vector as an exact integer.
static void atom_vector_length(Environment* env, int params)
{
    assert(params == 1);
    atom_push_number(env, atom_pop_a(env, TYPE_VECTOR)->data.vector.n);
}

// Return true if k is a valid index into vector
static bool valid_vector_index(Cell* vector, int k)
{
	return k >= 0 && k < vector->data.vector.n;
}

// (vector-ref vector k) procedure
// k must be a valid index of vector. Vector-ref returns the contents of element k of vector.
static void atom_vector_ref(Environment* env, int params)
{
    assert(params == 2);
    Cell* v = atom_pop_a(env, TYPE_VECTOR);
	int k = atom_pop_integer(env);

	if (!valid_vector_index(v, k))
	{
		signal_error(env->cont, "Invalid vector index");
	}

	Cell* result = vector_get(&v->data.vector, k);

	// check if unitialized.
	if (!result)
	{
		// todo: format error message better
		 signal_error(env->cont, "Cannot access unitialized vector");
	}

    atom_push_cell(env, result);
}

// (vector-set! vector k obj) procedure
// k must be a valid index of vector. Vector-set! stores obj in element k of
// vector. The value returned by vector-set! is unspecified.
static void atom_vector_set_b(Environment* env, int params)
{
    assert(params == 3);
	Cell* vector = atom_pop_a(env, TYPE_VECTOR);
	int   k      = atom_pop_integer(env);
	Cell* obj    = atom_pop_cell(env);

	if (!valid_vector_index(vector, k))
	{
		// todo: better error message.
		signal_error(env->cont, "Invalid vector index k");
	}
    vector_set(&vector->data.vector, k, obj);
    atom_push_undefined(env);
}

// (vector->list vector) library procedure
// Vector->list returns a newly allocated list of the objects contained in the
// elements of vector. List->vector returns a newly created vector initialized to
// the elements of the list list.
static void atom_vector_to_list(Environment* env, int params)
{
    assert(params == 1);
    Cell* vector = atom_pop_a(env, TYPE_VECTOR);

    Cell* list = NULL;

    // Build up the list backwards
    for(int i=vector->data.vector.n-1; i > -1; i--)
    {
        list = cons(env, vector_get(&vector->data.vector, i), list);
    }

    atom_push_cell(env, list);
}

// (list->vector list)   library procedure
// Vector->list returns a newly allocated list of the objects contained in the
// elements of vector. List->vector returns a newly created vector initialized to
// the elements of the list list .
static void atom_list_to_vector(Environment* env, int params)
{
    assert(params == 1);
    Cell* list = atom_pop_a(env, TYPE_PAIR);

    int length = 0;
    for (Cell* cell = list; is_pair(cell); cell = cdr(cell)) length++;

    Cell* vector = make_vector(env, length, NULL);

    int i = 0;
    for (Cell* cell = list; is_pair(cell); cell = cdr(cell))
    {
        vector_set(&vector->data.vector, i, car(cell));
        i++;
    }

    atom_push_cell(env, vector);
}


// (vector-fill! vector fill) library procedure
// Stores fill in every element of vector.
// The value returned by vector-fill! is unspecified.
static void atom_vector_fill_b(Environment* env, int params)
{
    assert(params == 2);
	Vector* vector = atom_pop_vector(env);
    vector_fill(vector, atom_pop_cell(env));
    atom_push_undefined(env);
}

// 6.4. Control features

// (procedure? obj)
// Returns #t if obj is a procedure, otherwise returns #f.
static void atom_procedure_q(Environment* env, int params)
{
    assert(params == 1);
    Cell* obj = atom_pop_cell(env);
    // TODO: check these are the right types
    atom_push_boolean(env, obj->type == TYPE_PROCEDURE || obj->type == TYPE_BUILT_IN);
}

// (scheme-report-environment version)  procedure
// (null-environment version)           procedure
// Version must be the exact integer 5, corresponding to this revision of the
// Scheme report (the Revised5 Report on Scheme).
// Scheme-report-environment returns a specifier for an environment that is
// empty except for all bindings defined in this report that are either
// required or both optional and supported by the implementation.
// Null-environment returns a specifier for an environment that is empty except
// for the (syntactic) bindings for all syntactic keywords defined in this report
// that are either required or both optional and supported by the implementation.
// Other values of version can be used to specify environments matching past
// revisions of this report, but their support is not required.
// An implementation will signal an error if version is neither 5 nor another
// value supported by the implementation.
// The effect of assigning (through the use of eval) a variable bound in a
// scheme-report-environment (for example car) is unspecified.
// Thus the environments specified by scheme-report-environment may be immutable.
static void atom_scheme_report_environment(Environment* env, int params)
{
    assert(params == 1);
    const int version = atom_pop_integer(env);

    if (version != 5)
    {
        signal_error(env->cont, "Expected version 5, but %d was specified", version);
    }

    // Get the root env
    while (env->parent) env = env->parent;

    Cell* environment = make_cell(env, TYPE_ENVIRONMENT);
    environment->data.env = env;
    atom_push_cell(env, environment);
}

// TODO: this is a copy and paste of scheme-report-environemnt
static void atom_null_environment(Environment* env, int params)
{
    atom_scheme_report_environment(env, params);
}

// (interaction-environment) optional procedure
// This procedure returns a specifier for the environment that contains
// implementation-defined bindings, typically a superset of those listed in the
// report. The intent is that this procedure will return the environment in which
// the implementation would evaluate expressions dynamically typed by the user.
// TODO: this is a copy and paste of scheme-report-environemnt
static void atom_interaction_environment(Environment* env, int params)
{
    atom_scheme_report_environment(env, params);
}

// output functions helper
// Many of the output functions take an optional port parameter, which if not present defaults to the output
// from current-output-port.
// This function encapsulates that logic.
// Given an env, params and a param number, it returns the specified output port, or the current output port
// It will throw an error if the param is present, but not the correct type.
static FILE* get_output_port_param(Environment* env, bool param_present)
{
    if (param_present)
    {
        Cell* port = atom_pop_cell(env);
        type_check(env->cont, TYPE_OUTPUT_PORT, port->type);
        return port->data.port;
    }
    return env->cont->output;
}

static FILE* get_input_port_param(Environment* env, bool param_present)
{
    if (param_present)
    {
        Cell* port = atom_pop_cell(env);
        type_check(env->cont, TYPE_INPUT_PORT, port->type);
        return port->data.port;
    }
    return env->cont->input;
}

// (call-with-input-file string proc) library procedure
// (call-with-output-file string proc) library procedure
// String should be a string naming a file, and proc should be a procedure that
// accepts one argument. For call-with-input-file, the file should already exist;
// for call-with-output-file, the effect is unspecified if the file already
// exists. These procedures call proc with one argument: the port obtained by
// opening the named file for input or output. If the file cannot be opened, an
// error is signalled. If proc returns, then the port is closed automatically
// and the value(s) yielded by the proc is(are) returned. If proc does not
// return, then the port will not be closed automatically unless it is possible
// to prove that the port will never again be used for a read or write operation.
static void atom_call_with_input_file(Environment* env, int params)
{
    // TODO: test these semantics
    assert(params == 2);

    //Cell* string = atom_pop_a(env, TYPE_STRING);
    Cell* proc   = atom_pop_cell(env);

    if (proc->type != TYPE_BUILT_IN ||
        proc->type != TYPE_PROCEDURE)
    {
        signal_error(env->cont, "Expected a procedure, got a %s", typenames[proc->type]);
    }
}

static void atom_call_with_output_file(Environment* env, int params)
{
}

// (input-port?  obj) procedure
// Returns #t if obj is an input port or output port respectively,
// otherwise returns #f.
static void atom_input_port_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_INPUT_PORT);
}

// (output-port? obj) procedure
// Returns #t if obj is an input port or output port respectively,
// otherwise returns #f.
static void atom_output_port_q(Environment* env, int params)
{
    type_q_helper(env, params, TYPE_OUTPUT_PORT);
}

// Grab a string from a given param, and open that file in the given mode.
static Cell* file_open_helper(Environment* env, int params, bool read)
{
    assert(params == 1);
    const char* filename = atom_pop_a(env, TYPE_STRING)->data.string.data;

    FILE* file = fopen(filename, (read ? "r" : "w"));

    if (!file)
    {
        signal_error(env->cont, "Error opening file: %s", filename);
    }

    return make_io_port(env, read ? TYPE_INPUT_PORT : TYPE_OUTPUT_PORT, file);
}

// (open-input-file filename) procedure
// Takes a string naming an existing file and returns an input port capable of
// delivering characters from the file. If the file cannot be opened, an error
// is signalled.
static void atom_open_input_file(Environment* env, int params)
{
    atom_push_cell(env, file_open_helper(env, params, true));
}

// (open-output-file filename) procedure
// Takes a string naming an output file to be created and returns an output port
// capable of writing characters to a new file by that name. If the file cannot
// be opened, an error is signalled. If a file with the given name already exists,
// the effect is unspecified.
static void atom_open_output_file(Environment* env, int params)
{
    atom_push_cell(env, file_open_helper(env, params, false));
}

static void close_port(Environment* env, int params, int type)
{
    assert(params == 1);
    fclose(atom_pop_a(env, type)->data.port);
    atom_push_undefined(env);
}

// (close-input-port port) procedure
// Closes the file associated with port, rendering the port incapable of delivering
// or accepting characters.	These routines have no effect if the file has already
// been closed. The value returned is unspecified.
static void atom_close_input_port(Environment* env, int params)
{
    close_port(env, params, TYPE_INPUT_PORT);
}

// (close-output-port port) procedure
// Closes the file associated with port, rendering the port incapable of delivering
// or accepting characters.	These routines have no effect if the file has already
// been closed. The value returned is unspecified.
static void atom_close_output_port(Environment* env, int params)
{
    close_port(env, params, TYPE_OUTPUT_PORT);
}

// (current-input-port) procedure
// Returns the current default input port.
static void atom_current_input_port(Environment* env, int params)
{
    assert(params == 0);
    atom_push_cell(env, make_input_port(env, env->cont->input));
}

static void atom_current_output_port(Environment*  env, int params)
{
    assert(params == 0);
    atom_push_cell(env, make_output_port(env, env->cont->output));
}

// (write obj) library procedure
// (write obj port)	library procedure
// Writes a written representation of obj to the given port. Strings that
// appear in the written representation are enclosed in doublequotes,
// and within those strings backslash and doublequote characters are
// escaped by backslashes. Character objects are written using the 'hash-slash'
// notation.
// Write returns an unspecified value.
// The port argument may be omitted, in which case it defaults to the value
// returned by current-output-port.
static void atom_write(Environment* env, int params)
{
    assert(params < 3);
    Cell* cell = atom_pop_cell(env);
    FILE* port = get_output_port_param(env, params == 2);
	print(port, cell, false);
    atom_push_boolean(env, false);
}

// (read)      library procedure
// (read port) library procedure
// Read converts external representations of Scheme objects into the objects
// themselves. That is, it is a parser for the nonterminal ⟨datum⟩. Read returns
// the next object parsable from the given input port, updating port to point to
// the first character past the end of the external representation of the object.
// If an end of file is encountered in the input before any characters are found
// that can begin an object, then an end of file object is returned. The port
// remains open, and further attempts to read will also return an end of file
// object. If an end of file is encountered after the beginning of an object’s
// external representation, but the external representation is incomplete and
// therefore not parsable, an error is signalled.
// The port argument may be omitted, in which case it defaults to the value
// returned by current-input-port. It is an error to read from a closed port.
static void atom_read(Environment* env, int params)
{
    assert(0);
}

// (read-char)      procedure
// (read-char port) procedure
// Returns the next character available from the input port, updating the port
// to point to the following character. If no more characters are available, an
// end of file object is returned. Port may be omitted, in which case it defaults
// to the value returned by current-input-port.
static void atom_read_char(Environment* env, int params)
{
    assert(params < 2);
    int c = fgetc(get_input_port_param(env, params == 1));
    // TODO: test this cast. fgetc returns an unsigned char cast to int,
    // which is sure to be error prone.
    // TODO: test if c == EOF. Is EOF a different type to the EOF value?
    atom_push_cell(env, make_character(env, (char)c));
}



// (peek-char) procedure
// (peek-char port) procedure
// Returns the next character available from the input port, without updating
// the port to point to the following character. If no more characters are
// available, an end of file object is returned. Port may be omitted, in which
// case it defaults to the value returned by current-input-port.
static void atom_peek_char(Environment* env, int params)
{
    assert(params < 2);
    FILE* file = get_input_port_param(env, params == 1);
    atom_push_cell(env, make_character(env, ungetc(fgetc(file), file)));
}

// (eof-object?	obj) procedure
// Returns #t if obj is an end of file object, otherwise returns #f. The precise
// set of end of file objects will vary among implementations, but in any case
// no end of file object will ever be an object that can be read in using read.
static void atom_eof_object_q(Environment* env, int params)
{
    Cell* eof = atom_pop_cell(env);
    type_check(env->cont, TYPE_CHARACTER, eof->type);
    atom_push_cell(env, make_boolean(eof->data.character == EOF));
}

// (display	obj)
// (display obj port)
// Writes a representation of obj to the given port. Strings that appear
// in the written representation are not enclosed in doublequotes, and no
// characters are escaped within those strings. Character objects appear
// in the representation as if written by write-char instead of by write.
// Display returns an unspecified value.
// The port argument may be omitted, in which case it defaults to the
// value returned by current-output-port.
static void atom_display(Environment* env, int params)
{
    Cell* obj = atom_pop_cell(env);
    FILE* port = get_output_port_param(env, params == 2);
	print(port, obj, true);
    atom_push_cell(env, make_boolean(false));
}



// (newline)
// (newline port)
// Writes an end of line to port.
// Exactly how this is done differs from one operating system to another.
// Returns an unspecified value.
// The port argument may be omitted, in which case it defaults to the
// value returned by current-output-port.
static void atom_newline(Environment* env, int params)
{
    assert(params < 2); // todo: fix  this.
    FILE* port = get_output_port_param(env, params == 1);
	fputc('\n', port);
	atom_push_boolean(env, false);
}

// 6.6.4. System interface
// (load filename)	optional procedure
// Filename should be a string naming an existing file containing Scheme
// source code. The load procedure reads expressions and definitions from the
// file and evaluates them sequentially. It is unspecified whether the
// results of the expressions are printed. The load procedure does not affect
// the values returned by current-input-port and current-output-port.
// Load returns an unspecified value.
// Rationale:
// For portability, load must operate on source files. Its operation on other
// kinds of files necessarily varies among implementations.
static void atom_load(Environment* env, int params)
{
    Cell* filename = atom_pop_cell(env);
    type_check(env->cont, TYPE_STRING, filename->type);
	atom_state_load_file(env->cont, filename->data.string.data);
    // TODO: return value here.
    atom_push_boolean(env, true);
}

// (write-char char)      procedure
// (write-char char port) procedure
// Writes the character char (not an external representation of the character) to
// the given port and returns an unspecified value. The port argument may be
// omitted, in which case it defaults to the value returned by current-output-port.
static void atom_write_char(Environment* env, int params)
{
    assert(params < 3);
    Cell* c = atom_pop_cell(env);
    type_check(env->cont, TYPE_CHARACTER, c->type);
    fputc(c->data.character, get_output_port_param(env, params == 2));
    atom_push_cell(env, make_boolean(false));
}

// (transcript-on filename) optional procedure
// (transcript-off)	        optional procedure

// Filename must be a string naming an output file to be created. The
// effect of transcript-on is to open the named file for output, and to
// cause a transcript of subsequent interaction between the user and the
// Scheme system to be written to the file. The transcript is ended by a call
// to transcript-off, which closes the transcript file. Only one transcript
// may be in progress at any time, though some implementations may relax this
// restriction. The values returned by these procedures are unspecified.


// This function always returns false.
// It is used as a proxy for functions like complex? that are never true.
static void always_false(Environment* env, int params)
{
    atom_push_boolean(env, false);
}

Instruction make_instruction(int op_code, int operand)
{
    Instruction instruction;
    instruction.op_code = op_code;
    instruction.operand = operand;
    return instruction;
}

static void procedure_init(Procedure* closure, Cell* source)
{
    closure->nparams            = 0;
    closure->num_instructions   = 0;
    closure->instructions       = 0;
    closure->source             = source;
    vector_init_empty(&closure->constants);
}

enum {
    INST_PUSH,
    INST_LOAD,
    INST_CALL,
    INST_DEFINE,
    INST_SET,
    INST_IF,
};

const static char* instruction_names [] = {
    [INST_PUSH]    = "push constant",
    [INST_LOAD]    = "load",
	[INST_CALL]    = "call",
	[INST_DEFINE]  = "define",
    [INST_SET]     = "set",
    [INST_IF]      = "if",
};

static int closure_add_constant(struct Procedure* closure, Cell* cell)
{
    // TODO: Share constants
    int type = cell->type;
    switch(type)
    {
        case TYPE_NUMBER:
        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_BOOLEAN:
        case TYPE_PROCEDURE:
        case TYPE_EMPTY_LIST:
        case TYPE_PAIR:
        case TYPE_BUILT_IN:
            break;
        default:
            assert(0);
            break;
    }

    //printf("Pushing constant: ");
    //print(stdout, cell, false);

    for (int i = 0; i < closure->constants.n; i++)
    {
        if (eq_helper(vector_get(&closure->constants, i), cell, true, true)){
            return i;
        }
    }

    vector_push(&closure->constants, cell);
    return vector_length(&closure->constants) - 1;
}


static void stack_track(FILE* file, atom_state* a) {
    printf("Stack Trace (% 2ld)\n============\n", kv_size(a->stack));
    for (size_t i=0; i < kv_size(a->stack); i++) {
        printf("% 2ld: ", i);
        print_rec(file, kv_a(Cell*, a->stack, i), true);
        puts("");
    }
}

static void compile(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell);

static int compile_reverse(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* list)
{
    if (list->type == TYPE_EMPTY_LIST) return 0;
    int depth = compile_reverse(env, closure, instructions, cdr(list));
    compile(env, closure, instructions, car(list));
    return 1 + depth;
}

static void compile_function_call(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell)
{
    int num_params = compile_reverse(env, closure, instructions, cell) - 1;
    kv_push(Instruction, *instructions, make_instruction(INST_CALL, num_params));
    //printf("Function call with %d params\n", num_params);
}

// 4.1.2
// Literal Expressions

// (quote <datum>) evaluates to <datum>. <Datum> may be any external
// representation of a Scheme object (see section 3.3). This notation is
// used to include literal constants in Scheme code.
static void compile_quote(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell)
{
    Cell* datum  = car(cell); cell = cdr(cell);
    assert(cell->type == TYPE_EMPTY_LIST);
    kv_push(Instruction, *instructions, make_instruction(INST_PUSH, closure_add_constant(closure, datum)));
}

static void print_procedure(const Procedure* p)
{
    printf("Procedure %p – expects %d params\n", p, p->nparams);
    if (p->source) print(stdout, p->source, true);
    printf("# Constants (%ld):\n", kv_size(p->constants));

    for(int i = 0; i < p->constants.n; i++) {
        Cell* cell = kv_A(p->constants, i);
        printf("# [%d] - ", i); print(stdout, cell, true);
    }

    printf("# Instructions (%d)\n", p->num_instructions);
    for (int i=0; i<p->num_instructions; i++) {
        Instruction ins = p->instructions[i];
        printf("#  - %s %d\n", instruction_names[ins.op_code], ins.operand);
    }
}

static void compile_closure(Environment* env, Procedure* parent, instruction_buffer* instructions, Cell* formals, Cell* body)
{
    // Make a new closure
    Procedure* child = (Procedure*)calloc(1, sizeof(Procedure));
    procedure_init(child, body);

    instruction_buffer child_instructions;
    kv_init(child_instructions);

    //printf("Compiling a new function.\n");

    // When a function is called there are N params on the stack. The top most
    // parameter is the first formal. The following loops emits a define instruction
    // for each formal parameter to put them in the environment
    for (Cell* formal = formals; is_pair(formal); formal = cdr(formal))
    {
        type_check(env->cont, TYPE_SYMBOL, car(formal)->type);
        size_t constant = closure_add_constant(child, car(formal));

        printf("Setting local variable: %s\n", car(formal)->data.symbol->name);

        kv_push(Instruction, child_instructions, make_instruction(INST_PUSH, (int)constant));
        kv_push(Instruction, child_instructions, make_instruction(INST_DEFINE, 0));

        child->nparams++;
    }


    compile(env, child, &child_instructions, body);

    //printf("Function compiled OK.\n");
    print_procedure(child);

    size_t c = closure_add_constant(parent, make_closure(env, child));

    kv_push(Instruction, *instructions, make_instruction(INST_PUSH, (int)c));
}


// 4.1.5 Conditionals

// (if <test> <consequent> <alternate>)  syntax
// (if <test> <consequent>)              syntax
// Syntax: <Test>, <consequent>, and <alternate> may be arbitrary
// expressions.
// Semantics: An if expression is evaluated as follows: first, <test> is
// evaluated. If it yields a true value (see section 6.3.1), then
// <consequent> is evaluated and its value(s) is(are) returned. Otherwise
// <alternate> is evaluated and its value(s) is(are) returned.
// If <test> yields a false value and no <alternate> is specified, then
// the result of the expression is unspecified.
static void compile_if(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell)
{
    //<test> <consequent> <alternate>
    Cell* test          = car(cell); cell = cdr(cell);
    Cell* consequent    = car(cell); cell = cdr(cell);
    Cell* alternate     = NULL;
    
    if (cell != &cell_empty_list) {
        alternate = car(cell);
    }

    // Put the test code on the stack. This will compile down to a value
    // that will be true or false.
    compile(env, closure, instructions, test);

    // Push things in reserse order
    compile_closure(env, closure, instructions, &cell_empty_list, consequent);
    
    if (alternate) {
        compile_closure(env, closure, instructions, &cell_empty_list, alternate);
    } else {
        // Add a dummy empty C function for the alternate case if one if not provided.
        // TODO: Make this a macro maybe?
        Cell* proc = make_cell(env, TYPE_BUILT_IN);
        proc->data.built_in = atom_null_function;
        size_t c = closure_add_constant(closure, proc);
        kv_push(Instruction, *instructions, make_instruction(INST_PUSH, c));
    }

    kv_push(Instruction, *instructions, make_instruction(INST_IF,   2));
    kv_push(Instruction, *instructions, make_instruction(INST_CALL, 0));
}

static void compile_lambda(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell)
{
    Cell* formals   = car(cell); cell = cdr(cell);
    Cell* body      = car(cell); cell = cdr(cell);
    assert(cell->type == TYPE_EMPTY_LIST);
    compile_closure(env, closure, instructions, formals, body);
}

// http://exo.willdonnelly.net/blog/scheme-syntax-rules/
static void compile_define_syntax(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell)
{
    Cell* keyword            = car(cell); cell = cdr(cell);
    Cell* transformer        = car(cell);
    assert(keyword->type     = TYPE_SYMBOL);
    assert(transformer->type = TYPE_SYNTAX_RULES);
    environment_define(env, keyword->data.symbol, transformer);
}

static void compile_mutation(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell, int instruction)
{
    // TODO: Handle dotted syntax.
    // Maybe as macro?
    // TODO: Handle function short syntax
    Cell* symbol = car(cell); cell = cdr(cell);
    Cell* expression = car(cell);

    if (symbol->type == TYPE_PAIR && instruction == INST_DEFINE) {
        Cell* signature =  symbol;
        Cell* name      = car(symbol);

        // Short function
        Cell* formals   = cdr(signature);
        Cell* body      = expression;
        type_check(env->cont, TYPE_SYMBOL, name->type);
        type_check(env->cont, TYPE_PAIR,   body->type);

        printf("Compiling short function %s\nFormals:", name->data.symbol->name);

        print_rec(stdout, formals, true);
        puts("");

        compile_closure(env, closure, instructions, formals, body);
        // Push the name
        size_t c = closure_add_constant(closure, name);
        kv_push(Instruction, *instructions, make_instruction(INST_PUSH, (int)c));
        kv_push(Instruction, *instructions, make_instruction(INST_DEFINE, 0));
        return;
    }

    // Push the expression
    compile(env, closure, instructions, expression);

    // Push the symbol
    size_t c = closure_add_constant(closure, symbol);
    kv_push(Instruction, *instructions, make_instruction(INST_PUSH, (int)c));

    // Define (2)
    kv_push(Instruction, *instructions, make_instruction(instruction, 0));
}

static int equal(const char* a, const char* b)
{
    return strcmp(a, b) == 0;
}

static void compile(Environment* env, Procedure* closure, instruction_buffer* instructions, Cell* cell)
{
    switch(cell->type)
    {
        case TYPE_PAIR:
        {
            Cell* head = car(cell);

            // TODO: This is nonsense - evaluating the head could return a function
            switch (head->type) {
                case TYPE_EMPTY_LIST:
                {
                    fprintf(stderr, "Syntax error: A function call must contain a list of at least 1 element.\nGot:");
                    print(stderr, head, true);
                    break;
                }

                case TYPE_SYMBOL:
                {
                    const char* symbol = head->data.symbol->name;

                    if (equal(symbol, "define"))
                    {
                        compile_mutation(env, closure, instructions, cdr(cell), INST_DEFINE);
                        //printf("define ^ 2\n");
                    }
                    else if (equal(symbol, "set!"))
                    {
                        compile_mutation(env, closure, instructions, cdr(cell), INST_SET);
                        //printf("set! ^ 2\n");
                    }
                    else if (equal(symbol, "if"))
                    {
                        compile_if(env, closure, instructions, cdr(cell));
                    }
                    else if (equal(symbol, "lambda"))
                    {
                        compile_lambda(env, closure, instructions, cdr(cell));
                    }
                    else if (equal(symbol, "quote"))
                    {
                        compile_quote(env, closure, instructions, cdr(cell));
                    }
                    else if (equal(symbol, "define-syntax"))
                    {
                        compile_define_syntax(env, closure, instructions, cdr(cell));
                    }
                    else
                    {
                        compile_function_call(env, closure, instructions, cell);
                    }
                    break;
                }
                default:
                {
                    fprintf(stderr, "Compile error: Expected symbol.\nGot:");
                    print(stderr, head, true);
                    break;
                }
            }
            break;
        }

        case TYPE_SYMBOL:
        {
            // Load the symbol
            size_t c = closure_add_constant(closure, cell);
            kv_push(Instruction, *instructions, make_instruction(INST_PUSH, c));
            kv_push(Instruction, *instructions, make_instruction(INST_LOAD, 0));
            break;
        }

		default:
        {
            kv_push(Instruction, *instructions, make_instruction(INST_PUSH, closure_add_constant(closure, cell)));
            break;
        }
    }

    closure->num_instructions = kv_size(*instructions);
    const size_t bytes = closure->num_instructions * sizeof(Instruction);
    closure->instructions = (struct Instruction*)malloc(bytes);
    memcpy(closure->instructions, instructions->a, bytes);
}

static Cell* apply_macros(Environment* env, Cell* cell)
{

    return cell;
}

void atom_state_load(atom_state* cont, const char* data)
{
    // TODO: Can read off the end of data.
    printf("Input String: %s\n", data);
	Environment* env = cont->env;

	jump_buffer* prev = cont->escape;
	jump_buffer  jb;

	int error = setjmp(jb.buffer);

	if (error)
	{
		printf("Recovering from an error\n");
		goto cleanup;
	}

	jb.prev = cont->escape;
	cont->escape = &jb;

	Input input;
    input_init(&input, data);

	for(;;)
	{
        Token next;
        read_token(&input, &next);
        Cell* parsed;
		if ((parsed = parse_datum(env, &input, &next)))
		{
            //printf("Input was parsed as: ");
            //print(stdout, parsed, false);

            Cell* after_macros = apply_macros(env, parsed);

            //printf("After macros: ");
            //print(stdout, after_macros, false);

            //printf("Compiling top level function\n");
            struct Procedure closure;
            procedure_init(&closure, after_macros);

            instruction_buffer instructions;
            kv_init(instructions);

            compile(env, &closure, &instructions, after_macros);
            print_procedure(&closure);
            eval(cont, &closure);

            int top = vector_length(&cont->stack);
            if (top > 0)
            {
                Cell* result = vector_get(&cont->stack, top-1);
                printf("> ");
                print(stdout, result, true);
            }

            kv_destroy(instructions); // todo - is this delete ok?
        }
        else break;
	}

cleanup:

	collect_garbage(cont);

	// restore the old jump buffer
	cont->escape = prev;
}

void atom_state_load_file(atom_state* cont, const char* filename)
{
	FILE* file = fopen(filename, "r");

	if (!file)
	{
        fprintf(stderr, "Error opening file %s\n", filename);
		return;
	}

	fseek (file, 0, SEEK_END);
	size_t size = ftell (file);
	rewind(file);
	char* buffer = (char*) malloc(size+1);
	size_t read = fread(buffer, 1, size, file);
	buffer[read] = 0;
	fclose (file);

	atom_state_load(cont, buffer);

	free(buffer);
}

static void eval(atom_state* cont, struct Procedure* closure)
{
	assert(cont);
    assert(closure);

    size_t pc = 0;
    Environment* env = cont->env;

    //printf("eval: function %p %d params\n", closure, closure->nparams);

    for (int i=0; i<vector_length(&closure->constants); i++)
    {
        //printf("Constant %d: ", i);
        //print(stdout, vector_get(&closure->constants, i), true);
    }

    for (;;)
    {
        // End of input
        if (pc == closure->num_instructions) return;

        assert(pc >= 0);
        assert(pc < closure->num_instructions);

        const Instruction instruction = closure->instructions[pc];
        pc++;

        //printf("operation: %s %d\n", instruction_names[instruction.op_code], instruction.operand);

        switch (instruction.op_code)
        {
            // Pop 3 values – the test and the 2 blocks of code to call.
            // If the test is true,  push consequent
            // If the test is false, push alternate
            // The next operations is call(0).
            case INST_IF:
            {
                Cell* alternate     = atom_pop_cell(env);
                Cell* consequent    = atom_pop_a(env, TYPE_PROCEDURE);

                Cell* result = is_truthy(atom_pop_cell(env)) ? consequent : alternate;
                atom_push_cell(env, result);
                break;
            }

            // (set! <variable> <expression>)
            // <Expression> is evaluated, and the resulting value is stored in the
            // location to which <variable> is bound. <Variable> must be bound either
            // in some region enclosing the set! expression or at top level. The result
            // of the set! expression is unspecified.
            case INST_SET:
            {
                Cell* symbol = atom_pop_a(env, TYPE_SYMBOL);
                Cell* value = atom_pop_cell(env);
                environment_set(env, symbol->data.symbol, value);
                atom_push_undefined(env);
                break;
            }

            case INST_DEFINE:
            {
                Cell* symbol = atom_pop_a(env, TYPE_SYMBOL);
                Cell* value = atom_pop_cell(env);
                environment_define(env, symbol->data.symbol, value);
                break;
            }

            case INST_PUSH:
            {
                vector_push(&cont->stack, vector_get(&closure->constants, instruction.operand));
                break;
            }

            case INST_LOAD:
            {
                atom_push_cell(env, environment_get(env, atom_pop_cell(env)));
                break;
            }

            case INST_CALL:
            {
                int num_params = instruction.operand;
                assert(num_params >= 0);

                Cell* function = atom_pop_cell(env);

                switch (function->type)
                {
                    case TYPE_BUILT_IN:
                    {
                        function->data.built_in(env, num_params);
                        break;
                    }
                    case TYPE_PROCEDURE:
                    {
                        // TODO: Make environment better
                        Environment* child = create_environment(cont, env);
                        cont->env = child;

                        if (num_params != function->data.closure->nparams)
                        {
                            signal_error(cont, "Error calling procedure: Expected %d params but was passed %d", function->data.closure->nparams, num_params);
                        }
                        eval(cont, function->data.closure);
                        break;
                    }

                    default:
                    {
                        stack_track(stdout, env->cont);
                        assert(0);
                        break;
                    }
                }
                break;
            }


            default:
                assert(0);
        }
    }
}


static Cell* load_register(atom_state* cont, int n)
{
    assert(n > 0);
    return vector_get(&cont->stack, n-1);
}

size_t atom_api_get_top(atom_state* cont)
{
    return vector_length(&cont->stack);
}

void atom_api_clear(atom_state* cont)
{
    // todo: encapsulate
    cont->stack.n = 0;
}

double atom_state_pop_number(atom_state* cont)
{
    // TODO: Error checking
    return atom_pop_number(cont->env);
}

bool atom_api_to_boolean(atom_state* cont, int n)
{
    return is_truthy(load_register(cont, n));
}

const char* atom_api_to_string(atom_state* cont, int n)
{
    Cell* cell = load_register(cont, n);
    if (cell->type == TYPE_STRING)
        return cell->data.string.data;
    return 0;
}

void atom_add_builtin(atom_state* state, const char* name, atom_builtin function)
{
	assert(state);
	assert(name);
	assert(function);

    Cell* cell = make_cell(state->env, TYPE_BUILT_IN);
    cell->data.built_in = function;
	environment_define(state->env, find_or_insert_symbol(state->env->cont, name), cell);
}

atom_state* atom_state_new()
{
	atom_state* state	= (atom_state*)calloc(1, sizeof(atom_state));
	Environment* env    = create_environment(state, NULL);
	state->env           = env;
	state->input     	= stdin;
    state->output        = stdout;
    state->log           = fopen(".atom_log", "w+");
    state->symbol_mask   = 0xFF;
    state->symbols       = (Symbol**)calloc(1+state->symbol_mask, sizeof(Symbol*));

    vector_init_empty(&state->stack);

    const struct Library libs [] = {

        // (and <test1> ...)  library syntax
        // The <test> expressions are evaluated from left to right, and the value of
        // the first expression that evaluates to a false value (see section 6.3.1)
        // is returned. Any remaining expressions are not evaluated. If all the
        // expressions evaluate to true values, the value of the last expression is
        // returned. If there are no expressions then #t is returned.

        // (or	<test1> ...) library syntax
        // The <test> expressions are evaluated from left to right, and the value of
        // the first expression that evaluates to a true value (see section 6.3.1) is
        // returned. Any remaining expressions are not evaluated. If all expressions
        // evaluate to false values, the value of the last expression is returned. If
        // there are no expressions then #f is returned.


        // 4.2.1. Conditionals
        // (cond <clause1> <clause2> ...) library syntax

        // Syntax: Each <clause> should be of the form
        // (<test> <expression1> ...)
        // where <test> is any expression.
        // Alternatively, a <clause> may be of the form
        // (<test> => <expression>)
        // The last <clause> may be an “else clause,” which has the form
        // (else <expression1> <expression2> ...)

        // Semantics: A cond expression is evaluated by evaluating the <test>
        // expressions of successive <clause>s in order until one of them evaluates
        // to a true value. When a <test> evaluates to a true value, then the
        // remaining <expression>s in its <clause> are evaluated in order, and the
        // result(s) of the last <expression> in the <clause> is(are) returned as
        // the result(s) of the entire cond expression. If the selected <clause>
        // contains only the <test> and no <expression>s, then the value of the
        // <test> is returned as the result.

        // If the selected <clause> uses the => alternate form, then the
        // <expression> is evaluated. Its value must be a procedure that accepts
        // one argument; this procedure is then called on the value of the <test>
        // and the value(s) returned by this procedure is(are) returned by the cond
        // expression. If all <test>s evaluate to false values, and there is no
        // else clause, then the result of the conditional expression is
        // unspecified; if there is an else clause, then its <expression>s are
        // evaluated, and the value(s) of the last one is(are) returned.


        // (case <key> <clause1> <clause2> ...) library syntax
        // Syntax: <Key> may be any expression. Each <clause> should have the form
        //  ((<datum1> ...) <expression1> <expression2> ...),
        // where each <datum> is an external representation of some object. All the
        // <datum>s must be distinct. The last <clause> may be an “else clause,”
        // which has the form
        //  (else <expression1> <expression2> ...).
        // Semantics:
        // A case expression is evaluated as follows. <Key> is evaluated and its
        // result is compared against each <datum>. If the result of evaluating <key>
        // is equivalent (in the sense of eqv?; see section 6.1) to a <datum>, then
        // the expressions in the corresponding <clause> are evaluated from left to
        // right and the result(s) of the last expression in
        // the <clause> is(are) returned as the result(s) of the case expression. If
        // the result of evaluating <key> is different from every <datum>, then if
        // there is an else clause its expressions are evaluated and the result(s) of
        // the last is(are) the result(s) of the case expression; otherwise the
        // result of the case expression is unspecified.
        //        {"let",				atom_let},
        //        {"let*",			atom_let_s},

        // 4.2.3 Sequencing

        // (begin <expression1> <expression> ...)	library syntax
        // The <expression>s are evaluated sequentially from left to right, and
        // the value(s) of the last <expression> is(are) returned. This expression
        // type is used to sequence side effects such as input and output.
        //  {"begin",      		atom_begin},


        // (quasiquote <qq template>) syntax
        // `<qq template>             syntax
        // “Backquote” or “quasiquote” expressions are useful for constructing a list or
        // vector structure when most but not all of the desired structure is known in
        // advance. If no commas appear within the ⟨qq template⟩, the result of evaluating
        // `⟨qq template⟩ is equivalent to the result of evaluating ’⟨qq template⟩. If a
        // comma appears within the ⟨qq template⟩, however, the expression following the
        // comma is evaluated (“unquoted”) and its result is inserted into the structure
        // instead of the comma and the expression. If a comma appears followed immediately
        // by an atsign (@), then the following expression must evaluate to a list; the
        // opening and closing parentheses of the list are then “stripped away” and the
        // elements of the list are inserted in place of the comma at-sign expression
        // sequence. A comma at-sign should only appear within a list or vector <qq template>.
        // {"quasiquote",      atom_quasiquote},


        // (apply proc arg1 ... args) procedure
        // Proc must be a procedure and args must be a list. Calls proc with the
        // elements of the list (append (list arg1 ...) args) as the actual arguments.
        //{"apply",	   		atom_apply},

        {"syntax-rules",    atom_syntax_rules},
        
        {"eqv?",			atom_eqv_q},
        {"eq?",				atom_eq_q},
        {"equal?",			atom_equal_q},
        
        // numeric
        {"number?",    		atom_number_q},
        {"complex?",   		always_false},
        {"real?",      		atom_number_q},
        {"rational?",  		always_false},

        {"integer?",   		atom_integer_q},

        {"exact?",			atom_exact_q},
        {"inexact?",		atom_inexact_q},
        {"=",				atom_comapre_equal},
        {"<",				atom_compare_less},
        {">",				atom_compare_greater},
        {"<=",				atom_compare_less_equal},
        {">=",				atom_compare_greater_equal},
        {"zero?",           atom_zero_q},
        {"positive?",       atom_positive_q},
        {"negative?",       atom_negative_q},
        {"odd?",            atom_odd_q},
        {"even?",           atom_even_q},
        {"min",				atom_min},
        {"max",				atom_max},
        {"sin",             atom_sin},
        {"cos",             atom_cos},
        {"tan",             atom_tan},
        {"asin",            atom_asin},
        {"acos",            atom_acos},
        {"atan",            atom_atan},

        // boolean
        {"not",		   		atom_not},
        {"boolean?",   		atom_boolean_q},

        // lists
        {"pair?",      		atom_pair_q},
        {"cons",       		atom_cons},
        {"car",        		atom_car},
        {"cdr",        		atom_cdr},
        {"set-car!",   		atom_set_car_b},
        {"set-cdr!",   		atom_set_cdr_b},
        {"null?",      		atom_null_q},
        {"list?",      		atom_list_q},
        {"list",       		atom_list},
        {"length",     		atom_length},
        {"append",     		atom_append},
		{"list-tail",		atom_list_tail},
		{"list-ref",		atom_list_ref},

        // char
        {"char?",			atom_char_q},
        {"char=?",          atom_char_equal_q},
        {"char<?",          atom_char_less_than_q},
        {"char>?",          atom_char_greater_than_q},
        {"char<=?",         atom_char_less_than_or_equal_q},
        {"char>=?",         atom_char_greater_than_or_equal_q},

        {"char-ci=?",       atom_char_ci_equal_q},
        {"char-ci<?",       atom_char_ci_less_than_q},
        {"char-ci>?",       atom_char_ci_greater_than_q},
        {"char-ci<=?",      atom_char_ci_less_than_or_equal_q},
        {"char-ci>=?",      atom_char_ci_greater_than_or_equal_q},

        {"char-alphabetic?", atom_char_alphabetic_q},
        {"char-numeric?",    atom_char_numeric_q},
        {"char-whitespace?", atom_char_whitespace_q},
        {"char-upper-case?", atom_char_upper_case_q},
        {"char-lower-case?", atom_char_lower_case_q},

        {"char-upcase",     atom_char_upcase},
        {"char-downcase",   atom_char_downcase},

        {"char->integer",	atom_char_to_integer},
        {"integer->char",	atom_integer_to_char},

        // string
        {"string?",	   		atom_string_q},
        {"string",			atom_string},
        {"make-string",		atom_make_string},
        {"string-length",	atom_string_length},
        {"string-ref",	   	atom_string_ref},
        {"string-set!",	   	atom_string_set},


        {"string=?",        atom_string_equal_q},
        {"string-ci=?",     atom_string_ci_equal_q},
        {"string<?",        atom_string_less_than_q},
        {"string>?",        atom_string_greater_than_q},

        {"string<=?",       atom_string_less_than_equal_q},
        {"string>=?",       atom_string_greater_than_equal_q},
        {"string-ci<?",     atom_string_ci_less_than_q},
        {"string-ci>?",     atom_string_ci_greater_than_q},
        {"string-ci<=?",    atom_string_ci_less_than_equal_q},
        {"string-ci>=?",    atom_string_ci_greater_than_equal_q},
        {"substring",       atom_substring},
        {"string-append",   atom_string_append},

        {"string-copy",     atom_string_copy},
        {"string-fill!",    atom_string_fill_b},
        {"string->list",    atom_string_to_list},
        {"list->string",    atom_list_to_string},

        // Vector
        {"vector?",	   		atom_vector_q},
        {"make-vector",	  	atom_make_vector},
        {"vector",	   		atom_vector},
        {"vector-length", 	atom_vector_length},
        {"vector-ref",		atom_vector_ref},
        {"vector->list",    atom_vector_to_list},
        {"list->vector",    atom_list_to_vector},
        {"vector-set!",		atom_vector_set_b},
        {"vector-fill!",	atom_vector_fill_b},

        // symbols
        {"symbol?",    		atom_symbol_q},
        {"symbol->string",	atom_symbol_to_string},
        {"string->symbol",	atom_string_to_symbol},

        // control
        {"procedure?", 		atom_procedure_q},

        {"call-with-input-file",    atom_call_with_input_file},
        {"call-with-output-file",   atom_call_with_output_file},

        {"scheme-report-environment",  atom_scheme_report_environment},
        {"null-environment",           atom_null_environment},
        {"interaction-environment",    atom_interaction_environment},

        {"close-input-port",        atom_close_input_port},
        {"close-output-port",       atom_close_output_port},
        {"open-input-file",         atom_open_input_file},
        {"open-output-file",        atom_open_output_file},

        // io
        {"input-port?",             atom_input_port_q},
        {"output-port?",            atom_output_port_q},

        // input
        {"peek-char",               atom_peek_char},
        {"eof-object?",             atom_eof_object_q},
        {"read-char",               atom_read_char},
        {"current-input-port",      atom_current_input_port},
        {"current-output-port",     atom_current_output_port},

        // output
        {"write",      		atom_write},
        {"display",	   		atom_display},
        {"newline",	   		atom_newline},
        {"write-char",      atom_write_char},

        // output
        {"load",	atom_load},
        {"read",    atom_read},

        // custom?
        {"error",	atom_error},
        {"env",     atom_env},

        {NULL, NULL}
    };

    for (const struct Library* library = &libs[0]; library->name; library++)
    {
        atom_add_builtin(state, library->name, library->func);
    }

    return state;
}

void atom_state_free(atom_state* cont)
{
    kv_destroy(cont->stack);

    for (size_t i=0; i <= cont->symbol_mask; i++)
    {
        Symbol* next;
        for (Symbol* symbol = cont->symbols[i]; symbol; symbol = next)
        {
            next = symbol->next;
            free(symbol->name);
            free(symbol);
        }
    }
    free(cont->symbols);
    free(cont);
}
