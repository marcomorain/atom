#include "atom.h"

#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <string>
#include <cassert>
#include <stdio.h>
#include <stdarg.h>

#define DEBUG_LEXER (0)

#if (DEBUG_LEXER)
#define LEXER_TRACE(format, ...) printf(format, __VA_ARGS__)
#else
#define LEXER_TRACE(format, ...)
#endif


enum CellType
{
	TYPE_BOOLEAN,
	TYPE_CHARACTER,
	TYPE_NUMBER,
	TYPE_STRING,
	TYPE_PAIR,
	TYPE_VECTOR,
	TYPE_SYMBOL,
	TYPE_PROCEDURE,
};

const static char* typenames [] = {
	"boolean",
	"character",
	"number",
	"string",
	"pair",
	"vector",
	"symbol",
	"procedure"
};

struct Environment;
struct Cell;

typedef Cell* (*Function) (Environment* env, Cell* params);

struct Cell
{
	struct Pair
	{
		Cell* car;
		Cell* cdr;
	};
	
	struct Procedure
	{
		// If function is null, then the procedure
		// was created in scheme, otherwise it is a built-in
		Function 		function;
		Cell*    		formals;
		Cell*    		body;
		Environment*	env;
	};

	// todo: add a const string type? or a flag?

	union Data
	{
		bool         boolean;
		char         character;
		double       number;
		char*		 string;
		Pair         pair;
		const char*  symbol;
		Procedure    procedure;
	};
	
	CellType type;
	Data data;
};

static Cell cell_true  = { TYPE_BOOLEAN, {true } };
static Cell cell_false = { TYPE_BOOLEAN, {false} };

enum Type
{
	TOKEN_IDENTIFIER,
	TOKEN_BOOLEAN,
	TOKEN_NUMBER,
	TOKEN_CHARACTER,
	TOKEN_STRING,
	TOKEN_LIST_START,
	TOKEN_LIST_END,
	TOKEN_HASH_LIST_START,
	TOKEN_QUOTE,
	TOKEN_BACKTICK,
	TOKEN_COMMA,
	TOKEN_COMMA_AT,
	TOKEN_DOT
};

static void print_rec(const Cell* cell, int is_car)
{
	if (!cell) return;

	switch(cell->type)
	{
	case TYPE_BOOLEAN:
		printf("#%c", (cell->data.boolean ? 't' : 'f'));
		break;

	case TYPE_NUMBER:
		printf("%lg", cell->data.number);
		break;

	case TYPE_CHARACTER:
		{
			char c = cell->data.character;
			switch(c)
			{
				case ' ':
				printf("#\\space");
				break;
				
				case '\n':
				printf("#\\newline");
				break;
				
				default:
				printf("#\\%c", c);
				break;
			}
			break;
		}

	case TYPE_STRING:
		printf("\"%s\"", cell->data.string);
		break;

	case TYPE_SYMBOL:
		printf("%s", cell->data.symbol);
		break;

	case TYPE_PAIR:
		if (is_car) printf("(");
		print_rec(cell->data.pair.car, 1);
		
		if (Cell* c = cell->data.pair.cdr)
		{
			printf(" ");
			if (c->type != TYPE_PAIR) printf(". ");
			print_rec(c, 0);
		}

		if (is_car) printf(")");
		break;
	}
}

static void print(const Cell* cell)
{
	print_rec(cell, 1);
	printf("\n");
}

static void signal_error(const char* message, ...)
{
	va_list args;
	va_start(args, message);
	fprintf(stderr, "Error: ");
	vfprintf(stderr, message, args);
	fprintf(stderr, "\n");
	va_end(args);
	exit(-1);
}

static void type_check(int expected, int actual)
{
	if (actual != expected)
	{
		signal_error("%s expected, got %s", typenames[expected], typenames[actual]);
	}
}

static Cell* make_cell(int type)
{
	Cell* result = (Cell*)malloc(sizeof(Cell));
	memset(result, 0, sizeof(Cell));
	result->type = (CellType)type;
	return result;	
}

static Cell* make_boolean(bool value)
{
	return value ? &cell_true : &cell_false;
}

static Cell* make_number(double value)
{
	Cell* number = make_cell(TYPE_NUMBER);
	number->data.number = value;
	return number;
}

static Cell* make_character(char c)
{
	Cell* character = make_cell(TYPE_CHARACTER);
	character->data.character = c;
	return character;	
}

static Cell* make_procedure(Environment* env, Cell* formals, Cell* body)
{
	type_check(TYPE_PAIR, formals->type);
	type_check(TYPE_PAIR, body->type);

	Cell* proc = make_cell(TYPE_PROCEDURE);
	proc->data.procedure.formals = formals;
	proc->data.procedure.body    = body;
	proc->data.procedure.env	 = env;
	return proc;	
}

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

static void set_car(Cell* list, Cell* car)
{
	assert(list->type == TYPE_PAIR);
	list->data.pair.car = car;
}

static void set_cdr(Cell* list, Cell* cdr)
{
	assert(list->type == TYPE_PAIR);
	list->data.pair.cdr = cdr;
}

static Cell* cons(Cell* car, Cell* cdr)
{
	Cell* cell = make_cell(TYPE_PAIR);
	set_car(cell, car);
	set_cdr(cell, cdr);
	return cell;
}

struct Token
{
	void print(void) const
	{
		switch(type)
		{
		case TOKEN_NUMBER:
			LEXER_TRACE("Token TOKEN_NUMBER %lg\n", data.number);
			break;

		case TOKEN_IDENTIFIER:
			LEXER_TRACE("Token TOKEN_IDENTIFIER %s\n", data.identifier);
			break;

		case TOKEN_STRING:
			LEXER_TRACE("Token TOKEN_STRING \"%s\"\n", data.string);
			break;

#define PRINT_CASE(id) case id: LEXER_TRACE("Token %s\n", #id); break

		PRINT_CASE(TOKEN_BOOLEAN);
		PRINT_CASE(TOKEN_CHARACTER);
		PRINT_CASE(TOKEN_LIST_START);
		PRINT_CASE(TOKEN_LIST_END);
		PRINT_CASE(TOKEN_HASH_LIST_START);
		PRINT_CASE(TOKEN_QUOTE);
		PRINT_CASE(TOKEN_BACKTICK);
		PRINT_CASE(TOKEN_COMMA);
		PRINT_CASE(TOKEN_COMMA_AT);
		PRINT_CASE(TOKEN_DOT);
#undef PRINT_CASE
		}

	}

	union Data
	{
		double		number;
		bool		boolean;
		char*		string;
		const char*	identifier;
		char		character;
	};

	Type type;
	Data data;
};


struct TokenList
{
private:

	void add_basic(Type t)
	{
		tokens[next].type = t;
		tokens[next].print();
		next++;
	}

	char* buffer_copy_and_reset(void)
	{
		char* dup = (char*)malloc(buffer_position + 1);
		memcpy(dup, buffer_data, buffer_position);
		dup[buffer_position] = 0; // null terminate
		buffer_position = 0;
		return dup;
	}

public:

	void buffer_push(char c)
	{
		if (buffer_position == buffer_length)
		{
			buffer_length *= 2;
			buffer_data = (char*)realloc(buffer_data, buffer_length);
		}
		buffer_data[buffer_position] = c;
		buffer_position++;
	}

	char*  buffer_data;
	size_t buffer_length;
	size_t buffer_position;

	Token*	tokens;
	int		next;
	int		length;
	
	void start_parse()
	{
		length = next;
		next = 0;
	}

	void add_backtick()
	{
		add_basic(TOKEN_BACKTICK);
	}

	void add_list_start()
	{
		add_basic(TOKEN_LIST_START);
	}

	void add_list_end()
	{
		add_basic(TOKEN_LIST_END);
	}

	void add_quote()
	{
		add_basic(TOKEN_QUOTE);
	}

	void add_dot()
	{
		add_basic(TOKEN_DOT);
	}

	void add_identifier(void)
	{
		tokens[next].type = TOKEN_IDENTIFIER;
		tokens[next].data.identifier = buffer_copy_and_reset();
		tokens[next].print();
		next++;
	}

	void add_string(void)
	{
		tokens[next].type = TOKEN_STRING;
		tokens[next].data.string = buffer_copy_and_reset();
		tokens[next].print();
		next++;
	}

	void add_number(double number)
	{
		tokens[next].type = TOKEN_NUMBER;
		tokens[next].data.number = number;
		tokens[next].print();
		next++;
	}
	
	void add_character(char value)
	{
		tokens[next].type = TOKEN_CHARACTER;
		tokens[next].data.character = value;
		tokens[next].print();
		next++;
	}

	void add_boolean(bool value)
	{
		tokens[next].type = TOKEN_BOOLEAN;
		tokens[next].data.boolean= value;
		tokens[next].print();
		next++;
	}

	void init(int size)
	{
		next   = 0;
		length = size;
		tokens = (Token*)malloc(size*sizeof(Token));

		buffer_position = 0;
		buffer_length	= 64;
		buffer_data = (char*)malloc(buffer_length);
		

	}

	void destroy()
	{
		free(tokens);
		free(buffer_data);
	}

	const Token* peek(void) const
	{
		if (next < length)
		{
			return tokens + next;
		}
		return NULL;
	}

	void skip(void)
	{
		next++;
	}
};


struct Input
{
	unsigned	line;
	unsigned	column;
	const char* data;
	TokenList*	tokens;

	void init(const char* d)
	{
		line	= 1;
		column	= 1;
		data	= d;
	}
	
	char get(void)  const
	{
		return *data;
	};
	
	char next(void)
	{
		assert(*data);

		column++;

		if (*data == '\n')
		{
			column = 1;
			line++;
		}
		
		data++;
		return get();
	};
};

void skip_whitespace(Input& input)
{
	for(char c = input.get(); c; c = input.next())
	{
		switch(c)
		{
			case '\n':
			case ' ':
			case '\t':
			continue;
			
			case ';':
			for (char d = input.next(); d != '\n'; d = input.next())
			{
				if (!d) return;
			}
			break;
			
			default: return;
		}
	}
}

bool is_digit(char c)
{
	return c >= '0' && c <= '9';
}

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
	return is_initial(c) || is_digit(c) || is_special_subsequent(c);
}

static void read_character(Input& input)
{
	char c = input.get();
	switch(c)
	{
		case 's':
		if (input.next() == 'p'){
			if (input.next() != 'a') signal_error("space expected");
			if (input.next() != 'c') signal_error("space expected");
			if (input.next() != 'e') signal_error("space expected");
			if (!is_delimeter(input.next())) signal_error("space expected");
			input.tokens->add_character(' ');
			return;
		}
		else goto success;

		case 'n':
			if (input.next() == 'e'){
				if (input.next() != 'w') signal_error("newline expected");
				if (input.next() != 'l') signal_error("newline expected");
				if (input.next() != 'i') signal_error("newline expected");
				if (input.next() != 'n') signal_error("newline expected");
				if (input.next() != 'e') signal_error("newline expected");
				if (!is_delimeter(input.next())) signal_error("newline expected");
				input.tokens->add_character('\n'); // newline
				return;
			}
			else goto success;

		default: goto success;
	}

success:

	if (is_delimeter(input.next()))
	{
		input.tokens->add_character(c);
		return;
	}

	signal_error("delimeter expected");
}

// Convert an ascii digit, '0' to '9' into
// a double 0.0 to 9.0
static double char_to_double(char c)
{
	assert(c >= '0' && c <= '9');
	return c - '0';
}

void read_number(Input& input)
{
	char c = input.get();

	double accum = char_to_double(c);

	for (;;)
	{
		c = input.next();

		if (!is_digit(c))
		{
			input.tokens->add_number(accum);
			return;
		}
		
		accum *= 10;
		accum += char_to_double(c);
	}
}

void read_string(Input& input)
{
	assert(input.get() == '"');

	for (;;)
	{
		char c = input.next();

		if (c == '"'){
			input.next();
			input.tokens->add_string();
			return;
		}

		if (c == '\\'){
			c = input.next();
			if (c == '"' || c == '\\')
			{
				input.tokens->buffer_push(c);
				continue;
			}
			signal_error("malformed string");
		}

		if (isprint(c))
		{
			input.tokens->buffer_push(c);
			continue;
		}

		signal_error("unexpected character in string");
	}
}

bool is_peculiar_identifier(char c)
{
	// @todo: ... can be accepted here.
	return c == '+' || c == '-';
}

void read_identifier(Input& input)
{
	char c = input.get();
	if (is_initial(c))
	{
		input.tokens->buffer_push(c);

		for (;;)
		{
			c = input.next();
			if (is_delimeter(c)) break;
			if (!is_subsequent(c))
			{
				signal_error("malformed identifier at line %d column %d", input.line, input.column);
			}
			input.tokens->buffer_push(c);
		}
	}
	else if (is_peculiar_identifier(c))
	{
		input.tokens->buffer_push(c);
		input.next();
	}
	else
	{
		signal_error("malformed identifier at line %d column %d", input.line, input.column);
	}

	input.tokens->add_identifier();

}

void read_token(Input& input)
{
	skip_whitespace(input);
	
	char c = input.get();
	
	switch(c)
	{
		case '(':  input.next(); input.tokens->add_list_start(); break;
		case ')':  input.next(); input.tokens->add_list_end();   break;
		case '\'': input.next(); input.tokens->add_quote();      break;
		case '`':  input.next(); input.tokens->add_backtick();   break;
		case '.':  input.next(); input.tokens->add_dot();        break;

		case '#':
		{
			c = input.next();
			switch(c)
			{
				// @todo: check for next character here (should be a delimiter)
				case 't':  input.next(); input.tokens->add_boolean(true);  break;
				case 'f':  input.next(); input.tokens->add_boolean(false); break;
				case '\\': input.next(); read_character(input);			   break;
			}

			break;
		}

		case '"': read_string(input); break;

		case 0: break;
		
		default:
		{
			if (is_digit(c))
			{
				read_number(input);
			}
			else
			{
				read_identifier(input);
			}

			break;
		}
	}
}


Cell* parse_datum(TokenList& tokens);

Cell* parse_vector(TokenList& tokens)
{
	return 0;
}

Cell* parse_abreviation(TokenList& tokens)
{
	const Token* t = tokens.peek();
	
	if (!t) signal_error("unexpected end of input");

	if(t->type == TOKEN_QUOTE)
	{
		Cell* quote        = make_cell(TYPE_SYMBOL);
		quote->data.symbol = "quote";
		tokens.skip();
		return cons(quote,
					cons(parse_datum(tokens),
				         cons(NULL, NULL)));
	}

	if (t->type == TOKEN_BACKTICK)
	{
		tokens.skip();
		return parse_datum(tokens);
	}

	if(t->type == TOKEN_COMMA)
	{
		tokens.skip();
		return parse_datum(tokens);
	}

	if (t->type == TOKEN_COMMA_AT)
	{
		tokens.skip();
		return parse_datum(tokens);
	}

	return NULL;
}

Cell* parse_list(TokenList& tokens)
{
	if (!tokens.peek()) return NULL;
	
	if (tokens.peek()->type != TOKEN_LIST_START)
	{
		return parse_abreviation(tokens);
	}

	// skip the start list token
	tokens.skip();
	
	Cell* cell = parse_datum(tokens);
	
	Cell* list = cons(cell, NULL);

	Cell* head = list;

	for (;;)
	{
		if (!tokens.peek())
		{
			signal_error("Unexpected end of input.");
		}
		
		if (tokens.peek()->type == TOKEN_DOT)
		{
			tokens.skip();
			Cell* cell = parse_datum(tokens);

			if (!cell)
			{
				signal_error("expecting a datum after a dot");
			}

			set_cdr(list, cell);

			if (tokens.peek()->type != TOKEN_LIST_END)
			{
				signal_error("expecting )");
			}

			tokens.skip();
			break;
		}
		else if (tokens.peek()->type == TOKEN_LIST_END)
		{
			tokens.skip();
			break; // success
		}
		
		Cell* car = parse_datum(tokens);

		if (!car)
		{
			signal_error("is this unexpected end of input? todo");
		}

		Cell* rest = cons(car, NULL);
		set_cdr(list, rest);
		list = rest;
	}

	// success, allocate a list
	return head;
}

Cell* parse_compound_datum(TokenList& tokens)
{
	if (Cell* cell = parse_list(tokens))
	{
		return cell;
	}

	return parse_vector(tokens);
}

Cell* parse_simple_datum(TokenList& tokens)
{
	const Token* t = tokens.peek();
	
	if (!t) return NULL;

	switch (t->type)
	{
		case TOKEN_BOOLEAN:
		{
			Cell* cell = make_cell(TYPE_BOOLEAN);
			cell->data.boolean = t->data.boolean;
			tokens.skip();
			return cell;
		}

		case TOKEN_CHARACTER:
		{
			
			Cell* cell = make_cell(TYPE_CHARACTER);
			cell->data.character = t->data.character;
			tokens.skip();
			return cell;
		}

		case TOKEN_NUMBER:
		{
			Cell* cell = make_number(t->data.number);
			tokens.skip();
			return cell;
		}

		case TOKEN_IDENTIFIER:
		{
			Cell* cell = make_cell(TYPE_SYMBOL);
			cell->data.symbol = t->data.identifier;
			tokens.skip();
			return cell;
		}
		
		case TOKEN_STRING:
		{
			Cell* cell = make_cell(TYPE_STRING);
			cell->data.string = t->data.string;
			tokens.skip();
			return cell;
		}

		default:
			return NULL;
	}
}

Cell* parse_datum(TokenList& tokens)
{
	if (Cell* cell = parse_simple_datum(tokens))
	{
		return cell;
	}
	
	return parse_compound_datum(tokens);
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


static bool power_of_two(int v)
{
	return v && !(v & (v - 1));
}

struct Environment
{
	struct Node
	{
		const char* symbol;
		Cell*       value;
		Node*		next;
	};

	Environment* parent;
	Node**		 data;
	unsigned	 mask;

	void init(int size, Environment* parent_env)
	{
		assert(power_of_two(size));
		mask = size-1;
		const size_t num_bytes = size * sizeof(Node*);
		data = (Node**)malloc(num_bytes);
		memset(data, 0, num_bytes);
		parent = parent_env;
	}

	Cell* get(const Cell* symbol) const
	{
		assert(symbol->type == TYPE_SYMBOL);
		const char* str = symbol->data.symbol;
		unsigned hash = mask & MurmurHash2(str, strlen(str));

		for (Node* node = data[hash]; node; node = node->next)
		{
			if (strcmp(str, node->symbol) == 0)
			{
				return node->value;
			}
		}
		
		if (parent)
		{
			return parent->get(symbol);
		}
		
		signal_error("reference to undefined identifier: %s", str);
		return NULL;
	}
};

void environment_define(Environment* env, const char* symbol, Cell* value)
{
	unsigned index = env->mask & MurmurHash2(symbol, strlen(symbol));

	for (Environment::Node* node = env->data[index]; node; node = node->next)
	{
		if (strcmp(symbol, node->symbol) == 0)
		{
			node->value = value;
			return;
		}
	}
	
	Environment::Node* node = new Environment::Node;
	node->symbol		= symbol;
	node->value			= value;
	node->next			= env->data[index];
	env->data[index]	= node;
}

void environment_set(Environment* env, const char* symbol, Cell* value)
{
	unsigned hash = MurmurHash2(symbol, strlen(symbol));
		
	do {
		
		unsigned index = hash & env->mask;

		for (Environment::Node* node = env->data[index]; node; node = node->next)
		{
			if (strcmp(symbol, node->symbol) == 0)
			{
				node->value = value;
				return;
			}
		}
		
		env = env->parent;
	
	} while (env);
		
	signal_error("No binding for %s in any scope.", symbol);
}

static Cell* eval(Environment* env, Cell* cell);

static Cell* type_q_helper(Environment* env, Cell* params, int type)
{
	Cell* obj = eval(env, car(params));
	return make_boolean(obj->type == type);
}

// Return the nth paramter to a function
// If the type does not match, then an error is signaled.
// n is indexed from 1 for the first parameter, 2 for the second.
static inline Cell* nth_param(Environment* env, Cell* params, int n, int type)
{
	for (int i=1; i<n; i++)
	{
		if (!(params = cdr(params)))
		{
			signal_error("Too few parameters passed (%d expected)", n);
		}
	}
	
	if (!params)
	{
		signal_error("Too few parameters passed (%d expected)", n);
	}
	
	Cell* result = eval(env, car(params));
	
	// todo: this message should include 'n'
	type_check(type, result->type);
	return result;
}

// 4.1.2
// Literal Expressions

// (quote <datum>) evaluates to <datum>. <Datum> may be any external
// representation of a Scheme object (see section 3.3). This notation is
// used to include literal constants in Scheme code.
static Cell* atom_quote(Environment* env, Cell* params)
{
	return car(params);
}

// 4.1.5 Conditionals

// (if <test> <consequent> <alternate>)  syntax
// (if <test> <consequent>)              syntax
// Syntax: ⟨Test⟩, ⟨consequent⟩, and ⟨alternate⟩ may be arbitrary
// expressions.
// Semantics: An if expression is evaluated as follows: first, ⟨test⟩ is
// evaluated. If it yields a true value (see section 6.3.1), then
// ⟨consequent⟩ is evaluated and its value(s) is(are) returned. Otherwise
// ⟨alternate⟩ is evaluated and its value(s) is(are) returned.
// If ⟨test⟩ yields a false value and no ⟨alternate⟩ is specified, then
// the result of the expression is unspecified.
static Cell* atom_if(Environment* env, Cell* params)
{
	Cell* test = eval(env, car(params));
	
	if (test->type == TYPE_BOOLEAN &&
		test->data.boolean == false)
	{
		Cell* alternate = cdr(cdr(params));
		if (alternate && car(alternate))
		{
			return eval(env, car(alternate));
		}
	
		// undefined, this is false though.
		return test;
	}
	
	// else eval consequent
	return eval(env, car(cdr(params)));
}

// 4.1.6. Assignments

// (set! ⟨variable⟩ ⟨expression⟩)
// ⟨Expression⟩ is evaluated, and the resulting value is stored in the
// location to which ⟨variable⟩ is bound. ⟨Variable⟩ must be bound either
// in some region enclosing the set! expression or at top level. The result
// of the set! expression is unspecified.

static Cell* atom_set_b(Environment* env, Cell* params)
{
	Cell* variable   = car(params);
	type_check(TYPE_SYMBOL, variable->type);
	Cell* expression = eval(env, car(cdr(params)));
	
	// @todo: seperate env->set and env->define
	environment_set(env, variable->data.symbol, expression);
	return expression;
}

// 4.2.1. Conditionals
// (cond <clause1> <clause2> ...) library syntax

// Syntax: Each <clause> should be of the form
// (⟨test⟩ ⟨expression1⟩ ...)
// where <test> is any expression.
// Alternatively, a <clause> may be of the form
// (⟨test⟩ => ⟨expression⟩)
// The last ⟨clause⟩ may be an “else clause,” which has the form
// (else ⟨expression1⟩ ⟨expression2⟩ ...)

// Semantics: A cond expression is evaluated by evaluating the <test>
// expressions of successive ⟨clause⟩s in order until one of them evaluates
// to a true value. When a ⟨test⟩ evaluates to a true value, then the
// remaining ⟨expression⟩s in its ⟨clause⟩ are evaluated in order, and the
// result(s) of the last ⟨expression⟩ in the ⟨clause⟩ is(are) returned as
// the result(s) of the entire cond expression. If the selected ⟨clause⟩
// contains only the ⟨test⟩ and no ⟨expression⟩s, then the value of the
// ⟨test⟩ is returned as the result.

// If the selected ⟨clause⟩ uses the => alternate form, then the
// ⟨expression⟩ is evaluated. Its value must be a procedure that accepts
// one argument; this procedure is then called on the value of the ⟨test⟩
// and the value(s) returned by this procedure is(are) returned by the cond
// expression. If all ⟨test⟩s evaluate to false values, and there is no
// else clause, then the result of the conditional expression is
// unspecified; if there is an else clause, then its ⟨expression⟩s are
// evaluated, and the value(s) of the last one is(are) returned.

static Cell* atom_cond(Environment* env, Cell* params)
{
	for(Cell* clause = params; clause; clause = cdr(clause))
	{
		Cell* test = car(clause);

		// @todo: make sure all symbols are stored in lowercase
		// @todo: assert that else is in the last place in the case
		// statement.
		Cell* t = car(test);
		if (t->type != TYPE_SYMBOL ||
			strcmp("else", t->data.string) != 0)
		{
			Cell* result = eval(env, t);	
			if (result->type == TYPE_BOOLEAN &&
				result->data.boolean == false)
			{
				continue;
			}
		}
		
		Cell* last_result = NULL;
		
		// @todo: assert there is at least one expression.
		for (Cell* expr = cdr(test); expr; expr = cdr(expr))
		{
			last_result = eval(env, car(expr));
		}
		
		return last_result;
	}
	
	// undefined.
	return make_boolean(false);
}

static Cell* atom_define(Environment* env, Cell* params)
{
	Cell* first  = car(params); // no eval

	Cell* variable = 0;
	Cell* value    = 0;
	
	switch(first->type)
	{
		case TYPE_SYMBOL:
		{
			variable	= first;
			value		= eval(env, car(cdr(params)));
			break;
		}
		
		case TYPE_PAIR:
		{
			// todo: handle dotted syntax
			variable		= car(first);
			Cell* formals	= cdr(first);
			Cell* body		= cdr(params);
			value = make_procedure(env, formals, body);
			break;
		}
		
		default:
		// todo: make this a syntax error.
		signal_error("symbol or pair expected as parameter 1 to define");
	}
	
	assert(variable && value);
	type_check(TYPE_SYMBOL, variable->type);
	environment_define(env, variable->data.symbol, value);
	// undefined result.
	return make_boolean(false);
}


static Cell* atom_error(Environment* env, Cell* params)
{
	Cell* message = eval(env, car(params));
	
	const char* str = "Error";
	
	// todo: symantics here
	if (message && message->type == TYPE_STRING)
	{
		str = message->data.string;
	}
	signal_error("%s", str);
	return NULL;
}

static Cell* atom_lambda(Environment* env, Cell* params)
{
	return make_procedure(env, car(params), cdr(params));
}

static Cell* atom_eqv_q(Environment* env, Cell* params)
{
	Cell* obj1 = eval(env, car(params));
	Cell* obj2 = eval(env, car(cdr(params)));
	
	bool result = false;
	
	const int type = obj1->type;
	
	if (type == obj2->type)
	{
		switch(type)
		{
			case TYPE_BOOLEAN:
			result = obj1->data.boolean == obj2->data.boolean;
			break;
			
			case TYPE_CHARACTER:
			result = obj1->data.character == obj2->data.character;
			break;
			
			case TYPE_SYMBOL:
			// @todo: intern symbols, use pointer equality
			result = 0 == strcmp(obj1->data.symbol, obj2->data.symbol);
			break;
			
			case TYPE_NUMBER:
			result = obj1->data.number == obj2->data.number;
			break;
			
			case TYPE_PAIR:
			case TYPE_VECTOR:
			case TYPE_STRING:
			result = obj1 == obj2;
			break;
			
			default:
			// unhandled case
			assert(0);
			break;
		}
	}
	
	return make_boolean(result);
}

// 4.2.3 Sequencing

// (begin <expression1> <expression> ...)	library syntax
// The <expression>s are evaluated sequentially from left to right, and
// the value(s) of the last ⟨expression⟩ is(are) re- turned. This expression
// type is used to sequence side ef- fects such as input and output.

static Cell* atom_begin(Environment* env, Cell* params)
{
	Cell* last = NULL;
	for (Cell* cell = params; cell; cell = cdr(cell))
	{
		// todo: tail recursion.
		last = eval(env, car(cell));
	}
	return last;
}

// 6.2.5 Numerical Operations


static Cell* plus_mul_helper(Environment* env,
							 Cell* params,
							 bool is_add,
							 double identity)
{
	double result = identity;
	
	for (Cell* z = params; z; z = cdr(z))
	{
		Cell* n = car(z);
		
		assert(n); // todo: trigger this assert and test
		
		Cell* value = eval(env, n);

		type_check(TYPE_NUMBER, value->type);
			
		if (is_add)
		{
			result += value->data.number;
		}
		else
		{
			result *= value->data.number;
		}
	}
	return make_number(result);
}

// (+ z1 ...)
// Return the sum or product of the arguments.
static Cell* atom_plus(Environment* env, Cell* params)
{
	return plus_mul_helper(env, params, true, 0);
}

// (* z1 ...)
// Return the product of the arguments.
static Cell* atom_mul(Environment* env, Cell* params)
{
	return plus_mul_helper(env, params, false, 1);
}


static Cell* atom_number_q(Environment* env, Cell* params)
{
	return type_q_helper(env, params, TYPE_NUMBER);
}

static Cell* atom_integer_q(Environment* env, Cell* params)
{
	Cell* obj = eval(env, car(params));
	
	bool integer =	obj->type == TYPE_NUMBER &&
					obj->data.number == (int)obj->data.number;
	
	return make_boolean(integer);
}

template <typename Compare>
static Cell* comparison_helper(Environment* env, Cell* params)
{
	int n = 2;

	Cell* a = nth_param(env, params, 1, TYPE_NUMBER);
	
	for (;;)
	{
		params = cdr(params);
		Cell* b = nth_param(env, params, 1, TYPE_NUMBER);
	
		double x = a->data.number;
		double y = b->data.number;
		if (!Compare::compare(x, y))
		{
			return make_boolean(false);
		}
	
		a = b;
		n++;
	
		if (!cdr(params))
		{
			break;
		}
	}

	return make_boolean(true);
};

struct Equal		{ static bool compare(double a, double b) { return a == b; } };
struct Less			{ static bool compare(double a, double b) { return a <  b; } };
struct Greater		{ static bool compare(double a, double b) { return a >  b; } };
struct LessEq		{ static bool compare(double a, double b) { return a <= b; } };
struct GreaterEq	{ static bool compare(double a, double b) { return a >= b; } };

static Cell* atom_comapre_equal(Environment* env, Cell* params)
{
	return comparison_helper<Equal>(env, params);
}

static Cell* atom_compare_less(Environment* env, Cell* params)
{
	return comparison_helper<Less>(env, params);
}

static Cell* atom_compare_greater(Environment* env, Cell* params)
{
	return comparison_helper<Greater>(env, params);
}

static Cell* atom_compare_less_equal(Environment* env, Cell* params)
{
	return comparison_helper<LessEq>(env, params);
}

static Cell* atom_compare_greater_equal(Environment* env, Cell* params)
{
	return comparison_helper<GreaterEq>(env, params);
}

// 6.3

// 6.3.1: Booleans

static Cell* atom_boolean_q(Environment* env, Cell* params)
{
	return type_q_helper(env, params, TYPE_BOOLEAN);	
}

static Cell* atom_not(Environment* env, Cell* params)
{
	Cell* obj = eval(env, car(params));
	bool is_truthy = obj->type != TYPE_BOOLEAN || obj->data.boolean;
	return make_boolean(!is_truthy);
}

// 6.3.2 Pairs and lists


static Cell* atom_pair_q(Environment* env, Cell* params)
{
	return type_q_helper(env, params, TYPE_PAIR);	
}

static Cell* atom_cons(Environment* env, Cell* params)
{
	Cell* first  = eval(env, car(params));
	Cell* second = eval(env, car(cdr(params)));
	return cons(first, second);
}

static Cell* atom_car(Environment* env, Cell* params)
{
	Cell* list = nth_param(env, params, 1, TYPE_PAIR);
	return car(list);
}

static Cell* atom_cdr(Environment* env, Cell* params)
{
	Cell* list = nth_param(env, params, 1, TYPE_PAIR);
	return cdr(list);
}

static Cell* set_car_cdr_helper(Environment* env, Cell* params, int is_car)
{
	// @todo: make an error here for constant lists	
	Cell* pair = nth_param(env, params, 1, TYPE_PAIR);
	Cell* obj  = eval(env, car(cdr(params)));
	
	if (is_car)
	{
		pair->data.pair.car = obj;
	}
	else
	{
		pair->data.pair.cdr = obj;	
	}
	
	// return value here is unspecified
	return pair;	
}

static Cell* atom_set_car_b(Environment* env, Cell* params)
{
	return set_car_cdr_helper(env, params, 1);
}

static Cell* atom_set_cdr_b(Environment* env, Cell* params)
{
	return set_car_cdr_helper(env, params, 0);
}

// Returns #t if obj is the empty list, otherwise returns #f.
static Cell* atom_null_q(Environment* env, Cell* params)
{
	Cell* obj = eval(env, car(params));
	return make_boolean(obj->type == TYPE_PAIR &&
						obj->data.pair.car == NULL &&
						obj->data.pair.cdr == NULL);
}

// (list? obj)
// Returns #t if obj is a list, otherwise returns #f. By definition, all
// lists have finite length and are terminated by the empty list.
static Cell* atom_list_q(Environment* env, Cell* params)
{
	Cell* obj = eval(env, car(params));
	
	if (obj->type == TYPE_PAIR)
	{
		if (Cell* rest = obj->data.pair.cdr)
		{
			// @todo: should this recurse O(N)
			// to see it list terminates?
			return make_boolean(rest->type == TYPE_PAIR);
		}
		
		return make_boolean(true);
	}
	
	return make_boolean(false);
}

// (list obj ...)
// Returns a newly allocated list of its arguments.
static Cell* atom_list(Environment* env, Cell* params)
{
	// @todo: use an empty list type here.
	Cell* result = cons(NULL, NULL);
	
	for (;;)
	{
		set_car(result, eval(env, car(params)));
		set_cdr(result, cons(NULL, NULL));
		params = cdr(params);
	}
	
	return result;
}

// (length list) Returns the length of list.
static Cell* atom_length(Environment* env, Cell* params)
{	
	int length = 1;

	for (Cell* list = eval(env, car(params)); list; list = list->data.pair.cdr)
	{
		type_check(TYPE_PAIR, list->type);
		length++;
	}
	
	return make_number((double)length);
}

// (append list ...)
// Returns a list consisting of the elements of the first list followed by
// the elements of the other lists.
static Cell* atom_append(Environment* env, Cell* params)
{
	Cell* result = cons(NULL, NULL);
	
	// for each list
	for (Cell* head = eval(env, car(params)); head; head = head->data.pair.cdr)
	{
		type_check(TYPE_PAIR, head->type);
		
		// append all of the items in the list
		for (Cell* obj = head; obj; obj = cdr(obj))
		{
			set_car(result, car(obj));
			set_cdr(result, cons(NULL, NULL));
			result = cdr(result);
		}
	}
	
	return result;
	
}

// a bunh of functions are missing here....

// 6.3.3. Symbols

// (symbol? obj)
// Returns #t if obj is a symbol, otherwise returns #f.
static Cell* atom_symbol_q(Environment* env, Cell* params)
{
	return type_q_helper(env, params, TYPE_SYMBOL);
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
static Cell* atom_symbol_to_string(Environment* env, Cell* params)
{
	const Cell* symbol = car(params);
	type_check(TYPE_SYMBOL, symbol->type);
	const char* data = symbol->data.symbol;
	size_t length = strlen(data);
	Cell* result = make_cell(TYPE_STRING);
	result->data.string = (char*)malloc(length+1);
	memcpy(result->data.string, data, length);
	result->data.string[length] = 0;
	return result;
}

// (string->symbol string) procedure
// Returns the symbol whose name is string. This procedure can create symbols
// with names containing special characters or letters in the non-standard
// case, but it is usually a bad idea to create such symbols because in some
// implementations of Scheme they cannot be read as themselves.
static Cell* atom_string_to_symbol(Environment* env, Cell* params)
{
	// todo: this is a copy-and-paste of symbol->string
	const Cell* symbol = car(params);
	type_check(TYPE_STRING, symbol->type);
	const char* data = symbol->data.string;
	size_t length = strlen(data);
	Cell* result = make_cell(TYPE_SYMBOL);
	
	char* scratch = (char*)malloc(length+1);
	memcpy(scratch, data, length);
	scratch[length] = 0;
	result->data.symbol = scratch;
	return result;
}


// 6.3.5 Strings

// (string? obj)	procedure
// Returns #t if obj is a string, otherwise returns #f.
static Cell* atom_string_q(Environment* env, Cell* params)
{
	return type_q_helper(env, params, TYPE_STRING);
}

// (make-string k)      procedure
// (make-string k char) procedure
// Make-string returns a newly allocated string of length k. If char is
// given, then all elements of the string are ini- tialized to char,
// otherwise the contents of the string are unspecified.
// ATOM: The contents are zero.
static Cell* atom_make_string(Environment* env, Cell* params)
{
	Cell* k = nth_param(env, params, 1, TYPE_NUMBER);

	char fill = 0;
	
	// todo: macro for getting the nth param
	if (Cell* rest = cdr(params))
	{
		rest = eval(env, car(rest));
		type_check(TYPE_CHARACTER, rest->type);
		fill = rest->data.character;
	}
	
	int length = (int)k->data.number;
	
	if (length < 0)
	{
		signal_error("positive integer length required");
	}
	
	Cell* result = make_cell(TYPE_STRING);
	result->data.string = (char*)malloc(length + 1);
	memset(result->data.string, fill, length);
	result->data.string[length] = 0;
	return result;
}



// (string char ...) library procedure
// Returns a newly allocated string composed of the arguments.
// todo

// (string-length string)	procedure
// Returns the number of characters in the given string.
static Cell* atom_string_length(Environment* env, Cell* params)
{
	Cell* string = nth_param(env, params, 1, TYPE_STRING);
	return make_number(strlen(string->data.string));
}

// (string-ref string k)	procedure
// k must be a valid index of string. String-ref returns character k of
// string using zero-origin indexing.
static Cell* atom_string_ref(Environment* env, Cell* params)
{
	Cell* string = nth_param(env, params, 1, TYPE_STRING);
	Cell* k      = nth_param(env, params, 2, TYPE_NUMBER);
	
	// todo: assert k is an integer - double to size_t
	int index = (int)k->data.number;
	
	// todo: watch this cast.
	if (index < 0 || index < (int)strlen(string->data.string))
	{
		signal_error("k is not a valid index of the given string");
	}
	
	return make_character(string->data.string[index]);
}


// (string-set! string k char)	procedure
// k must be a valid index of string.
// String-set! stores char in element k of string and returns an
// unspecified value.
static Cell* atom_string_set(Environment* env, Cell* params)
{
	Cell* string = nth_param(env, params, 1, TYPE_STRING);
	Cell* k      = nth_param(env, params, 2, TYPE_NUMBER);
	Cell* c      = nth_param(env, params, 3, TYPE_CHARACTER);
	
	// todo: assert k is integer - double to size_t
	int index = (int)k->data.number;
	char* data = string->data.string;
	
	// todo: watch this case.
	if (index < 0 || index >= (int)strlen(data))
	{
		signal_error("invalid string index");
	}
	
	data[index] = c->data.character;
	return string;
}
// 6.4. Control features

// (procedure? obj)
// Returns #t if obj is a procedure, otherwise returns #f.
static Cell* atom_procedure_q(Environment* env, Cell* params)
{
	return type_q_helper(env, params, TYPE_PROCEDURE);
}

// (apply proc arg1 ... args) procedure
// Proc must be a procedure and args must be a list. Calls proc with the
// elements of the list (append (list arg1 ...) args) as the actual arguments.
static Cell* atom_apply(Environment* env, Cell* params)
{
	Cell* proc = car(params);
	Cell* args = nth_param(env, params, 2, TYPE_PAIR);
	Cell* caller = cons(proc, args);
	return eval(env, caller);
}

// (write obj) library procedure
// (write obj port)	library procedure
// Writes a written representation of obj to the given port. Strings that
// appear in the written representation are en- closed in doublequotes,
// and within those strings backslash and doublequote characters are
// escaped by backslashes. Character objects are written using the 'hash-slash'
// notation.
// Write returns an unspecified value.
// The port argument may be omitted, in which case it defaults to the value
// returned by current-output-port.
static Cell* atom_write(Environment* env, Cell* params)
{
	// todo: handle ports
	Cell* obj = eval(env, car(params));
	print(obj);
	return obj; // unspecified
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
static Cell* atom_display(Environment* env, Cell* params)
{
	// @todo: handle port
	// @todo: this should produce human readable output
	// so no quotes on strings, etc.
	Cell* obj = eval(env, car(params));
	print(obj);
	return obj; // unspecified
}

// (newline)
// (newline port)
// Writes an end of line to port.
// Exactly how this is done differs from one operating system to another.
// Returns an unspecified value.
// The port argument may be omitted, in which case it defaults to the
// value returned by current-output-port.
static Cell* atom_newline(Environment* env, Cell* params)
{
	printf("\n");
	return params; // unspecified
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
static Cell* atom_load(Environment* env, Cell* params)
{
	Cell* filename = nth_param(env, params, 1, TYPE_STRING);
	atom_api_loadfile(env, filename->data.string);
	return make_boolean(true);	
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
static Cell* always_false(Environment* env, Cell* params)
{
	return make_boolean(false);
}


static Environment* create_environment(Environment* parent)
{
	Environment* env = (Environment*)malloc(sizeof(Environment));
	env->init(1, parent);
	return env;
}

void atom_api_loadfile(Environment* env, const char* filename)
{
	FILE* file = fopen(filename, "r");
	
	if (!file)
	{
		signal_error("Error opening file %s", filename);
	}
	
	fseek (file, 0, SEEK_END);
	size_t size = ftell (file);
	rewind(file);
	char* buffer = (char*) malloc(size+1);
	size_t read = fread(buffer, 1, size, file);
	buffer[read] = 0;
	fclose (file);
	
	{
		Input input;
		input.init(buffer);
		TokenList tokens;
		input.tokens = &tokens;

		tokens.init(1000);

		while (input.get())
		{
			read_token(input);
		}

		tokens.start_parse();

		for(;;)
		{
			Cell* cell = parse_datum(tokens);

			if (!cell)
			{
				break;
			}

			printf("> ");
			print(cell);
			const Cell* result = eval(env, cell);

			print(result);
		}

		tokens.destroy();
	}
	
	free(buffer);
}

static Cell* eval(Environment* env, Cell* cell)
{

tailcall:

	assert(cell);

	switch(cell->type)
	{
		// basic types will self evaluate
		case TYPE_BOOLEAN:
		case TYPE_NUMBER:
		case TYPE_STRING:
		case TYPE_CHARACTER:
			return cell;

		case TYPE_SYMBOL:
			return env->get(cell);

		case TYPE_PAIR:
		{
			Cell* symbol = car(cell);
			
			type_check(TYPE_SYMBOL, symbol->type);
			
			const Cell* function = env->get(symbol);
			
			if (!function)
			{
				signal_error("Undefined symbol '%s'", symbol->data.symbol);
			}

			if (function->type != TYPE_PROCEDURE)
			{
				signal_error("%s is not a function", symbol->data.symbol);
			}
			
			const Cell::Procedure* proc = &function->data.procedure;
			
			if (Function f = proc->function)
			{
				return f(env, cdr(cell));
			}
			
			Environment* new_env = create_environment(proc->env);
			
			Cell* params = cdr(cell);
			for (const Cell* formals = proc->formals; formals; formals = cdr(formals))
			{
				// @todo: formals should be NULL for (lambda () 'noop)
				if (car(formals))
				{
					assert(car(formals)->type == TYPE_SYMBOL);
					environment_define(new_env, car(formals)->data.symbol, car(params));
					params = cdr(params);
				}
			}
			
			Cell* last_result = NULL;
			
			for (const Cell* statement = proc->body; statement; statement = cdr(statement))
			{
				bool last = cdr(statement) == NULL;
				
				// tailcall optimization for the 
				// last statement in the list.
				if (last)
				{
					env  = new_env;
					cell = car(statement);
					goto tailcall;
				}
				
				last_result = eval(new_env, car(statement));
			}
			
			assert(false); // @todo - i don't think this can happen
			return last_result;
		}

		default:
			assert(false);
			return 0;
	}
}

static void add_builtin(Environment* env, const char* name, Function function)
{
	assert(env);
	assert(name);
	assert(function);
	
	Cell* cell = make_cell(TYPE_PROCEDURE);
	cell->data.procedure.function = function;
	environment_define(env, name, cell);
}

Environment* atom_api_open()
{
	Environment* env = create_environment(NULL);
	
	add_builtin(env, "if",			atom_if);
	add_builtin(env, "quote",		atom_quote);
	add_builtin(env, "define",		atom_define);
	add_builtin(env, "set!",		atom_set_b);
	add_builtin(env, "cond",		atom_cond);
	add_builtin(env, "eqv?",		atom_eqv_q);
	add_builtin(env, "begin",      	atom_begin);
	add_builtin(env, "number?",    	atom_number_q);
	add_builtin(env, "complex?",   	always_false);
	add_builtin(env, "real?",      	atom_number_q);
	add_builtin(env, "rational?",  	always_false);
	add_builtin(env, "integer?",   	atom_integer_q);
	add_builtin(env, "+",		   	atom_plus);
	add_builtin(env, "*",		   	atom_mul);
	add_builtin(env, "=",			atom_comapre_equal);
	add_builtin(env, "<",			atom_compare_less);
	add_builtin(env, ">",			atom_compare_greater);
	add_builtin(env, "<=",			atom_compare_less_equal);
	add_builtin(env, ">=",			atom_compare_greater_equal);
	add_builtin(env, "not",		   	atom_not);
	add_builtin(env, "boolean?",   	atom_boolean_q);
	add_builtin(env, "string?",	   		atom_string_q);
	add_builtin(env, "make-string",		atom_make_string);
	add_builtin(env, "string-length",	atom_string_length);
	add_builtin(env, "string-ref",	   	atom_string_ref);
	add_builtin(env, "string-set",	   	atom_string_set);
	add_builtin(env, "pair?",      		atom_pair_q);
	add_builtin(env, "cons",       		atom_cons);
	add_builtin(env, "car",        		atom_car);
	add_builtin(env, "cdr",        		atom_cdr);
	add_builtin(env, "set-car!",   		atom_set_car_b);
	add_builtin(env, "set-cdr!",   		atom_set_cdr_b);
	add_builtin(env, "null?",      		atom_null_q);
	add_builtin(env, "list?",      		atom_list_q);
	add_builtin(env, "list",       		atom_list);
	add_builtin(env, "length",     		atom_length);
	add_builtin(env, "append",     		atom_append);
	add_builtin(env, "symbol?",    		atom_symbol_q);
	add_builtin(env, "symbol->string",	atom_symbol_to_string);
	add_builtin(env, "string->symbol",	atom_string_to_symbol);
	add_builtin(env, "procedure?", atom_procedure_q);
	add_builtin(env, "apply",	   atom_apply);
	add_builtin(env, "lambda",     atom_lambda);
	add_builtin(env, "write",      atom_write);
	add_builtin(env, "display",	   atom_display);
	add_builtin(env, "newline",	   atom_newline);
	add_builtin(env, "error",	   atom_error);
	add_builtin(env, "load",	   atom_load);
	
	return env;	
}


void atom_api_close(Environment* env)
{	
}

int main (int argc, char * const argv[])
{
	Environment* atom = atom_api_open();

	const char* filename = (argc == 1) ? "/Users/marcomorain/dev/scheme/test/test.scm" : argv[1];
	
	atom_api_loadfile(atom, filename);
	
	atom_api_close(atom);

	system ("pause");
	return 0;
}
