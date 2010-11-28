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


enum
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

struct Environment;
struct Cell;

typedef Cell* (*Procedure) (Environment* env, Cell* params);

struct Cell
{
	struct Pair
	{
		Cell* car;
		Cell* cdr;
	};

	union Data
	{
		bool         boolean;
		char         character;
		double       number;
		const char*  string;
		Pair         pair;
		const char*  symbol;
		Procedure    procedure;
	};
	
	int  type;
	Data data;
};

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

		if (cell->data.pair.cdr)
		{
			printf(" ");
			print_rec(cell->data.pair.cdr, 0);
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

static Cell* make_boolean(bool value)
{
	Cell* result = (Cell*)malloc(sizeof(Cell));
	result->type = TYPE_BOOLEAN;
	result->data.boolean = value;
	return result;
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
	Cell* cell = new Cell;
	cell->type = TYPE_PAIR;
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
		const char*	string;
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

bool is_letter(char c)
{
	return !!isalpha(c);
}

bool is_initial(char c)
{
	return is_letter(c) || is_special_initial(c);
}

bool is_delimeter(char c)
{
	switch (c)
	{
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
				signal_error("malformed identifier");
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
		signal_error("not at identifier");
	}

	input.tokens->add_identifier();

}

void read_token(Input& input)
{
	skip_whitespace(input);
	
	char c = input.get();
	
	switch(c){
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
		tokens.skip();
		return parse_datum(tokens);
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
			Cell* cell = new Cell;
			cell->type = TYPE_BOOLEAN;
			cell->data.boolean = t->data.boolean;
			tokens.skip();
			return cell;
		}

		case TOKEN_CHARACTER:
		{
			Cell* cell = new Cell;
			cell->type = TYPE_CHARACTER;
			cell->data.character = t->data.character;
			tokens.skip();
			return cell;
		}

		case TOKEN_NUMBER:
		{
			Cell* cell = new Cell;
			cell->type = TYPE_NUMBER;
			cell->data.number = t->data.number;
			tokens.skip();
			return cell;
		}

		case TOKEN_IDENTIFIER:
		{
			Cell* cell = new Cell;
			cell->type = TYPE_SYMBOL;
			cell->data.symbol = t->data.identifier;
			tokens.skip();
			return cell;
		}
		
		case TOKEN_STRING:
		{
			Cell* cell = new Cell;
			cell->type = TYPE_STRING;
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

	Node**		data;
	unsigned	mask;


	void init(int size)
	{
		assert(power_of_two(size));
		mask = size-1;
		const size_t num_bytes = size * sizeof(Node*);
		data = (Node**)malloc(num_bytes);
		memset(data, 0, num_bytes);
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

		return NULL;
	}

	void set(const char* symbol, Cell* value)
	{
		unsigned hash = mask & MurmurHash2(symbol, strlen(symbol));

		for (Node* node = data[hash]; node; node = node->next)
		{
			if (strcmp(symbol, node->symbol) == 0)
			{
				node->value = value;
				return;
			}
		}

		Node* node = new Node;
		node->symbol = symbol;
		node->value  = value;
		node->next   = data[hash];
		data[hash] = node;
	}

};

static Cell* eval(Environment* env, Cell* cell);

static Cell* type_q_helper(Environment* env, Cell* params, int type)
{
	Cell* obj = eval(env, car(params));
	return make_boolean(obj->type == type);
}

static Cell* atom_define(Environment* env, Cell* params)
{
	Cell* first  = car(params);

	if (first->type != TYPE_SYMBOL)
	{
		signal_error("symbol expected instead of ...");
	}

	Cell* second = car(cdr(params));
	env->set(first->data.symbol, eval(env, second));
	return NULL;
}

static Cell* atom_cons(Environment* env, Cell* params)
{
	Cell* first  = eval(env, car(params));
	Cell* second = eval(env, car(cdr(params)));
	return cons(first, second);
}

static Cell* atom_car(Environment* env, Cell* params)
{
	Cell* list = eval(env, car(params));

	if (list->type != TYPE_PAIR)
	{
		signal_error("list expected in call to car");
	}

	return car(list);
}

static Cell* atom_cdr(Environment* env, Cell* params)
{
	Cell* list = eval(env, car(params));

	if (list->type != TYPE_PAIR)
	{
		signal_error("list expected in call to cdr");
	}

	return cdr(list);
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

static Cell* always_false(Environment* env, Cell* params)
{
	return make_boolean(false);
}


static Environment* create_environment(void)
{
	Environment* env = (Environment*)malloc(sizeof(Environment));
	env->init(16);
	return env;
}

static Cell* eval(Environment* env, Cell* cell)
{
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
			const Cell* function = env->get(symbol);
			
			if (!function)
			{
				signal_error("Undefined symbol");
			}

			if (function->type == TYPE_PROCEDURE)
			{
				return function->data.procedure(env, cdr(cell));
			}
			return 0;
		}


		default:
			assert(false);
			return 0;
	}
}

static void add_builtin(Environment* env, const char* name, Procedure procedure)
{
	assert(env);
	assert(name);
	assert(procedure);
	Cell* cell = new Cell;
	cell->type = TYPE_PROCEDURE;
	cell->data.procedure = procedure;
	env->set(name, cell);
}

void lexer(const char* data)
{
	Input input;
	input.init(data);
	TokenList tokens;
	input.tokens = &tokens;

	tokens.init(1000);

	Environment* env = create_environment();

	add_builtin(env, "define",    atom_define);
	add_builtin(env, "cons",      atom_cons);
	add_builtin(env, "car",       atom_car);
	add_builtin(env, "cdr",       atom_cdr);
	add_builtin(env, "eqv?",      atom_eqv_q);
	add_builtin(env, "number?",   atom_number_q);
	add_builtin(env, "complex?",  always_false);
	add_builtin(env, "real?",     atom_number_q);
	add_builtin(env, "rational?", always_false);
	add_builtin(env, "integer?",  atom_integer_q);
	add_builtin(env, "not",		  atom_not);
	add_builtin(env, "boolean?",  atom_boolean_q);
	add_builtin(env, "pair?",     atom_pair_q);

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
	free(env);
}

char* file_to_string(const char* filename)
{
	FILE* file = fopen(filename, "r");
	assert(file);
	fseek (file, 0, SEEK_END);
	size_t size = ftell (file);
	rewind(file);
	char* buffer = (char*) malloc(size+1);
	size_t read = fread(buffer, 1, size, file);
	buffer[read] = 0;
	fclose (file);
	return buffer;
}

int main (int argc, char * const argv[])
{
	const char* filename = (argc == 1) ? "awy.scheme" : argv[1];

	char* input = file_to_string(filename);
	//printf("%s\n", input);
	lexer(input);
	free(input);
	system ("pause");
	return 0;
}
