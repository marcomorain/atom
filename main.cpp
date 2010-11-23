#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <string>
#include <cassert>

enum
{
	TYPE_BOOLEAN,
	TYPE_CHARACTER,
	TYPE_NUMBER,
	TYPE_STRING,
	TYPE_LIST,
	TYPE_VECTOR,
	TYPE_SYMBOL
};

struct Cell
{
	struct List
	{
		Cell* car;
		Cell* cdr;
	};

	union Data
	{
		bool   boolean;
		char   character;
		double number;
		const char*  string;
		List   list;
		const char*  symbol;
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

static void print_rec(const Cell* cell)
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
		printf("%c", cell->data.character);
		break;

	case TYPE_STRING:
		printf("\"%s\"", cell->data.string);
		break;

	case TYPE_SYMBOL:
		printf("%s", cell->data.symbol);
		break;

	case TYPE_LIST:
		printf("(");
		print_rec(cell->data.list.car);
		printf(" ");
		print_rec(cell->data.list.cdr);
		printf(")");
		break;
	}
}

static void print(const Cell* cell)
{
	print_rec(cell);
	printf("\n");
}

static void set_car(Cell* list, Cell* car)
{
	assert(list->type == TYPE_LIST);
	list->data.list.car = car;
}

static void set_cdr(Cell* list, Cell* cdr)
{
	assert(list->type == TYPE_LIST);
	list->data.list.cdr = cdr;
}

static Cell* cons(Cell* car, Cell* cdr)
{
	Cell* cell = new Cell;
	cell->type = TYPE_LIST;
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
			printf("Token TOKEN_NUMBER %lg\n", data.number);
			break;

		case TOKEN_IDENTIFIER:
			printf("Token TOKEN_IDENTIFIER \"%s\"\n", data.identifier);
			break;

		case TOKEN_STRING:
			printf("Token TOKEN_STRING \"%s\"\n", data.string);
			break;

#define PRINT_CASE(id) case id: printf("Token %s\n", #id); break

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
			input.next(); // skip the newline
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


bool is_subsequent(char c)
{
	return is_initial(c) || is_digit(c) || is_special_subsequent(c);
}


void read_character(Input& input)
{
	char c = input.get();
	switch(c)
	{
		case 's':
		if (input.next() == 'p'){
			if (input.next() != 'a') exit(-1);
			if (input.next() != 'c') exit(-1);
			if (input.next() != 'e') exit(-1);
			input.tokens->add_character(' ');
			return;
		}
		else goto success;

		case 'n':
			if (input.next() == 'e'){
				if (input.next() != 'w') exit(-1);
				if (input.next() != 'l') exit(-1);
				if (input.next() != 'i') exit(-1);
				if (input.next() != 'n') exit(-1);
				if (input.next() != 'e') exit(-1);
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
	
	exit(-1);
}

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
			exit(-1); // malformed string.
		}

		if (isprint(c))
		{
			input.tokens->buffer_push(c);
			continue;
		}

		exit(-1); // strange character in string
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
				// malformed identifier
				exit(-1);
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
		// not at identifier.
		exit(-1);
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
	if (tokens.peek()->type != TOKEN_LIST_START)
	{
		return parse_abreviation(tokens);
	}

	// skip the start list token
	tokens.skip();

	Cell* list = cons(NULL, NULL);

	Cell* head = list;

	for (;;)
	{
		if (tokens.peek()->type == TOKEN_DOT)
		{
			tokens.skip();
			Cell* cell = parse_datum(tokens);

			if (!cell)
			{
				// error
				exit(-1);
			}

			set_cdr(list, cell);

			if (tokens.peek()->type != TOKEN_LIST_END)
			{
				// error
				exit(-1);
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
			int x = 1; x++;
			exit(-1);
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

void lexer(const char* data)
{
	Input input;
	input.init(data);
	TokenList tokens;
	input.tokens = &tokens;

	tokens.init(1000);
	
	while (input.get())
	{
		read_token(input);
	}

	tokens.next = 0;

	for(;;)
	{
		Cell* cell = parse_datum(tokens);

		if (!cell)
		{
			break;
		}
		print(cell);
	}

	tokens.destroy();
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
	char* input = file_to_string("input.txt");
	printf("%s\n", input);
	lexer(input);
	free(input);
	system ("pause");
	return 0;
}
