#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <string>
#include <cassert>

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


Token* parse_datum(Token* tokens);

Token* parse_vector(Token* tokens)
{
	return 0;
}

Token* parse_abreviation(Token* tokens)
{
	if(tokens->type == TOKEN_QUOTE)
	{
		tokens++;
		return parse_datum(tokens);
	}

	if (tokens->type == TOKEN_BACKTICK)
	{
		tokens++;
		return parse_datum(tokens);
	}

	if(tokens->type == TOKEN_COMMA)
	{
		tokens++;
		return parse_datum(tokens);
	}

	if (tokens->type == TOKEN_COMMA_AT)
	{
		tokens++;
		return parse_datum(tokens);
	}

	return NULL;
}

Token* parse_list(Token* tokens)
{
	if (tokens->type != TOKEN_LIST_START)
	{
		return parse_abreviation(tokens);
	}

	tokens++;

	for (;;)
	{
		if (tokens->type == TOKEN_DOT)
		{
			tokens++;
			tokens = parse_datum(tokens);

			if (tokens->type != TOKEN_LIST_END)
			{
				// error
			}

			tokens++;
			break;
		}
		else if (tokens->type == TOKEN_LIST_END)
		{
			tokens++;
			break; // success
		}
		
		Token* t = parse_datum(tokens);

		if (!t)
		{
			int x = 1; x++;
			// error
		}

		tokens = t;
	}

	// success, allocate a list
	return tokens;
}

Token* parse_compound_datum(Token* tokens)
{
	if (Token* t = parse_list(tokens))
	{
		return t;
	}

	return parse_vector(tokens);
}

Token* parse_simple_datum(Token* tokens)
{
	switch (tokens->type)
	{
		case TOKEN_NUMBER:
		case TOKEN_IDENTIFIER:
		case TOKEN_BOOLEAN:
		case TOKEN_CHARACTER:
		case TOKEN_STRING:
		{
			tokens++;
			return tokens;
		}

		default:
			return NULL;
	}
}

Token* parse_datum(Token* tokens)
{
	if (Token* t = parse_simple_datum(tokens))
	{
		return t;
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

	parse_datum(tokens.tokens);

	tokens.destroy();
}

int main (int argc, char * const argv[])
{

	FILE* file = fopen("input.txt", "r");
	assert(file);
	fseek (file, 0, SEEK_END);
    size_t size = ftell (file);
	rewind(file);
	char* buffer = (char*) malloc(size+1);
	size_t read = fread(buffer, 1, size, file);
	buffer[read] = 0;
    fclose (file);
	printf("%s\n", buffer);

	lexer(buffer);

	free(buffer);
	system ("pause");
	return 0;
}
