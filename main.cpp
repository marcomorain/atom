#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <string>
#include <cassert>

struct Token
{
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

	void print(void) const
	{
		switch(type)
		{
		case TOKEN_NUMBER:
			printf("Token TOKEN_NUMBER %lg\n", data.number);
			break;

#define PRINT_CASE(id) case id: printf("Token %s\n", #id); break
		PRINT_CASE(TOKEN_IDENTIFIER);
		PRINT_CASE(TOKEN_BOOLEAN);
		PRINT_CASE(TOKEN_CHARACTER);
		PRINT_CASE(TOKEN_STRING);
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

	void add_basic(Token::Type t)
	{
		tokens[next].type = t;
		tokens[next].print();
		next++;
	}

public:
	Token*	tokens;
	int		next;
	int		length;



	void add_backtick()
	{
		add_basic(Token::TOKEN_BACKTICK);
	}

	void add_list_start()
	{
		add_basic(Token::TOKEN_LIST_START);
	}

	void add_list_end()
	{
		add_basic(Token::TOKEN_LIST_END);
	}

	void add_quote()
	{
		add_basic(Token::TOKEN_QUOTE);
	}

	void add_dot()
	{
		add_basic(Token::TOKEN_DOT);
	}

	void add_identifier(const char* value)
	{
		// todo: dynamic string allocation
		tokens[next].type = Token::TOKEN_IDENTIFIER;
		tokens[next].data.identifier = value;
		tokens[next].print();
		next++;
	}

	void add_string(const char* value)
	{
		// todo: dynamic string allocation
		tokens[next].type = Token::TOKEN_STRING;
		tokens[next].data.string = value;
		tokens[next].print();
		next++;
	}

	void add_number(double number)
	{
		tokens[next].type = Token::TOKEN_NUMBER;
		tokens[next].data.number = number;
		tokens[next].print();
		next++;
	}
	
	void add_character(char value)
	{
		tokens[next].type = Token::TOKEN_CHARACTER;
		tokens[next].data.character = value;
		tokens[next].print();
		next++;
	}

	void add_boolean(bool value)
	{
		tokens[next].type = Token::TOKEN_BOOLEAN;
		tokens[next].data.boolean= value;
		tokens[next].print();
		next++;
	}

	void init(int size)
	{
		next   = 0;
		length = size;
		tokens = (Token*)malloc(size*sizeof(Token));
	}

	void destroy()
	{
		free(tokens);
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
			input.tokens->add_string("some string");
			return;
		}

		if (c == '\\'){
			c = input.next();
			if (c == '"') continue;
			if (c == '\\') continue;
			exit(-1); // malformed string.
		}

		if (isprint(c)) continue;

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
	if (is_initial(input.get()))
	{

		for (;;)
		{
			char c = input.next();
			if (is_delimeter(c)) break;
			if (!is_subsequent(c))
			{
				// malformed identifier
				exit(-1);
			}
		}

		input.tokens->add_identifier("some ident");
		return;
	}
	else if (is_peculiar_identifier(input.get()))
	{
		input.next();
		input.tokens->add_identifier("some peculiar ident");
		return;
	}

	// not at identifier.
	exit(-1);
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
