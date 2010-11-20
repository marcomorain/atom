#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <string>
#include <cassert>


struct Token
{
	enum Type
	{
		TOKEN_EOF,
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
#define PRINT_CASE(id) case id: printf("Token %s\n", #id); break
		PRINT_CASE(TOKEN_EOF);
		PRINT_CASE(TOKEN_IDENTIFIER);
		PRINT_CASE(TOKEN_BOOLEAN);
		PRINT_CASE(TOKEN_NUMBER);
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
	
	Type type;
	
	Token(Type type) : type(type)
	{
		print();
	};
};


struct Input
{
	//unsigned line;
	//unsigned column;
	const char* data;
	
	char get(void)  const
	{
		return *data;
	};
	
	char next(void)
	{
		assert(*data);
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
			printf("skipping space/tab/newlines\n");
			continue;
			
			case ';':
			printf("skipping comment\n");
			for (char d = input.next(); d && d != '\n'; d = input.next())
			{
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


Token read_character(Input& input)
{
	char c = input.get();
	switch(c)
	{
		case 's':
		if (input.next() == 'p'){
			if (input.next() != 'a') exit(-1);
			if (input.next() != 'c') exit(-1);
			if (input.next() != 'e') exit(-1);
			return Token(Token::TOKEN_CHARACTER); // space
		}
		else goto success;

		case 'n':
			if (input.next() == 'e'){
				if (input.next() != 'w') exit(-1);
				if (input.next() != 'l') exit(-1);
				if (input.next() != 'i') exit(-1);
				if (input.next() != 'n') exit(-1);
				if (input.next() != 'e') exit(-1);
				return Token(Token::TOKEN_CHARACTER); // newline
			}
			else goto success;

		default: goto success;
	}

success:

	if (is_delimeter(input.next()))
	{
		return Token(Token::TOKEN_CHARACTER); // character
	}
	else
	{
		exit(-1);
	}
}

Token read_number(Input& input)
{
	for (;;)
	{
		if (is_digit(input.get()))
		{
			input.next();
		}
		else
		{
			return Token(Token::TOKEN_NUMBER);
		}
	}
}



Token read_string(Input& input)
{
	assert(input.get() == '"');

	for (;;)
	{
		char c = input.next();

		if (c == '"'){
			return Token(Token::TOKEN_STRING);
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

Token read_identifier(Input& input)
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

		return Token(Token::TOKEN_IDENTIFIER);
	}
	else if (is_peculiar_identifier(input.get()))
	{
		input.next();
		return Token(Token::TOKEN_IDENTIFIER);
	}

	// not at identifier.
	exit(-1);
}

Token read_token(Input& input)
{
	skip_whitespace(input);
	
	char c = input.get();
	
	switch(c){
		case '(':  input.next(); return Token(Token::TOKEN_LIST_START);
		case ')':  input.next(); return Token(Token::TOKEN_LIST_END);
		case '\'': input.next(); return Token(Token::TOKEN_QUOTE);
		case '`':  input.next(); return Token(Token::TOKEN_BACKTICK);
		case '.':  input.next(); return Token(Token::TOKEN_DOT);

		case '#':
		{
			c = input.next();
			switch(c)
			{
				case 't':  input.next(); return Token(Token::TOKEN_BOOLEAN);
				case 'f':  input.next(); return Token(Token::TOKEN_BOOLEAN);
				case '\\': input.next(); return read_character(input);
			}
		}

		case '"':
		{
			return read_string(input);
		}

		case 0: break;
		
		default:
		{
			if (is_digit(c))
			{
				return read_number(input);
			}

			return read_identifier(input);
		}
	}

	return Token(Token::TOKEN_EOF);
}

const char* parse_boolean(const char* data, bool& value)
{
	if (*data != '#') return NULL;
	data++;
	
	switch (*data)
	{
		case 'f':
		case 't':
		value = *data == 't';
		return data;
		
		default:
		return NULL;
	}
}


using namespace std;

void parse_program(const char* data)
{
	Input input;
	input.data = data;
	
	while (input.get())
	{
		Token token = read_token(input);
	}
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
	parse_program(buffer);
	free(buffer);
	system ("pause");
	return 0;
}
