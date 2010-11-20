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
	
	Type type;
	
	Token(Type type) : type(type)
	{
		printf("Token type %d\n", (int)type);
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
			case ' ':
			case '\t':
			printf("skipping space/tab\n");
			continue;
			
			case ';':
			printf("skipping comment\n");
			for (char d = input.next(); d && d != '\n'; d = input.next())
			{
			}
			
			default:
			break;
		}
	}
}

Token read_token(Input& input)
{
	skip_whitespace(input);
	
	const char c = input.get();
	
	switch(c){
		case '(':  return Token(Token::TOKEN_LIST_START);
		case ')':  return Token(Token::TOKEN_LIST_END);
		case '\'': return Token(Token::TOKEN_QUOTE);
		case '`':  return Token(Token::TOKEN_BACKTICK);
		case '.':  return Token(Token::TOKEN_DOT);
		
		default:
		printf("unknown character\n");
		break;
	}
	
	
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
		printf("reading token\n");
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
	fread(buffer, size, 1, file);
	buffer[size] = 0;
    fclose (file);
	parse_program(buffer);
	free(buffer);
	return 0;
}
