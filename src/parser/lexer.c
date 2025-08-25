#include "fortbite.h"
#include <ctype.h>
#include <string.h>

typedef struct {
    const char* input;
    size_t position;
    size_t length;
    char current_char;
} fortbite_lexer_t;

static void lexer_advance(fortbite_lexer_t* lexer) {
    if (lexer->position < lexer->length) {
        lexer->position++;
        lexer->current_char = (lexer->position < lexer->length) 
            ? lexer->input[lexer->position] 
            : '\0';
    }
}

static void lexer_skip_whitespace(fortbite_lexer_t* lexer) {
    while (lexer->current_char != '\0' && isspace(lexer->current_char)) {
        lexer_advance(lexer);
    }
}

static fortbite_token_t lexer_number(fortbite_lexer_t* lexer) {
    fortbite_token_t token;
    size_t start = lexer->position;
    bool has_dot = false;
    bool has_exp = false;
    
    while (lexer->current_char != '\0' && 
           (isdigit(lexer->current_char) || 
            lexer->current_char == '.' ||
            lexer->current_char == 'e' ||
            lexer->current_char == 'E' ||
            lexer->current_char == '+' ||
            lexer->current_char == '-')) {
        
        if (lexer->current_char == '.') {
            if (has_dot || has_exp) break;
            has_dot = true;
        } else if (lexer->current_char == 'e' || lexer->current_char == 'E') {
            if (has_exp) break;
            has_exp = true;
            lexer_advance(lexer);
            if (lexer->current_char == '+' || lexer->current_char == '-') {
                lexer_advance(lexer);
            }
            continue;
        } else if (lexer->current_char == '+' || lexer->current_char == '-') {
            if (!has_exp) break;
        }
        
        lexer_advance(lexer);
    }
    
    size_t length = lexer->position - start;
    token.type = TOKEN_NUMBER;
    token.value = fortbite_malloc(length + 1);
    strncpy(token.value, &lexer->input[start], length);
    token.value[length] = '\0';
    token.length = length;
    token.position = start;
    
    return token;
}

static fortbite_token_t lexer_identifier(fortbite_lexer_t* lexer) {
    fortbite_token_t token;
    size_t start = lexer->position;
    
    while (lexer->current_char != '\0' && 
           (isalnum(lexer->current_char) || lexer->current_char == '_')) {
        lexer_advance(lexer);
    }
    
    size_t length = lexer->position - start;
    token.type = TOKEN_IDENTIFIER;
    token.value = fortbite_malloc(length + 1);
    strncpy(token.value, &lexer->input[start], length);
    token.value[length] = '\0';
    token.length = length;
    token.position = start;
    
    return token;
}

static fortbite_token_t lexer_operator(fortbite_lexer_t* lexer) {
    fortbite_token_t token;
    size_t start = lexer->position;
    
    switch (lexer->current_char) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '^':
        case '%':
            lexer_advance(lexer);
            break;
        case ':':
            lexer_advance(lexer);
            if (lexer->current_char == '=') {
                lexer_advance(lexer);
                token.type = TOKEN_ASSIGN;
            } else if (lexer->current_char == ':') {
                lexer_advance(lexer);
                token.type = TOKEN_PRECISION;
            } else {
                token.type = TOKEN_ERROR;
            }
            break;
        case '=':
            lexer_advance(lexer);
            if (lexer->current_char == '=') {
                lexer_advance(lexer);
            }
            break;
        case '<':
        case '>':
        case '!':
            lexer_advance(lexer);
            if (lexer->current_char == '=') {
                lexer_advance(lexer);
            }
            break;
        default:
            token.type = TOKEN_ERROR;
            lexer_advance(lexer);
            break;
    }
    
    if (token.type != TOKEN_ASSIGN && token.type != TOKEN_PRECISION && token.type != TOKEN_ERROR) {
        token.type = TOKEN_OPERATOR;
    }
    
    size_t length = lexer->position - start;
    token.value = fortbite_malloc(length + 1);
    strncpy(token.value, &lexer->input[start], length);
    token.value[length] = '\0';
    token.length = length;
    token.position = start;
    
    return token;
}

static fortbite_token_t lexer_next_token(fortbite_lexer_t* lexer) {
    fortbite_token_t token;
    
    lexer_skip_whitespace(lexer);
    
    if (lexer->current_char == '\0') {
        token.type = TOKEN_EOF;
        token.value = NULL;
        token.length = 0;
        token.position = lexer->position;
        return token;
    }
    
    if (isdigit(lexer->current_char)) {
        return lexer_number(lexer);
    }
    
    if (isalpha(lexer->current_char) || lexer->current_char == '_') {
        return lexer_identifier(lexer);
    }
    
    token.position = lexer->position;
    
    switch (lexer->current_char) {
        case '(':
            token.type = TOKEN_LPAREN;
            lexer_advance(lexer);
            break;
        case ')':
            token.type = TOKEN_RPAREN;
            lexer_advance(lexer);
            break;
        case '[':
            token.type = TOKEN_LBRACKET;
            lexer_advance(lexer);
            break;
        case ']':
            token.type = TOKEN_RBRACKET;
            lexer_advance(lexer);
            break;
        case ';':
            token.type = TOKEN_SEMICOLON;
            lexer_advance(lexer);
            break;
        case ',':
            token.type = TOKEN_COMMA;
            lexer_advance(lexer);
            break;
        case 'i':
            if (lexer->position + 1 < lexer->length && 
                !isalnum(lexer->input[lexer->position + 1])) {
                token.type = TOKEN_IDENTIFIER;
                lexer_advance(lexer);
            } else {
                return lexer_identifier(lexer);
            }
            break;
        default:
            return lexer_operator(lexer);
    }
    
    if (token.type != TOKEN_IDENTIFIER) {
        token.value = fortbite_malloc(2);
        token.value[0] = lexer->input[token.position];
        token.value[1] = '\0';
        token.length = 1;
    }
    
    return token;
}

fortbite_token_t* fortbite_tokenize(const char* input) {
    if (!input) return NULL;
    
    fortbite_lexer_t lexer;
    lexer.input = input;
    lexer.position = 0;
    lexer.length = strlen(input);
    lexer.current_char = (lexer.length > 0) ? input[0] : '\0';
    
    size_t token_capacity = 16;
    size_t token_count = 0;
    fortbite_token_t* tokens = fortbite_malloc(token_capacity * sizeof(fortbite_token_t));
    
    fortbite_token_t token;
    do {
        token = lexer_next_token(&lexer);
        
        if (token_count >= token_capacity) {
            token_capacity *= 2;
            tokens = fortbite_realloc(tokens, token_capacity * sizeof(fortbite_token_t));
        }
        
        tokens[token_count++] = token;
    } while (token.type != TOKEN_EOF && token.type != TOKEN_ERROR);
    
    return tokens;
}

void fortbite_tokens_free(fortbite_token_t* tokens) {
    if (!tokens) return;
    
    for (size_t i = 0; tokens[i].type != TOKEN_EOF; i++) {
        if (tokens[i].value) {
            fortbite_free(tokens[i].value);
        }
    }
    
    fortbite_free(tokens);
}