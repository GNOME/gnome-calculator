#ifndef LEXER_H
#define LEXER_H

#include "prelexer.h"

/* Structure to hold single token. */
typedef struct
{
    gchar* string;			/* Poniter to local copy of token string. */
    guint start_index;			/* Start index in original stream. */
    guint end_index;			/* End index in original stream. */
    LexerTokenType token_type;		/* Type of token. */
} LexerToken;

/* Structure to hold lexer state and all the tokens. */
typedef struct
{
    PreLexerState *prelexer;		/* Pre-lexer state. Pre-lexer is part of lexer. */
    LexerToken *tokens;			/* Pointer to the dynamic array of LexerTokens. */
    guint token_count;			/* Count of tokens in array. */
    guint next_token;			/* Index of next, to be sent, token. */
    struct parser_state *parent;	/* Pointer to the parent parser. */
} LexerState;

/* Create a new LexerState object and fill the dynamic array with tokens. */
LexerState* l_create_lexer(const gchar*, struct parser_state*);

/* Destroy LexerState object and free up space. */
void l_destroy_lexer(LexerState*);

/* Tokanize complete string. */
void l_insert_all_tokens(LexerState*);

/* Return next, to be sent, token. */
LexerToken* l_get_next_token(LexerState*);

/* Roll back one token. */
void l_roll_back(LexerState*);

#endif /* LEXER_H */
