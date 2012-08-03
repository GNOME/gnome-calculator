#ifndef PARSER_H
#define PARSER_H

#include <lexer.h>

#include "mp-equation.h"
#include "mp.h"

/* Operator Associativity. */
typedef enum
{
    LEFT_ASSOCIATIVE,
    RIGHT_ASSOCIATIVE
} Associativity;

/* Operator Precedence. */
typedef enum
{
    P_Unknown = 0,
    P_AddSubtract=1,
    P_Multiply=2,
    P_Mod=3,
    P_Divide=4,
    P_Not=5,
    P_Root=6,
    P_Function=7,
    P_Boolean=8,
    P_Percentage=9,
    /* UnaryMinus and Power must have same precedence. */
    P_UnaryMinus=10,
    P_Power=10,
    P_Factorial=11,
    P_NumberVariable=12,
    /* P_Depth should be always at the bottom. It stops node jumping off the current depth level. */
    P_Depth
} Precedence;

/* ParseNode structure for parse tree. */
typedef struct parse_node
{
    struct parse_node *parent;
    struct parse_node *left, *right;
    LexerToken *token;
    guint precedence;
    Associativity associativity;
    void* value;
    struct parser_state* state;
    void* (*evaluate) (struct parse_node* self);
} ParseNode;

/* ParserState structure. Stores parser state. */
typedef struct parser_state
{
    ParseNode *root;
    ParseNode *right_most;
    LexerState *lexer;
    guint depth_level;
    MPEquationOptions *options;
    int error;
    char *error_token;
    MPNumber ret;
    int (*variable_is_defined)(struct parser_state *state, const char *name);
    int (*get_variable)(struct parser_state *state, const char *name, MPNumber *z);
    void (*set_variable)(struct parser_state *state, const char *name, const MPNumber *x);
    int (*function_is_defined)(struct parser_state *state, const char *name);
    int (*get_function)(struct parser_state *state, const char *name, const MPNumber *x, MPNumber *z);
    int (*convert)(struct parser_state *state, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z);
} ParserState;

/* Create ParserState object. */
ParserState* p_create_parser(const gchar*, MPEquationOptions*);

/* Destroy ParserState object. */
void p_destroy_parser(ParserState*);

/* Parse string from ParserState. */
guint p_parse(ParserState*);

#endif /* PARSER_H */
