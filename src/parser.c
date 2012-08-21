#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "parser.h"
#include "parserfunc.h"
#include "mp-equation.h"

/* Converts LexerTokenType to Precedence value. */
static guint
p_get_precedence(LexerTokenType type)
{
    /* WARNING: This function doesn't work for Unary Plus and Unary Minus. Use their precedence directly while inserting them in tree. */
    if(type == T_ADD
     ||type == T_SUBTRACT)
        return P_AddSubtract;
    if(type == T_MULTIPLY)
        return P_Multiply;
    if(type == T_MOD)
        return P_Mod;
    if(type == T_DIV)
        return P_Divide;
    if(type == T_NOT)
        return P_Not;
    if(type == T_ROOT
     ||type == T_ROOT_3
     ||type == T_ROOT_4)
        return P_Root;
    if(type == T_FUNCTION)
        return P_Function;
    if(type == T_AND
     ||type == T_OR
     ||type == T_XOR)
        return P_Boolean;
    if(type == T_PERCENTAGE)
        return P_Percentage;
    if(type == T_POWER)
        return P_Power;
    if(type == T_FACTORIAL)
        return P_Factorial;
    if(type == T_NUMBER
     ||type == T_VARIABLE)
        return P_NumberVariable;
    return P_Unknown;
}

/* Return associativity of specific token type from precedence. */
static Associativity
p_get_associativity_p(Precedence type)
{
    if(type == P_Boolean
     ||type == P_Divide
     ||type == P_Mod
     ||type == P_Multiply
     ||type == P_AddSubtract)
        return LEFT_ASSOCIATIVE;
    if(type == P_Power)
        return RIGHT_ASSOCIATIVE;
    /* For all remaining / non-associative operators, return Left Associativity. */
    return LEFT_ASSOCIATIVE;
}

/* Return associativity of specific token by converting it to precedence first. */
static Associativity
p_get_associativity(LexerToken* token)
{
    return p_get_associativity_p(p_get_precedence(token->token_type));
}

/* Generate precedence for a node from precedence value. Includes depth_level. */
static guint
p_make_precedence_p(ParserState* state, Precedence p)
{
    return (p + (state->depth_level * P_Depth));
}

/* Generate precedence for a node from lexer token type. Includes depth_level. */
static guint
p_make_precedence_t(ParserState* state, LexerTokenType type)
{
    return (p_get_precedence(type) + (state->depth_level * P_Depth));
}

/* Allocate and create a new node. */
static ParseNode*
p_create_node(ParserState* state, LexerToken* token, guint precedence, Associativity associativity, void* value, void* (*function)(ParseNode*))
{
    ParseNode* new;
    new = (ParseNode*) malloc(sizeof(ParseNode));
    assert(new != NULL);
    new->parent = NULL;
    new->left = NULL;
    new->right = NULL;
    new->token = token;
    new->precedence = precedence;
    new->associativity = associativity;
    new->value = value;
    new->state = state;
    new->evaluate = function;
    return new;
}

/* Compares two nodes to decide, which will be parent and which willbe child. */
static gint
p_cmp_nodes(ParseNode* left, ParseNode* right)
{
    /* Return values.
       1 = right goes up (near root) in parse tree.
       0 = left  goes up (near root) in parse tree.
    */
    if(left == NULL)
        return 0;
    if(left->precedence > right->precedence)
    {
        return 1;
    }
    else if(left->precedence < right->precedence)
    {
        return 0;
    }
    else
    {
        if(right->associativity == RIGHT_ASSOCIATIVE)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
}

/* Unified interface (unary and binary nodes) to insert node into parse tree. */
static void
p_insert_into_tree_all(ParserState* state, ParseNode* node, guint unary_function)
{
    if(state->root == NULL)
    {
        state->root = node;
        state->right_most = state->root;
        return;
    }
    ParseNode* tmp = state->right_most;
    while(p_cmp_nodes(tmp, node))
        tmp = tmp->parent;
    if(unary_function)
    {
        /* If tmp is null, that means, we have to insert new node at root. */
        if(tmp == NULL)
        {
            node->right = state->root;
            node->right->parent = node;

            state->root = node;
        }
        else
        {
            node->right = tmp->right;
            if(node->right)
                node->right->parent = node;

            tmp->right = node;
            if(tmp->right)
                tmp->right->parent = tmp;

        }
        state->right_most = node;
        while(state->right_most->right != NULL)
            state->right_most = state->right_most->right;
    }
    else
    {
        /* If tmp is null, that means, we have to insert new node at root. */
        if(tmp == NULL)
        {
            node->left = state->root;
            node->left->parent = node;

            state->root = node;
        }
        else
        {
            node->left = tmp->right;
            if(node->left)
                node->left->parent = node;

            tmp->right = node;
            if(tmp->right)
                tmp->right->parent = tmp;

        }
        state->right_most = node;
    }
}

/* Insert binary node into the parse tree. */
static void
p_insert_into_tree(ParserState* state, ParseNode* node)
{
    p_insert_into_tree_all(state, node, 0);
}

/* Insert unary node into the parse tree. */
static void
p_insert_into_tree_unary(ParserState* state, ParseNode* node)
{
    p_insert_into_tree_all(state, node, 1);
}

/* Recursive call to free every node of parse-tree. */
static void
p_destroy_all_nodes(ParseNode* node)
{
    if(node == NULL)
        return;
    p_destroy_all_nodes(node->left);
    p_destroy_all_nodes(node->right);
    /* Don't call free for tokens, as they are allocated and freed in lexer. */
    /* WARNING: If node->value is freed elsewhere, please assign it NULL before calling p_destroy_all_nodes(). */
    if(node->value)
        free(node->value);
    free(node);
}

/* Create parser state. */
ParserState*
p_create_parser(const gchar* input, MPEquationOptions* options)
{
    ParserState* state;
    state = (ParserState*) malloc(sizeof(ParserState));
    assert(state != NULL);
    state->lexer = l_create_lexer(input, state);
    state->root = NULL;
    state->depth_level = 0;
    state->right_most = NULL;
    state->options = options;
    state->error = 0;
    state->error_token = NULL;
    return state;
}

static guint statement (ParserState*);
/* Start parsing input string. And call evaluate on success. */
guint
p_parse(ParserState* state)
{
    guint ret;
    LexerToken* token;
    MPNumber* ans;
    l_insert_all_tokens(state->lexer);
    ret = statement(state);
    token = l_get_next_token(state->lexer);
    if(token->token_type == T_ASSIGN)
    {
        token = l_get_next_token(state->lexer);
        if(token->token_type != PL_EOS)
        {
        /* Full string is not parsed. */
            if(!state->error)
                set_error(state, PARSER_ERR_INVALID, token->string);
            return PARSER_ERR_INVALID;
        }
    }
    if(token->token_type != PL_EOS)
    {
        /* Full string is not parsed. */
        if(!state->error)
            set_error(state, PARSER_ERR_INVALID, token->string);
        return PARSER_ERR_INVALID;
    }
    if(ret == 0)
        /* Input can't be parsed with grammar. */
        return PARSER_ERR_INVALID;
    ans = (MPNumber *) (*(state->root->evaluate))(state->root);
    if(ans)
    {
        mp_set_from_mp(ans, &state->ret);
        free(ans);
        return PARSER_ERR_NONE;
    }
    return PARSER_ERR_INVALID;
}

/* Destroy parser state. */
void
p_destroy_parser(ParserState* state)
{
    /* If state has a parse tree, destroy it first. */
    if(state->root)
    {
        p_destroy_all_nodes(state->root);
    }
    l_destroy_lexer(state->lexer);
    free(state);
}

/* LL (*) parser. Lookahead count depends on tokens. Handle with care. :P */

static guint expression(ParserState* state);
static guint expression_1(ParserState* state);
static guint expression_2(ParserState* state);
static guint unit(ParserState* state);
static guint variable(ParserState* state);
static guint term(ParserState* state);
static guint term_2(ParserState* state);

/* Helping function to p_check_variable. */
static gchar*
utf8_next_char(const gchar* c)
{
    c++;
    while((*c & 0xC0) == 0x80)
        c++;
    return (gchar *)c;
}

/* Check if string "name" is a valid variable for given ParserState. It is the same code, used to get the value of variable in parserfunc.c. */
static gboolean
p_check_variable(ParserState* state, gchar* name)
{
    gint result = 0;

    const gchar *c, *next;
    gchar *buffer;
    MPNumber temp;

    if(!(state->get_variable))
    {
        return FALSE;
    }

    /* If defined, then get the variable */
    if((*(state->get_variable))(state, name, &temp))
    {
        return TRUE;
    }

    /* If has more than one character then assume a multiplication of variables */
    if(utf8_next_char(name)[0] != '\0')
    {
        result = 1;
        buffer = (gchar*) malloc(sizeof(gchar) * strlen(name));
        for(c = name; *c != '\0'; c = next)
        {
            next = utf8_next_char(c);
            snprintf(buffer, next - c + 1, "%s", c);
            if(!(*(state->get_variable))(state, buffer, &temp))
            {
                result = 0;
                break;
            }
        }
        free(buffer);
    }
    if(!result)
    {
        return FALSE;
    }
    return TRUE;
}

static guint
statement(ParserState* state)
{
    LexerToken* token;
    LexerToken* token_old;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    if(token->token_type == T_VARIABLE)
    {
        token_old = token;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_ASSIGN)
        {
            /* VARIABLE = expression. */

            node = p_create_node(state, token_old, p_make_precedence_p(state, P_NumberVariable), p_get_associativity(token_old), NULL, pf_none);
            p_insert_into_tree(state, node);

            node = p_create_node(state, token, 0, p_get_associativity(token), NULL, pf_set_var);
            p_insert_into_tree(state, node);

            if(!expression(state))
                return 0;
            return 1;
        }
        else if(token->token_type == T_IN)
        {
            /* UNIT in UNIT. */
            l_roll_back(state->lexer);
            l_roll_back(state->lexer);
            if(!unit(state))
                return 0;
            l_get_next_token(state->lexer);

            node = p_create_node(state, token, 0, p_get_associativity(token), NULL, pf_convert_1);
            p_insert_into_tree(state, node);

            if(!unit(state))
                return 0;
            return 1;
        }
        else if(token->token_type == T_SUP_NUMBER)
        {
            token = l_get_next_token(state->lexer);
            if(token->token_type == T_IN)
            {
                /* UNIT in UNIT */
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);
                if(!unit(state))
                    return 0;
                l_get_next_token(state->lexer);

                node = p_create_node(state, token, 0, p_get_associativity(token), NULL, pf_convert_1);
                p_insert_into_tree(state, node);

                if(!unit(state))
                    return 0;
                return 1;
            }
            else
            {
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);
                if(!expression(state))
                    return 0;
                return 1;
            }
        }
        else
        {
            l_roll_back(state->lexer);
            l_roll_back(state->lexer);
            if(!expression(state))
                return 0;
            return 1;
        }
    }
    else if(token->token_type == T_NUMBER)
    {
        token_old = token;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_VARIABLE)
        {
            token = l_get_next_token(state->lexer);
            if(token->token_type == T_IN)
            {
                /* NUMBER UNIT in UNIT */
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);

                node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token), NULL, pf_constant);
                p_insert_into_tree(state, node);

                if(!unit(state))
                    return 0;
                token = l_get_next_token(state->lexer);

                node = p_create_node(state, token, 0, p_get_associativity(token), NULL, pf_convert_number);
                p_insert_into_tree(state, node);

                if(!unit(state))
                    return 0;
                return 1;
            }
            else if(token->token_type == T_SUP_NUMBER)
            {
                token = l_get_next_token(state->lexer);
                if(token->token_type == T_IN)
                {
                    /* NUMBER UNIT in UNIT */
                    l_roll_back(state->lexer);
                    l_roll_back(state->lexer);
                    l_roll_back(state->lexer);

                    node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token), NULL, pf_constant);
                    p_insert_into_tree(state, node);

                    if(!unit(state))
                        return 0;
                    token = l_get_next_token(state->lexer);

                    node = p_create_node(state, token, 0, p_get_associativity(token), NULL, pf_convert_number);
                    p_insert_into_tree(state, node);

                    if(!unit(state))
                        return 0;
                    return 1;
                }
                else
                {
                    l_roll_back(state->lexer);
                    l_roll_back(state->lexer);
                    l_roll_back(state->lexer);
                    l_roll_back(state->lexer);
                    if(!expression(state))
                        return 0;
                    return 1;
                }
            }
            else
            {
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);
                l_roll_back(state->lexer);
                if(!expression(state))
                    return 0;
                return 1;
            }
        }
        else
        {
            l_roll_back(state->lexer);
            l_roll_back(state->lexer);
            if(!expression(state))
                return 0;
            return 1;
        }
    }
    else
    {
        l_roll_back(state->lexer);
        if(!expression(state))
            return 0;
        return 1;
    }
}

static guint
unit(ParserState* state)
{
    LexerToken* token;
    LexerToken* token_old;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    if(token->token_type == T_VARIABLE)
    {
        token_old = token;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_SUP_NUMBER)
        {
            /* VARIABLE POWER */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), pf_make_unit(token_old->string, token->string), pf_none);
            p_insert_into_tree(state, node);

            return 1;
        }
        else
        {
            l_roll_back(state->lexer);
            /* VARIABLE */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), NULL, pf_none);
            p_insert_into_tree(state, node);

            return 1;
        }
    }
    else
    {
        l_roll_back(state->lexer);
        return 0;
    }
}

static guint
expression(ParserState* state)
{
    if(!expression_1(state))
        return 0;
    if(!expression_2(state))
        return 0;
    return 1;
}

static guint
expression_1(ParserState* state)
{
    LexerToken* token;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    if(token->token_type == PL_EOS
     ||token->token_type == T_ASSIGN)
    {
        l_roll_back(state->lexer);
        return 0;
    }
    if(token->token_type == T_L_R_BRACKET)
    {
        state->depth_level++;
        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_R_R_BRACKET)
        {
            state->depth_level--;
            return 1;
        }
        else
        //Expected ")" here...
            return 0;
    }
    else if(token->token_type == T_L_S_BRACKET)
    {
        state->depth_level++;

        /* Give round, preference of P_Unknown aka 0, to keep it on the top of expression. */

        node = p_create_node(state, token, p_make_precedence_p(state, P_Unknown), p_get_associativity(token), NULL, pf_do_round);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_R_S_BRACKET)
        {
            state->depth_level--;
            return 1;
        }
        else
        //Expected "]" here...
            return 0;
    }
    else if(token->token_type == T_L_C_BRACKET)
    {
        state->depth_level++;

        /* Give fraction, preference of P_Unknown aka 0, to keep it on the top of expression. */

        node = p_create_node(state, token, p_make_precedence_p(state, P_Unknown), p_get_associativity(token), NULL, pf_do_fraction);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_R_C_BRACKET)
        {
            state->depth_level--;
            return 1;
        }
        else
        //Expected "}" here...
            return 0;
    }
    else if(token->token_type == T_ABS)
    {
        state->depth_level++;

        /* Give abs, preference of P_Unknown aka 0, to keep it on the top of expression. */

        node = p_create_node(state, token, p_make_precedence_p(state, P_Unknown), p_get_associativity(token), NULL, pf_do_abs);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_ABS)
        {
            state->depth_level--;
            return 1;
        }
        else
        //Expected "|" here...
            return 0;
    }
    else if(token->token_type == T_NOT)
    {
        /* NOT expression */

        node = p_create_node(state, token, p_make_precedence_p(state, P_Not), p_get_associativity(token), NULL, pf_do_not);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_NUMBER)
    {
        /* NUMBER */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_constant);
        p_insert_into_tree(state, node);

        token = l_get_next_token(state->lexer);
        l_roll_back(state->lexer);

        if(token->token_type == T_FUNCTION
         ||token->token_type == T_VARIABLE
         ||token->token_type == T_SUB_NUMBER
         ||token->token_type == T_ROOT
         ||token->token_type == T_ROOT_3
         ||token->token_type == T_ROOT_4)
        {
            /* NUMBER variable. */

            node = p_create_node(state, NULL, p_make_precedence_p(state, P_Multiply), p_get_associativity_p(P_Multiply), NULL, pf_do_multiply);
            p_insert_into_tree(state, node);

            if(!variable(state))
                return 0;
            else
                return 1;
        }
        else
        {
            return 1;
        }
    }
    else if(token->token_type == T_L_FLOOR)
    {
        state->depth_level++;
        /* Give floor, preference of P_Unknown aka 0, to keep it on the top of expression. */

        node = p_create_node(state, NULL, p_make_precedence_p(state, P_Unknown), p_get_associativity_p(P_Unknown), NULL, pf_do_floor);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_R_FLOOR)
        {
            state->depth_level--;
            return 1;
        }
        else
        //Expected ⌋ here...
            return 0;
    }
    else if(token->token_type == T_L_CEILING)
    {
        state->depth_level++;
        /* Give ceiling, preference of P_Unknown aka 0, to keep it on the top of expression. */

        node = p_create_node(state, NULL, p_make_precedence_p(state, P_Unknown), p_get_associativity_p(P_Unknown), NULL, pf_do_ceiling);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_R_CEILING)
        {
            state->depth_level--;
            return 1;
        }
        else
        //Expected ⌉ here...
            return 0;
    }
    else if(token->token_type == T_SUBTRACT)
    {
        /* UnaryMinus expression */

        node = p_create_node(state, token, p_make_precedence_p(state, P_UnaryMinus), p_get_associativity_p(P_UnaryMinus), NULL, pf_unary_minus);
        p_insert_into_tree_unary(state, node);

        if(!expression_1(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_ADD)
    {
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_NUMBER)
        {
            /* UnaryPlus expression */
            /* Ignore T_ADD. It is not required. */

            node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_constant);
            p_insert_into_tree(state, node);
            return 1;
        }
        else
        {
            return 0;
        }
    }
    else
    {
        l_roll_back(state->lexer);
        if(!variable(state))
            return 0;
        else
            return 1;
    }
}

static guint
expression_2(ParserState* state)
{
    LexerToken* token;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    if(token->token_type == T_L_R_BRACKET)
    {
        /* expression "(" expression ")" */

        node = p_create_node(state, NULL, p_make_precedence_p(state, P_Multiply), p_get_associativity_p(P_Multiply), NULL, pf_do_multiply);
        p_insert_into_tree(state, node);

        state->depth_level++;
        if(!expression(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_R_R_BRACKET)
        {
            state->depth_level--;
            if(!expression_2(state))
                return 0;
            return 1;
        }
        else
        {
            return 0;
        }
    }
    else if(token->token_type == T_POWER)
    {
        /* expression "^" expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_x_pow_y);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_SUP_NUMBER)
    {
        /* expression T_SUP_NUMBER */

        node = p_create_node(state, NULL, p_make_precedence_p(state, P_Power), p_get_associativity_p(P_Power), NULL, pf_do_x_pow_y_int);
        p_insert_into_tree(state, node);

        node = p_create_node(state, token, p_make_precedence_p(state, P_NumberVariable), p_get_associativity_p(P_NumberVariable), NULL, pf_none);
        p_insert_into_tree(state, node);

        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_NSUP_NUMBER)
    {
        /* expression T_NSUP_NUMBER */

        node = p_create_node(state, NULL, p_make_precedence_p(state, P_Power), p_get_associativity_p(P_Power), NULL, pf_do_x_pow_y_int);
        p_insert_into_tree(state, node);

        node = p_create_node(state, token, p_make_precedence_p(state, P_NumberVariable), p_get_associativity_p(P_NumberVariable), NULL, pf_none);
        p_insert_into_tree(state, node);

        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_FACTORIAL)
    {
        /* expression T_FACTORIAL */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_factorial);
        p_insert_into_tree_unary(state, node);

        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_MULTIPLY)
    {
        /* expression T_MULTIPLY expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_multiply);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_PERCENTAGE)
    {
        /* expression % */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_percent);
        p_insert_into_tree_unary(state, node);

        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_AND)
    {
        /* expression T_AND expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_and);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_OR)
    {
        /* expression T_OR expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_or);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_XOR)
    {
        /* expression T_XOR expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_xor);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_DIV)
    {
        /* expression T_DIV expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_divide);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_MOD)
    {
        /* expression T_MOD expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_mod);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_ADD)
    {
        /* expression T_ADD expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_add);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_PERCENTAGE)
        {
            //FIXME: This condition needs to be verified for all cases.. :(
            if(node->right->precedence > P_Percentage)
            {
                node->precedence = P_Percentage;
                node->evaluate = pf_do_add_percent;
                return 1;
            }
            else
            {
                /* Assume '%' to be part of 'expression T_PERCENTAGE' statement. */
                l_roll_back(state->lexer);
                if(!expression_2(state))
                    return 1;
            }
        }
        else
        {
            l_roll_back(state->lexer);
        }
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_SUBTRACT)
    {
        /* expression T_SUBTRACT expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_subtract);
        p_insert_into_tree(state, node);

        if(!expression_1(state))
            return 0;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_PERCENTAGE)
        {
            //FIXME: This condition needs to be verified for all cases.. :(
            if(node->right->precedence > P_Percentage)
            {
                node->precedence = P_Percentage;
                node->evaluate = pf_do_subtract_percent;
                return 1;
            }
            else
            {
                /* Assume '%' to be part of 'expression T_PERCENTAGE' statement. */
                l_roll_back(state->lexer);
                if(!expression_2 (state))
                    return 1;
            }
        }
        else
        {
            l_roll_back(state->lexer);
        }
        if(!expression_2(state))
            return 0;
        return 1;
    }
    else
    {
        l_roll_back(state->lexer);
        return 1;
    }
}

static guint
variable(ParserState* state)
{
    LexerToken* token;
    LexerToken* token_old;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    if(token->token_type == T_FUNCTION)
    {
        token_old = token;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_SUP_NUMBER)
        {
            /* FUNCTION SUP_NUMBER expression */
            /* Pass power as void * value. That will be taken care in pf_apply_func_with_powre. */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), token, pf_apply_func_with_power);
            p_insert_into_tree_unary(state, node);

            if(!expression(state))
                return 0;
            return 1;
        }
        else if(token->token_type == T_NSUP_NUMBER)
        {
            /* FUNCTION NSUP_NUMBER expression */
            /* Pass power as void * value. That will be taken care in pf_apply_func_with_npowre. */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), token, pf_apply_func_with_npower);
            p_insert_into_tree_unary(state, node);

            if(!expression(state))
                return 0;
            return 1;
        }
        else
        {
            l_roll_back(state->lexer);
            /* FUNCTION expression */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), NULL, pf_apply_func);
            p_insert_into_tree_unary(state, node);

            if(!expression(state))
                return 0;
            return 1;
        }
    }
    else if(token->token_type == T_SUB_NUMBER)
    {
        token_old = token;
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_ROOT)
        {
            /* SUB_NUM ROOT expression */
            /* Pass SUB_NUM as void* value in node. pf_do_nth_root will take care of it. */

            node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), token_old, pf_do_nth_root);
            p_insert_into_tree_unary(state, node);

            if(!expression (state))
                return 0;
            return 1;
        }
        else
        {
            return 0;
        }
    }
    else if(token->token_type == T_ROOT)
    {
        /* ROOT expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_sqrt);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_ROOT_3)
    {
        /* ROOT_3 expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_root_3);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_ROOT_4)
    {
        /* ROOT_4 expression */

        node = p_create_node(state, token, p_make_precedence_t(state, token->token_type), p_get_associativity(token), NULL, pf_do_root_4);
        p_insert_into_tree_unary(state, node);

        if(!expression(state))
            return 0;
        return 1;
    }
    else if(token->token_type == T_VARIABLE)
    {
        l_roll_back(state->lexer);
        //TODO: unknown function ERROR for (T_VARIABLE T_SUP_NUMBER expression).
        if(!term(state))
            return 0;
        return 1;
    }
    else
    {
        return 0;
    }
}

static guint
term(ParserState* state)
{
    LexerToken* token;
    LexerToken* token_old;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    if(token->token_type == T_VARIABLE)
    {
        token_old = token;
        /* Check if the token is a valid variable or not. */
        if(!p_check_variable(state, token->string))
        {
            set_error(state, PARSER_ERR_UNKNOWN_VARIABLE, token->string);
            return 0;
        }
        token = l_get_next_token(state->lexer);
        if(token->token_type == T_SUP_NUMBER)
        {
            /* VARIABLE SUP_NUMBER */
            /* Pass power as void* value. pf_get_variable_with_power will take care of it. */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), token, pf_get_variable_with_power);
            p_insert_into_tree(state, node);

        }
        else
        {
            l_roll_back(state->lexer);
            /* VARIABLE */

            node = p_create_node(state, token_old, p_make_precedence_t(state, token_old->token_type), p_get_associativity(token_old), NULL, pf_get_variable);
            p_insert_into_tree(state, node);

        }
        if(!term_2(state))
            return 0;
        return 1;
    }
    else
    {
        return 0;
    }
}

static guint
term_2(ParserState* state)
{
    LexerToken* token;
    ParseNode* node;
    token = l_get_next_token(state->lexer);
    l_roll_back(state->lexer);
    if(token->token_type == PL_EOS
     ||token->token_type == T_ASSIGN)
    {
        return 1;
    }
    if(token->token_type == T_VARIABLE)
    {
        /* Insert multiply in between two distinct (variable). */

        node = p_create_node(state, NULL, p_make_precedence_p(state, P_Multiply), p_get_associativity_p(P_Multiply), NULL, pf_do_multiply);
        p_insert_into_tree(state, node);

        if(!term(state))
            return 0;
        return 1;
    }
    else
    {
        return 1;
    }
}
