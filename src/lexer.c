#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "lexer.h"
#include "parserfunc.h"
#include "mp-equation.h"

static gboolean
l_check_if_function(LexerState* state)
{
    gchar* name = pl_get_marked_substring(state->prelexer);
    if(!state->parent->function_is_defined)
    {
        free(name);
        return FALSE;
    }
    if ((*(state->parent->function_is_defined))(state->parent, name))
    {
        free(name);
        return TRUE;
    }
    else
    {
        free(name);
        return FALSE;
    }
}

static gboolean
l_check_if_number(LexerState* state)
{
    MPNumber tmp;
    int count = 0;
    gchar* text = pl_get_marked_substring(state->prelexer);
    if(mp_set_from_string(text, state->parent->options->base, &tmp) == 0)
    {
        free(text);
        return TRUE;
    }
    else
    {
        /* Try to rollback several characters to see, if that yeilds any number. */
        while(strlen (text) > 0)
        {
            if(mp_set_from_string(text, state->parent->options->base, &tmp) == 0)
            {
                free(text);
                return TRUE;
            }
            free(text);
            count++;
            pl_roll_back(state->prelexer);
            text = pl_get_marked_substring(state->prelexer);
        }
        /* Undo all rollbacks. */
        while(count--)
            pl_get_next_token (state->prelexer);
        free(text);
        return FALSE;
    }
}

/* Insert generated token to the LexerState structure. */
static LexerToken*
l_insert_token(LexerState* state, const LexerTokenType type)
{
    state->tokens = (LexerToken *) realloc(state->tokens, (state->token_count + 1) * sizeof(LexerToken));
    assert(state->tokens != NULL);
    state->tokens[state->token_count].string = pl_get_marked_substring(state->prelexer);
    state->tokens[state->token_count].start_index = state->prelexer->mark_index;
    state->tokens[state->token_count].end_index = state->prelexer->next_index;
    state->tokens[state->token_count].token_type = type;
    state->token_count++;
    return &state->tokens[state->token_count - 1];
}

/* Generates next token from pre-lexer stream and call l_insert_token() to insert it at the end. */
static LexerToken*
l_insert_next_token(LexerState* lstate)
{
    PreLexerState* state = lstate->prelexer;
    LexerTokenType type;
    gchar* tmp;
    pl_set_marker(state);
    /* Ignore all blank spaces. :) */
    while((type = pl_get_next_token(state)) == PL_SKIP)
        /* Set marker. Beginning of new token. */
        pl_set_marker(state);
    if(type == T_AND
     ||type == T_OR
     ||type == T_XOR
     ||type == T_NOT
     ||type == T_ADD
     ||type == T_SUBTRACT
     ||type == T_MULTIPLY
     ||type == T_DIV
     ||type == T_L_FLOOR
     ||type == T_R_FLOOR
     ||type == T_L_CEILING
     ||type == T_R_CEILING
     ||type == T_ROOT
     ||type == T_ROOT_3
     ||type == T_ROOT_4
     ||type == T_ASSIGN
     ||type == T_L_R_BRACKET
     ||type == T_R_R_BRACKET
     ||type == T_L_S_BRACKET
     ||type == T_R_S_BRACKET
     ||type == T_L_C_BRACKET
     ||type == T_R_C_BRACKET
     ||type == T_ABS
     ||type == T_POWER
     ||type == T_FACTORIAL
     ||type == T_PERCENTAGE)
    {
        return l_insert_token(lstate, type);
    }
    /* [PL_SUPER_MINUS][PL_SUPER_DIGIT]+ */
    if(type == PL_SUPER_MINUS)
    {
        if((type = pl_get_next_token(state)) != PL_SUPER_DIGIT)
        {
            /* ERROR: expected PL_SUP_DIGIT */
            set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring (state));
            free(tmp);
            return l_insert_token(lstate, T_UNKNOWN);
        }
        /* Get all PL_SUPER_DIGITs. */
        while (pl_get_next_token(state) == PL_SUPER_DIGIT);
        pl_roll_back(state);
        return l_insert_token(lstate, T_NSUP_NUMBER);
    }
    /* [PL_SUPER_DIGIT]+ */
    if(type == PL_SUPER_DIGIT)
    {
        while(pl_get_next_token(state) == PL_SUPER_DIGIT);
        pl_roll_back(state);
        return l_insert_token(lstate, T_SUP_NUMBER);
    }
    /* [PL_SUB_DIGIT]+ */
    if(type == PL_SUB_DIGIT)
    {
        while(pl_get_next_token(state) == PL_SUB_DIGIT);
        pl_roll_back(state);
        return l_insert_token(lstate, T_SUB_NUMBER);
    }
    /* [PL_FRACTION] */
    if(type == PL_FRACTION)
    {
        return l_insert_token(lstate, T_NUMBER);
    }
    if(type == PL_DIGIT)
    {
        while((type = pl_get_next_token(state)) == PL_DIGIT);
        if(type == PL_FRACTION)
        {
            return l_insert_token(lstate, T_NUMBER);
        }
        else if(type == PL_SUB_DIGIT)
        {
            while(pl_get_next_token(state) == PL_SUB_DIGIT);
            pl_roll_back(state);
            return l_insert_token(lstate, T_NUMBER);
        }
        else if(type == PL_DEGREE)
        {
            type = pl_get_next_token(state);
            if(type == PL_DIGIT)
            {
                while((type = pl_get_next_token(state)) == PL_DIGIT);
                if(type == PL_DECIMAL)
                {
                    goto ANGLE_NUM_DM_STATE;
                }
                else if(type == PL_MINUTE)
                {
                    type = pl_get_next_token(state);
                    if(type == PL_DIGIT)
                    {
                        while((type = pl_get_next_token(state)) == PL_DIGIT);
                        if(type == PL_DECIMAL)
                        {
                            goto ANGLE_NUM_DMS_STATE;
                        }
                        else if(type == PL_SECOND)
                        {
                            return l_insert_token(lstate, T_NUMBER);
                        }
                        else
                        {
                            /* ERROR: expected PL_SECOND */
                            set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring (state));
                            free(tmp);
                            return l_insert_token(lstate, T_UNKNOWN);
                        }
                    }
                    else if(type == PL_DECIMAL)
                    {
ANGLE_NUM_DMS_STATE:
                        if((type = pl_get_next_token (state)) != PL_DIGIT)
                        {
                            /* ERROR: expected PL_DIGIT */
                            set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                            free(tmp);
                            return l_insert_token(lstate, T_UNKNOWN);
                        }
                        while((type = pl_get_next_token(state)) == PL_DIGIT);
                        if(type == PL_SECOND)
                        {
                            return l_insert_token(lstate, T_NUMBER);
                        }
                        else
                        {
                            /* ERROR: expected PL_SECOND */
                            set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                            free(tmp);
                            return l_insert_token(lstate, T_UNKNOWN);
                        }
                    }
                    else
                    {
                        pl_roll_back(state);
                        return l_insert_token(lstate, T_NUMBER);
                    }
                }
                else
                {
                    /* ERROR: expected PL_MINUTE | PL_DIGIT */
                    set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                    free(tmp);
                    return l_insert_token(lstate, T_UNKNOWN);
                }
            }
            else if(type == PL_DECIMAL)
            {
ANGLE_NUM_DM_STATE:
                if((type = pl_get_next_token(state)) != PL_DIGIT)
                {
                    /* ERROR: expected PL_DIGIT */
                    set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                    free(tmp);
                    return l_insert_token(lstate, T_UNKNOWN);
                }
                while((type = pl_get_next_token(state)) == PL_DIGIT);
                if(type == PL_MINUTE)
                {
                    return l_insert_token(lstate, T_NUMBER);
                }
                else
                {
                    /* ERROR: expected PL_MINUTE */
                    set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                    free(tmp);
                    return l_insert_token(lstate, T_UNKNOWN);
                }
            }
            else
            {
                return l_insert_token(lstate, T_NUMBER);
            }
        }
        else if(type == PL_DECIMAL)
        {
            goto DECIMAL_STATE;
        }
        else if(type == PL_HEX)
        {
            goto HEX_DEC_STATE;
        }
        else
        {
            pl_roll_back(state);
            return l_insert_token(lstate, T_NUMBER);
        }
    }
    if(type == PL_DECIMAL)
    {
DECIMAL_STATE:
        type = pl_get_next_token(state);
        if(type == PL_DIGIT)
        {
            while((type = pl_get_next_token(state)) == PL_DIGIT);
            if(type == PL_DEGREE)
            {
                return l_insert_token(lstate, T_NUMBER);
            }
            else if(type == PL_HEX)
            {
                goto DECIMAL_HEX_STATE;
            }
            else if(type == PL_SUB_DIGIT)
            {
                while(pl_get_next_token(state) == PL_SUB_DIGIT);
                pl_roll_back(state);
                return l_insert_token(lstate, T_NUMBER);
            }
            else
            {
                pl_roll_back(state);
                return l_insert_token(lstate, T_NUMBER);
            }
        }
        else if(type == PL_HEX)
        {
            goto DECIMAL_HEX_STATE;
        }
        else
        {
            /* ERROR: expected PL_DIGIT | PL_HEX */
            set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
            free(tmp);
            return l_insert_token(lstate, T_UNKNOWN);
        }
    }
    if(type == PL_HEX)
    {
        while((type = pl_get_next_token(state)) == PL_HEX);
        if(type == PL_DIGIT)
        {
HEX_DEC_STATE:
            while(1)
            {
                type = pl_get_next_token(state);
                if(type == PL_DIGIT || type == PL_HEX)
                {
                    continue;
                }
                else if(type == PL_DECIMAL)
                {
                    goto DECIMAL_HEX_STATE;
                }
                else if(type == PL_SUB_DIGIT)
                {
                    while(pl_get_next_token(state) == PL_SUB_DIGIT);
                    pl_roll_back(state);
                    return l_insert_token(lstate, T_NUMBER);
                }
                else
                {
                    if(l_check_if_number(lstate))
                        return l_insert_token(lstate, T_NUMBER);
                    /* ERROR: expected PL_DECIMAL | PL_DIGIT | PL_HEX */
                    set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                    free(tmp);
                    return l_insert_token(lstate, T_UNKNOWN);
                }
            }
        }
        else if(type == PL_DECIMAL)
        {
DECIMAL_HEX_STATE:
            type = pl_get_next_token(state);
            if(!(type == PL_DIGIT || type == PL_HEX))
            {
                /* ERROR: expected PL_DIGIT | PL_HEX */
                set_error(lstate->parent, PARSER_ERR_MP, tmp = pl_get_marked_substring(state));
                free(tmp);
                return l_insert_token(lstate, T_UNKNOWN);
            }
            while(1)
            {
                type = pl_get_next_token(state);
                if(type == PL_DIGIT || type == PL_HEX)
                {
                    continue;
                }
                else if(type == PL_SUB_DIGIT)
                {
                    while(pl_get_next_token(state) == PL_SUB_DIGIT);
                    pl_roll_back(state);
                    return l_insert_token(lstate, T_NUMBER);
                }
                else
                {
                    pl_roll_back(state);
                    return l_insert_token(lstate, T_NUMBER);
                }
            }
        }
        else if(type == PL_SUB_DIGIT)
        {
            while(pl_get_next_token(state) == PL_SUB_DIGIT);
            pl_roll_back(state);
            if(l_check_if_number(lstate))
            {
                /* NUMBER */
                return l_insert_token(lstate, T_NUMBER);
            }
            else
            {
                /* VARIABLE */
                if(l_check_if_function(lstate))
                {
                    return l_insert_token(lstate, T_FUNCTION);
                }
                else
                {
                    return l_insert_token(lstate, T_VARIABLE);
                }
            }
        }
        else if(type == PL_LETTER)
        {
            goto LETTER_STATE;
        }
        else
        {
            pl_roll_back(state);
            if(l_check_if_number(lstate))
            {
                /* NUMBER */
                return l_insert_token(lstate, T_NUMBER);
            }
            else
            {
                /* VARIABLE */
                if(l_check_if_function(lstate))
                {
                    return l_insert_token(lstate, T_FUNCTION);
                }
                else
                {
                    return l_insert_token(lstate, T_VARIABLE);
                }
            }
        }
    }
    if(type == PL_LETTER)
    {
LETTER_STATE:
        while(1)
        {
            type = pl_get_next_token(state);
            if(type == PL_LETTER || type == PL_HEX)
            {
                continue;
            }
            else if(type == PL_SUB_DIGIT)
            {
                while(pl_get_next_token(state) == PL_SUB_DIGIT);
                pl_roll_back(state);
                tmp = g_ascii_strdown(pl_get_marked_substring(state), -1);
                if(g_strcmp0(tmp, "mod") == 0)
                {
                    return l_insert_token(lstate, T_MOD);
                }
                if(g_strcmp0(tmp, "and") == 0)
                {
                    return l_insert_token(lstate, T_AND);
                }
                if(g_strcmp0(tmp, "or") == 0)
                {
                    return l_insert_token(lstate, T_OR);
                }
                if(g_strcmp0(tmp, "xor") == 0)
                {
                    return l_insert_token(lstate, T_XOR);
                }
                if(g_strcmp0(tmp, "not") == 0)
                {
                    return l_insert_token(lstate, T_NOT);
                }
                if(g_strcmp0(tmp, "in") == 0)
                {
                    return l_insert_token(lstate, T_IN);
                }
                if(l_check_if_function(lstate))
                {
                    return l_insert_token(lstate, T_FUNCTION);
                }
                else
                {
                    return l_insert_token(lstate, T_VARIABLE);
                }
            }
            else
            {
                pl_roll_back(state);
                tmp = g_ascii_strdown(pl_get_marked_substring(state), -1);
                if(g_strcmp0(tmp, "mod") == 0)
                {
                    return l_insert_token(lstate, T_MOD);
                }
                if(g_strcmp0(tmp, "and") == 0)
                {
                    return l_insert_token(lstate, T_AND);
                }
                if(g_strcmp0(tmp, "or") == 0)
                {
                    return l_insert_token(lstate, T_OR);
                }
                if(g_strcmp0(tmp, "xor") == 0)
                {
                    return l_insert_token(lstate, T_XOR);
                }
                if(g_strcmp0(tmp, "not") == 0)
                {
                    return l_insert_token(lstate, T_NOT);
                }
                if(g_strcmp0(tmp, "in") == 0)
                {
                    return l_insert_token(lstate, T_IN);
                }
                if(l_check_if_function(lstate))
                {
                    return l_insert_token(lstate, T_FUNCTION);
                }
                else
                {
                    return l_insert_token(lstate, T_VARIABLE);
                }
            }
        }
    }
    if(type == PL_EOS)
    {
        return l_insert_token(lstate, PL_EOS);
    }
    /* ERROR: Unexpected token.. X( */
    set_error(lstate->parent, PARSER_ERR_INVALID, tmp = pl_get_marked_substring(state));
    free(tmp);
    return l_insert_token(lstate, T_UNKNOWN);
}

/* Call l_insert_next_token() as many times as needed to completely tokenize the string. */
void
l_insert_all_tokens(LexerState* state)
{
    LexerToken* token;
    while(1)
    {
        token = l_insert_next_token(state);
        assert(token != NULL);
        if(token->token_type == PL_EOS)
        {
            break;
        }
    }
}

/* Create a lexer state from given input string. This will take care of pre-lexer state. */
LexerState*
l_create_lexer(const gchar* input, struct parser_state* parent)
{
    LexerState* ret;
    ret = (LexerState *) malloc(sizeof(LexerState));
    assert(ret != NULL);
    ret->prelexer = pl_create_scanner(input);
    ret->tokens = NULL;
    ret->token_count = 0;
    ret->next_token = 0;
    ret->parent = parent;
    return ret;
}

/* Destroy lexer state and free memory. */
void
l_destroy_lexer(LexerState* state)
{
    int l;
    pl_destroy_scanner(state->prelexer);
    for(l = 0; l < state->token_count; l++)
    {
        free(state->tokens[l].string);
    }
    free(state->tokens);
    free(state);
}

/* Get next token interface. Will be called by parser to get pointer to next token in token stream. */
LexerToken*
l_get_next_token(LexerState* state)
{
    /* Return PL_EOS token after token stream reaches to its end. */
    if(state->next_token >= state->token_count)
        return &state->tokens[state->token_count - 1];
    return &state->tokens[state->next_token++];
}

/* Roll back one lexer token. */
void
l_roll_back(LexerState* state)
{
    if(state->next_token > 0)
        state->next_token--;
}
