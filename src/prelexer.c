#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <assert.h>

#include "prelexer.h"

/* Creates a scanner state which will be useful for accessing the lexer later. */
PreLexerState*
pl_create_scanner(const gchar* input)
{
    PreLexerState* state;
    assert(input != NULL);
    assert(g_utf8_validate(input, -1, NULL));
    state = (PreLexerState *) malloc(sizeof(PreLexerState));
    assert(state != NULL);
    state->stream = g_strdup(input);
    state->length = strlen(state->stream);    /* Can't find a GLib replacement of strlen. The mailing list discussion says, it is not implemented because strlen is perfectly capable. :) */
    state->next_index = 0;
    state->mark_index = 0;
    return state;
}

/* Destroy and free memory used by LexerState object. */
void
pl_destroy_scanner(PreLexerState* state)
{
    free(state->stream);
    free(state);
}

/* Roll back last scanned unichar. */
void
pl_roll_back(PreLexerState* state)
{
    gchar* tmp;
    tmp = g_utf8_find_prev_char(state->stream, state->stream + state->next_index);
    if(tmp == NULL)
        /* Already at the beginning of the stram. Reset index. */
        state->next_index = 0;
    else
        state->next_index = tmp - state->stream;
}

/* Get validated gunichar from input stream. */
gunichar
pl_get_next_gunichar(PreLexerState* state)
{
    gunichar ret;
    if(state->next_index >= state->length)
    {
        /* To prevent scanning last letter multiple times, when a single unconditional rollback is used. */
        if(state->next_index == state->length)
            state->next_index++;
        return 0;
    }
    ret = g_utf8_get_char_validated(state->stream + state->next_index, -1);
    state->next_index = g_utf8_next_char(state->stream + state->next_index) - state->stream;
    return ret;
}

/* Set marker index. To be used for highlighting and error reporting. */
void
pl_set_marker(PreLexerState* state)
{
    state->mark_index = state->next_index;
}

/* Get marked substring. To be used for error reporting. */
gchar*
pl_get_marked_substring(const PreLexerState* state)
{
    return g_strndup(state->stream + state->mark_index, state->next_index - state->mark_index);
}

/* Compares a list of strings with given unichar. To be used by pl_get_next_token() only. */
static gboolean
pl_compare_all(const gunichar ch, const gint count, gchar *arr[])
{
    gint l;
    for(l = 0; l < count; l++)
    {
        if(ch == g_utf8_get_char_validated(arr[l], -1))
            return TRUE;
    }
    return FALSE;
}

/* Pre-Lexer tokanizer. To be called only by Lexer. */
LexerTokenType
pl_get_next_token(PreLexerState* state)
{
    gunichar ch = pl_get_next_gunichar(state);
    if(pl_compare_all(ch, 2, (gchar*[]){",","."}))
        return PL_DECIMAL;

    if(g_unichar_isdigit(ch) || pl_compare_all(ch, 10, (gchar*[]){"〇","〡","〢","〣","〤","〥","〦","〧","〨","〩"}))
        return PL_DIGIT;    /* 0-9 */

    if(g_unichar_isxdigit(ch))
        return PL_HEX;        /* This is supposed to report just the A-F. */

    if(pl_compare_all(ch, 10, (gchar*[]){"⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"}))
        return PL_SUPER_DIGIT;

    if(pl_compare_all(ch, 1, (gchar*[]){"⁻"}))
        return PL_SUPER_MINUS;

    if(pl_compare_all(ch, 10, (gchar*[]){"₀","₁","₂","₃","₄","₅","₆","₇","₈","₉"}))
        return PL_SUB_DIGIT;

    if(pl_compare_all(ch, 15, (gchar*[]){"½","⅓","⅔","¼","¾","⅕","⅖","⅗","⅘","⅙","⅚","⅛","⅜","⅝","⅞"}))
        return PL_FRACTION;

    if(pl_compare_all(ch, 1, (gchar*[]){"°"}))
        return PL_DEGREE;

    if(pl_compare_all(ch, 1, (gchar*[]){"'"}))
        return PL_MINUTE;

    if(pl_compare_all(ch, 1, (gchar*[]){"\""}))
        return PL_SECOND;

    if(g_unichar_isalpha(ch))
        return PL_LETTER;    /* All alphabets excluding A-F. [a-fA-F] are reported as PL_HEX. */

    if(pl_compare_all(ch, 1, (gchar*[]){"∧"}))
        return T_AND;

    if(pl_compare_all(ch, 1, (gchar*[]){"∨"}))
        return T_OR;

    if(pl_compare_all(ch, 2, (gchar*[]){"⊻","⊕"}))
        return T_XOR;

    if(pl_compare_all(ch, 2, (gchar*[]){"¬","~"}))
        return T_NOT;

    if(pl_compare_all(ch, 1, (gchar*[]){"+"}))
        return T_ADD;

    if(pl_compare_all(ch, 3, (gchar*[]){"-","−","–"}))
        return T_SUBTRACT;

    if(pl_compare_all(ch, 2, (gchar*[]){"*","×"}))
        return T_MULTIPLY;

    if(pl_compare_all(ch, 3, (gchar*[]){"/","∕","÷"}))
        return T_DIV;

    if(pl_compare_all(ch, 1, (gchar*[]){"⌊"}))
        return T_L_FLOOR;

    if(pl_compare_all(ch, 1, (gchar*[]){"⌋"}))
        return T_R_FLOOR;

    if(pl_compare_all(ch, 1, (gchar*[]){"⌈"}))
        return T_L_CEILING;

    if(pl_compare_all(ch, 1, (gchar*[]){"⌉"}))
        return T_R_CEILING;

    if(pl_compare_all(ch, 1, (gchar*[]){"√"}))
        return T_ROOT;

    if(pl_compare_all(ch, 1, (gchar*[]){"∛"}))
        return T_ROOT_3;

    if(pl_compare_all(ch, 1, (gchar*[]){"∜"}))
        return T_ROOT_4;

    if(pl_compare_all(ch, 1, (gchar*[]){"="}))
        return T_ASSIGN;

    if(pl_compare_all(ch, 1, (gchar*[]){"("}))
        return T_L_R_BRACKET;

    if(pl_compare_all(ch, 1, (gchar*[]){")"}))
        return T_R_R_BRACKET;

    if(pl_compare_all(ch, 1, (gchar*[]){"["}))
        return T_L_S_BRACKET;

    if(pl_compare_all(ch, 1, (gchar*[]){"]"}))
        return T_R_S_BRACKET;

    if(pl_compare_all(ch, 1, (gchar*[]){"{"}))
        return T_L_C_BRACKET;

    if(pl_compare_all(ch, 1, (gchar*[]){"}"}))
        return T_R_C_BRACKET;

    if(pl_compare_all(ch, 1, (gchar*[]){"|"}))
        return T_ABS;

    if(pl_compare_all(ch, 1, (gchar*[]){"^"}))
        return T_POWER;

    if(pl_compare_all(ch, 1, (gchar*[]){"!"}))
        return T_FACTORIAL;

    if(pl_compare_all(ch, 1, (gchar*[]){"%"}))
        return T_PERCENTAGE;

    if(pl_compare_all(ch, 4, (gchar*[]){" ","\r","\t","\n"}))
    /* Gotta ignore'Em all!!! ;) */
        return PL_SKIP;

    if(ch == 0)
        return PL_EOS;

    /* There is no spoon. */
    return T_UNKNOWN;
}
