
/*  $Header$
 *
 *  Copyright (c) 2004 Sami Pietila
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 *  02111-1307, USA.
 */

#include <stdio.h>
#include <libintl.h>
#include <malloc.h>
#include <assert.h>
#include <string.h>

#include "syntax_translation.h"

#define gettext_noop(String) String
#define N_(String) gettext_noop (String)

static struct word {
  char *word;
} words[] = {
  { N_("abs") }, /* Absolute value */
  { N_("acosh") }, /* Inversion of hyperbolic cosine */
  { N_("acos") }, /* Inversion of cosine */
  { N_("and") }, /* Bitwise AND */
  { N_("ans") }, /* Answer of previous calculation*/
  { N_("asinh") }, /* Inversion of hyperbolic sine */
  { N_("asin") }, /* Hyperbolic sine */
  { N_("atanh") }, /* Inversion of hyperbolic tangent */
  { N_("atan") }, /* Inversion of tangent */
  { N_("cbrt") }, /* Cubic root */
  { N_("chs") }, /* Change sign */
  { N_("clr") }, /* Clear (display) */
  { N_("cosh") }, /* Hyperbolic cosine */
  { N_("cos") }, /* Cosine */
  { N_("ddb") }, /* Double-declining deprecation */ 
  { N_("exp") }, /* x*10^y */
  { N_("frac") }, /* Fraction of a decimal number */
  { N_("fv") }, /* Future value */
  { N_("int") }, /* Integer part of a decimal number */
  { N_("ln") }, /* Natural logarithm */
  { N_("log") }, /* Logarithm with 10 as a base number */
  { N_("not") }, /* Bitwise NOT */
  { N_("or") }, /* Bitwise OR */
  { N_("pi") }, /* PI */
  { N_("pmt") }, /* Periodic payment */
  { N_("pv") }, /* Present value */
  { N_("rand") }, /* A random number */
  { N_("rate") }, /* Periodic interest rate */
  { N_("rcl") }, /* Recall number from memory register */
  { N_("sinh") }, /* Hyperbolic sine */
  { N_("sin") }, /* Sine */
  { N_("sln") }, /* Straight-line depreciation */
  { N_("sqrt") }, /* Square root */
  { N_("sto") }, /* Store number at memory register */
  { N_("syd") }, /* Sum-of-the years'-digits depreciation */
  { N_("tanh") }, /* Hyperbolic tangent */
  { N_("tan") }, /* Tangent */
  { N_("term") }, /* Payment period */
  { N_("u16") }, /* 16-bit unsigned integer value */
  { N_("u32") }, /* 32-bit unsigned integer value */
  { N_("xnor") }, /* Bitwise XNOR */
  { N_("xor") }, /* Bitwise XOR */
  { NULL }
};

static struct word_map *word_map = NULL;

static int 
word_count() {
    int i = 0;

    while (words[i++].word) {
        /* do nothing */;
    }
    i--;

    return(i);
}


void
build_word_map() 
{
    int count, i, size;

    assert(!word_map);

    count = word_count();
    size = sizeof(struct word_map)*(count+1);
    word_map = malloc(size);
    assert(word_map);
    memset(word_map, 0, size);

    for (i = 0; words[i].word; i++) {
        word_map[i].eng.word = words[i].word;
        word_map[i].eng.len = strlen(word_map[i].eng.word);
        word_map[i].native.word = gettext(word_map[i].eng.word);
        word_map[i].native.len = strlen(word_map[i].native.word);
    }
}


static void
replace_str_at(char **str, int loc, int len, char *subst)
{
    char *prefix, *postfix, *target;
    int total;

    if (!len || !subst) {
        return;
    }

    prefix = malloc(loc+1);
    memset(prefix, 0, loc+1);
    memcpy(prefix, *str, loc);
    postfix = *str + loc + len;
  
    total = strlen(prefix) + strlen(subst) + strlen(postfix) +1;
    target = malloc(total);

    snprintf(target, total, "%s%s%s", prefix, subst, postfix);

    free(prefix);
    free(*str);
    *str = target;
}


void
translate_tokens(char **str)
{
    int i = 0, j = 0, len;

    struct loc {
        int i;
        int wlen;
    } loc = { 0, 0};

    assert(word_map);

    len = strlen(*str);

    for (j = 0; (*str)[j]; j++) {
        int left = len - j;
        char *token = *str + j;

        for (i = 0; word_map[i].native.word; i++) {
            int wlen = word_map[i].native.len;

            if ((left >= wlen) && 
	         !strncasecmp(word_map[i].native.word, token, wlen)) {
	        if (loc.wlen < wlen) {
	            loc.i = i;
	            loc.wlen = wlen;
	        }
            }
        }
        if (loc.wlen) {
            replace_str_at(str, j, loc.wlen, word_map[loc.i].eng.word);
            memset(&loc, 0, sizeof(struct loc));
            j = j + strlen(word_map[loc.i].eng.word);
        }
    }
}

