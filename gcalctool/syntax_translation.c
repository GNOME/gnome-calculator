
/*  $Header$
 *
 *  Copyright (c) 2004-2005 Sami Pietila
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
  { /* Absolute value */                         N_("abs")   },
  { /* Inversion of hyperbolic cosine */         N_("acosh") },
  { /* Inversion of cosine */                    N_("acos" ) },
  { /* Bitwise AND */                            N_("and")   },
  { /* Answer of previous calculation */         N_("ans")   },
  { /* Inversion of hyperbolic sine */           N_("asinh") },
  { /* Hyperbolic sine */                        N_("asin")  },
  { /* Inversion of hyperbolic tangent */        N_("atanh") },
  { /* Inversion of tangent */                   N_("atan")  },
  { /* Cubic root */                             N_("cbrt")  },
  { /* Change sign */                            N_("chs")   },
  { /* Clear (display) */                        N_("clr")   },
  { /* Hyperbolic cosine */                      N_("cosh")  },
  { /* Cosine */                                 N_("cos")   },
  { /* Double-declining deprecation */           N_("ddb")   },
  { /* x*10^y */                                 N_("exp")   },
  { /* Fraction of a decimal number */           N_("frac")  },
  { /* Future value */                           N_("fv")    },
  { /* Integer part of a decimal number */       N_("int")   },
  { /* Natural logarithm */                      N_("ln")    },
  { /* Logarithm with 10 as a base number */     N_("log")   },
  { /* Bitwise NOT */                            N_("not")   },
  { /* Bitwise OR */                             N_("or")    },
  { /* PI */                                     N_("pi")    },
  { /* Periodic payment */                       N_("pmt")   },
  { /* Present value */                          N_("pv")    },
  { /* A random number */                        N_("rand")  },
  { /* Periodic interest rate */                 N_("rate")  },
  { /* Recall number from memory register */     N_("rcl")   },
  { /* Hyperbolic sine */                        N_("sinh")  },
  { /* Sine */                                   N_("sin")   },
  { /* Straight-line depreciation */             N_("sln")   },
  { /* Square root */                            N_("sqrt")  },
  { /* Store number at memory register */        N_("sto")   },
  { /* Sum-of-the years'-digits depreciation */  N_("syd")   },
  { /* Hyperbolic tangent */                     N_("tanh")  },
  { /* Tangent */                                N_("tan")   },
  { /* Payment period */                         N_("term")  },
  { /* 16-bit unsigned integer value */          N_("u16")   },
  { /* 32-bit unsigned integer value */          N_("u32")   },
  { /* Bitwise XNOR */                           N_("xnor")  },
  { /* Bitwise XOR */                            N_("xor")   },
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

