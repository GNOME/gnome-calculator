#ifndef SYNTAX_TRANSLATION_C
#define SYNTAX_TRANSLATION_C

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
  { N_("abs")},
  { N_("acosh")},
  { N_("acos")},
  { N_("and")},
  { N_("ans")}, 
  { N_("asinh")},
  { N_("asin")}, 
  { N_("atanh")},
  { N_("atan")},
  { N_("cbrt")},
  { N_("chs")}, 
  { N_("clr")}, 
  { N_("cosh")},
  { N_("cos")}, 
  { N_("ddb")}, 
  { N_("exp")}, 
  { N_("frac")}, 
  { N_("fv")}, 
  { N_("int")},
  { N_("ln")}, 
  { N_("log")}, 
  { N_("not")}, 
  { N_("or")}, 
  { N_("pi")}, 
  { N_("pmt")},
  { N_("pv")}, 
  { N_("rand")},
  { N_("rate")},
  { N_("rcl")}, 
  { N_("sinh")},
  { N_("sin")}, 
  { N_("sln")}, 
  { N_("sqrt")},
  { N_("sto")}, 
  { N_("syd")}, 
  { N_("tanh")},
  { N_("tan")},
  { N_("term")},
  { N_("u16")}, 
  { N_("u32")}, 
  { N_("xnor")},
  { N_("xor")},
  { NULL}
};

struct word_map *word_map = NULL;

int 
word_count() {
  int i = 0;
  while (words[i++].word);
  i--;
  return i;
}

void
build_word_map() 
{
  assert(!word_map);

  int count = word_count();
  int size = sizeof(struct word_map)*(count+1);
  word_map = malloc(size);
  assert(word_map);
  memset(word_map, 0, size);

  int i;
  for (i = 0; words[i].word; i++) {
    word_map[i].eng.word = words[i].word;
    word_map[i].eng.len = strlen(word_map[i].eng.word);
    word_map[i].native.word = gettext(word_map[i].eng.word);
    word_map[i].native.len = strlen(word_map[i].native.word);
  }
}

void
replace_str_at(char **str, 
	       int loc,
	       int len,
	       char *subst)
{
  if (!len || !subst) return;
  
  char *prefix = malloc(loc+1);
  memset(prefix, 0, loc+1);
  memcpy(prefix, *str, loc);
  char *postfix = *str + loc + len;
  
  int total = strlen(prefix) + strlen(subst) + strlen(postfix) +1;
  char *target = malloc(total);
  
  snprintf(target, total, "%s%s%s", prefix, subst, postfix);

  free(prefix);
  free(*str);
  *str = target;

}


void
translate_tokens(char **str)
{
  struct loc {
    int i;
    int wlen;
  } loc = { 0, 0};

  assert(word_map);

  int i = 0;
  int j = 0;
  int len = strlen(*str);

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

#endif
