#ifndef SYNTAX_TRANSLATION_H
#define SYNTAX_TRANSLATION_H

#ifndef NULL
#define NULL 0
#endif

struct word_map {
  struct map {
    char *word;
    char len;
  } eng, native;
};


// Initialization of word map
void
build_word_map();

// Translates native tokens into english tokens
void
translate_tokens(char **str);

#endif
