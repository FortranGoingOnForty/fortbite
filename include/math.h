#ifndef FORTBITE_MATH_H
#define FORTBITE_MATH_H

#include "types.h"

fortbite_token_t* fortbite_tokenize(const char* input);
void fortbite_tokens_free(fortbite_token_t* tokens);

void* fortbite_malloc(size_t size);
void* fortbite_calloc(size_t count, size_t size);
void* fortbite_realloc(void* ptr, size_t size);
void fortbite_free(void* ptr);
char* fortbite_strdup(const char* str);

#ifdef DEBUG
void fortbite_memory_stats(void);
#endif

#endif