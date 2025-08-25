#include "fortbite.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef DEBUG
static size_t memory_allocated = 0;
static size_t memory_freed = 0;
#endif

void* fortbite_malloc(size_t size) {
    void* ptr = malloc(size);
    if (!ptr) {
        fprintf(stderr, "FORTBITE: Memory allocation failed for %zu bytes\n", size);
        exit(EXIT_FAILURE);
    }
    
#ifdef DEBUG
    memory_allocated += size;
#endif
    
    return ptr;
}

void* fortbite_calloc(size_t count, size_t size) {
    void* ptr = calloc(count, size);
    if (!ptr) {
        fprintf(stderr, "FORTBITE: Memory allocation failed for %zu * %zu bytes\n", count, size);
        exit(EXIT_FAILURE);
    }
    
#ifdef DEBUG
    memory_allocated += count * size;
#endif
    
    return ptr;
}

void* fortbite_realloc(void* ptr, size_t size) {
    void* new_ptr = realloc(ptr, size);
    if (!new_ptr && size > 0) {
        fprintf(stderr, "FORTBITE: Memory reallocation failed for %zu bytes\n", size);
        exit(EXIT_FAILURE);
    }
    
    return new_ptr;
}

void fortbite_free(void* ptr) {
    if (ptr) {
        free(ptr);
#ifdef DEBUG
        memory_freed++;
#endif
    }
}

char* fortbite_strdup(const char* str) {
    if (!str) return NULL;
    
    size_t len = strlen(str) + 1;
    char* copy = fortbite_malloc(len);
    memcpy(copy, str, len);
    return copy;
}

#ifdef DEBUG
void fortbite_memory_stats(void) {
    printf("Memory allocated: %zu bytes\n", memory_allocated);
    printf("Memory freed: %zu objects\n", memory_freed);
}
#endif