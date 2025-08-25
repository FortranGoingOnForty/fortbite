#include "fortbite.h"
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

struct fortbite_context {
    fortbite_variable_t* variables;
    int default_precision;
    bool verbose;
};

fortbite_context_t* fortbite_context_new(void) {
    fortbite_context_t* ctx = fortbite_malloc(sizeof(fortbite_context_t));
    ctx->variables = NULL;
    ctx->default_precision = DEFAULT_PRECISION;
    ctx->verbose = false;
    return ctx;
}

void fortbite_context_free(fortbite_context_t* ctx) {
    if (!ctx) return;
    
    fortbite_variable_t* var = ctx->variables;
    while (var) {
        fortbite_variable_t* next = var->next;
        fortbite_variable_free(var);
        var = next;
    }
    
    fortbite_free(ctx);
}

static void print_banner(void) {
    printf("FORTBITE v%s - High Precision Calculator\n", FORTBITE_VERSION);
    printf("Type 'help' for commands or 'exit' to quit.\n");
    printf("Use :: for precision specification (e.g., 3.14159::100)\n");
    printf("Use := for variable assignment (e.g., x := 42)\n\n");
}

static void print_help(void) {
    printf("FORTBITE Commands:\n");
    printf("  Basic arithmetic: +, -, *, /, ^ (power), %% (modulo)\n");
    printf("  Variables: x := value\n");
    printf("  Precision: value::digits\n");
    printf("  Complex: 3+4i, cis(angle)\n");
    printf("  Functions: sin, cos, tan, log, exp, sqrt, abs\n");
    printf("  Constants: pi, e, i\n");
    printf("  Commands: help, exit, clear\n");
    printf("  Arrow keys: command history\n\n");
}

static bool process_command(fortbite_context_t* ctx, const char* input) {
    if (!input || strlen(input) == 0) {
        return true;
    }
    
    char* line = fortbite_strdup(input);
    char* trimmed = line;
    
    while (*trimmed && isspace(*trimmed)) trimmed++;
    
    if (strcmp(trimmed, "exit") == 0 || strcmp(trimmed, "quit") == 0) {
        fortbite_free(line);
        return false;
    }
    
    if (strcmp(trimmed, "help") == 0) {
        print_help();
        fortbite_free(line);
        return true;
    }
    
    if (strcmp(trimmed, "clear") == 0) {
        system("clear");
        fortbite_free(line);
        return true;
    }
    
    if (strncmp(trimmed, "precision", 9) == 0) {
        char* precision_str = trimmed + 9;
        while (*precision_str && isspace(*precision_str)) precision_str++;
        if (*precision_str) {
            int precision = atoi(precision_str);
            if (precision > 0 && precision <= 10000) {
                ctx->default_precision = precision;
                printf("Default precision set to %d bits\n", precision);
            } else {
                printf("Invalid precision. Use 1-10000.\n");
            }
        } else {
            printf("Current default precision: %d bits\n", ctx->default_precision);
        }
        fortbite_free(line);
        return true;
    }
    
    fortbite_token_t* tokens = fortbite_tokenize(trimmed);
    if (!tokens) {
        printf("Error: Failed to tokenize input\n");
        fortbite_free(line);
        return true;
    }
    
    if (tokens[0].type == TOKEN_ERROR) {
        printf("Error: Invalid token '%s' at position %zu\n", 
               tokens[0].value ? tokens[0].value : "unknown", tokens[0].position);
    } else {
        printf("Parsed tokens: ");
        for (size_t i = 0; tokens[i].type != TOKEN_EOF; i++) {
            printf("[%s:%s] ", 
                   tokens[i].type == TOKEN_NUMBER ? "NUM" :
                   tokens[i].type == TOKEN_IDENTIFIER ? "ID" :
                   tokens[i].type == TOKEN_OPERATOR ? "OP" :
                   tokens[i].type == TOKEN_ASSIGN ? "ASSIGN" :
                   tokens[i].type == TOKEN_PRECISION ? "PREC" : "OTHER",
                   tokens[i].value ? tokens[i].value : "");
        }
        printf("\n");
    }
    
    fortbite_tokens_free(tokens);
    fortbite_free(line);
    return true;
}

int fortbite_init(void) {
    mpfr_set_default_prec(DEFAULT_PRECISION);
    return 0;
}

void fortbite_cleanup(void) {
    mpfr_free_cache();
}

static void setup_readline(void) {
    rl_bind_key('\t', rl_complete);
}

int main(void) {
    if (fortbite_init() != 0) {
        fprintf(stderr, "Failed to initialize FORTBITE\n");
        return EXIT_FAILURE;
    }
    
    fortbite_context_t* ctx = fortbite_context_new();
    setup_readline();
    print_banner();
    
    char* input;
    bool running = true;
    
    while (running && (input = readline("fortbite> ")) != NULL) {
        if (strlen(input) > 0) {
            add_history(input);
        }
        
        running = process_command(ctx, input);
        free(input);
    }
    
    if (!input) {
        printf("\n");
    }
    
    printf("Goodbye!\n");
    
    fortbite_context_free(ctx);
    fortbite_cleanup();
    
#ifdef DEBUG
    fortbite_memory_stats();
#endif
    
    return EXIT_SUCCESS;
}