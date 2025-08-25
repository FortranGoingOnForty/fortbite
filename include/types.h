#ifndef TYPES_H
#define TYPES_H

#include <gmp.h>
#include <mpfr.h>
#include <stddef.h>

typedef enum {
    FORTBITE_TYPE_SCALAR,
    FORTBITE_TYPE_COMPLEX,
    FORTBITE_TYPE_MATRIX,
    FORTBITE_TYPE_UNIT,
    FORTBITE_TYPE_FUNCTION,
    FORTBITE_TYPE_UNDEFINED
} fortbite_type_t;

typedef struct {
    mpfr_t value;
    int precision;
} fortbite_scalar_t;

typedef struct {
    fortbite_scalar_t real;
    fortbite_scalar_t imag;
} fortbite_complex_t;

typedef struct {
    size_t rows;
    size_t cols;
    fortbite_complex_t** data;
    bool is_sparse;
} fortbite_matrix_t;

typedef struct fortbite_unit {
    char* name;
    char* symbol;
    double factor;
    int dimension[7];  // SI base units: m, kg, s, A, K, mol, cd
    struct fortbite_unit* base_unit;
} fortbite_unit_t;

typedef struct {
    fortbite_complex_t value;
    fortbite_unit_t* unit;
} fortbite_quantity_t;

typedef struct {
    fortbite_type_t type;
    union {
        fortbite_scalar_t scalar;
        fortbite_complex_t complex;
        fortbite_matrix_t matrix;
        fortbite_quantity_t quantity;
    } data;
} fortbite_value_t;

typedef struct fortbite_variable {
    char* name;
    fortbite_value_t value;
    struct fortbite_variable* next;
} fortbite_variable_t;

typedef enum {
    TOKEN_NUMBER,
    TOKEN_IDENTIFIER,
    TOKEN_OPERATOR,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_SEMICOLON,
    TOKEN_COMMA,
    TOKEN_ASSIGN,
    TOKEN_PRECISION,
    TOKEN_UNIT,
    TOKEN_EOF,
    TOKEN_ERROR
} fortbite_token_type_t;

typedef struct {
    fortbite_token_type_t type;
    char* value;
    size_t length;
    size_t position;
} fortbite_token_t;

fortbite_value_t* fortbite_value_new(fortbite_type_t type);
void fortbite_value_free(fortbite_value_t* value);
fortbite_value_t* fortbite_value_copy(const fortbite_value_t* value);

void fortbite_scalar_new_init(fortbite_scalar_t* scalar, int precision);
void fortbite_scalar_clear(fortbite_scalar_t* scalar);
void fortbite_scalar_set_d(fortbite_scalar_t* scalar, double value);
void fortbite_scalar_set_str(fortbite_scalar_t* scalar, const char* str, int base);
void fortbite_scalar_set(fortbite_scalar_t* dest, const fortbite_scalar_t* src);

void fortbite_complex_new_init(fortbite_complex_t* complex, int precision);
void fortbite_complex_clear(fortbite_complex_t* complex);
void fortbite_complex_set(fortbite_complex_t* dest, const fortbite_complex_t* src);

fortbite_matrix_t fortbite_matrix_new(size_t rows, size_t cols, int precision);
void fortbite_matrix_clear(fortbite_matrix_t* matrix);
fortbite_matrix_t fortbite_matrix_copy(const fortbite_matrix_t* src);
void fortbite_matrix_free(fortbite_matrix_t* matrix);
fortbite_complex_t* fortbite_matrix_get(const fortbite_matrix_t* matrix, size_t row, size_t col);
void fortbite_matrix_set(fortbite_matrix_t* matrix, size_t row, size_t col, const fortbite_complex_t* value);

fortbite_variable_t* fortbite_variable_new(const char* name, const fortbite_value_t* value);
void fortbite_variable_free(fortbite_variable_t* var);

#endif