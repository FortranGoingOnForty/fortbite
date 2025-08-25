#include "fortbite.h"

fortbite_value_t* fortbite_value_new(fortbite_type_t type) {
    fortbite_value_t* value = fortbite_malloc(sizeof(fortbite_value_t));
    value->type = type;
    
    switch (type) {
        case FORTBITE_TYPE_SCALAR:
            fortbite_scalar_new_init(&value->data.scalar, DEFAULT_PRECISION);
            break;
        case FORTBITE_TYPE_COMPLEX:
            fortbite_complex_new_init(&value->data.complex, DEFAULT_PRECISION);
            break;
        case FORTBITE_TYPE_MATRIX:
            value->data.matrix.rows = 0;
            value->data.matrix.cols = 0;
            value->data.matrix.data = NULL;
            value->data.matrix.is_sparse = false;
            break;
        case FORTBITE_TYPE_UNIT:
            value->data.quantity.unit = NULL;
            fortbite_complex_new_init(&value->data.quantity.value, DEFAULT_PRECISION);
            break;
        default:
            break;
    }
    
    return value;
}

void fortbite_value_free(fortbite_value_t* value) {
    if (!value) return;
    
    switch (value->type) {
        case FORTBITE_TYPE_SCALAR:
            fortbite_scalar_clear(&value->data.scalar);
            break;
        case FORTBITE_TYPE_COMPLEX:
            fortbite_complex_clear(&value->data.complex);
            break;
        case FORTBITE_TYPE_MATRIX:
            fortbite_matrix_clear(&value->data.matrix);
            break;
        case FORTBITE_TYPE_UNIT:
            fortbite_complex_clear(&value->data.quantity.value);
            break;
        default:
            break;
    }
    
    fortbite_free(value);
}

fortbite_value_t* fortbite_value_copy(const fortbite_value_t* value) {
    if (!value) return NULL;
    
    fortbite_value_t* copy = fortbite_value_new(value->type);
    
    switch (value->type) {
        case FORTBITE_TYPE_SCALAR:
            fortbite_scalar_set(&copy->data.scalar, &value->data.scalar);
            break;
        case FORTBITE_TYPE_COMPLEX:
            fortbite_complex_set(&copy->data.complex, &value->data.complex);
            break;
        case FORTBITE_TYPE_MATRIX:
            if (value->data.matrix.data) {
                copy->data.matrix = fortbite_matrix_copy(&value->data.matrix);
            }
            break;
        case FORTBITE_TYPE_UNIT:
            fortbite_complex_set(&copy->data.quantity.value, &value->data.quantity.value);
            copy->data.quantity.unit = value->data.quantity.unit;
            break;
        default:
            break;
    }
    
    return copy;
}

void fortbite_scalar_new_init(fortbite_scalar_t* scalar, int precision) {
    mpfr_init2(scalar->value, precision);
    scalar->precision = precision;
}

void fortbite_scalar_clear(fortbite_scalar_t* scalar) {
    mpfr_clear(scalar->value);
}

void fortbite_scalar_set_d(fortbite_scalar_t* scalar, double value) {
    mpfr_set_d(scalar->value, value, MPFR_RNDN);
}

void fortbite_scalar_set_str(fortbite_scalar_t* scalar, const char* str, int base) {
    mpfr_set_str(scalar->value, str, base, MPFR_RNDN);
}

void fortbite_scalar_set(fortbite_scalar_t* dest, const fortbite_scalar_t* src) {
    mpfr_set(dest->value, src->value, MPFR_RNDN);
    dest->precision = src->precision;
}

void fortbite_complex_new_init(fortbite_complex_t* complex, int precision) {
    fortbite_scalar_new_init(&complex->real, precision);
    fortbite_scalar_new_init(&complex->imag, precision);
}

void fortbite_complex_clear(fortbite_complex_t* complex) {
    fortbite_scalar_clear(&complex->real);
    fortbite_scalar_clear(&complex->imag);
}

void fortbite_complex_set(fortbite_complex_t* dest, const fortbite_complex_t* src) {
    fortbite_scalar_set(&dest->real, &src->real);
    fortbite_scalar_set(&dest->imag, &src->imag);
}

fortbite_matrix_t fortbite_matrix_new(size_t rows, size_t cols, int precision) {
    fortbite_matrix_t matrix;
    matrix.rows = rows;
    matrix.cols = cols;
    matrix.is_sparse = false;
    
    if (rows > 0 && cols > 0) {
        matrix.data = fortbite_malloc(rows * sizeof(fortbite_complex_t*));
        for (size_t i = 0; i < rows; i++) {
            matrix.data[i] = fortbite_malloc(cols * sizeof(fortbite_complex_t));
            for (size_t j = 0; j < cols; j++) {
                fortbite_complex_new_init(&matrix.data[i][j], precision);
            }
        }
    } else {
        matrix.data = NULL;
    }
    
    return matrix;
}

void fortbite_matrix_clear(fortbite_matrix_t* matrix) {
    if (matrix->data) {
        for (size_t i = 0; i < matrix->rows; i++) {
            for (size_t j = 0; j < matrix->cols; j++) {
                fortbite_complex_clear(&matrix->data[i][j]);
            }
            fortbite_free(matrix->data[i]);
        }
        fortbite_free(matrix->data);
        matrix->data = NULL;
    }
    matrix->rows = 0;
    matrix->cols = 0;
}

fortbite_matrix_t fortbite_matrix_copy(const fortbite_matrix_t* src) {
    fortbite_matrix_t copy = fortbite_matrix_new(src->rows, src->cols, DEFAULT_PRECISION);
    copy.is_sparse = src->is_sparse;
    
    if (src->data) {
        for (size_t i = 0; i < src->rows; i++) {
            for (size_t j = 0; j < src->cols; j++) {
                fortbite_complex_set(&copy.data[i][j], &src->data[i][j]);
            }
        }
    }
    
    return copy;
}

fortbite_complex_t* fortbite_matrix_get(const fortbite_matrix_t* matrix, size_t row, size_t col) {
    if (!matrix->data || row >= matrix->rows || col >= matrix->cols) {
        return NULL;
    }
    return &matrix->data[row][col];
}

void fortbite_matrix_set(fortbite_matrix_t* matrix, size_t row, size_t col, const fortbite_complex_t* value) {
    if (!matrix->data || row >= matrix->rows || col >= matrix->cols) {
        return;
    }
    fortbite_complex_set(&matrix->data[row][col], value);
}

fortbite_variable_t* fortbite_variable_new(const char* name, const fortbite_value_t* value) {
    fortbite_variable_t* var = fortbite_malloc(sizeof(fortbite_variable_t));
    var->name = fortbite_strdup(name);
    if (value) {
        var->value = *fortbite_value_copy(value);
    } else {
        var->value.type = FORTBITE_TYPE_UNDEFINED;
    }
    var->next = NULL;
    return var;
}

void fortbite_variable_free(fortbite_variable_t* var) {
    if (!var) return;
    
    fortbite_free(var->name);
    fortbite_value_free(&var->value);
    fortbite_free(var);
}