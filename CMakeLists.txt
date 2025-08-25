cmake_minimum_required(VERSION 3.12)
project(fortbite VERSION 1.0.0 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Find required packages
find_package(PkgConfig REQUIRED)

# Find GMP and MPFR for arbitrary precision arithmetic
pkg_check_modules(GMP REQUIRED gmp)
pkg_check_modules(MPFR REQUIRED mpfr)

# Find readline for CLI features
find_library(READLINE_LIB readline)
if(NOT READLINE_LIB)
    message(FATAL_ERROR "libreadline not found")
endif()

# Include directories
include_directories(include)
include_directories(${GMP_INCLUDE_DIRS})
include_directories(${MPFR_INCLUDE_DIRS})

# Compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wpedantic")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -DDEBUG")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")

# Source files
set(CORE_SOURCES
    src/core/main.c
    src/core/engine.c
    src/core/types.c
    src/core/memory.c
)

set(PARSER_SOURCES
    src/parser/lexer.c
    src/parser/parser.c
    src/parser/ast.c
    src/parser/syntax.c
)

set(MATH_SOURCES
    src/math/precision.c
    src/math/arithmetic.c
    src/math/complex.c
    src/math/matrix.c
    src/math/functions.c
)

set(UNITS_SOURCES
    src/units/converter.c
    src/units/database.c
    src/units/parser.c
)

set(PLOTTING_SOURCES
    src/plotting/ascii_plot.c
    src/plotting/data.c
    src/plotting/export.c
)

set(EXPORT_SOURCES
    src/export/formats.c
    src/export/csv.c
    src/export/json.c
    src/export/latex.c
)

set(CONFIG_SOURCES
    src/config/config.c
    src/config/defaults.c
    src/config/validation.c
)

set(PLUGINS_SOURCES
    src/plugins/loader.c
    src/plugins/api.c
    src/plugins/registry.c
)

set(CLI_SOURCES
    src/cli/interface.c
    src/cli/history.c
    src/cli/completion.c
    src/cli/display.c
)

set(UTILS_SOURCES
    src/utils/string.c
    src/utils/file.c
    src/utils/debug.c
)

set(ALL_SOURCES
    ${CORE_SOURCES}
    ${PARSER_SOURCES}
    ${MATH_SOURCES}
    ${UNITS_SOURCES}
    ${PLOTTING_SOURCES}
    ${EXPORT_SOURCES}
    ${CONFIG_SOURCES}
    ${PLUGINS_SOURCES}
    ${CLI_SOURCES}
    ${UTILS_SOURCES}
)

# Create executable
add_executable(fortbite ${ALL_SOURCES})

# Link libraries
target_link_libraries(fortbite 
    ${GMP_LIBRARIES}
    ${MPFR_LIBRARIES}
    ${READLINE_LIB}
    dl  # for dynamic loading
    m   # math library
)

# Install target
install(TARGETS fortbite DESTINATION bin)
install(DIRECTORY data/ DESTINATION share/fortbite/data)

# Enable testing
enable_testing()

# Unit tests
file(GLOB TEST_SOURCES tests/unit/*.c)
foreach(test_source ${TEST_SOURCES})
    get_filename_component(test_name ${test_source} NAME_WE)
    add_executable(${test_name} ${test_source} ${ALL_SOURCES})
    target_compile_definitions(${test_name} PRIVATE TESTING)
    target_link_libraries(${test_name} 
        ${GMP_LIBRARIES}
        ${MPFR_LIBRARIES}
        ${READLINE_LIB}
        dl
        m
    )
    add_test(NAME ${test_name} COMMAND ${test_name})
endforeach()