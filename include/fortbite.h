#ifndef FORTBITE_H
#define FORTBITE_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <gmp.h>
#include <mpfr.h>

#define FORTBITE_VERSION "1.0.0"
#define DEFAULT_PRECISION 50

typedef struct fortbite_context fortbite_context_t;

int fortbite_init(void);
void fortbite_cleanup(void);
fortbite_context_t* fortbite_context_new(void);
void fortbite_context_free(fortbite_context_t* ctx);

#include "types.h"
#include "math.h"
#include "units.h"
#include "plotting.h"
#include "export.h"
#include "config.h"
#include "plugins.h"

#endif