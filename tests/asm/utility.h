#ifndef UTILITY_H
#define UTILITY_H

#include <inttypes.h>

typedef int64_t val_t;

static val_t word_size_bytes = 8;
static val_t word_size_bits = 64;
static val_t max_signed   = 0x7fffffffffffffff;
static val_t min_signed   = 0x8000000000000000;
static val_t max_unsigned = 0xffffffffffffffff;
static val_t min_unsigned = 0x0000000000000000;

void test_add(val_t a, val_t b);
void test_sub(val_t a, val_t b);

#endif
