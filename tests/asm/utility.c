#include "utility.h"
#include <stdbool.h>
#include <stdio.h>

val_t add();
val_t sub();

static int test_index = 1;

int dec_width = 20;
int hex_width = 16;
int oct_width = 22;

static val_t flags = 0;
static val_t rax = 0;
static val_t rbx = 0;
static val_t rcx = 0;
static val_t rdx = 0;
static val_t rbp = 0;
static val_t rsp = 0;
static val_t rsi = 0;
static val_t rdi = 0;
static val_t r8 = 0;
static val_t r9 = 0;
static val_t r10 = 0;
static val_t r11 = 0;
static val_t r12 = 0;
static val_t r13 = 0;
static val_t r14 = 0;
static val_t r15 = 0;

void set_flags(val_t fs) { flags = fs; }
void set_rax(val_t v) { rax = v; }
void set_rbx(val_t v) { rbx = v; }
void set_rcx(val_t v) { rcx = v; }
void set_rdx(val_t v) { rdx = v; }
void set_rbp(val_t v) { rbp = v; }
void set_rsp(val_t v) { rsp = v; }
void set_rsi(val_t v) { rsi = v; }
void set_rdi(val_t v) { rdi = v; }
void set_r8 (val_t v) { r8  = v; }
void set_r9 (val_t v) { r9  = v; }
void set_r10(val_t v) { r10 = v; }
void set_r11(val_t v) { r11 = v; }
void set_r12(val_t v) { r12 = v; }
void set_r13(val_t v) { r13 = v; }
void set_r14(val_t v) { r14 = v; }
void set_r15(val_t v) { r15 = v; }

val_t make_mask(int n) {
  val_t result;
  if (n <= -64) {
    return max_unsigned;
  } else if (n < 0) {
    return min_signed >> ((-1 * n) - 1);
  } else if (n >= word_size_bits) {
    return max_unsigned;
  } else {
    return ((val_t)1 << n) - 1;
  }
}

void print_dec(val_t value) {
  printf("\t%*"PRId64, dec_width, value);
}

void print_hex(val_t value) {
  printf("\t0x%0*"PRIx64, hex_width, value);
}

void print_binary(val_t value) {
  int i;
  printf("        ");
  for (i = 63; i >= 0; --i) {
    if ((value >> i) & 1) { printf("1"); } else { printf("0"); }
    if (i && i % 4  == 0) { printf(" "); }
    if (i && i % 8  == 0) { printf(" "); }
    if (i && i % 16 == 0) { printf("  "); }
  }
}

void print_arg(val_t arg) {
  if      (arg == max_signed)   { printf("MAX_SIGNED"); }
  else if (arg == min_signed)   { printf("MIN_SIGNED"); }
  else if (arg == max_unsigned) { printf("MAX_UNSIGNED"); }
  else if (arg == min_unsigned) { printf("MIN_UNSIGNED"); }
  else                          { printf("%"PRId64, arg); }
}

void print_reg(val_t reg) {
  print_dec(reg);
  print_hex(reg);
  print_binary(reg);
  printf("\n");
}

void space(bool print_it) { if (print_it) { printf(", "); } }

void print_flags() {
  printf("flags:\t");
  bool cf = (flags >>  0) & 1;
  bool pf = (flags >>  2) & 1;
  bool af = (flags >>  4) & 1;
  bool zf = (flags >>  6) & 1;
  bool sf = (flags >>  7) & 1;
  bool of = (flags >> 11) & 1;
  bool printed = false;
  int left_pad = dec_width;
  int flag_count = cf+pf+af+zf+sf+of;
  if (flag_count) { left_pad -= 2; }
  for (int i = flag_count - 1; i > 0; --i) { left_pad -= 4; }
  printf("%*s", left_pad, " ");
  if (of) {                 printf("OF"); printed = true; }
  if (sf) { space(printed); printf("SF"); printed = true; }
  if (zf) { space(printed); printf("ZF"); printed = true; }
  if (cf) { space(printed); printf("CF"); printed = true; }
  if (af) { space(printed); printf("AF"); printed = true; }
  if (pf) { space(printed); printf("PF"); printed = true; }
  print_hex(flags);
  print_binary(flags);
  printf("\n");
}

void print_rax() {
  printf("rax:");
  print_reg(rax);
}

void print_rbx() {
  printf("rbx:");
  print_reg(rbx);
}

void print_rcx() {
  printf("rcx:");
  print_reg(rcx);
}

void print_rdx() {
  printf("rdx:");
  print_reg(rdx);
}

void print_rbp() {
  printf("rbp:");
  print_reg(rbp);
}

void print_rsp() {
  printf("rsp:");
  print_reg(rsp);
}

void print_rsi() {
  printf("rsi:");
  print_reg(rsi);
}

void print_rdi() {
  printf("rdi:");
  print_reg(rdi);
}

void print_r8() {
  printf("r8:");
  print_reg(r8);
}

void print_r9() {
  printf("r9:");
  print_reg(r9);
}

void print_r10() {
  printf("r10:");
  print_reg(r10);
}

void print_r11() {
  printf("r11:");
  print_reg(r11);
}

void print_r12() {
  printf("r12:");
  print_reg(r12);
}

void print_r13() {
  printf("r13:");
  print_reg(r13);
}

void print_r14() {
  printf("r14:");
  print_reg(r14);
}

void print_r15() {
  printf("r15:");
  print_reg(r15);
}

void print_results() {
  print_flags();
  print_rax();
  print_rbx();
  print_rcx();
  print_rdx();
  print_rbp();
  print_rsp();
  print_rsi();
  print_rdi();
  print_r8();
  print_r9();
  print_r10();
  print_r11();
  print_r12();
  print_r13();
  print_r14();
  print_r15();
  printf("\n");
}

void test_add(val_t a, val_t b) {
  printf("Test %d: ADD (", test_index++);
  print_arg(a);
  printf(" + ");
  print_arg(b);
  printf(")\n");

  add(a, b);

  print_results();
}

void test_sub(val_t a, val_t b) {
  printf("Test %d: SUB (", test_index++);
  print_arg(a);
  printf(" - ");
  print_arg(b);
  printf(")\n");

  sub(a, b);

  print_results();
}
