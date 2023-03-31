  default rel
  section .text

  extern _set_regs
  extern _print_results

  global _entry

_entry:
  mov rax, 0
  add rax, 0
  call _set_regs
  call _print_results
  ret
