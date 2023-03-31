  default rel
  section .text

  extern _set_flags
  extern _set_rax
  extern _set_rbx
  extern _set_rcx
  extern _set_rdx
  extern _set_rbp
  extern _set_rsp
  extern _set_rsi
  extern _set_rdi
  extern _set_r8
  extern _set_r9
  extern _set_r10
  extern _set_r11
  extern _set_r12
  extern _set_r13
  extern _set_r14
  extern _set_r15

  global _set_regs
  global _add
  global _sub

_set_regs:
  push rdi
  ;; Set rax first, because subsequent calls will clobber it.
  mov rdi, rax
  call _set_rax

  ;; Set the flags.
  pushf
  pop rdi
  call _set_flags

  ;; Set the remaining registers.
  mov rdi, rbx
  call _set_rbx
  mov rdi, rcx
  call _set_rcx
  mov rdi, rdx
  call _set_rdx
  mov rdi, rbp
  call _set_rbp
  mov rdi, rsp
  call _set_rsp
  mov rdi, rsi
  call _set_rsi
  mov rdi, rdi
  call _set_rdi
  mov rdi, r8
  call _set_r8
  mov rdi, r9
  call _set_r9
  mov rdi, r10
  call _set_r10
  mov rdi, r11
  call _set_r11
  mov rdi, r12
  call _set_r12
  mov rdi, r13
  call _set_r13
  mov rdi, r14
  call _set_r14
  mov rdi, r15
  call _set_r15

  pop rdi
  ret

_add:
  mov rax, rdi
  add rax, rsi
  call _set_regs
  ret

_sub:
  mov rax, rdi
  sub rax, rsi
  call _set_regs
  ret
