  default rel
  section .text
  ;; extern _flag_printer
  extern _set_flags
  global _add
  ;; global _get_flags

_add:
  mov rax, rdi
  add rax, rsi
  call _set_flags
  ret

_sub:
  mov rax, rdi
  sub rax, rsi
  call _set_flags
  ret

;; _get_flags:
;;   pushf
;;   pop rax
;;   ret

;; _print_flags:
;;   push rax
;;   pushf
;;   call _flag_printer
;;   popf
;;   pop rax
;;   ret
