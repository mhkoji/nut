        bits 64
        global nut_eval
        extern is_cons
        extern car
        extern cdr
        extern number_int
        extern make_number
        extern is_symbol
        extern symbol_string
        extern strcmp
        extern exit

        section .data
        plus db "+", 0

        section .text
nut_eval_arg_list:
        ;; TODO
        mov rax, rdi
        ret

apply_plus:
        push rbp
        mov rbp, rsp

        push rdi                ; args
        push 0                  ; retval

.loop:
        mov rdi, [rbp-8]
        call is_cons

        cmp rax, 1
        jne .loop_over

        mov rdi, [rbp-8]
        call car

        mov rdi, rax
        call number_int

        add [rbp-16], rax

        mov rdi, [rbp-8]
        call cdr

        mov [rbp-8], rax

        jmp .loop

.loop_over:
        pop rdi

        call make_number

        mov rsp, rbp
        pop rbp
        ret

nut_eval:
        push rbp
        mov rbp, rsp

        push rdi                ; expr

        mov rdi, [rbp-8]
        call is_cons

        cmp rax, 1
        je .eval_cons

        mov rdi, 1
        call exit

.eval_cons:
        mov rdi, [rbp-8]
        call car
        push rax                ; op

        mov rdi, [rbp-8]
        call cdr

        mov rdi, rax
        call nut_eval_arg_list
        push rax                ; args

        mov rdi, [rbp-16]
        call is_symbol

        cmp rax, 1
        je .eval_cons_op_symbol

        mov rdi, 1
        call exit

.eval_cons_op_symbol:
        mov rdi, [rbp-16]
        call symbol_string
        mov rdi, rax
        mov rsi, plus
        call strcmp

        cmp rax, 0
        je .eval_cons_op_symbol_plus

        mov rdi, 1
        call exit

.eval_cons_op_symbol_plus:
        mov rdi, [rbp-24]
        call apply_plus

        mov rsp, rbp
        pop rbp
        ret
