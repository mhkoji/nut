        bits 64
        global nut_eval
        extern is_cons
        extern car
        extern cdr
        extern number_int
        extern make_number

        section .text

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

        call cdr

        mov rdi, rax
        call apply_plus

        mov rsp, rbp
        pop rbp
        ret
