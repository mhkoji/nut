        bits 64
        global nut_eval
        extern car
        extern cdr

        section .text

nut_eval:
        push rbp
        mov rbp, rsp

        call cdr

        mov rdi, rax
        call car

        mov rsp, rbp
        pop rbp
        ret
