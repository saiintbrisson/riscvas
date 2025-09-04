.option norvc
.section .text
.globl _start

_start:
    li a0, 0x10000000
    lla a1, hello_world
    lb a2, (a1)
print:
    sb a2, (a0)
    addi a1, a1, 1
    lb a2, (a1)
    bnez a2, print
exit:
    j exit

.section .rodata
hello_world: .string "Hello, World!\n\0"
