.option norvc
.section .text
.globl _start

_start:
    li a0, 0x10000000
    lla a1, hello_world
    jal puts
    lla a1, foo_bar
    jal puts
.exit:
    j .exit

puts:
    mv t0, a1
    lb t1, (t0)
    beqz t1, .L2
.L1:
    sb t1, (a0)
    addi t0, t0, 1
    lb t1, (t0)
    bnez t1, .L1
.L2:
    ret

.section .rodata
hello_world: .string "Hello, World!\n\0"
foo_bar: .string "Foo? Bar!\n\0"
