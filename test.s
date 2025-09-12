.option norvc

.L0:
mv a0, a0
lla a1, t0 + 1000000000
lla a1, 1000000000
lb t2, (t0) - 1
lb t1, (1) + t3
lb t1, 1(t4)
j .L0 + 1
sb t2, (t0)
sb t2, 1(t0)

# auipc a0, %pcrel_hi(msg + 1)
# addi  a0, a0, %pcrel_lo(.L0)

msg:

# lla a1, mythingykkk
.local L0
# j L1

# .section .text

# mv a1, a1

# .section bundamole

# mv a2, a2
# .globl mythingykkk
# mythingykkk: .string "Bla?\0"
# .globl mythingykkk
# .local mythingykkk
# .globl mythingykkk

# .text 0
# mv s2, s2

# .section .text
# mv s1, s1

# .section .bss
# mv s0, s0

# .section .data
# mv s0, s0

# .section .text
# mv s0, s0

# .section .text.bla
# mv s0, s0


# .text 2
# mv s3, s3

# .text 1
# mv s2, s2

# .section .shstrtab
# mv s0, s0

# .section .rodata
# mv s0, s0

