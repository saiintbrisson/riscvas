.option norvc

.L0:
mv a0, a0
lla a1, mythingykkk
j .L0
lla a1, mythingykkk
.local L0
j L1

.section .text

mv a1, a1

.section bundamole

mv a2, a2
.globl mythingykkk
mythingykkk: .string "Bla?\0"
.globl mythingykkk
.local mythingykkk
.globl mythingykkk

.text 0
mv s2, s2

.section .text
mv s1, s1

.section .bss
mv s0, s0

.section .data
mv s0, s0

.section .text
mv s0, s0

.section .text.bla
mv s0, s0


.text 2
mv s3, s3

.text 1
mv s2, s2

.section .shstrtab
mv s0, s0

.section .rodata
mv s0, s0

