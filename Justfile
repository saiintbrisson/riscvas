toolchain := "/opt/riscv/bin"

clean:
    rm -r out
    cargo clean

build in="example.s":
    cargo r -- {{in}} out/{{in}}.rel.o

link in: (build in)
    {{toolchain}}/riscv32-unknown-elf-ld out/{{in}}.rel.o -Ttext=0x80000000 -o out/{{in}}.o

run in="example.s": (link in)
    qemu-system-riscv32 -machine virt -bios out/{{in}}.o -nographic

dump in="example.s": (build in)
    {{toolchain}}/riscv32-unknown-elf-objdump -xD out/{{in}}.rel.o --special-syms
