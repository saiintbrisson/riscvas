build:
    cargo r -- example.s out/relobj.o
    /opt/riscv/bin/riscv32-unknown-elf-ld out/relobj.o -Ttext=0x80000000 -o out/obj.o

run: build
    qemu-system-riscv32 -machine virt -bios out/obj.o -nographic

