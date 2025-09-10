use std::{collections::HashMap, path::Path};

use bytes::{BufMut, BytesMut};
use section::SectionHeader;
use symbol::{ElfSymbol, SymbolBinding, SymbolType};

use crate::assembler::Assembler;

struct ElfFile {
    sections: Vec<SectionHeader>,
    symbols: Vec<ElfSymbol>,

    shstrtab: Vec<u8>,
    strtab: Vec<u8>,
}

impl ElfFile {
    fn new() -> Self {
        Self {
            sections: vec![SectionHeader::default()],
            symbols: vec![ElfSymbol::default()],
            shstrtab: vec![0],
            strtab: vec![0],
        }
    }

    fn add_section(&mut self, sh: SectionHeader) -> usize {
        self.sections.push(sh);
        self.sections.len() - 1
    }

    fn add_symbol(&mut self, name: &str, mut sym: ElfSymbol) {
        sym.name = self.strtab.len() as u32;

        self.strtab.extend(name.as_bytes());
        self.strtab.put_u8(0u8);

        self.symbols.push(sym);
    }

    fn add_section_str(&mut self, str: &str) -> usize {
        self.shstrtab.extend(str.as_bytes());
        self.shstrtab.put_u8(0u8);
        self.shstrtab.len() - str.as_bytes().len() - 1
    }
}

pub mod elf_header {
    pub const EI_MAG: [u8; 4] = *b"\x7FELF";
    pub const EI_VERSION: u8 = 1;
    pub const EI_OSABI: u8 = 0;
    pub const EI_ABIVERSION: u8 = 0;
    pub const EI_PAD: [u8; 7] = [0; 7];

    pub const E_MACHINE: u16 = 0xF3;
    pub const E_VERSION: u32 = 1;

    #[repr(u8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ElfData {
        LittleEndian = 1,
        BigEndian = 2,
    }

    #[repr(u8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ElfClass {
        C32 = 1,
        C64 = 2,
    }

    impl ElfClass {
        pub fn ei_class(&self) -> u8 {
            match self {
                Self::C32 => 1,
                Self::C64 => 2,
            }
        }

        pub fn e_ehsize(&self) -> u16 {
            match self {
                Self::C32 => 52,
                Self::C64 => 64,
            }
        }

        pub fn e_phentsize(&self) -> u16 {
            match self {
                Self::C32 => 32,
                Self::C64 => 56,
            }
        }

        pub fn e_shentsize(&self) -> u16 {
            match self {
                Self::C32 => 40,
                Self::C64 => 64,
            }
        }

        pub fn sym_entsize(&self) -> usize {
            match self {
                ElfClass::C32 => 16,
                ElfClass::C64 => 24,
            }
        }

        pub fn rel_entsize(&self) -> usize {
            match self {
                ElfClass::C32 => 8,
                ElfClass::C64 => 16,
            }
        }

        pub fn rela_entsize(&self) -> usize {
            match self {
                ElfClass::C32 => 12,
                ElfClass::C64 => 24,
            }
        }
    }

    #[repr(u16)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ElfType {
        None = 0x00,
        Rel = 0x01,
        Exec = 0x02,
        Dyn = 0x03,
        Core = 0x04,
    }

    mod elf_flags {
        /// On merger, set RVC if any input file has it set.
        pub const EF_RISCV_RVC: u32 = 0x01;
        /// This is the default float ABI. If none other flag is set, soft is assumed.
        /// On merger, report error if the ABI has different values.
        pub const EF_RISCV_FLOAT_ABI_SOFT: u32 = 0x00;
        pub const EF_RISCV_FLOAT_ABI_SINGLE: u32 = 0x02;
        pub const EF_RISCV_FLOAT_ABI_DOUBLE: u32 = 0x04;
        pub const EF_RISCV_FLOAT_ABI_QUAD: u32 = 0x06;
        /// On merger, report error if the ABI has different values.
        pub const EF_RISCV_RVE: u32 = 0x08;
        /// On merger, set TSO if any input file has it set.
        pub const EF_RISCV_TSO: u32 = 0x10;
        pub const EF_RISCV_RV64ILP32: u32 = 0x20;
        /// Reserved bits for future use, should be left unset.
        pub const EF_RESERVED: u32 = 0x00FFFFE0;
        /// Non-standard extension flags.
        pub const EF_EXTENSION: u32 = 0xFF000000;

        /// This is the mask for the float ABI. Apply it to e_flags and test against the float flags.
        pub const EF_RISCV_FLOAT_ABI: u32 = 0x06;
    }

    #[derive(Debug)]
    pub struct ElfHeader {
        pub class: ElfClass,
        pub data: ElfData,
        pub r#type: ElfType,
        pub entry: u64,
        pub phoff: u64,
        pub shoff: u64,
        pub flags: u32,
        pub phnum: u16,
        pub shnum: u16,
        pub shstrndx: u16,
    }
}

pub mod program_header {
    #[repr(u32)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ProgramType {
        /// Program header table entry unused.
        Null = 0x00000000,
        /// Loadable segment.
        Load = 0x00000001,
        /// Dynamic linking information.
        Dynamic = 0x00000002,
        /// Interpreter information.
        Interp = 0x00000003,
        /// Auxiliary information.
        Note = 0x00000004,
        /// Reserved.
        Shlib = 0x00000005,
        /// Segment containing program header table itself.
        Phdr = 0x00000006,
        /// Thread-Local Storage template.
        Tls = 0x00000007,
    }

    pub mod program_flags {
        /// Executable segment.
        pub const PF_X: u32 = 0x01;
        /// Writable segment.
        pub const PF_W: u32 = 0x02;
        /// Readable segment.
        pub const PF_R: u32 = 0x04;
    }

    #[derive(Debug)]
    pub struct ProgramHeader {
        pub r#type: ProgramType,
        pub flags: u32,
        pub offset: u64,
        pub vaddr: u64,
        pub paddr: u64,
        pub filesz: u64,
        pub memsz: u64,
        pub align: u64,
    }
}

pub mod section {
    #[repr(u32)]
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub enum SectionType {
        /// Section header table entry unused
        #[default]
        Null = 0x0,
        /// Program data
        Progbits = 0x1,
        /// Symbol table
        Symtab = 0x2,
        /// String table
        Strtab = 0x3,
        /// Relocation entries with addends
        Rela = 0x4,
        /// Symbol hash table
        Hash = 0x5,
        /// Dynamic linking information
        Dynamic = 0x6,
        /// Notes
        Note = 0x7,
        /// Program space with no data (bss)
        Nobits = 0x8,
        /// Relocation entries, no addends
        Rel = 0x9,
        /// Reserved
        Shlib = 0x0A,
        /// Dynamic linker symbol table
        Dynsym = 0x0B,
        /// Array of constructors
        InitArray = 0x0E,
        /// Array of destructors
        FiniArray = 0x0F,
        /// Array of pre-constructors
        PreinitArray = 0x10,
        /// Section group
        Group = 0x11,
        /// Extended section indices
        SymtabShndx = 0x12,
        /// Number of defined types.
        Num = 0x13,
    }

    pub mod section_flags {
        /// Writable
        pub const SHF_WRITE: u64 = 0x1;
        /// Occupies memory during execution
        pub const SHF_ALLOC: u64 = 0x2;
        /// Executable
        pub const SHF_EXECINSTR: u64 = 0x4;
        /// Might be merged
        pub const SHF_MERGE: u64 = 0x10;
        /// Contains null-terminated strings
        pub const SHF_STRINGS: u64 = 0x20;
        /// 'sh_info' contains SHT index
        pub const SHF_INFO_LINK: u64 = 0x40;
        /// Preserve order after combining
        pub const SHF_LINK_ORDER: u64 = 0x80;
        /// Non-standard OS specific handling required
        pub const SHF_OS_NONCONFORMING: u64 = 0x100;
        /// Section is member of a group
        pub const SHF_GROUP: u64 = 0x200;
        /// Section hold thread-local data
        pub const SHF_TLS: u64 = 0x400;
    }

    #[derive(Clone, Debug, Default)]
    pub struct SectionHeader {
        pub name: u32,
        pub r#type: SectionType,
        pub flags: u64,
        pub addr: u64,
        pub offset: u64,
        pub size: u64,
        pub link: u32,
        pub info: u32,
        pub addralign: u64,
        pub entsize: u64,
    }
}

pub mod symbol {
    #[derive(Debug, Default)]
    pub struct ElfSymbol {
        pub name: u32,
        pub value: u64,
        pub size: u64,
        pub info: (SymbolType, SymbolBinding),
        pub other: u8,
        pub shndx: u16,
    }

    #[repr(u8)]
    #[derive(Clone, Copy, Debug, Default)]
    pub enum SymbolType {
        /// The symbol's type is not defined
        #[default]
        NoType = 0,
        /// The symbol is associated with a data object.
        Object = 1,
        /// The symbol is associated with a function or other
        /// executable code.
        Func = 2,
        /// The symbol is associated with a section.  Symbol
        /// table entries of this type exist primarily for
        /// relocation and normally have STB_LOCAL bindings.
        Section = 3,
        /// By convention, the symbol's name gives the name of
        /// the source file associated with the object file.  A
        /// file symbol has STB_LOCAL bindings, its section
        /// index is SHN_ABS, and it precedes the other
        /// STB_LOCAL symbols of the file, if it is present.
        File = 4,
    }

    #[repr(u8)]
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
    pub enum SymbolBinding {
        /// Local symbols are not visible outside the object
        /// file containing their definition.  Local symbols of
        /// the same name may exist in multiple files without
        /// interfering with each other.
        #[default]
        Local = 0,
        /// Global symbols are visible to all object files being
        /// combined.  One file's definition of a global symbol
        /// will satisfy another file's undefined reference to
        /// the same symbol.
        Global = 1,
        /// Weak symbols resemble global symbols, but their
        /// definitions have lower precedence.
        Weak = 2,
    }

    #[repr(u8)]
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
    pub enum SymbolVisibility {
        /// The visibility of symbols with the STV_DEFAULT attribute
        /// is as specified by the symbol's binding type. That is,
        /// global and weak symbols are visible outside of their
        /// defining component, the executable file or shared object.
        /// Local symbols are hidden. Global and weak symbols can
        /// also be preempted, that is, they may by interposed by
        /// definitions of the same name in another component.
        #[default]
        Default = 0,
        /// A symbol defined in the current component is hidden if
        /// its name is not visible to other components. Such a
        /// symbol is necessarily protected. This attribute is used
        /// to control the external interface of a component. An
        /// object named by such a symbol may still be referenced
        /// from another component if its address is passed outside.
        ///
        /// A hidden symbol contained in a relocatable object is
        /// either removed or converted to STB_LOCAL binding by the
        /// link-editor when the relocatable object is included in an
        /// executable file or shared object.
        Hidden = 2,
    }

    impl From<crate::assembler::SymbolType> for SymbolBinding {
        fn from(value: crate::assembler::SymbolType) -> Self {
            match value {
                crate::assembler::SymbolType::FileLocal => todo!(),

                crate::assembler::SymbolType::Section => Self::Local,
                crate::assembler::SymbolType::Global => Self::Global,
                crate::assembler::SymbolType::Local => Self::Local,
                crate::assembler::SymbolType::LocalInternal => Self::Local,
                crate::assembler::SymbolType::Unknown => Self::Global,
            }
        }
    }
}

pub mod reloc {
    pub struct ElfRel {
        pub info: (ElfRelocType, u64),
        pub offset: u64,
        pub addend: Option<i64>,
    }

    #[repr(u8)]
    #[derive(Clone, Copy, Debug)]
    pub enum ElfRelocType {
        BRANCH = 16,
        JAL = 17,
        PCREL_HI20 = 23,
        PCREL_LO12_I = 24,
        PCREL_LO12_S = 25,
        HI20 = 26,
        LO12_I = 27,
        RELAX = 51,
    }
}

pub fn write_elf_file<P: AsRef<Path>>(out: P, assembler: &Assembler) {
    let class = elf_header::ElfClass::C32;
    let mut elf_file = ElfFile::new();

    let mut relocations = vec![];

    let mut section_by_name = HashMap::with_capacity(assembler.sections.len() * 4);

    let symbols_by_id: HashMap<_, _> = assembler
        .symbols
        .iter()
        .enumerate()
        .map(|(idx, sym)| (sym.id, idx))
        .collect();

    for sym in &assembler.symbols {
        let r#type = if sym.r#type == crate::assembler::SymbolType::Section {
            SymbolType::Section
        } else {
            SymbolType::NoType
        };

        let Some(val) = sym.value.as_ref() else {
            elf_file.add_symbol(
                &sym.name,
                ElfSymbol {
                    info: (r#type, sym.r#type.into()),
                    ..Default::default()
                },
            );
            continue;
        };

        elf_file.add_symbol(
            &sym.name,
            ElfSymbol {
                value: val.1 as u64,
                info: (r#type, sym.r#type.into()),
                shndx: assembler
                    .sections
                    .iter()
                    .position(|sec| sec.borrow().name == val.0)
                    .unwrap() as u16
                    + 1,
                ..Default::default()
            },
        );
    }

    for section in &assembler.sections {
        let section = section.borrow();

        let name = elf_file.add_section_str(&section.name) as u32;

        let r#type = match section.r#type {
            crate::assembler::SectionType::Bss => section::SectionType::Nobits,
            _ => section::SectionType::Progbits,
        };

        let flags = match section.r#type {
            crate::assembler::SectionType::Text => {
                section::section_flags::SHF_ALLOC | section::section_flags::SHF_EXECINSTR
            }
            crate::assembler::SectionType::Bss | crate::assembler::SectionType::Data => {
                section::section_flags::SHF_ALLOC | section::section_flags::SHF_WRITE
            }
            crate::assembler::SectionType::Rodata => section::section_flags::SHF_ALLOC,
            crate::assembler::SectionType::Unknown => 0,
        };

        let shndx = elf_file.add_section(SectionHeader {
            name,
            r#type,
            flags,
            offset: 0,
            size: section.buf.len() as u64,
            addralign: 4,
            ..Default::default()
        });

        section_by_name.insert(section.name.clone(), shndx);

        let mut rels = Vec::with_capacity(section.relocs.len());
        let mut relas = Vec::with_capacity(section.relocs.len());

        for reloc in &section.relocs {
            let list = if reloc.addend.is_some() {
                &mut relas
            } else {
                &mut rels
            };

            let sym_idx = reloc
                .symbol
                .and_then(|id| symbols_by_id.get(&id))
                .map_or(0, |idx| idx + 1);

            list.push(reloc::ElfRel {
                info: (reloc.r#type, sym_idx as u64),
                offset: reloc.offset as u64,
                addend: reloc.addend.map(|a| a as i64),
            });
        }

        if !rels.is_empty() {
            let n = elf_file.add_section_str(&format!(".rel{}", section.name));
            relocations.push((
                SectionHeader {
                    name: n as u32,
                    r#type: section::SectionType::Rel,
                    flags: section::section_flags::SHF_INFO_LINK,
                    size: (class.rel_entsize() * rels.len()) as u64,
                    entsize: class.rel_entsize() as u64,
                    info: shndx as u32,
                    ..Default::default()
                },
                rels,
            ));
        }

        if !relas.is_empty() {
            let n = elf_file.add_section_str(&format!(".rela{}", section.name));
            relocations.push((
                SectionHeader {
                    name: n as u32,
                    r#type: section::SectionType::Rela,
                    flags: section::section_flags::SHF_INFO_LINK,
                    size: (class.rela_entsize() * relas.len()) as u64,
                    entsize: class.rela_entsize() as u64,
                    info: shndx as u32,
                    ..Default::default()
                },
                relas,
            ));
        }
    }

    let first_non_local_symbol = elf_file
        .symbols
        .iter()
        .position(|sym| sym.info.1 != SymbolBinding::Local)
        .unwrap_or(elf_file.symbols.len());

    let strtab = elf_file.add_section_str(".strtab") as u32;
    let strtab = elf_file.add_section(SectionHeader {
        name: strtab,
        r#type: section::SectionType::Strtab,
        flags: section::section_flags::SHF_STRINGS,
        size: elf_file.strtab.len() as u64,
        ..Default::default()
    });

    let symtab = elf_file.add_section_str(".symtab") as u32;
    let symtab = elf_file.add_section(SectionHeader {
        name: symtab,
        r#type: section::SectionType::Symtab,
        size: (elf_file.symbols.len() * class.sym_entsize()) as u64,
        link: strtab as u32,
        entsize: class.sym_entsize() as u64,
        info: first_non_local_symbol as u32,
        ..Default::default()
    });

    for (sh, _) in &mut relocations {
        sh.link = symtab as u32;
        elf_file.add_section(sh.clone());
    }

    let shstrtab = elf_file.add_section_str(".shstrtab") as u32;
    let shstrndx = elf_file.add_section(SectionHeader {
        name: shstrtab,
        r#type: section::SectionType::Strtab,
        flags: section::section_flags::SHF_STRINGS,
        size: elf_file.shstrtab.len() as u64,
        ..Default::default()
    });

    let mut data_offset =
        class.e_ehsize() as usize + (elf_file.sections.len() * class.e_shentsize() as usize);

    for section in elf_file.sections.iter_mut() {
        if matches!(section.r#type, section::SectionType::Null) {
            continue;
        }
        section.offset = data_offset as u64;
        data_offset += section.size as usize;
    }

    let elf_header = elf_header::ElfHeader {
        class,
        data: elf_header::ElfData::LittleEndian,
        r#type: elf_header::ElfType::Rel,
        entry: 0,
        phoff: 0,
        shoff: class.e_ehsize() as u64,
        flags: 0,
        phnum: 0,
        shnum: elf_file.sections.len() as u16,
        shstrndx: shstrndx as u16,
    };

    let mut buf = BytesMut::new();
    write_elf_header(&mut buf, &elf_header);

    for section in &elf_file.sections {
        write_section_header(&mut buf, class, section);
    }

    for section in &assembler.sections {
        let section = section.borrow();
        buf.put_slice(&section.buf);
    }

    buf.put_slice(&elf_file.strtab);

    for sym in &elf_file.symbols {
        write_symbol(&mut buf, elf_header.class, sym);
    }

    for (_, relocations) in &relocations {
        for reloc in relocations {
            write_relocation(&mut buf, class, reloc);
        }
    }

    buf.put_slice(&elf_file.shstrtab);

    let out = out.as_ref().to_path_buf();
    std::fs::create_dir_all(out.parent().unwrap()).unwrap();
    std::fs::write(out, &buf).unwrap();
}

fn write_elf_header(bytes: &mut bytes::BytesMut, elf_header: &elf_header::ElfHeader) {
    bytes.put_slice(&elf_header::EI_MAG);
    bytes.put_u8(elf_header.class as u8);
    bytes.put_u8(elf_header.data as u8);
    bytes.put_u8(elf_header::EI_VERSION);
    bytes.put_u8(elf_header::EI_OSABI);
    bytes.put_u8(elf_header::EI_ABIVERSION);
    bytes.put_slice(&elf_header::EI_PAD);

    // TODO: data endianness starts from here, assuming LE for now

    bytes.put_u16_le(elf_header.r#type as u16);
    bytes.put_u16_le(elf_header::E_MACHINE);
    bytes.put_u32_le(elf_header::E_VERSION);

    match elf_header.class {
        elf_header::ElfClass::C32 => {
            bytes.put_u32_le(elf_header.entry as u32);
            bytes.put_u32_le(elf_header.phoff as u32);
            bytes.put_u32_le(elf_header.shoff as u32);
        }
        elf_header::ElfClass::C64 => {
            bytes.put_u64_le(elf_header.entry as u64);
            bytes.put_u64_le(elf_header.phoff as u64);
            bytes.put_u64_le(elf_header.shoff as u64);
        }
    }

    bytes.put_u32_le(elf_header.flags);
    bytes.put_u16_le(elf_header.class.e_ehsize());
    bytes.put_u16_le(elf_header.class.e_phentsize());
    bytes.put_u16_le(elf_header.phnum);
    bytes.put_u16_le(elf_header.class.e_shentsize());
    bytes.put_u16_le(elf_header.shnum);
    bytes.put_u16_le(elf_header.shstrndx);
}

fn write_prog_header(
    bytes: &mut bytes::BytesMut,
    elf_class: elf_header::ElfClass,
    ph: program_header::ProgramHeader,
) {
    bytes.put_u32_le(ph.r#type as u32);

    match elf_class {
        elf_header::ElfClass::C32 => {
            bytes.put_u32_le(ph.offset as u32);
            bytes.put_u32_le(ph.vaddr as u32);
            bytes.put_u32_le(ph.paddr as u32);
            bytes.put_u32_le(ph.filesz as u32);
            bytes.put_u32_le(ph.memsz as u32);
            bytes.put_u32_le(ph.flags);
            bytes.put_u32_le(ph.align as u32);
        }
        elf_header::ElfClass::C64 => {
            bytes.put_u32_le(ph.flags);
            bytes.put_u64_le(ph.offset as u64);
            bytes.put_u64_le(ph.vaddr as u64);
            bytes.put_u64_le(ph.paddr as u64);
            bytes.put_u64_le(ph.filesz as u64);
            bytes.put_u64_le(ph.memsz as u64);
            bytes.put_u64_le(ph.align as u64);
        }
    }
}

fn write_section_header(
    bytes: &mut bytes::BytesMut,
    elf_class: elf_header::ElfClass,
    sh: &SectionHeader,
) {
    bytes.put_u32_le(sh.name);
    bytes.put_u32_le(sh.r#type as u32);

    match elf_class {
        elf_header::ElfClass::C32 => {
            bytes.put_u32_le(sh.flags as u32);
            bytes.put_u32_le(sh.addr as u32);
            bytes.put_u32_le(sh.offset as u32);
            bytes.put_u32_le(sh.size as u32);

            bytes.put_u32_le(sh.link);
            bytes.put_u32_le(sh.info);

            bytes.put_u32_le(sh.addralign as u32);
            bytes.put_u32_le(sh.entsize as u32);
        }
        elf_header::ElfClass::C64 => {
            bytes.put_u64_le(sh.flags as u64);
            bytes.put_u64_le(sh.addr as u64);
            bytes.put_u64_le(sh.offset as u64);
            bytes.put_u64_le(sh.size as u64);

            bytes.put_u32_le(sh.link);
            bytes.put_u32_le(sh.info);

            bytes.put_u64_le(sh.addralign as u64);
            bytes.put_u64_le(sh.entsize as u64);
        }
    }
}

fn write_symbol(bytes: &mut bytes::BytesMut, elf_class: elf_header::ElfClass, sym: &ElfSymbol) {
    bytes.put_u32_le(sym.name);

    let info = {
        let bind = (sym.info.1 as u8) << 4;
        let r#type = (sym.info.0 as u8) & 0xF;
        bind | r#type
    };

    match elf_class {
        elf_header::ElfClass::C32 => {
            bytes.put_u32_le(sym.value as u32);
            bytes.put_u32_le(sym.size as u32);
            bytes.put_u8(info);
            bytes.put_u8(sym.other);
            bytes.put_u16_le(sym.shndx);
        }
        elf_header::ElfClass::C64 => {
            bytes.put_u8(info);
            bytes.put_u8(sym.other);
            bytes.put_u16_le(sym.shndx);
            bytes.put_u64_le(sym.value as u64);
            bytes.put_u64_le(sym.size as u64);
        }
    }
}

fn write_relocation(
    bytes: &mut bytes::BytesMut,
    elf_class: elf_header::ElfClass,
    reloc: &reloc::ElfRel,
) {
    match elf_class {
        elf_header::ElfClass::C32 => {
            let info = ((reloc.info.1 as u32) << 8) | (reloc.info.0 as u8) as u32;

            bytes.put_u32_le(reloc.offset as u32);
            bytes.put_u32_le(info);
            if let Some(addend) = reloc.addend {
                bytes.put_u32_le(addend as u32);
            }
        }
        elf_header::ElfClass::C64 => {
            todo!();
            let info = ((reloc.info.1 as u32) << 8) & (reloc.info.0 as u8) as u32;

            bytes.put_u32_le(reloc.offset as u32);
            bytes.put_u32_le(info);
            if let Some(addend) = reloc.addend {
                bytes.put_u32_le(addend as u32);
            }
        }
    }
}
