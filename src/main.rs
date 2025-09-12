mod elf;

fn main() {
    let program = std::env::args().nth(1).unwrap();
    let program = std::fs::read_to_string(program).unwrap();

    let mut assembler = assembler::Assembler::new(&program);
    assembler.assemble();

    let out = std::env::args().nth(2).unwrap();
    elf::write_elf_file(&out, &assembler);
}

type Reg = u8;

/// The RISC-V ISA is separated in 4 different variants.
///
/// It's important to note that the 32-bit variants are not a
/// strict subset of the 64-bit counterparts. This allows for
/// the base variants to specialize for its needs.
enum Isa {
    RV32I,
    RV32E,
    RV64I,
    RV64E,
}

impl Isa {
    /// The XLEN refers to the width of the integer registers
    /// in bits.
    fn xlen(&self) -> u32 {
        match self {
            Isa::RV32I => 32,
            Isa::RV32E => 32,
            Isa::RV64I => 64,
            Isa::RV64E => 64,
        }
    }
}

type HALFWORD = u16;
type WORD = u32;
type DOUBLEWORD = u64;
type QUADWORD = u128;

mod lexer {
    use std::{iter::Peekable, str::CharIndices};

    pub type Lexeme = ecow::EcoString;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum TokenType {
        Comma,
        Hyphen,
        Plus,
        Colon,
        LParen,
        RParen,
        NewLine,
        Eof,

        Identifier,
        String,
        Integer,
    }

    #[derive(Clone, Copy)]
    enum IntNotation {
        Binary,
        Octal,
        Decimal,
        Hexadecimal,
    }

    impl IntNotation {
        fn from_char(c: char) -> Option<Self> {
            match c {
                'b' => Some(Self::Binary),
                'o' => Some(Self::Octal),
                'x' => Some(Self::Hexadecimal),
                _ => None,
            }
        }

        fn accepts_char(&self, c: char) -> bool {
            match self {
                IntNotation::Binary => c.is_digit(2),
                IntNotation::Octal => c.is_digit(8),
                IntNotation::Decimal => c.is_digit(10),
                IntNotation::Hexadecimal => c.is_digit(16),
            }
        }
    }

    fn take_while<F: FnMut(char) -> bool>(
        chars: &mut Peekable<impl Iterator<Item = (usize, char)>>,
        mut f: F,
    ) -> Option<usize> {
        while let Some((end, c)) = chars.peek() {
            if !f(*c) {
                return Some(end - 1);
            }
            let _ = chars.next();
        }
        None
    }

    /// An iterator that yields each token and its lexeme.
    pub struct Lexer<'a> {
        file: &'a str,
        chars: Peekable<CharIndices<'a>>,
    }

    impl<'a> Lexer<'a> {
        pub fn new(file: &'a str) -> Self {
            Self {
                file,
                chars: file.char_indices().peekable(),
            }
        }

        fn lex_number(&mut self, idx: usize, c: char) -> (TokenType, &'a str) {
            let mut expect_integer_type = c == '0';
            let mut notation = if c.is_ascii_digit() {
                Some(IntNotation::Decimal)
            } else {
                None
            };

            let mut last = c;
            let end = take_while(&mut self.chars, |c| {
                if !c.is_ascii_alphanumeric() && !matches!(c, '_' | '.') {
                    return false;
                }
                if let Some(not) = notation {
                    if expect_integer_type {
                        expect_integer_type = false;
                        if !not.accepts_char(c) {
                            notation = IntNotation::from_char(c);
                        }
                    } else if !not.accepts_char(c) {
                        notation = None;
                    }
                }

                if c == '.' && last == '.' {
                    panic!("identifier cannot have adjacent dots");
                }

                last = c;
                true
            })
            .unwrap_or(self.file.len() - 1);

            let r#type = if notation.is_some() {
                TokenType::Integer
            } else {
                TokenType::Identifier
            };

            (r#type, &self.file[idx..=end])
        }
    }

    impl<'a> Iterator for Lexer<'a> {
        type Item = (TokenType, &'a str);

        fn next(&mut self) -> Option<Self::Item> {
            let (idx, c) = self.chars.next()?;
            let token = match c {
                ',' => (TokenType::Comma, &self.file[idx..=idx]),
                ':' => (TokenType::Colon, &self.file[idx..=idx]),
                '-' => (TokenType::Hyphen, &self.file[idx..=idx]),
                '+' => (TokenType::Plus, &self.file[idx..=idx]),
                '(' => (TokenType::LParen, &self.file[idx..=idx]),
                ')' => (TokenType::RParen, &self.file[idx..=idx]),
                '\n' => (TokenType::NewLine, &self.file[idx..=idx]),
                '"' => {
                    let mut escaping = false;
                    let end = self
                        .chars
                        .by_ref()
                        .find(|(_, c)| {
                            match c {
                                _ if escaping => escaping = false,
                                '"' => return true,
                                '\\' if !escaping => escaping = true,
                                _ => {}
                            }
                            false
                        })
                        .expect("unclosed string")
                        .0;
                    (TokenType::String, &self.file[idx..=end])
                }
                '#' => {
                    let _ = take_while(&mut self.chars, |c| c != '\n');
                    return self.next();
                }
                c if c.is_whitespace() => return self.next(),
                _ => self.lex_number(idx, c),
            };

            Some(token)
        }
    }

    #[test]
    fn test() {
        let file = "
            .text.data
            # this is a comment
            val: 0x100000 # this is also a comment

            010341
            01034d
            0x1f
            0x1t
            0o18
            0o17
            0b01
            0b02

            bomdia:
                add rs1, 0
        ";

        let lexer = Lexer::new(file);
        let tokens: Vec<_> = lexer.collect();

        dbg!(tokens);
    }
}

mod parser {
    use std::{
        iter::Peekable,
        ops::{Deref, DerefMut},
    };

    use crate::{
        isa::find_register,
        lexer::{Lexeme, Lexer, TokenType},
    };

    #[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Ident(Lexeme);

    impl Ident {
        pub const fn new_inline(s: &str) -> Self {
            Self(Lexeme::inline(s))
        }
    }

    impl std::fmt::Display for Ident {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }

    impl From<&str> for Ident {
        fn from(value: &str) -> Self {
            Self(value.into())
        }
    }

    impl Deref for Ident {
        type Target = Lexeme;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl DerefMut for Ident {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    #[derive(Clone, Debug)]
    pub enum TopLevel {
        Directive(Ident, Vec<Expr>),
        Label(Ident),
        Instruction(Ident, Vec<Expr>),
    }

    #[derive(Clone, Debug)]
    pub enum Expr {
        String(Vec<u8>),
        Number(isize),
        Symbol(Ident),
        RelocFunction(Ident, Box<Expr>),
        Deref(Ident),
        App(Box<Expr>, Box<Expr>),
        BinOp(Box<Expr>, OperationType, Box<Expr>),
        Unary(OperationType, Box<Expr>),
    }

    impl Expr {
        /// Extracts a base address reference and the offset relative to this base.
        ///
        /// Valid syntaxes are:
        /// ```asm
        /// lb t0, my_symbol     # my_symbol as symbol
        /// lb t0, my_symbol + 1 # my_symbol as symbol, offset = 1
        /// lb t0, (t1)          # t1 as register
        /// lb t0, -1(t1)        # t1 as register, offset = 1
        /// lb t0, 10            # offset = 10
        /// lb t0, 10 - 2        # offset = 8
        /// lb t0, (t1) + 1      # t1 as symbol, offset = 1
        /// ```
        pub fn into_offset(self) -> (OffsetRefrerence, isize) {
            match self {
                Expr::Number(offset) => (OffsetRefrerence::None, offset),
                Expr::Symbol(sym) => (OffsetRefrerence::Symbol(sym), 0),
                Expr::BinOp(lhs, op, rhs) => {
                    let (sym, offset) = match (*lhs, *rhs) {
                        (Expr::Number(lhs), Expr::Number(rhs)) => {
                            let offset = match op {
                                OperationType::Plus => lhs + rhs,
                                OperationType::Minus => lhs - rhs,
                            };

                            return (OffsetRefrerence::None, offset);
                        }
                        (Expr::Number(offset), Expr::Symbol(ident) | Expr::Deref(ident))
                        | (Expr::Symbol(ident) | Expr::Deref(ident), Expr::Number(offset)) => {
                            (ident, offset)
                        }
                        (lhs, rhs) => panic!("invalid expression: {lhs:?} {op:?} {rhs:?}"),
                    };

                    let offset = match op {
                        OperationType::Plus => offset,
                        OperationType::Minus => offset.wrapping_neg(),
                    };

                    (OffsetRefrerence::Symbol(sym), offset)
                }
                Expr::Deref(reg) => (OffsetRefrerence::Reg(find_register(&reg)), 0),
                Expr::App(expr, expr1) => match (*expr, *expr1) {
                    (Expr::Number(offset), Expr::Symbol(sym)) => {
                        (OffsetRefrerence::Reg(find_register(&sym)), offset)
                    }
                    _ => panic!(),
                },
                expr => panic!("invalid expr: {expr:?}"),
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum OffsetRefrerence {
        Reg(crate::Reg),
        Symbol(Ident),
        None,
    }

    #[derive(Clone, Copy, Debug)]
    pub enum OperationType {
        Plus,
        Minus,
    }

    pub struct Parser<'a> {
        lexer: Peekable<Lexer<'a>>,
    }

    impl<'a> Parser<'a> {
        pub fn new(file: &'a str) -> Self {
            Self {
                lexer: Lexer::new(file).peekable(),
            }
        }

        fn peek(&mut self) -> &(TokenType, &'a str) {
            self.lexer.peek().unwrap_or(&(TokenType::Eof, ""))
        }

        fn advance(&mut self) -> (TokenType, &'a str) {
            self.lexer.next().unwrap_or((TokenType::Eof, ""))
        }

        fn is(&mut self, token: TokenType) -> bool {
            self.peek().0 == token
        }

        fn eat(&mut self, token: TokenType) -> Option<&'a str> {
            if self.is(token) {
                Some(self.advance().1)
            } else {
                None
            }
        }

        fn parse_token(&mut self) -> TopLevel {
            let identifier = self.eat(TokenType::Identifier).unwrap();

            if self.eat(TokenType::Colon).is_some() {
                return TopLevel::Label(Ident(identifier.into()));
            }

            let args = self.parse_list();
            if identifier.starts_with('.') {
                TopLevel::Directive(Ident(identifier.trim_start_matches('.').into()), args)
            } else {
                TopLevel::Instruction(Ident(identifier.into()), args)
            }
        }

        fn parse_list(&mut self) -> Vec<Expr> {
            let mut values = Vec::new();

            while let Some(val) = self.parse_value() {
                if self.is(TokenType::LParen) {
                } else if self.eat(TokenType::Comma).is_none()
                    && !self.is(TokenType::NewLine)
                    && !self.is(TokenType::Eof)
                {
                    panic!();
                }

                values.push(val);
            }

            if !self.is(TokenType::NewLine) && !self.is(TokenType::Eof) {
                panic!("expected EOF or new line");
            }

            values
        }

        fn parse_value(&mut self) -> Option<Expr> {
            let val = match &self.peek().0 {
                TokenType::LParen => {
                    self.advance();
                    let val = self.parse_value().expect("expected value");
                    self.eat(TokenType::RParen).expect("expected r-paren");

                    if let Expr::Symbol(ident) = val {
                        Expr::Deref(ident)
                    } else {
                        val
                    }
                }
                TokenType::Hyphen => {
                    self.advance();
                    if let Some(int) = self.eat(TokenType::Integer) {
                        let (int, radix) = extract_radix(int);
                        Expr::Number(isize::from_str_radix(&format!("-{int}"), radix).unwrap())
                    } else {
                        Expr::Unary(OperationType::Minus, Box::new(self.parse_value().unwrap()))
                    }
                }
                TokenType::Plus => {
                    self.advance();
                    Expr::Unary(OperationType::Plus, Box::new(self.parse_value().unwrap()))
                }
                TokenType::Identifier => self.parse_ident(),
                TokenType::String => self.parse_string(),
                TokenType::Integer => self.parse_int(),
                _ => return None,
            };

            let lhs = if self.eat(TokenType::LParen).is_some() {
                let inner = self.parse_value().expect("expected value");
                self.eat(TokenType::RParen).expect("expected r-paren");
                Expr::App(Box::new(val), Box::new(inner))
            } else {
                val
            };

            let op = match self.peek().0 {
                TokenType::Hyphen => OperationType::Minus,
                TokenType::Plus => OperationType::Plus,
                _ => return Some(lhs),
            };
            self.advance();

            let rhs = self.parse_value().expect("missing right bin op expression");

            Some(Expr::BinOp(Box::new(lhs), op, Box::new(rhs)))
        }

        fn parse_ident(&mut self) -> Expr {
            let ident = self.eat(TokenType::Identifier).unwrap();
            if ident.starts_with('%') {
                let func_name = Ident((&ident[1..]).into());

                self.eat(TokenType::LParen).expect("invalid function call");
                let val = self.parse_value().expect("call must have a parameter");
                self.eat(TokenType::RParen).expect("invalid function call");

                Expr::RelocFunction(func_name, Box::new(val))
            } else {
                Expr::Symbol(Ident((*ident).into()))
            }
        }

        fn parse_string(&mut self) -> Expr {
            let str = self.eat(TokenType::String).unwrap();
            let str = str.get(1..str.len() - 1).unwrap_or_default().to_string();

            let mut buf = Vec::with_capacity(str.as_bytes().len());
            let mut escaping = false;
            for c in str.chars() {
                match c {
                    '0'..'9' if escaping => {
                        escaping = false;
                        buf.push(c as u8 - '0' as u8);
                    }
                    'n' if escaping => {
                        escaping = false;
                        buf.push('\n' as u8);
                    }
                    c if escaping => panic!("unexpected escaped character {c:?}"),
                    '\\' => escaping = true,
                    c => buf.push(c as u8),
                }
            }

            Expr::String(buf)
        }

        fn parse_int(&mut self) -> Expr {
            let int = self.eat(TokenType::Integer).unwrap();
            let (int, radix) = extract_radix(int);
            Expr::Number(isize::from_str_radix(int, radix).unwrap())
        }
    }

    fn extract_radix(int: &str) -> (&str, u32) {
        let (int, radix) = if let Some(int) = int.strip_prefix("0b") {
            (int, 2)
        } else if let Some(int) = int.strip_prefix("0o") {
            (int, 8)
        } else if let Some(int) = int.strip_prefix("0x") {
            (int, 16)
        } else {
            (int, 10)
        };
        (int, radix)
    }

    impl<'a> Iterator for Parser<'a> {
        type Item = TopLevel;

        fn next(&mut self) -> Option<Self::Item> {
            match self.peek().0 {
                TokenType::NewLine => {
                    self.advance();
                    self.next()
                }
                TokenType::Eof => return None,
                _ => Some(self.parse_token()),
            }
        }
    }
}

mod isa {
    use super::Reg;

    pub fn find_register(reg: &str) -> Reg {
        static REGISTERS: phf::Map<&'static str, u8> = phf::phf_map! {
            "x0" => 0, "x1" => 1, "x2" => 2, "x3" => 3, "x4" => 4, "x5" => 5, "x6" => 6, "x7" => 7, "x8" => 8, "x9" => 9, "x10" => 10, "x11" => 11, "x12" => 12, "x13" => 13,
            "x14" => 14, "x15" => 15, "x16" => 16, "x17" => 17, "x18" => 18, "x19" => 19, "x20" => 20, "x21" => 21, "x22" => 22, "x23" => 23, "x24" => 24, "x25" => 25, "x26" => 26, "x27" => 27,
            "x28" => 28, "x29" => 29, "x30" => 30, "x31" => 31,

            "zero" => 0, "ra" => 1, "sp" => 2, "gp" => 3, "tp" => 4, "t0" => 5, "t1" => 6, "t2" => 7, "s0" => 8, "s1" => 9, "a0" => 10, "a1" => 11, "a2" => 12, "a3" => 13,
            "a4" => 14, "a5" => 15, "a6" => 16, "a7" => 17, "s2" => 18, "s3" => 19, "s4" => 20, "s5" => 21, "s6" => 22, "s7" => 23, "s8" => 24, "s9" => 25, "s10" => 26, "s11" => 27,
            "t3" => 28, "t4" => 29, "t5" => 30, "t6" => 31,

            "fp" => 8
        };

        *REGISTERS.get(reg).expect("unknown reg")
    }
    pub fn lui(rd: Reg, imm: u32) -> [u8; 4] {
        u(0b0110111, rd, imm).to_le_bytes()
    }

    pub fn auipc(rd: Reg, imm: u32) -> [u8; 4] {
        u(0b0010111, rd, imm).to_le_bytes()
    }

    pub fn addi(rd: Reg, rs1: Reg, imm: u16) -> [u8; 4] {
        i(0b0010011, rd, 0b000, rs1, imm).to_le_bytes()
    }

    pub fn sb(src: Reg, base: Reg, offset: u16) -> [u8; 4] {
        s(0b0100011, offset, 0b000, base, src).to_le_bytes()
    }

    pub fn lb(rd: Reg, base: Reg, offset: u16) -> [u8; 4] {
        i(0b0000011, rd, 0b000, base, offset).to_le_bytes()
    }

    pub fn jal(rd: Reg, offset: u32) -> [u8; 4] {
        j(0b1101111, rd, offset).to_le_bytes()
    }

    pub fn jalr(rd: Reg, rs: Reg, offset: u16) -> [u8; 4] {
        i(0b1100111, rd, 0, rs, offset).to_le_bytes()
    }

    pub fn beq(rs1: Reg, rs2: Reg, offset: u16) -> [u8; 4] {
        b(0b1100011, offset, 0b000, rs1, rs2).to_le_bytes()
    }

    pub fn bne(rs1: Reg, rs2: Reg, offset: u16) -> [u8; 4] {
        b(0b1100011, offset, 0b001, rs1, rs2).to_le_bytes()
    }

    macro_rules! insn_type {
    ($n:ident { $($field:ident: $ftype:ty => $($range:expr)+),+ $(,)? }) => {
        fn $n($($field: $ftype),+) -> u32 {
            let mut insn = 0;

            $({
                let mut field_val = $field as u32;
                $(
                    #[allow(unused_assignments)]
                    {
                        let len = $range.end - $range.start;
                        insn |= (field_val & !(u32::MAX << len)) << $range.start;
                        field_val >>= len;
                    }
                )+
            })+

            insn
        }
    };
}

    insn_type!(r {
        opcode: u8 => 0..7,
        rd: u8 => 7..12,
        funct3: u8 => 12..15,
        rs1: u8 => 15..20,
        rs2: u8 => 20..25,
        funct7: u8 => 25..32,
    });

    insn_type!(i {
        opcode: u8 => 0..7,
        rd: u8 => 7..12,
        funct3: u8 => 12..15,
        rs: u8 => 15..20,
        imm: u16 => 20..32,
    });

    insn_type!(s {
        opcode: u8 => 0..7,
        imm: u16 => 7..12 25..32,
        funct3: u8 => 12..15,
        rs1: u8 => 15..20,
        rs2: u8 => 20..25,
    });

    insn_type!(b {
        opcode: u8 => 0..7,
        imm: u16 => 8..12 25..31 7..8 31..32,
        funct3: u8 => 12..15,
        rs1: u8 => 15..20,
        rs2: u8 => 20..25,
    });

    insn_type!(u {
        opcode: u8 => 0..7,
        rd: u8 => 7..12,
        imm: u32 => 12..32,
    });

    insn_type!(j {
        opcode: u8 => 0..7,
        rd: u8 => 7..12,
        imm: u32 => 21..31 20..21 12..20 31..32,
    });
}

mod assembler {
    use std::{cell::RefCell, collections::HashSet, rc::Rc};

    use crate::{
        elf::reloc::ElfRelocType,
        parser::{Expr, Ident, OffsetRefrerence, Parser, TopLevel},
    };

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
    pub enum SymbolType {
        FileLocal,
        Section,
        LocalInternal,
        Local,
        Global,
        #[default]
        Unknown,
    }

    #[derive(Debug, Default)]
    pub struct Symbol {
        pub id: usize,
        pub name: Ident,
        pub r#type: SymbolType,
        pub value: Option<(Ident, usize)>,
    }

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
    pub enum SectionType {
        #[default]
        Progbits,
        Nobits,
    }

    bitflags::bitflags! {
        pub struct SectionFlags: u32 {
            const WRITABLE = 0b001;
            const ALLOCATABLE = 0b010;
            const EXECUTABLE = 0b100;
        }
    }

    pub struct Section {
        pub name: Ident,
        pub r#type: SectionType,
        pub flags: SectionFlags,
        pub buf: Vec<u8>,
        pub relocs: Vec<Reloc>,
    }

    impl Section {
        pub fn new(name: Ident) -> Self {
            let (flags, r#type) = match name.as_str() {
                ".text" => (
                    SectionFlags::ALLOCATABLE | SectionFlags::EXECUTABLE,
                    SectionType::Progbits,
                ),
                ".bss" => (
                    SectionFlags::WRITABLE | SectionFlags::ALLOCATABLE,
                    SectionType::Nobits,
                ),
                ".data" => (
                    SectionFlags::WRITABLE | SectionFlags::ALLOCATABLE,
                    SectionType::Progbits,
                ),
                ".rodata" => (SectionFlags::ALLOCATABLE, SectionType::Progbits),
                _ => (SectionFlags::empty(), SectionType::Progbits),
            };

            Self {
                name,
                r#type,
                flags,
                buf: Default::default(),
                relocs: Default::default(),
            }
        }
    }

    impl Default for Section {
        fn default() -> Self {
            Self::new(Ident::new_inline(".text"))
        }
    }

    pub struct Reloc {
        pub r#type: ElfRelocType,
        pub symbol: Option<usize>,
        pub offset: usize,
        pub addend: Option<isize>,
    }

    pub struct Assembler<'a> {
        parser: Option<Parser<'a>>,

        pub symbols: Vec<Symbol>,

        pub sections: Vec<Rc<RefCell<Section>>>,
        pub open_section: Rc<RefCell<Section>>,

        pub active_options: HashSet<Ident>,
    }

    impl<'a> Assembler<'a> {
        pub fn new(file: &'a str) -> Self {
            let open_section = Rc::new(RefCell::new(Section::default()));

            Self {
                parser: Some(Parser::new(file)),

                symbols: Default::default(),

                sections: vec![open_section.clone()],
                open_section,

                active_options: HashSet::new(),
            }
        }

        fn consume_directive(&mut self, mut ident: Ident, mut exprs: Vec<Expr>) {
            macro_rules! extract_values {
                ($($ty:ident),+) => {
                    $(match exprs.remove(0) {
                        Expr::$ty(arg0) => arg0,
                        expr => panic!("unexpected expr {expr:?}"),
                    }),+
                };
            }

            ident.make_mut().make_ascii_lowercase();
            match ident.as_str() {
                "globl" | "global" => {
                    self.emit_symbol(extract_values!(Symbol), SymbolType::Global, None);
                }
                "local" => {
                    self.emit_symbol(extract_values!(Symbol), SymbolType::Local, None);
                }
                "string" | "asciz" => self.write(&extract_values!(String)),

                "section" => {
                    let name = extract_values!(Symbol);
                    self.emit_section(name);

                    if !exprs.is_empty() {
                        match exprs.remove(0) {
                            Expr::String(flags_str) => {
                                let flags_str = String::from_utf8(flags_str)
                                    .expect("valid section flags are: w a x");
                                let mut flags = SectionFlags::empty();
                                for c in flags_str.chars() {
                                    match c {
                                        'w' => flags |= SectionFlags::WRITABLE,
                                        'a' => flags |= SectionFlags::ALLOCATABLE,
                                        'x' => flags |= SectionFlags::EXECUTABLE,
                                        _ => panic!("valid section flags are: w a x"),
                                    }
                                }

                                let mut sec = self.open_section.borrow_mut();
                                sec.flags = flags;

                                if !exprs.is_empty() {
                                    if let Expr::Symbol(mut r#type) = exprs.remove(0) {
                                        r#type.make_mut().make_ascii_lowercase();
                                        sec.r#type = match r#type.as_str().strip_prefix('@') {
                                            Some("progbits") => SectionType::Progbits,
                                            Some("nobits") => SectionType::Nobits,
                                            r#type => panic!("unknown section type: {type:?}"),
                                        };
                                    } else {
                                        panic!("invalid section type expression");
                                    }
                                }
                            }
                            expr => panic!("unexpected expr: {expr:?}"),
                        };
                    }
                }
                "text" => {
                    if !exprs.is_empty() {
                        panic!("subsections are not supported");
                    }
                    self.emit_section(".text".into())
                }
                "data" => {
                    if !exprs.is_empty() {
                        panic!("subsections are not supported");
                    }
                    self.emit_section(".data".into())
                }
                "rodata" => {
                    if !exprs.is_empty() {
                        panic!("subsections are not supported");
                    }
                    self.emit_section(".rodata".into())
                }
                "bss" => {
                    if !exprs.is_empty() {
                        panic!("subsections are not supported");
                    }

                    self.emit_section(".bss".into())
                }
                "option" => {
                    while let Some(option) = exprs.pop() {
                        match option {
                            Expr::Symbol(mut ident) => {
                                ident.make_mut().make_ascii_lowercase();
                                match ident.as_str() {
                                    // TODO: mark norvc/rvc as deprecated in favor of arch +/-c
                                    // for some reason unknown to me, GAS doesn't allow arch -c
                                    // and calls it deprecated, while the risc-v asm manual marks
                                    // norvc as the deprecated alternative
                                    "norvc" => {
                                        self.active_options.insert(ident);
                                    }
                                    "rvc" => {
                                        self.active_options.remove(&Ident::new_inline("norvc"));
                                    }
                                    "arch" => {
                                        todo!();
                                    }
                                    opt => panic!("unknown option {opt:?}"),
                                }
                            }
                            opt => panic!("unknown option {opt:?}"),
                        }
                    }
                }

                // "byte" => {
                //     let v = extract_values!(Number);
                //     assert!((-128isize..=127).contains(&v));
                //     let mut val = Vec::with_capacity(exprs.len());
                //     loop {
                //         let v = extract_values!(Number);
                //         assert!((-128isize..=127).contains(&v));
                //         val.push(v as u8);
                //         if exprs.is_empty() {
                //             break;
                //         }
                //     }
                //     self.write(&val);
                // }
                _ => {}
            }

            assert!(exprs.is_empty(), "{exprs:?}");
        }

        fn emit_section(&mut self, name: Ident) {
            if let Some(section) = self
                .sections
                .iter()
                .find(|section| section.borrow().name == name)
            {
                self.open_section = section.clone();
            } else {
                let section = Rc::new(RefCell::new(Section::new(name)));
                self.sections.push(section.clone());
                self.open_section = section;
            }
        }

        fn emit_symbol(
            &mut self,
            name: Ident,
            r#type: SymbolType,
            value: Option<(Ident, usize)>,
        ) -> usize {
            if matches!(r#type, SymbolType::LocalInternal) {
                self.symbols.push(Symbol {
                    id: self.symbols.len(),
                    name: name.clone(),
                    r#type,
                    value,
                });

                return self.symbols.len() - 1;
            }

            let Some(sym) = self.symbols.iter_mut().find(|sym| sym.name == name) else {
                self.symbols.push(Symbol {
                    id: self.symbols.len(),
                    name: name.clone(),
                    r#type,
                    value,
                });

                return self.symbols.len() - 1;
            };

            if value.is_some() && sym.value.is_some() {
                panic!("duplicate symbol declaration: {:?}", sym.name);
            }

            if r#type != SymbolType::Unknown {
                sym.r#type = r#type;
            }

            if value.is_some() {
                sym.value = sym.value.take().or(value);
            }

            sym.id
        }

        fn emit_instruction(&mut self, mut ident: Ident, mut exprs: Vec<Expr>) {
            use crate::isa::*;

            macro_rules! extract {
                (v: Reg) => {
                    match exprs.remove(0) {
                        Expr::Symbol(arg0) => find_register(&arg0),
                        r => panic!("unexpected {r:?}, expected Reg"),
                    }
                };
                (v: $ty:ident) => {
                    match exprs.remove(0) {
                        Expr::$ty(arg0) => arg0,
                        r => panic!("unexpected {r:?}, expected {}", stringify!($ty)),
                    }
                };
                ($($ty:ident),+) => {
                    ($(extract!(v: $ty),)+)
                };
            }

            ident.make_mut().make_ascii_lowercase();
            match ident.as_str() {
                "addi" => {
                    let (rd, rs, n) = extract!(Reg, Reg, Number);
                    self.write(&addi(rd, rs, n as u16));
                }
                "lui" => {
                    let (rd, imm) = extract!(Reg, Number);
                    self.write(&lui(rd, imm as u32));
                }
                "sb" => {
                    let (src,) = extract!(Reg);

                    let expr = exprs.pop().unwrap();
                    match expr.into_offset() {
                        (OffsetRefrerence::Reg(dst), offset) => {
                            self.write(&sb(src, dst, offset as u16));
                        }
                        offset => panic!("invalid arguemnt for sb: {offset:?}"),
                    }
                }
                "lb" => {
                    let (rd,) = extract!(Reg);

                    let expr = exprs.pop().unwrap();
                    match expr.into_offset() {
                        (OffsetRefrerence::None, offset) => {
                            self.write(&lb(rd, 0, offset as u16));
                        }
                        (OffsetRefrerence::Reg(src), offset) => {
                            self.write(&lb(rd, src, offset as u16));
                        }
                        (OffsetRefrerence::Symbol(sym), offset) => {
                            let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                            self.emit_relocation(ElfRelocType::PCREL_HI20, Some(sym), Some(offset));
                            self.emit_relocation(ElfRelocType::RELAX, None, Some(offset));
                            let local_sym = self.emit_relocation_label();
                            self.write(&auipc(rd, 0));

                            self.emit_relocation(ElfRelocType::PCREL_LO12_I, Some(local_sym), None);
                            self.emit_relocation(ElfRelocType::RELAX, None, Some(0));
                            self.write(&lb(rd, rd, 0));
                        }
                    }
                }
                "auipc" => {
                    let (rd, imm) = extract!(Reg, Number);
                    self.write(&auipc(rd, imm as u32));
                }
                "jal" => {
                    if exprs.len() == 1 {
                        let (sym,) = extract!(Symbol);
                        let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                        self.emit_relocation(ElfRelocType::JAL, Some(sym), None);
                        self.write(&jal(1, 0));
                    } else {
                        let (rd, offset) = extract!(Reg, Number);
                        self.write(&jal(rd, offset as u32));
                    }
                }
                "beq" => {
                    let (rs1, rs2, sym) = extract!(Reg, Reg, Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::BRANCH, Some(sym), None);
                    self.write(&beq(rs1, rs2, 0));
                }
                "bne" => {
                    let (rs1, rs2, sym) = extract!(Reg, Reg, Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::BRANCH, Some(sym), None);
                    self.write(&bne(rs1, rs2, 0));
                }

                // Pseudoinstructions
                "nop" => self.write(&addi(0, 0, 0)),
                "mv" => {
                    let (rd, rs) = extract!(Reg, Reg);
                    self.write(&addi(rd, rs, 0));
                }
                "lla" => {
                    let (rd,) = extract!(Reg);

                    match exprs.pop().unwrap().into_offset() {
                        (OffsetRefrerence::None, offset) => {
                            let offset = offset as u32;
                            self.write(&lui(rd, offset >> 12));
                            let lo = (offset & (u32::MAX >> 20)) as u16;
                            if lo != 0 {
                                self.write(&addi(rd, rd, lo));
                            }
                        }
                        (OffsetRefrerence::Reg(_), _) => {
                            panic!("invalid expression");
                        }
                        (OffsetRefrerence::Symbol(sym), offset) => {
                            let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                            self.emit_relocation(ElfRelocType::PCREL_HI20, Some(sym), Some(offset));
                            self.emit_relocation(ElfRelocType::RELAX, None, Some(offset));
                            let local_sym = self.emit_relocation_label();
                            self.write(&auipc(rd, 0));

                            self.emit_relocation(ElfRelocType::PCREL_LO12_I, Some(local_sym), None);
                            self.emit_relocation(ElfRelocType::RELAX, None, Some(0));
                            self.write(&addi(rd, rd, 0));
                        }
                    }
                }
                "li" => {
                    let (rd, n) = extract!(Reg, Number);
                    let n = n as u32;
                    self.write(&lui(rd, n >> 12));
                    let lo = (n & (u32::MAX >> 20)) as u16;
                    if lo != 0 {
                        self.write(&addi(rd, rd, lo));
                    }
                }
                "beqz" => {
                    let (rs, sym) = extract!(Reg, Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::BRANCH, Some(sym), None);
                    self.write(&beq(rs, 0, 0));
                }
                "bnez" => {
                    let (rs, sym) = extract!(Reg, Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::BRANCH, Some(sym), None);
                    self.write(&bne(rs, 0, 0));
                }
                "j" => match exprs.pop().expect("missing argument").into_offset() {
                    (OffsetRefrerence::None, offset) => {
                        self.emit_relocation(ElfRelocType::JAL, None, Some(offset));
                        self.write(&jal(0, offset as u32));
                    }
                    (OffsetRefrerence::Symbol(sym), offset) => {
                        let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                        self.emit_relocation(ElfRelocType::JAL, Some(sym), Some(offset));
                        self.write(&jal(0, 0));
                    }
                    e => panic!("unexpected {e:?}"),
                },
                "ret" => {
                    self.write(&jalr(0, 1, 0));
                }

                insn => panic!("unsupported instruction {insn:?}"),
            }
        }

        /// Emits a new relocation entry at the current address.
        fn emit_relocation(
            &mut self,
            reloc: ElfRelocType,
            symbol: Option<usize>,
            addend: Option<isize>,
        ) {
            let reloc = Reloc {
                r#type: reloc,
                symbol,
                offset: self.open_section.borrow().buf.len(),
                addend,
            };

            self.open_section.borrow_mut().relocs.push(reloc);
        }

        /// This function emits a label at the current address used for
        /// relaxable sequences.
        ///
        /// Due to how relaxation works, we must "link" the HI and LO
        /// relocations somehow, otherwise, it's considered unsafe. The way
        /// GAS does is that, instead of the two relocations pointing to the
        /// same symbol, the first points to the symbol itself, and the second
        /// points to a fake label at the address of the first relocation:
        ///
        /// Here's the output for the pseudoinstruction `lla a1, my_label`:
        /// ```
        /// 00000000 <.L0 >:
        ///    0:   00000597                auipc   a1,0x0
        ///                       0: R_RISCV_PCREL_HI20   my_label
        ///                       0: R_RISCV_RELAX        *ABS*
        ///    4:   00058593                mv      a1,a1
        ///                       4: R_RISCV_PCREL_LO12_I .L0
        ///                       4: R_RISCV_RELAX        *ABS*
        /// ```
        ///
        /// This "fake" label is a real, local symbol present in ELF's .symtab
        /// but with a twist: it uses a special name (".L0 ", with the space).
        /// libopcodes, from Binutils, then uses this name to decide wether or
        /// not to display/emit the fake label. For example, this label won't
        /// show up in the disassembler output from objdump.
        ///
        /// Reference: <https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-elf.adoc#pc-relative-symbol-addresses>.
        fn emit_relocation_label(&mut self) -> usize {
            /// From libopcodes:
            ///
            /// These fake label defines are use by both the assembler, and
            /// libopcodes.  The assembler uses this when it needs to generate a fake
            /// label, and libopcodes uses it to hide the fake labels in its output.
            const RISCV_FAKE_LABEL_NAME: Ident = Ident::new_inline(".L0 ");
            #[allow(dead_code)]
            const RISCV_FAKE_LABEL_CHAR: char = ' ';

            let (section_name, pos) = {
                let section = self.open_section.borrow();
                (section.name.clone(), section.buf.len())
            };

            self.emit_symbol(
                RISCV_FAKE_LABEL_NAME.clone(),
                SymbolType::LocalInternal,
                Some((section_name, pos)),
            )
        }

        fn write(&mut self, bytes: &[u8]) {
            self.open_section.borrow_mut().buf.extend_from_slice(bytes);
        }

        pub fn assemble(&mut self) {
            for item in self.parser.take().unwrap() {
                match item {
                    TopLevel::Directive(ident, exprs) => self.consume_directive(ident, exprs),
                    TopLevel::Label(label) => {
                        let section = self.open_section.borrow();
                        let section_name = section.name.clone();
                        let val = section.buf.len();
                        drop(section);
                        let label = label.clone();

                        self.emit_symbol(label, SymbolType::Unknown, Some((section_name, val)));
                    }
                    TopLevel::Instruction(ident, exprs) => self.emit_instruction(ident, exprs),
                }
            }

            for sec in &self.sections {
                let sec = sec.borrow();
                self.symbols.push(Symbol {
                    id: self.symbols.len(),
                    name: sec.name.clone(),
                    r#type: SymbolType::Section,
                    value: Some((sec.name.clone(), 0)),
                });
            }

            for sym in &mut self.symbols {
                if sym.r#type == SymbolType::Unknown {
                    sym.r#type = if sym.value.is_some() {
                        SymbolType::Local
                    } else {
                        SymbolType::Global
                    };
                }
            }

            self.symbols.sort_unstable_by_key(|sym| sym.r#type);
        }
    }

    fn reloc_from_func(insn: &str, func: Ident) -> ElfRelocType {
        let r#type = match func.as_str() {
            "hi" => ElfRelocType::HI20,
            "lo" if matches!(insn, "addi") => ElfRelocType::LO12_I,
            "pcrel_hi" => ElfRelocType::PCREL_HI20,
            "pcrel_lo" if matches!(insn, "addi") => ElfRelocType::PCREL_LO12_I,
            reloc => panic!("unknown combination {reloc:?} for insn {insn:?}"),
        };
        r#type
    }
}
