fn main() {
    let program = std::env::args().nth(1).unwrap();
    let program = std::fs::read_to_string(program).unwrap();

    let tokens = lexer::Lexer::new(&program).run();
    let parsed = parser::Parser::new(tokens).parse();
    let mut assembler = assembler::Assembler::new(parsed);
    assembler.assemble();

    elf::write_elf_file(&mut assembler);
}

type Reg = u8;

fn lui(rd: Reg, imm: u32) -> [u8; 4] {
    u(0b0110111, rd, imm).to_le_bytes()
}

fn auipc(rd: Reg, imm: u32) -> [u8; 4] {
    u(0b0010111, rd, imm).to_le_bytes()
}

fn addi(rd: Reg, rs1: Reg, imm: u16) -> [u8; 4] {
    i(0b0010011, rd, 0b000, rs1, imm).to_le_bytes()
}

fn sb(src: Reg, base: Reg, offset: u16) -> [u8; 4] {
    s(0b0100011, offset, 0b000, base, src).to_le_bytes()
}

fn lb(rd: Reg, base: Reg, offset: u16) -> [u8; 4] {
    i(0b0000011, rd, 0b000, base, offset).to_le_bytes()
}

fn jal(rd: Reg, offset: u32) -> [u8; 4] {
    j(0b1101111, rd, offset).to_le_bytes()
}

fn bne(rs1: Reg, rs2: Reg, offset: u16) -> [u8; 4] {
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
    rs1: u8 => 15..20,
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
    use std::iter::Peekable;

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

    pub struct Lexer<'a> {
        file: &'a str,
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

    impl<'a> Lexer<'a> {
        pub fn new(file: &'a str) -> Self {
            Self { file }
        }

        fn take_while<F: FnMut(char) -> bool>(
            &mut self,
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

        pub fn run(mut self) -> Vec<(TokenType, Lexeme)> {
            let mut iter = self.file.char_indices().peekable();
            let mut tokens = Vec::new();

            while let Some((idx, c)) = iter.next() {
                let token = match c {
                    ',' => (TokenType::Comma, c.into()),
                    ':' => (TokenType::Colon, c.into()),
                    '-' => (TokenType::Hyphen, c.into()),
                    '+' => (TokenType::Plus, c.into()),
                    '(' => (TokenType::LParen, c.into()),
                    ')' => (TokenType::RParen, c.into()),
                    '\n' => (TokenType::NewLine, c.into()),
                    '"' => {
                        let mut escaping = false;
                        let end = iter
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
                        (TokenType::String, self.file[idx..=end].into())
                    }
                    '#' => {
                        let _ = self.take_while(&mut iter, |c| c != '\n');
                        continue;
                    }
                    c if c.is_whitespace() => continue,
                    _ => {
                        let mut expect_integer_type = c == '0';
                        let mut notation = if c.is_ascii_digit() {
                            Some(IntNotation::Decimal)
                        } else {
                            None
                        };

                        let mut last = c;
                        let end = self
                            .take_while(&mut iter, |c| {
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

                        (r#type, self.file[idx..=end].into())
                    }
                };

                tokens.push(token);
            }

            tokens
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
        let lexer = Lexer { file };
        let tokens = lexer.run();

        dbg!(tokens);
    }
}

mod parser {
    use std::ops::{Deref, DerefMut};

    use ecow::EcoString;

    use crate::lexer::Lexeme;

    use super::lexer::TokenType;

    #[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Ident(Lexeme);

    impl Ident {
        pub const fn new_inline(s: &str) -> Self {
            Self(Lexeme::inline(s))
        }
    }

    impl From<&str> for Ident {
        fn from(value: &str) -> Self {
            Self(value.into())
        }
    }

    impl std::fmt::Debug for Ident {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!("Ident({self})"))
        }
    }

    impl std::fmt::Display for Ident {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(&self.0)
        }
    }

    impl Deref for Ident {
        type Target = EcoString;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl DerefMut for Ident {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    #[derive(Debug, Clone)]
    pub enum TopLevel {
        Directive(Ident, Vec<Expr>),
        Label(Ident),
        Instruction(Ident, Vec<Expr>),
    }

    #[derive(Clone)]
    pub enum Expr {
        String(Vec<u8>),
        Number(isize),
        Symbol(Ident),
        RelocFunction(Ident, Box<Expr>),
        Deref(Box<Expr>),
        App(Box<Expr>, Box<Expr>),
        BinOp(Box<Expr>, OperationType, Box<Expr>),
        Unary(OperationType, Box<Expr>),
    }

    impl std::fmt::Debug for Expr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::String(arg0) => f.write_fmt(format_args!(
                    "String({:?})",
                    String::from_utf8_lossy(arg0).escape_debug()
                )),
                Self::Number(arg0) => f.write_fmt(format_args!("Number({arg0})")),
                Self::Symbol(arg0) => f.write_fmt(format_args!("Symbol({arg0:?})")),
                Self::Deref(arg0) => f.write_fmt(format_args!("Deref({arg0:?})")),
                Self::RelocFunction(arg0, arg1) => {
                    f.write_fmt(format_args!("RelocFunction({arg0:?}, {arg1:?})"))
                }
                Self::App(arg0, arg1) => f.write_fmt(format_args!("App({arg0:?}, {arg1:?})")),
                Self::BinOp(arg0, arg1, arg2) => {
                    f.write_fmt(format_args!("BinOp({arg0:?}, {arg1:?}, {arg2:?})"))
                }
                Self::Unary(arg0, arg1) => f.write_fmt(format_args!("Unary({arg0:?}, {arg1:?})")),
            }
        }
    }

    impl std::fmt::Display for Expr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::String(arg0) => f.write_fmt(format_args!(
                    r#""{}""#,
                    String::from_utf8_lossy(arg0).escape_debug()
                )),
                Self::Number(arg0) => f.write_fmt(format_args!("{arg0}")),
                Self::Symbol(arg0) => f.write_fmt(format_args!("{arg0}")),
                Self::Deref(arg0) => f.write_fmt(format_args!("({arg0})")),
                Self::RelocFunction(arg0, arg1) => f.write_fmt(format_args!("%{arg0}({arg1})")),
                Self::App(arg0, arg1) => f.write_fmt(format_args!("{arg0}({arg1})")),
                Self::BinOp(arg0, arg1, arg2) => f.write_fmt(format_args!("{arg0} {arg1} {arg2}")),
                Self::Unary(arg0, arg1) => f.write_fmt(format_args!("{arg0}{arg1}")),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum OperationType {
        Plus,
        Minus,
    }

    impl std::fmt::Display for OperationType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OperationType::Plus => f.write_str("+"),
                OperationType::Minus => f.write_str("-"),
            }
        }
    }

    pub struct Parser {
        tokens: Vec<(TokenType, Lexeme)>,
        cursor: usize,
    }

    impl Parser {
        pub fn new(tokens: Vec<(TokenType, Lexeme)>) -> Self {
            Self { tokens, cursor: 0 }
        }

        fn peek(&mut self) -> TokenType {
            self.tokens
                .get(self.cursor)
                .map(|(ty, _)| *ty)
                .unwrap_or(TokenType::Eof)
        }

        fn step(&mut self) -> (TokenType, &Lexeme) {
            static EMPTY: Lexeme = Lexeme::inline("");
            if let Some(token) = self.tokens.get(self.cursor) {
                self.cursor += 1;
                (token.0, &token.1)
            } else {
                (TokenType::Eof, &EMPTY)
            }
        }

        fn is(&mut self, token: TokenType) -> bool {
            self.peek() == token
        }

        fn eat(&mut self, token: TokenType) -> Option<&Lexeme> {
            if self.is(token) {
                Some(self.step().1)
            } else {
                None
            }
        }

        pub fn parse(mut self) -> Vec<TopLevel> {
            let mut parsed = Vec::new();
            loop {
                match self.peek() {
                    TokenType::NewLine => {
                        self.step();
                        continue;
                    }
                    TokenType::Eof => break,
                    _ => parsed.push(self.parse_token()),
                }
            }
            parsed
        }

        fn parse_token(&mut self) -> TopLevel {
            let identifier = self.eat(TokenType::Identifier).unwrap().clone();

            if self.eat(TokenType::Colon).is_some() {
                return TopLevel::Label(Ident(identifier));
            }

            let args = self.parse_list();
            if identifier.starts_with('.') {
                TopLevel::Directive(Ident(identifier.trim_start_matches('.').into()), args)
            } else {
                TopLevel::Instruction(Ident(identifier), args)
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

            let val = if self.eat(TokenType::LParen).is_some() {
                let val = self.parse_value().expect("expected value");
                self.eat(TokenType::RParen).expect("expected r-paren");
                Expr::Deref(Box::new(val))
            } else if self.eat(TokenType::Hyphen).is_some() {
                if let Some(int) = self.eat(TokenType::Integer) {
                    let (int, radix) = extract_radix(int);
                    Expr::Number(isize::from_str_radix(&format!("-{int}"), radix).unwrap())
                } else {
                    Expr::Unary(OperationType::Minus, Box::new(self.parse_value().unwrap()))
                }
            } else if self.eat(TokenType::Plus).is_some() {
                Expr::Unary(OperationType::Plus, Box::new(self.parse_value().unwrap()))
            } else if let Some(int) = self.eat(TokenType::Integer) {
                let (int, radix) = extract_radix(int);
                Expr::Number(isize::from_str_radix(int, radix).unwrap())
            } else if let Some(ident) = self.eat(TokenType::Identifier) {
                if ident.starts_with('%') {
                    let func_name = Ident((&ident[1..]).into());

                    self.eat(TokenType::LParen).expect("invalid function call");
                    let val = self.parse_value().expect("call must have a parameter");
                    self.eat(TokenType::RParen).expect("invalid function call");

                    Expr::RelocFunction(func_name, Box::new(val))
                } else {
                    Expr::Symbol(Ident(ident.into()))
                }
            } else if let Some(fo) = self.eat(TokenType::String) {
                let string = fo.get(1..fo.len() - 1).unwrap_or_default().to_string();
                let mut buf = Vec::with_capacity(string.as_bytes().len());
                let mut escaping = false;
                for c in string.chars() {
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
            } else {
                return None;
            };

            let lhs = if self.eat(TokenType::LParen).is_some() {
                let inner = self.parse_value().expect("expected value");
                self.eat(TokenType::RParen).expect("expected r-paren");
                Expr::App(Box::new(val), Box::new(inner))
            } else {
                val
            };

            let op = if self.eat(TokenType::Plus).is_some() {
                OperationType::Plus
            } else if self.eat(TokenType::Hyphen).is_some() {
                OperationType::Minus
            } else {
                return Some(lhs);
            };

            let rrhs = self.parse_value().expect("missing right bin op expression");

            Some(Expr::BinOp(Box::new(lhs), op, Box::new(rrhs)))
        }
    }

    #[test]
    fn foo() {
        let file = include_str!("../example.s");
        let tokens = super::lexer::Lexer::new(file).run();
        let parsed = Parser { tokens, cursor: 0 }.parse();

        let mut string = String::new();
        for p in &parsed {
            let fmt = match p {
                TopLevel::Directive(identifier, values) => {
                    let mut foo = format!(".{} ", identifier.0.to_string());
                    for v in values {
                        foo.push_str(&v.to_string());
                        foo.push_str(", ");
                    }
                    foo
                }
                TopLevel::Label(identifier) => format!("{}:", identifier.0),
                TopLevel::Instruction(identifier, values) => {
                    let mut foo = format!("    {} ", identifier.0.to_string());
                    for v in values {
                        foo.push_str(&v.to_string());
                        foo.push_str(", ");
                    }
                    foo
                }
            };
            string.push_str(&fmt);
            string.push('\n');
        }

        eprintln!("{string}"); // dbg!(parsed);
    }
}

mod assembler {
    use std::{cell::RefCell, rc::Rc, str::FromStr};

    use crate::{addi, auipc, bne, elf::reloc::ElfRelocType, jal, lb, lui, sb};

    use super::{
        Reg,
        parser::{Expr, Ident, OperationType, TopLevel},
    };

    fn register_from_name(mut reg: &str) -> Reg {
        const REGISTERS: &[&str] = &[
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3",
            "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
            "t3", "t4", "t5", "t6",
        ];

        if reg == "fp" {
            reg = "s0";
        }

        REGISTERS
            .iter()
            .enumerate()
            .find(|(_, o)| **o == reg)
            .expect("unknown reg")
            .0 as u8
    }

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
        Text,
        Bss,
        Data,
        Rodata,
        Unknown,
    }

    pub struct Section {
        pub name: Ident,
        pub r#type: SectionType,
        pub buf: Vec<u8>,
        pub relocs: Vec<Reloc>,
    }

    impl Default for Section {
        fn default() -> Self {
            Self {
                name: ".text".into(),
                r#type: Default::default(),
                buf: Default::default(),
                relocs: Default::default(),
            }
        }
    }

    pub struct Reloc {
        pub r#type: ElfRelocType,
        pub symbol: Option<usize>,
        pub offset: usize,
        pub addend: Option<isize>,
    }

    pub struct Assembler {
        items: Vec<TopLevel>,

        pub symbols: Vec<Symbol>,

        pub sections: Vec<Rc<RefCell<Section>>>,
        pub open_section: Rc<RefCell<Section>>,
    }

    impl Assembler {
        pub fn new(items: Vec<TopLevel>) -> Self {
            let open_section = Rc::new(RefCell::new(Section::default()));

            Self {
                items,

                symbols: Default::default(),

                sections: vec![open_section.clone()],
                open_section,
            }
        }

        fn consume_directive(&mut self, mut ident: Ident, mut exprs: Vec<Expr>) {
            macro_rules! extract_values {
                ($($ty:ident),+) => {
                    $(match exprs.pop().unwrap() {
                        Expr::$ty(arg0) => arg0,
                        _ => panic!(),
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
                    let sym = extract_values!(Symbol);
                    let r#type = match sym.as_str() {
                        _ if sym.starts_with(".text") => SectionType::Text,
                        _ if sym.starts_with(".data") => SectionType::Data,
                        _ if sym.starts_with(".rodata") => SectionType::Rodata,
                        _ if sym.starts_with(".bss") => SectionType::Bss,
                        _ => SectionType::Unknown,
                    };
                    self.emit_section(sym, r#type)
                }
                "text" => self.emit_section(".text".into(), SectionType::Text),
                "data" => self.emit_section(".data".into(), SectionType::Data),
                "rodata" => self.emit_section(".rodata".into(), SectionType::Rodata),
                "bss" => self.emit_section(".bss".into(), SectionType::Bss),

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
            };
        }

        fn emit_section(&mut self, section_name: Ident, r#type: SectionType) {
            if let Some(section) = self
                .sections
                .iter()
                .find(|section| section.borrow().name == section_name)
            {
                self.open_section = section.clone();
            } else {
                let section = Rc::new(RefCell::new(Section {
                    name: section_name,
                    r#type,
                    ..Default::default()
                }));
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

        fn consume_instruction(&mut self, mut ident: Ident, mut exprs: Vec<Expr>) {
            macro_rules! extract {
                (v: Reg) => {
                    match exprs.remove(0) {
                        Expr::Symbol(arg0) => register_from_name(&arg0),
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
                    let (dst, offset) = match &expr {
                        Expr::Deref(expr) => match &**expr {
                            Expr::Symbol(ident) => (ident, 0),
                            _ => panic!(),
                        },
                        Expr::App(expr, expr1) => match (&**expr, &**expr1) {
                            (Expr::Number(offset), Expr::Symbol(ident)) => (ident, *offset),
                            _ => panic!(),
                        },
                        _ => panic!(),
                    };
                    let dst = register_from_name(&dst);

                    self.write(&sb(src, dst, offset as u16));
                }
                "lb" => {
                    let (rd,) = extract!(Reg);

                    let expr = exprs.pop().unwrap();
                    let (src, offset) = match &expr {
                        Expr::Deref(expr) => match &**expr {
                            Expr::Symbol(ident) => (ident, 0),
                            _ => panic!(),
                        },
                        Expr::App(expr, expr1) => match (&**expr, &**expr1) {
                            (Expr::Number(offset), Expr::Symbol(ident)) => (ident, *offset),
                            _ => panic!(),
                        },
                        _ => panic!(),
                    };
                    let src = register_from_name(&src);

                    self.write(&lb(rd, src, offset as u16));
                }
                "auipc" => {
                    let (rd, imm) = extract!(Reg, Number);
                    self.write(&auipc(rd, imm as u32));
                }
                "jal" => {
                    let (rd, offset) = extract!(Reg, Number);
                    self.write(&jal(rd, offset as u32));
                }
                "bne" => {
                    let (rs1, rs2, sym) = extract!(Reg, Reg, Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::BRANCH, Some(sym), None);
                    self.write(&bne(rs1, rs2, 0));
                }
                "j" => {
                    let (sym,) = extract!(Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::JAL, Some(sym), None);
                    self.write(&jal(0, 0));
                }

                // Pseudoinstructions
                "nop" => self.write(&addi(0, 0, 0)),
                "mv" => {
                    let (rd, rs) = extract!(Reg, Reg);
                    self.write(&addi(rd, rs, 0));
                }
                "lla" => {
                    let (rd,) = extract!(Reg);
                    let (sym, addend) = Self::process_addr(exprs.pop().unwrap());
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::PCREL_HI20, Some(sym), Some(addend));
                    self.emit_relocation(ElfRelocType::RELAX, None, Some(0));
                    let local_sym = self.emit_relocation_label();
                    self.write(&auipc(rd, 0));

                    self.emit_relocation(ElfRelocType::PCREL_LO12_I, Some(local_sym), Some(addend));
                    self.emit_relocation(ElfRelocType::RELAX, None, Some(0));
                    self.write(&addi(rd, rd, 0));
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
                "bnez" => {
                    let (rs, sym) = extract!(Reg, Symbol);
                    let sym = self.emit_symbol(sym, SymbolType::Unknown, None);

                    self.emit_relocation(ElfRelocType::BRANCH, Some(sym), None);
                    self.write(&bne(rs, 0, 0));
                }

                _ => panic!(),
            }
        }

        fn process_addr(expr: Expr) -> (Ident, isize) {
            match expr {
                Expr::Symbol(sym) => (sym, 0),
                Expr::BinOp(lhs, OperationType::Plus, rhs) => {
                    let (Expr::Symbol(lhs), Expr::Number(offset)) = (&*lhs, &*rhs) else {
                        panic!();
                    };
                    (lhs.clone(), *offset)
                }
                _ => panic!(),
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
            for item in self.items.clone() {
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
                    TopLevel::Instruction(ident, exprs) => self.consume_instruction(ident, exprs),
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

mod elf {
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

    pub fn write_elf_file(assembler: &mut Assembler) {
        let class = elf_header::ElfClass::C32;
        let mut elf_file = ElfFile::new();

        let mut relocations = vec![];

        let mut section_by_name = HashMap::with_capacity(assembler.sections.len() * 4);

        assembler.symbols.sort_unstable_by_key(|sym| sym.r#type);
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

        let out = Path::new(&std::env::args().nth(2).unwrap()).to_path_buf();
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
}
