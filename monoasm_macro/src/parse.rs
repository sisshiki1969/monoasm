use proc_macro2::Punct;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Expr,
};
use syn::{token, Error, Ident, LitInt, Token};

///----------------------------------------------------------------------
///
///  General Registers.
///
///----------------------------------------------------------------------
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Reg {
    Rax = 0,
    Rcx = 1,
    Rdx = 2,
    Rbx = 3,
    Rsp = 4,
    Rbp = 5,
    Rsi = 6,
    Rdi = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
    RIP = 16,
}

impl Reg {
    pub fn none() -> Self {
        Reg::Rax
    }

    pub fn is_rip(&self) -> bool {
        *self == Reg::RIP
    }

    pub fn from(num: u64) -> Self {
        match num {
            0 => Reg::Rax,
            1 => Reg::Rcx,
            2 => Reg::Rdx,
            3 => Reg::Rbx,
            4 => Reg::Rsp,
            5 => Reg::Rbp,
            6 => Reg::Rsi,
            7 => Reg::Rdi,
            8 => Reg::R8,
            9 => Reg::R9,
            10 => Reg::R10,
            11 => Reg::R11,
            12 => Reg::R12,
            13 => Reg::R13,
            14 => Reg::R14,
            15 => Reg::R15,
            16 => Reg::RIP,
            _ => unreachable!("Illegal register number."),
        }
    }

    pub fn from_str(string: impl Into<String>) -> Option<Reg> {
        let mut string = string.into();
        string.make_ascii_lowercase();
        let reg = match string.as_str() {
            "rax" => Reg::Rax,
            "rcx" => Reg::Rcx,
            "rdx" => Reg::Rdx,
            "rbx" => Reg::Rbx,
            "rsp" => Reg::Rsp,
            "rbp" => Reg::Rbp,
            "rsi" => Reg::Rsi,
            "rdi" => Reg::Rdi,
            "r8" => Reg::R8,
            "r9" => Reg::R9,
            "r10" => Reg::R10,
            "r11" => Reg::R11,
            "r12" => Reg::R12,
            "r13" => Reg::R13,
            "r14" => Reg::R14,
            "r15" => Reg::R15,
            "rip" => Reg::RIP,
            _ => return None,
        };
        Some(reg)
    }
}

impl ToTokens for Reg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let r = *self as u64;
        let ts = quote!(Reg::from(#r));
        tokens.extend(ts);
    }
}

///----------------------------------------------------------------------
///
///  Addressing modes.
///
///----------------------------------------------------------------------
#[derive(Copy, Clone, PartialEq)]
pub enum Mode {
    Ind = 0,   // [reg]
    InD8 = 1,  // [reg + disp8]
    InD32 = 2, // [reg + disp32]
    Reg = 3,   // reg
}

///----------------------------------------------------------------------
///
///  Designation for general registers.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Register(TokenStream);

impl Parse for Register {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        Self::parse_register(input, &input.parse::<Ident>()?.to_string())
    }
}

impl ToTokens for Register {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let reg = self.0.clone();
        let ts = quote! ( Reg::from(#reg) );
        tokens.extend(ts);
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R({})", self.0)
    }
}

impl Register {
    pub fn new(ts: TokenStream) -> Self {
        Self(ts)
    }

    pub fn get(self) -> TokenStream {
        self.0
    }

    pub fn parse_register(input: ParseStream, ident: &String) -> Result<Register, Error> {
        if ident == "R" {
            // e.g. "R(13)"
            let content;
            syn::parenthesized!(content in input);
            let s = content.parse::<Expr>()?;
            Ok(Register::new(quote!(#s)))
        } else {
            // e.g. "rax"
            let reg = Reg::from_str(ident).ok_or(input.error("Expected register name."))? as u64;
            Ok(Register::new(quote!(#reg)))
        }
    }
}

///----------------------------------------------------------------------
///
///  Indirect addressing modes.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct IndAddr {
    pub base: Register,
    pub disp: Disp,
}

impl IndAddr {
    pub fn new(base: Register, disp: Disp) -> Self {
        Self { base, disp }
    }
}

impl Parse for IndAddr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let content;
        syn::bracketed!(content in input);
        let base = content.parse::<Register>()?;
        let disp = content.parse::<Disp>()?;
        Ok(IndAddr { base, disp })
    }
}

impl std::fmt::Display for IndAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})[{:?}]", self.disp, self.base)
    }
}

impl ToTokens for IndAddr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { base, disp } = self;
        tokens.extend(quote!( Or::new(#base, #disp) ));
    }
}

///----------------------------------------------------------------------
///
///  EFLAGS.
///
///----------------------------------------------------------------------
#[derive(Clone, PartialEq, Debug)]
pub enum Flag {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

///----------------------------------------------------------------------
///
///  Destination operand for jump and call instructions.
///
///----------------------------------------------------------------------
#[derive(Clone, PartialEq, Debug)]
pub enum Dest {
    Reg(Reg),
    Rel(Ident),
}

impl Parse for Dest {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let dest: Ident = input.parse()?;
            let reg = Reg::from_str(&dest.to_string());
            match reg {
                Some(reg) => Ok(Dest::Reg(reg)),
                None => Ok(Dest::Rel(dest)),
            }
        } else {
            Err(lookahead.error())
        }
    }
}

pub fn is_single(input: ParseStream) -> bool {
    input.peek2(Token![,]) || input.peek2(Token![;])
}

///----------------------------------------------------------------------
///
///  Displacement for indirect addressing.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Disp {
    Imm(TokenStream),
    Label(Ident),
}

impl ToTokens for Disp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Disp::Label(label) => quote!(
                Mode::from_label(#label)
            ),
            Disp::Imm(ts) => quote!(
                Mode::from_disp(#ts)
            ),
        };
        tokens.extend(ts);
    }
}

impl std::fmt::Display for Disp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(ts) => write!(f, "{}", ts),
            Self::Label(label) => write!(f, "{}", label),
        }
    }
}

impl Parse for Disp {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if input.is_empty() {
            // e.g. "[rax]"
            Ok(Disp::Imm(quote!(0i32)))
        } else if lookahead.peek(Token![-]) || lookahead.peek(Token![+]) {
            let sign = match input.parse::<Punct>()?.as_char() {
                '-' => -1,
                '+' => 1,
                _ => return Err(lookahead.error()),
            };
            let lookahead = input.lookahead1();
            if lookahead.peek(token::Paren) {
                // e.g. "[rax - (4)]"
                let content;
                syn::parenthesized!(content in input);
                let expr = content.parse::<Expr>()?;
                let expr = if sign == 1 {
                    quote!(#expr as i32)
                } else {
                    quote!(-(#expr) as i32)
                };
                Ok(Disp::Imm(expr))
            } else if lookahead.peek(LitInt) {
                // e.g. "[rax + 4]"
                let ofs: i32 = input.parse::<LitInt>()?.base10_parse()?;
                let expr = if sign == 1 {
                    quote!(#ofs as i32)
                } else {
                    quote!(-(#ofs) as i32)
                };
                Ok(Disp::Imm(expr))
            } else if lookahead.peek(Ident) {
                let label = input.parse::<Ident>().unwrap();
                Ok(Disp::Label(label))
            } else {
                Err(lookahead.error())
            }
        } else {
            Err(lookahead.error())
        }
    }
}
