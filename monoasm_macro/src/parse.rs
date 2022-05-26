use proc_macro2::Group;
use proc_macro2::Punct;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::ParseBuffer;
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
            "cl" => Reg::Rcx,
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

    fn check_register(input: &ParseBuffer, ident: &Ident) -> Result<Option<Register>, Error> {
        if ident == "R" {
            // e.g. "R(13)"
            let content;
            syn::parenthesized!(content in input);
            let s = content.parse::<Expr>()?;
            Ok(Some(Register::new(quote!(#s))))
        } else {
            // e.g. "rax"
            match Reg::from_str(ident.to_string()) {
                Some(reg) => {
                    let reg = reg as u64;
                    Ok(Some(Register::new(quote!(#reg))))
                }
                None => Ok(None),
            }
        }
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
///  Floating pointer register(xmm).
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Xmm(pub TokenStream);

fn parse_xmm(input: ParseStream, ident: &String) -> Result<TokenStream, Error> {
    assert!(ident.starts_with("xmm"));
    if ident.len() == 3 {
        if input.peek(token::Paren) {
            let gr = input.parse::<Group>()?;
            Ok(gr.stream())
        } else {
            Err(input.error(format!(
                "Expected xmm register number. e.g. xmm0 or xmm(0) actual:{}",
                ident,
            )))
        }
    } else {
        if let Ok(no) = ident[3..].parse::<u8>() {
            if no > 15 {
                Err(input.error(format!("Invalid xmm register name. {}", ident)))
            } else {
                Ok(quote!(#no as u64))
            }
        } else {
            Err(input.error(format!("Invalid xmm register name. {}", ident)))
        }
    }
}

impl Parse for Xmm {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let reg = input.parse::<Ident>()?.to_string();
            if reg.starts_with("xmm") {
                Ok(Xmm(parse_xmm(input, &reg)?))
            } else {
                Err(input.error("Expected xmm register name."))
            }
        } else {
            Err(input.error("Expected xmm register name."))
        }
    }
}

impl std::fmt::Display for Xmm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "xmm({})", self.0)
    }
}

impl ToTokens for Xmm {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let r = &self.0;
        let ts = quote!(Rm::reg(Reg::from(#r)));
        tokens.extend(ts);
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
    pub scale: Scale,
    pub disp: Disp,
}

impl IndAddr {
    pub fn new(base: Register, scale: Scale, disp: Disp) -> Self {
        Self { base, scale, disp }
    }
}

impl Parse for IndAddr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let content;
        syn::bracketed!(content in input);
        let base = content.parse::<Register>()?;
        if content.peek(Token![-]) || content.peek(Token![+]) {
            let negate = match content.parse::<Punct>()?.as_char() {
                '-' => true,
                '+' => false,
                _ => unreachable!(),
            };

            if negate {
                let disp = content.parse::<Disp>()?;
                Ok(IndAddr {
                    base,
                    scale: Scale::None,
                    disp: disp.neg(),
                })
            } else if content.peek(Ident) {
                let ident = content.parse::<Ident>().unwrap();
                match Register::check_register(&content, &ident)? {
                    Some(reg) => {
                        content.parse::<Token![*]>()?;
                        let scale = match content.parse::<LitInt>()?.base10_parse::<u8>()? {
                            1 => Scale::S1(reg),
                            2 => Scale::S2(reg),
                            4 => Scale::S4(reg),
                            8 => Scale::S8(reg),
                            _ => unreachable!("invalid scale number."),
                        };
                        Ok(IndAddr {
                            base,
                            scale,
                            disp: Disp::Imm(quote!(0)),
                        })
                    }
                    None => Ok(IndAddr {
                        base,
                        scale: Scale::None,
                        disp: Disp::Label(ident),
                    }),
                }
            } else {
                let disp = content.parse::<Disp>()?;
                Ok(IndAddr {
                    base,
                    scale: Scale::None,
                    disp,
                })
            }
        } else {
            Ok(IndAddr {
                base,
                scale: Scale::None,
                disp: Disp::Imm(quote!(0i32)),
            })
        }
    }
}

impl std::fmt::Display for IndAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} + ({})]", self.base, self.disp)
    }
}

impl ToTokens for IndAddr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { base, disp, scale } = self;
        tokens.extend(quote!( Rm::ind(#base, #disp, #scale) ));
    }
}

#[derive(Clone, Debug)]
pub enum Scale {
    None,
    S1(Register),
    S2(Register),
    S4(Register),
    S8(Register),
}

impl ToTokens for Scale {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            Self::None => quote!(Scale::None),
            Self::S1(reg) => quote!( Scale::S1(#reg) ),
            Self::S2(reg) => quote!( Scale::S2(#reg) ),
            Self::S4(reg) => quote!( Scale::S4(#reg) ),
            Self::S8(reg) => quote!( Scale::S8(#reg) ),
        });
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
    A,
    Ae,
    B,
    Be,
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
            match Reg::from_str(&dest.to_string()) {
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

impl Disp {
    fn neg(self) -> Self {
        match self {
            Disp::Imm(ts) => Disp::Imm(quote!(-(#ts))),
            disp => disp,
        }
    }
}

impl ToTokens for Disp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Disp::Label(label) => quote!(
                Disp::from_label(#label)
            ),
            Disp::Imm(ts) => quote!(
                Disp::from_disp(#ts)
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
        if input.peek(token::Paren) {
            // e.g. "[rax - (4)]"
            let content;
            syn::parenthesized!(content in input);
            let expr = content.parse::<Expr>()?;
            let expr = quote!(#expr as i32);
            Ok(Disp::Imm(expr))
        } else if input.peek(LitInt) {
            // e.g. "[rax + 4]"
            let ofs: i32 = input.parse::<LitInt>()?.base10_parse()?;
            let expr = quote!(#ofs as i32);
            Ok(Disp::Imm(expr))
        } else if input.peek(Ident) {
            let label = input.parse::<Ident>().unwrap();
            Ok(Disp::Label(label))
        } else {
            Err(input.error("invalid displacement expression."))
        }
    }
}
