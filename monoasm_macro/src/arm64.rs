//! Parser and code generator for the [`monoasm_arm64!`](crate::monoasm_arm64)
//! macro — an AArch64 (A64) assembly DSL that mirrors the x86-64
//! [`monoasm!`](crate::monoasm) macro.
//!
//! A proc-macro always runs on the host and cannot observe the *target*
//! architecture, so the two ISAs cannot share one entry point: pick
//! `monoasm!` for x86-64 code and `monoasm_arm64!` for AArch64 code (gate
//! the call site with `#[cfg(target_arch = ...)]` if a crate emits both).
//!
//! The generated code drives the builder methods on
//! `monoasm::JitMemory` from the `arm64` backend and refers to the
//! re-exported `GReg` / `FReg` / `Cond` / `DestLabel` types, so the call
//! site needs `use monoasm::*;` in scope (the same convention as
//! `monoasm!`).

use proc_macro2::{Group, Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{token, Error, Expr, Ident, LitFloat, LitInt, Token};

// ---------------------------------------------------------------------------
// Statements: `base_expr , <instr> ;  <instr> ; ...`
// ---------------------------------------------------------------------------

pub(crate) struct Stmts {
    pub(crate) base: Expr,
    pub(crate) contents: Vec<Inst>,
}

impl Parse for Stmts {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let base: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let mut contents = vec![];
        while !input.is_empty() {
            contents.push(input.parse::<Inst>()?);
        }
        Ok(Stmts { base, contents })
    }
}

// ---------------------------------------------------------------------------
// Registers
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum RegKind {
    /// 64-bit general-purpose view (`Xn`, `sp`, `xzr`, `lr`, `fp`).
    X,
    /// 32-bit general-purpose view (`Wn`, `wzr`).
    W,
    /// Scalar double-precision SIMD&FP view (`Dn`).
    D,
}

#[derive(Clone)]
pub(crate) struct Reg {
    kind: RegKind,
    /// Token stream constructing the backend register (`GReg(..)` or
    /// `FReg(..)`).
    ts: TokenStream,
    /// True when written as the `sp` keyword (selects the SP-form encoding
    /// for `mov`).
    is_sp: bool,
}

impl Reg {
    fn greg(&self) -> TokenStream {
        if self.kind == RegKind::D {
            panic!("monoasm_arm64: expected a general-purpose register, found a D register");
        }
        self.ts.clone()
    }

    fn freg(&self) -> TokenStream {
        if self.kind != RegKind::D {
            panic!("monoasm_arm64: expected a D register, found a general-purpose register");
        }
        self.ts.clone()
    }
}

fn parse_reg(input: ParseStream) -> Result<Reg, Error> {
    let ident: Ident = input.parse()?;
    let s = ident.to_string();
    let g = |ts, is_sp| Reg {
        kind: RegKind::X,
        ts,
        is_sp,
    };
    match s.as_str() {
        "sp" => return Ok(g(quote!(GReg(31u32)), true)),
        "xzr" => return Ok(g(quote!(GReg(31u32)), false)),
        "lr" => return Ok(g(quote!(GReg(30u32)), false)),
        "fp" => return Ok(g(quote!(GReg(29u32)), false)),
        "wzr" => {
            return Ok(Reg {
                kind: RegKind::W,
                ts: quote!(GReg(31u32)),
                is_sp: false,
            })
        }
        "x" | "w" | "d" => {
            // Dynamic form: `x(expr)`, `w(expr)`, `d(expr)`.
            let content;
            syn::parenthesized!(content in input);
            let e: Expr = content.parse()?;
            let (kind, ctor) = match s.as_str() {
                "x" => (RegKind::X, quote!(GReg)),
                "w" => (RegKind::W, quote!(GReg)),
                _ => (RegKind::D, quote!(FReg)),
            };
            return Ok(Reg {
                kind,
                ts: quote!(#ctor((#e) as u32)),
                is_sp: false,
            });
        }
        _ => {}
    }
    let (kind, ctor, max) = match s.as_bytes()[0] {
        b'x' => (RegKind::X, quote!(GReg), 30u32),
        b'w' => (RegKind::W, quote!(GReg), 30u32),
        b'd' => (RegKind::D, quote!(FReg), 31u32),
        _ => return Err(Error::new(ident.span(), "expected an AArch64 register")),
    };
    let num: u32 = s[1..]
        .parse()
        .map_err(|_| Error::new(ident.span(), "invalid AArch64 register name"))?;
    if num > max {
        return Err(Error::new(
            ident.span(),
            "AArch64 register number out of range",
        ));
    }
    Ok(Reg {
        kind,
        ts: quote!(#ctor(#num)),
        is_sp: false,
    })
}

// ---------------------------------------------------------------------------
// Immediates and condition codes
// ---------------------------------------------------------------------------

/// An immediate as a Rust integer expression. Large values that do not fit
/// `i32` (the default for unsuffixed literals) should be written as a
/// parenthesized, suffixed expression, e.g. `mov x0, (0x1234_5678_9abc_def0u64)`.
#[derive(Clone)]
pub(crate) struct Imm(TokenStream);

fn parse_imm(input: ParseStream) -> Result<Imm, Error> {
    if input.peek(Token![#]) {
        input.parse::<Token![#]>()?;
    }
    let neg = if input.peek(Token![-]) {
        input.parse::<Token![-]>()?;
        true
    } else {
        false
    };
    let body = if input.peek(token::Paren) {
        let g: Group = input.parse()?;
        let s = g.stream();
        quote!((#s))
    } else if input.peek(LitInt) {
        let lit: LitInt = input.parse()?;
        quote!(#lit)
    } else if input.peek(LitFloat) {
        let lit: LitFloat = input.parse()?;
        quote!(#lit)
    } else {
        return Err(input.error("expected an immediate (#imm or a parenthesized expression)"));
    };
    Ok(Imm(if neg { quote!(-(#body)) } else { body }))
}

fn parse_cond(input: ParseStream) -> Result<TokenStream, Error> {
    let id: Ident = input.parse()?;
    let name = match id.to_string().as_str() {
        "eq" => "Eq",
        "ne" => "Ne",
        "hs" | "cs" => "Hs",
        "lo" | "cc" => "Lo",
        "mi" => "Mi",
        "pl" => "Pl",
        "vs" => "Vs",
        "vc" => "Vc",
        "hi" => "Hi",
        "ls" => "Ls",
        "ge" => "Ge",
        "lt" => "Lt",
        "gt" => "Gt",
        "le" => "Le",
        "al" => "Al",
        _ => return Err(Error::new(id.span(), "invalid AArch64 condition code")),
    };
    let c = Ident::new(name, id.span());
    Ok(quote!(Cond::#c))
}

/// Either a register or an immediate (third operand of add/sub, shifts, …).
pub(crate) enum RegOrImm {
    Reg(Reg),
    Imm(Imm),
}

fn parse_reg_or_imm(input: ParseStream) -> Result<RegOrImm, Error> {
    if input.peek(Ident) {
        Ok(RegOrImm::Reg(parse_reg(input)?))
    } else {
        Ok(RegOrImm::Imm(parse_imm(input)?))
    }
}

/// Optional `, lsl #amount` modifier.
fn parse_opt_lsl(input: ParseStream) -> Result<Option<Imm>, Error> {
    if input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
        let id: Ident = input.parse()?;
        if id != "lsl" {
            return Err(Error::new(id.span(), "expected `lsl`"));
        }
        Ok(Some(parse_imm(input)?))
    } else {
        Ok(None)
    }
}

// ---------------------------------------------------------------------------
// Memory operands
// ---------------------------------------------------------------------------

pub(crate) enum Mem {
    /// `[base {, #off}]`
    Off(Reg, Option<Imm>),
    /// `[base, #off]!`
    Pre(Reg, Imm),
    /// `[base], #off`
    Post(Reg, Imm),
    /// `[base, index {, lsl #3}]`
    RegOff(Reg, Reg, bool),
}

fn parse_mem(input: ParseStream) -> Result<Mem, Error> {
    let content;
    syn::bracketed!(content in input);
    let base = parse_reg(&content)?;

    let mut inner_off: Option<Imm> = None;
    let mut reg_index: Option<(Reg, bool)> = None;
    if content.peek(Token![,]) {
        content.parse::<Token![,]>()?;
        if content.peek(Ident) {
            let idx = parse_reg(&content)?;
            let scaled = if content.peek(Token![,]) {
                content.parse::<Token![,]>()?;
                let lsl: Ident = content.parse()?;
                if lsl != "lsl" {
                    return Err(Error::new(lsl.span(), "expected `lsl`"));
                }
                let _ = parse_imm(&content)?;
                true
            } else {
                false
            };
            reg_index = Some((idx, scaled));
        } else {
            inner_off = Some(parse_imm(&content)?);
        }
    }
    if !content.is_empty() {
        return Err(content.error("unexpected tokens in memory operand"));
    }

    if let Some((idx, scaled)) = reg_index {
        return Ok(Mem::RegOff(base, idx, scaled));
    }
    if input.peek(Token![!]) {
        input.parse::<Token![!]>()?;
        let off = inner_off.ok_or_else(|| input.error("pre-indexed addressing requires #off"))?;
        return Ok(Mem::Pre(base, off));
    }
    if inner_off.is_none() && input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
        let off = parse_imm(input)?;
        return Ok(Mem::Post(base, off));
    }
    Ok(Mem::Off(base, inner_off))
}

// ---------------------------------------------------------------------------
// Instructions
// ---------------------------------------------------------------------------

pub(crate) enum Inst {
    Label(Ident),

    MovReg(Reg, Reg),
    MovImm(Reg, Imm),
    MovWide(String, Reg, Imm, Option<Imm>),

    AddSub(String, Reg, Reg, RegOrImm, Option<Imm>),
    CmpCmn(String, Reg, RegOrImm, Option<Imm>),
    Neg(Reg, Reg),
    Logic(String, Reg, Reg, Reg, Option<Imm>),
    Mvn(Reg, Reg),
    Tst(Reg, Reg),

    Mul(Reg, Reg, Reg),
    MulH(String, Reg, Reg, Reg),
    MAddSub(String, Reg, Reg, Reg, Reg),
    Div(String, Reg, Reg, Reg),

    Shift(String, Reg, Reg, RegOrImm),
    ShiftVar(String, Reg, Reg, Reg),
    /// `ror Xd, Xn, Xm` (register) or `ror Xd, Xn, #shift` (immediate).
    Ror(Reg, Reg, RegOrImm),
    /// `rorv Xd, Xn, Xm` — the explicit register-form spelling.
    Rorv(Reg, Reg, Reg),
    /// `rol Xd, Xn, #shift` — synthesized as `ror Xd, Xn, #(64 - shift)`
    /// since AArch64 has no left-rotate instruction.
    Rol(Reg, Reg, Imm),
    Sxtw(Reg, Reg),

    CSel(String, Reg, Reg, Reg, TokenStream),
    CSet(String, Reg, TokenStream),

    Ldr(Reg, Mem),
    Str(Reg, Mem),
    LdStByteHalf(String, Reg, Mem),
    /// `stur`/`ldur` — load/store with an unscaled signed 9-bit offset.
    LdStUnscaled(String, Reg, Mem),
    Stp(Reg, Reg, Mem),
    Ldp(Reg, Reg, Mem),

    Fmov(Reg, Reg),
    FArith(String, Reg, Reg, Reg),
    /// Floating-point data-processing (1 source): `fabs`/`fneg`/`fsqrt`.
    FUnary(String, Reg, Reg),
    Fcmp(Reg, Option<Reg>),
    Scvtf(Reg, Reg),
    Fcvtzs(Reg, Reg),

    B(Ident),
    Bl(Ident),
    Bcond(TokenStream, Ident),
    Br(Reg),
    Blr(Reg),
    Ret(Option<Reg>),
    Cbz(Reg, Ident),
    Cbnz(Reg, Ident),
    Tbz(Reg, Imm, Ident),
    Tbnz(Reg, Imm, Ident),
    Adr(Reg, Ident),

    Nop,
    Brk(Imm),
}

impl Parse for Inst {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let ident: Ident = input.parse()?;
        if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            return Ok(Inst::Label(ident));
        }
        let m = ident.to_string();

        macro_rules! comma {
            () => {
                input.parse::<Token![,]>()?
            };
        }

        let inst = match m.as_str() {
            "mov" => {
                let rd = parse_reg(input)?;
                comma!();
                if input.peek(Ident) {
                    Inst::MovReg(rd, parse_reg(input)?)
                } else {
                    Inst::MovImm(rd, parse_imm(input)?)
                }
            }
            "movz" | "movn" | "movk" => {
                let rd = parse_reg(input)?;
                comma!();
                let imm = parse_imm(input)?;
                let lsl = parse_opt_lsl(input)?;
                Inst::MovWide(m, rd, imm, lsl)
            }
            "add" | "adds" | "sub" | "subs" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                let op3 = parse_reg_or_imm(input)?;
                let lsl = parse_opt_lsl(input)?;
                Inst::AddSub(m, rd, rn, op3, lsl)
            }
            "cmp" | "cmn" => {
                let rn = parse_reg(input)?;
                comma!();
                let op2 = parse_reg_or_imm(input)?;
                let lsl = parse_opt_lsl(input)?;
                Inst::CmpCmn(m, rn, op2, lsl)
            }
            "neg" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Neg(rd, parse_reg(input)?)
            }
            "and" | "orr" | "eor" | "ands" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                let rm = parse_reg(input)?;
                let lsl = parse_opt_lsl(input)?;
                Inst::Logic(m, rd, rn, rm, lsl)
            }
            "mvn" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Mvn(rd, parse_reg(input)?)
            }
            "tst" => {
                let rn = parse_reg(input)?;
                comma!();
                Inst::Tst(rn, parse_reg(input)?)
            }
            "mul" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::Mul(rd, rn, parse_reg(input)?)
            }
            "smulh" | "umulh" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::MulH(m, rd, rn, parse_reg(input)?)
            }
            "madd" | "msub" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                let rm = parse_reg(input)?;
                comma!();
                Inst::MAddSub(m, rd, rn, rm, parse_reg(input)?)
            }
            "sdiv" | "udiv" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::Div(m, rd, rn, parse_reg(input)?)
            }
            "lsl" | "lsr" | "asr" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::Shift(m, rd, rn, parse_reg_or_imm(input)?)
            }
            "lslv" | "lsrv" | "asrv" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::ShiftVar(m, rd, rn, parse_reg(input)?)
            }
            "ror" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::Ror(rd, rn, parse_reg_or_imm(input)?)
            }
            "rorv" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::Rorv(rd, rn, parse_reg(input)?)
            }
            "rol" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::Rol(rd, rn, parse_imm(input)?)
            }
            "sxtw" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Sxtw(rd, parse_reg(input)?)
            }
            "csel" | "csinc" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                let rm = parse_reg(input)?;
                comma!();
                Inst::CSel(m, rd, rn, rm, parse_cond(input)?)
            }
            "cset" | "csetm" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::CSet(m, rd, parse_cond(input)?)
            }
            "ldr" => {
                let rt = parse_reg(input)?;
                comma!();
                Inst::Ldr(rt, parse_mem(input)?)
            }
            "str" => {
                let rt = parse_reg(input)?;
                comma!();
                Inst::Str(rt, parse_mem(input)?)
            }
            "ldrb" | "strb" | "ldrh" | "strh" | "ldrsw" => {
                let rt = parse_reg(input)?;
                comma!();
                Inst::LdStByteHalf(m, rt, parse_mem(input)?)
            }
            "stur" | "ldur" => {
                let rt = parse_reg(input)?;
                comma!();
                Inst::LdStUnscaled(m, rt, parse_mem(input)?)
            }
            "stp" => {
                let rt = parse_reg(input)?;
                comma!();
                let rt2 = parse_reg(input)?;
                comma!();
                Inst::Stp(rt, rt2, parse_mem(input)?)
            }
            "ldp" => {
                let rt = parse_reg(input)?;
                comma!();
                let rt2 = parse_reg(input)?;
                comma!();
                Inst::Ldp(rt, rt2, parse_mem(input)?)
            }
            "fmov" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Fmov(rd, parse_reg(input)?)
            }
            "fadd" | "fsub" | "fmul" | "fdiv" => {
                let rd = parse_reg(input)?;
                comma!();
                let rn = parse_reg(input)?;
                comma!();
                Inst::FArith(m, rd, rn, parse_reg(input)?)
            }
            "fabs" | "fneg" | "fsqrt" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::FUnary(m, rd, parse_reg(input)?)
            }
            "fcmp" => {
                let rn = parse_reg(input)?;
                comma!();
                if input.peek(Ident) {
                    Inst::Fcmp(rn, Some(parse_reg(input)?))
                } else {
                    let _ = parse_imm(input)?;
                    Inst::Fcmp(rn, None)
                }
            }
            "scvtf" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Scvtf(rd, parse_reg(input)?)
            }
            "fcvtzs" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Fcvtzs(rd, parse_reg(input)?)
            }
            "b" => {
                if input.peek(Token![.]) {
                    input.parse::<Token![.]>()?;
                    let cond = parse_cond(input)?;
                    Inst::Bcond(cond, input.parse()?)
                } else {
                    Inst::B(input.parse()?)
                }
            }
            "bl" => Inst::Bl(input.parse()?),
            "br" => Inst::Br(parse_reg(input)?),
            "blr" => Inst::Blr(parse_reg(input)?),
            "ret" => {
                if input.peek(Token![;]) {
                    Inst::Ret(None)
                } else {
                    Inst::Ret(Some(parse_reg(input)?))
                }
            }
            "cbz" => {
                let rt = parse_reg(input)?;
                comma!();
                Inst::Cbz(rt, input.parse()?)
            }
            "cbnz" => {
                let rt = parse_reg(input)?;
                comma!();
                Inst::Cbnz(rt, input.parse()?)
            }
            "tbz" => {
                let rt = parse_reg(input)?;
                comma!();
                let bit = parse_imm(input)?;
                comma!();
                Inst::Tbz(rt, bit, input.parse()?)
            }
            "tbnz" => {
                let rt = parse_reg(input)?;
                comma!();
                let bit = parse_imm(input)?;
                comma!();
                Inst::Tbnz(rt, bit, input.parse()?)
            }
            "adr" => {
                let rd = parse_reg(input)?;
                comma!();
                Inst::Adr(rd, input.parse()?)
            }
            "nop" => Inst::Nop,
            "brk" => Inst::Brk(parse_imm(input)?),
            _ => return Err(Error::new(ident.span(), "unknown AArch64 instruction")),
        };
        input.parse::<Token![;]>()?;
        Ok(inst)
    }
}

// ---------------------------------------------------------------------------
// Code generation
// ---------------------------------------------------------------------------

fn id(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}

/// `shift12` flag for add/sub immediate: `lsl #0` → 0, `lsl #12` → 1.
fn shift12_ts(lsl: &Option<Imm>) -> TokenStream {
    match lsl {
        None => quote!(0u32),
        Some(i) => {
            let e = &i.0;
            quote!({
                let __s = (#e) as u32;
                assert!(
                    __s == 0 || __s == 12,
                    "monoasm_arm64: add/sub immediate shift must be #0 or #12"
                );
                __s / 12
            })
        }
    }
}

/// `hw` field for the MOV-wide family: `lsl #0/#16/#32/#48` → `0/1/2/3`.
fn hw_ts(lsl: &Option<Imm>) -> TokenStream {
    match lsl {
        None => quote!(0u32),
        Some(i) => {
            let e = &i.0;
            quote!({
                let __h = (#e) as u32;
                assert!(
                    __h % 16 == 0 && __h < 64,
                    "monoasm_arm64: MOV shift must be #0, #16, #32, or #48"
                );
                __h / 16
            })
        }
    }
}

fn off_u32(o: &Option<Imm>) -> TokenStream {
    match o {
        None => quote!(0u32),
        Some(i) => {
            let e = &i.0;
            quote!((#e) as u32)
        }
    }
}

fn off_i32(o: &Option<Imm>) -> TokenStream {
    match o {
        None => quote!(0i32),
        Some(i) => {
            let e = &i.0;
            quote!((#e) as i32)
        }
    }
}

/// Implicit zero register / stack-pointer operand (`GReg(31)`), used to
/// synthesize aliases such as `mov`/`cmp`/`neg`/`mvn`/`tst`.
fn xzr() -> TokenStream {
    quote!(GReg(31u32))
}

/// Base word for the add/subtract shifted-register form.
fn addsub_reg_base(name: &str) -> u32 {
    match name {
        "add" => 0x8b00_0000,
        "adds" => 0xab00_0000,
        "sub" => 0xcb00_0000,
        "subs" => 0xeb00_0000,
        _ => unreachable!(),
    }
}

/// Base word for the add/subtract immediate form.
fn addsub_imm_base(name: &str) -> u32 {
    match name {
        "add" => 0x9100_0000,
        "adds" => 0xb100_0000,
        "sub" => 0xd100_0000,
        "subs" => 0xf100_0000,
        _ => unreachable!(),
    }
}

/// Base word for the logical shifted-register form.
fn logical_base(name: &str) -> u32 {
    match name {
        "and" => 0x8a00_0000,
        "orr" => 0xaa00_0000,
        "eor" => 0xca00_0000,
        "ands" => 0xea00_0000,
        _ => unreachable!(),
    }
}

/// Base word for the variable-shift (`LSLV`/`LSRV`/`ASRV`) form. Accepts
/// both the `lsl`/`lsr`/`asr` (register operand) and `lslv`/`lsrv`/`asrv`
/// spellings.
fn shiftv_base(name: &str) -> u32 {
    match name {
        "lsl" | "lslv" => 0x9ac0_2000,
        "lsr" | "lsrv" => 0x9ac0_2400,
        "asr" | "asrv" => 0x9ac0_2800,
        _ => unreachable!(),
    }
}

fn ldst(kind: &str, rt: Reg, mem: Mem) -> TokenStream {
    let is_ldr = kind == "ldr";
    match rt.kind {
        RegKind::X => {
            let rtg = rt.greg();
            // (unsigned-offset, pre-index, post-index, register-offset) bases.
            let (uimm, pre, post, reg) = if is_ldr {
                (0xf940_0000u32, 0xf840_0c00u32, 0xf840_0400u32, 0xf860_6800u32)
            } else {
                (0xf900_0000u32, 0xf800_0c00u32, 0xf800_0400u32, 0xf820_6800u32)
            };
            match mem {
                Mem::Off(b, o) => {
                    let bg = b.greg();
                    let off = off_u32(&o);
                    quote!(jit.ldst_uimm(#uimm, 3, (#rtg).enc(), #bg, #off);)
                }
                Mem::Pre(b, o) => {
                    let bg = b.greg();
                    let off = o.0;
                    quote!(jit.ldst_idx(#pre, (#rtg).enc(), #bg, (#off) as i32);)
                }
                Mem::Post(b, o) => {
                    let bg = b.greg();
                    let off = o.0;
                    quote!(jit.ldst_idx(#post, (#rtg).enc(), #bg, (#off) as i32);)
                }
                Mem::RegOff(b, idx, scaled) => {
                    let bg = b.greg();
                    let ig = idx.greg();
                    let s = if scaled { quote!(true) } else { quote!(false) };
                    quote!(jit.ldst_reg(#reg, #rtg, #bg, #ig, #s);)
                }
            }
        }
        RegKind::W => match mem {
            Mem::Off(b, o) => {
                let rtg = rt.greg();
                let bg = b.greg();
                let off = off_u32(&o);
                let uimm = if is_ldr { 0xb940_0000u32 } else { 0xb900_0000u32 };
                quote!(jit.ldst_uimm(#uimm, 2, (#rtg).enc(), #bg, #off);)
            }
            _ => panic!("monoasm_arm64: 32-bit {kind} only supports [base, #off] addressing"),
        },
        RegKind::D => match mem {
            Mem::Off(b, o) => {
                let rtf = rt.freg();
                let bg = b.greg();
                let off = off_u32(&o);
                let uimm = if is_ldr { 0xfd40_0000u32 } else { 0xfd00_0000u32 };
                quote!(jit.ldst_uimm(#uimm, 3, (#rtf).enc(), #bg, #off);)
            }
            _ => panic!("monoasm_arm64: D-register {kind} only supports [base, #off] addressing"),
        },
    }
}

pub(crate) fn compile(inst: Inst) -> TokenStream {
    match inst {
        Inst::Label(id) => quote!(jit.bind_label(#id.clone());),

        Inst::MovReg(rd, rm) => {
            let a = rd.greg();
            let b = rm.greg();
            if rd.is_sp || rm.is_sp {
                // MOV to/from SP = ADD Xd|SP, Xn|SP, #0.
                quote!(jit.addsub_imm(0x9100_0000u32, #a, #b, 0, 0);)
            } else {
                // MOV = ORR Xd, XZR, Xm.
                let z = xzr();
                quote!(jit.logical_reg(0xaa00_0000u32, #a, #z, #b, 0);)
            }
        }
        Inst::MovImm(rd, imm) => {
            let a = rd.greg();
            let i = imm.0;
            quote!(jit.mov_imm(#a, (#i) as u64);)
        }
        Inst::MovWide(name, rd, imm, lsl) => {
            let a = rd.greg();
            let i = imm.0;
            let hw = hw_ts(&lsl);
            let base = match name.as_str() {
                "movz" => 0xd280_0000u32,
                "movn" => 0x9280_0000u32,
                "movk" => 0xf280_0000u32,
                _ => unreachable!(),
            };
            quote!(jit.movewide(#base, #a, (#i) as u32, #hw);)
        }

        Inst::AddSub(name, rd, rn, op3, lsl) => {
            let rdg = rd.greg();
            let rng = rn.greg();
            match op3 {
                RegOrImm::Reg(rm) => {
                    let rmg = rm.greg();
                    let base = addsub_reg_base(&name);
                    match lsl {
                        Some(s) if name == "add" => {
                            let sh = s.0;
                            quote!(jit.addsub_reg(#base, #rdg, #rng, #rmg, (#sh) as u32);)
                        }
                        Some(_) => panic!(
                            "monoasm_arm64: shifted-register form is only supported for `add`"
                        ),
                        None => quote!(jit.addsub_reg(#base, #rdg, #rng, #rmg, 0);),
                    }
                }
                RegOrImm::Imm(i) => {
                    let imm = i.0;
                    let base = addsub_imm_base(&name);
                    let sh = shift12_ts(&lsl);
                    quote!(jit.addsub_imm(#base, #rdg, #rng, (#imm) as u32, #sh);)
                }
            }
        }
        Inst::CmpCmn(name, rn, op2, lsl) => {
            let rng = rn.greg();
            let z = xzr();
            match op2 {
                RegOrImm::Reg(rm) => {
                    let rmg = rm.greg();
                    // cmp = SUBS XZR, Xn, Xm; cmn = ADDS XZR, Xn, Xm.
                    let base = if name == "cmp" { 0xeb00_0000u32 } else { 0xab00_0000u32 };
                    quote!(jit.addsub_reg(#base, #z, #rng, #rmg, 0);)
                }
                RegOrImm::Imm(i) => {
                    let imm = i.0;
                    let sh = shift12_ts(&lsl);
                    // cmp = SUBS XZR, Xn, #imm; cmn = ADDS XZR, Xn, #imm.
                    let base = if name == "cmp" { 0xf100_0000u32 } else { 0xb100_0000u32 };
                    quote!(jit.addsub_imm(#base, #z, #rng, (#imm) as u32, #sh);)
                }
            }
        }
        Inst::Neg(rd, rm) => {
            let a = rd.greg();
            let b = rm.greg();
            let z = xzr();
            // NEG = SUB Xd, XZR, Xm.
            quote!(jit.addsub_reg(0xcb00_0000u32, #a, #z, #b, 0);)
        }
        Inst::Logic(name, rd, rn, rm, lsl) => {
            let rdg = rd.greg();
            let rng = rn.greg();
            let rmg = rm.greg();
            let base = logical_base(&name);
            match lsl {
                Some(s) if name == "orr" => {
                    let sh = s.0;
                    quote!(jit.logical_reg(#base, #rdg, #rng, #rmg, (#sh) as u32);)
                }
                Some(_) => {
                    panic!(
                        "monoasm_arm64: shifted-register logical form is only supported for `orr`"
                    )
                }
                None => quote!(jit.logical_reg(#base, #rdg, #rng, #rmg, 0);),
            }
        }
        Inst::Mvn(rd, rm) => {
            let a = rd.greg();
            let b = rm.greg();
            let z = xzr();
            // MVN = ORN Xd, XZR, Xm.
            quote!(jit.logical_reg(0xaa20_0000u32, #a, #z, #b, 0);)
        }
        Inst::Tst(rn, rm) => {
            let a = rn.greg();
            let b = rm.greg();
            let z = xzr();
            // TST = ANDS XZR, Xn, Xm.
            quote!(jit.logical_reg(0xea00_0000u32, #z, #a, #b, 0);)
        }

        Inst::Mul(rd, rn, rm) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            let z = xzr();
            // MUL = MADD Xd, Xn, Xm, XZR.
            quote!(jit.dp_3src(0x9b00_0000u32, #a, #b, #c, #z);)
        }
        Inst::MulH(name, rd, rn, rm) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            let z = xzr();
            // SMULH/UMULH Xd, Xn, Xm: Xd <- (Xn * Xm)<127:64>.
            // Encoded as a 3-source data-processing op with Ra fixed to XZR.
            let base = if name == "smulh" {
                0x9b40_0000u32
            } else {
                0x9bc0_0000u32
            };
            quote!(jit.dp_3src(#base, #a, #b, #c, #z);)
        }
        Inst::MAddSub(name, rd, rn, rm, ra) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            let d = ra.greg();
            let base = if name == "madd" { 0x9b00_0000u32 } else { 0x9b00_8000u32 };
            quote!(jit.dp_3src(#base, #a, #b, #c, #d);)
        }
        Inst::Div(name, rd, rn, rm) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            let base = if name == "sdiv" { 0x9ac0_0c00u32 } else { 0x9ac0_0800u32 };
            quote!(jit.dp_2src(#base, #a, #b, #c);)
        }

        Inst::Shift(name, rd, rn, op3) => {
            let rdg = rd.greg();
            let rng = rn.greg();
            match op3 {
                RegOrImm::Reg(rm) => {
                    let rmg = rm.greg();
                    let base = shiftv_base(&name);
                    quote!(jit.dp_2src(#base, #rdg, #rng, #rmg);)
                }
                RegOrImm::Imm(i) => {
                    let imm = i.0;
                    // LSL/LSR/ASR #imm are UBFM/SBFM aliases.
                    match name.as_str() {
                        "lsl" => quote!({
                            let __s = (#imm) as u32;
                            jit.bfm(0xd340_0000u32, #rdg, #rng, (64 - __s) & 63, 63 - __s);
                        }),
                        "lsr" => quote!({
                            let __s = (#imm) as u32;
                            jit.bfm(0xd340_0000u32, #rdg, #rng, __s, 63);
                        }),
                        "asr" => quote!({
                            let __s = (#imm) as u32;
                            jit.bfm(0x9340_0000u32, #rdg, #rng, __s, 63);
                        }),
                        _ => unreachable!(),
                    }
                }
            }
        }
        Inst::ShiftVar(name, rd, rn, rm) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            let base = shiftv_base(&name);
            quote!(jit.dp_2src(#base, #a, #b, #c);)
        }
        Inst::Ror(rd, rn, op3) => {
            let rdg = rd.greg();
            let rng = rn.greg();
            match op3 {
                RegOrImm::Reg(rm) => {
                    // RORV Xd, Xn, Xm.
                    let rmg = rm.greg();
                    quote!(jit.dp_2src(0x9ac0_2c00u32, #rdg, #rng, #rmg);)
                }
                RegOrImm::Imm(i) => {
                    // ROR Xd, Xn, #shift = EXTR Xd, Xn, Xn, #shift.
                    let imm = i.0;
                    quote!({
                        let __rn = #rng;
                        let __s = (#imm) as u32 & 63;
                        jit.extr(0x93c0_0000u32, #rdg, __rn, __rn, __s);
                    })
                }
            }
        }
        Inst::Rorv(rd, rn, rm) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            quote!(jit.dp_2src(0x9ac0_2c00u32, #a, #b, #c);)
        }
        Inst::Rol(rd, rn, imm) => {
            let rdg = rd.greg();
            let rng = rn.greg();
            let i = imm.0;
            // ROL Xd, Xn, #shift = ROR Xd, Xn, #(64 - shift) = EXTR Xd, Xn, Xn, #(64 - shift).
            quote!({
                let __rn = #rng;
                let __s = (#i) as u32 & 63;
                jit.extr(0x93c0_0000u32, #rdg, __rn, __rn, (64 - __s) & 63);
            })
        }
        Inst::Sxtw(rd, rn) => {
            let a = rd.greg();
            let b = rn.greg();
            // SXTW = SBFM Xd, Xn, #0, #31.
            quote!(jit.bfm(0x9340_0000u32, #a, #b, 0, 31);)
        }

        Inst::CSel(name, rd, rn, rm, cond) => {
            let a = rd.greg();
            let b = rn.greg();
            let c = rm.greg();
            let m = id(&name);
            quote!(jit.#m(#a, #b, #c, #cond);)
        }
        Inst::CSet(name, rd, cond) => {
            let a = rd.greg();
            let m = id(&name);
            quote!(jit.#m(#a, #cond);)
        }

        Inst::Ldr(rt, mem) => ldst("ldr", rt, mem),
        Inst::Str(rt, mem) => ldst("str", rt, mem),
        Inst::LdStByteHalf(name, rt, mem) => match mem {
            Mem::Off(b, o) => {
                let rtg = rt.greg();
                let bg = b.greg();
                let off = off_u32(&o);
                let (base, scale) = match name.as_str() {
                    "ldrb" => (0x3940_0000u32, 0u32),
                    "strb" => (0x3900_0000u32, 0u32),
                    "ldrh" => (0x7940_0000u32, 1u32),
                    "strh" => (0x7900_0000u32, 1u32),
                    "ldrsw" => (0xb980_0000u32, 2u32),
                    _ => unreachable!(),
                };
                quote!(jit.ldst_uimm(#base, #scale, (#rtg).enc(), #bg, #off);)
            }
            _ => panic!("monoasm_arm64: {name} only supports [base, #off] addressing"),
        },
        Inst::LdStUnscaled(name, rt, mem) => match mem {
            Mem::Off(b, o) => {
                let bg = b.greg();
                let off = off_i32(&o);
                let is_ldur = name == "ldur";
                let (base, rt_enc) = match rt.kind {
                    RegKind::X => {
                        let r = rt.greg();
                        let base = if is_ldur { 0xf840_0000u32 } else { 0xf800_0000u32 };
                        (base, quote!((#r).enc()))
                    }
                    RegKind::W => {
                        let r = rt.greg();
                        let base = if is_ldur { 0xb840_0000u32 } else { 0xb800_0000u32 };
                        (base, quote!((#r).enc()))
                    }
                    RegKind::D => {
                        let r = rt.freg();
                        let base = if is_ldur { 0xfc40_0000u32 } else { 0xfc00_0000u32 };
                        (base, quote!((#r).enc()))
                    }
                };
                quote!(jit.ldst_idx(#base, #rt_enc, #bg, #off);)
            }
            _ => panic!("monoasm_arm64: {name} only supports [base, #off] addressing"),
        },
        Inst::Stp(rt, rt2, mem) => {
            let a = rt.greg();
            let b2 = rt2.greg();
            match mem {
                Mem::Off(b, o) => {
                    let bg = b.greg();
                    let off = off_i32(&o);
                    quote!(jit.ldstp(0xa900_0000u32, #a, #b2, #bg, #off);)
                }
                Mem::Pre(b, o) => {
                    let bg = b.greg();
                    let off = o.0;
                    quote!(jit.ldstp(0xa980_0000u32, #a, #b2, #bg, (#off) as i32);)
                }
                _ => panic!("monoasm_arm64: stp supports [base, #off] or [base, #off]!"),
            }
        }
        Inst::Ldp(rt, rt2, mem) => {
            let a = rt.greg();
            let b2 = rt2.greg();
            match mem {
                Mem::Off(b, o) => {
                    let bg = b.greg();
                    let off = off_i32(&o);
                    quote!(jit.ldstp(0xa940_0000u32, #a, #b2, #bg, #off);)
                }
                Mem::Post(b, o) => {
                    let bg = b.greg();
                    let off = o.0;
                    quote!(jit.ldstp(0xa8c0_0000u32, #a, #b2, #bg, (#off) as i32);)
                }
                _ => panic!("monoasm_arm64: ldp supports [base, #off] or [base], #off"),
            }
        }

        Inst::Fmov(rd, rn) => match (rd.kind, rn.kind) {
            (RegKind::D, RegKind::D) => {
                let a = rd.freg();
                let b = rn.freg();
                quote!(jit.emit_rr(0x1e60_4000u32, (#a).enc(), (#b).enc());)
            }
            (RegKind::D, _) => {
                let a = rd.freg();
                let b = rn.greg();
                quote!(jit.emit_rr(0x9e67_0000u32, (#a).enc(), (#b).enc());)
            }
            (_, RegKind::D) => {
                let a = rd.greg();
                let b = rn.freg();
                quote!(jit.emit_rr(0x9e66_0000u32, (#a).enc(), (#b).enc());)
            }
            _ => panic!("monoasm_arm64: fmov requires at least one D register"),
        },
        Inst::FArith(name, rd, rn, rm) => {
            let a = rd.freg();
            let b = rn.freg();
            let c = rm.freg();
            let base = match name.as_str() {
                "fadd" => 0x1e60_2800u32,
                "fsub" => 0x1e60_3800u32,
                "fmul" => 0x1e60_0800u32,
                "fdiv" => 0x1e60_1800u32,
                _ => unreachable!(),
            };
            quote!(jit.fp_3op(#base, #a, #b, #c);)
        }
        Inst::FUnary(name, rd, rn) => {
            // FP data-processing (1 source), double precision (type=01).
            let a = rd.freg();
            let b = rn.freg();
            let base = match name.as_str() {
                "fabs" => 0x1e60_c000u32,
                "fneg" => 0x1e61_4000u32,
                "fsqrt" => 0x1e61_c000u32,
                _ => unreachable!(),
            };
            quote!(jit.emit_rr(#base, (#a).enc(), (#b).enc());)
        }
        Inst::Fcmp(rn, rm) => {
            let a = rn.freg();
            match rm {
                Some(rm) => {
                    let b = rm.freg();
                    quote!(jit.fp_cmp(0x1e60_2000u32, #a, #b);)
                }
                None => quote!(jit.emit_rr(0x1e60_2008u32, 0, (#a).enc());),
            }
        }
        Inst::Scvtf(rd, rn) => {
            let a = rd.freg();
            let b = rn.greg();
            quote!(jit.emit_rr(0x9e62_0000u32, (#a).enc(), (#b).enc());)
        }
        Inst::Fcvtzs(rd, rn) => {
            let a = rd.greg();
            let b = rn.freg();
            quote!(jit.emit_rr(0x9e78_0000u32, (#a).enc(), (#b).enc());)
        }

        Inst::B(label) => quote!(jit.b_label(&#label);),
        Inst::Bl(label) => quote!(jit.bl_label(&#label);),
        Inst::Bcond(cond, label) => quote!(jit.bcond_label(#cond, &#label);),
        Inst::Br(rn) => {
            let a = rn.greg();
            quote!(jit.emit_rr(0xd61f_0000u32, 0, (#a).enc());)
        }
        Inst::Blr(rn) => {
            let a = rn.greg();
            quote!(jit.emit_rr(0xd63f_0000u32, 0, (#a).enc());)
        }
        Inst::Ret(rn) => match rn {
            // RET = ret to LR (X30).
            None => quote!(jit.emit_rr(0xd65f_0000u32, 0, 30u32);),
            Some(rn) => {
                let a = rn.greg();
                quote!(jit.emit_rr(0xd65f_0000u32, 0, (#a).enc());)
            }
        },
        Inst::Cbz(rt, label) => {
            let a = rt.greg();
            quote!(jit.cbz_label(#a, &#label);)
        }
        Inst::Cbnz(rt, label) => {
            let a = rt.greg();
            quote!(jit.cbnz_label(#a, &#label);)
        }
        Inst::Tbz(rt, bit, label) => {
            let a = rt.greg();
            let b = bit.0;
            quote!(jit.tbz_label(#a, (#b) as u32, &#label);)
        }
        Inst::Tbnz(rt, bit, label) => {
            let a = rt.greg();
            let b = bit.0;
            quote!(jit.tbnz_label(#a, (#b) as u32, &#label);)
        }
        Inst::Adr(rd, label) => {
            let a = rd.greg();
            quote!(jit.adr(#a, &#label);)
        }

        Inst::Nop => quote!(jit.emitl(0xd503_201fu32);),
        Inst::Brk(imm) => {
            let i = imm.0;
            // BRK #imm16 = base | imm16 << 5.
            quote!(jit.emit_rr(0xd420_0000u32, 0, (#i) as u32);)
        }
    }
}
