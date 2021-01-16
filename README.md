# monoasm

monoasm is a x86-64 run-time code generator engine written in Rust.

This tool generates x86-64 machine codes on memory and you can exexute them as a Rust function.
You can write x64 assembly in your Rust code using intel-like syntax with the aid of procedual macro, which is a powerful feature of Rust 2018.

```Rust
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::JitMemory;
use monoasm_macro::monoasm;

#[test]
fn factorial() {
    let func = fac();
    let ret = func(10);
    assert_eq!(3628800, ret);
}

// This function returns function pointer which requires one u64 argument,
// and returns u64.
fn fac() -> fn(u64) -> u64 {
    let fmt = "%d\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let printf_addr = libc::printf as u64;
    let fac = jit.label();
    let l2 = jit.label();
    let l3 = jit.label();
    let i: i32 = 8;

    monoasm!(
        // prologue
        pushq rbp;
        movq rbp, rsp;

        // fac(rdi:u64) -> rax:u64
        call fac;

        movq r15, rax;
        movq rsi, rax;
        movq rdi, (fmt.as_ptr() as u64);
        movq rax, 0;
        movq rcx, (printf_addr);
        // printf(fmt, rsi)
        call rcx;
        // epilogue
        popq rbp;
        movq rax, r15;
        ret;

    // fac(rdi:u64) -> rax:u64
    fac:
        // prologue
        pushq rbp;
        movq rbp, rsp;
        // local variable
        subq rsp, 16;

        // You can interpolate expression of Rust (cf. 'i') using ().
        movq [rbp-(i)], rdi;
        cmpq [rbp-(i)], 1;
        // if arg != 1 then goto l2
        jne l2;
        // else return 1
        movq rax, 1;
        jmp l3;
    l2:
        // fac(arg - 1) * [rbp - 8]->
        movq rdi, [rbp-(i)];
        subq rdi, 1;
        // recursive call
        call fac;
        imull rax, [rbp-(i)];
    l3:
        // epilogue
        movq rsp, rbp;
        popq rbp;
        ret;
    );

    // finalize() returns the top address of generated machine code as a function pointer.
    jit.finalize()
}
```

In compile time, the code inside of `monoasm!()` will be converted to a Rust code below.

```Rust
jit . emitb(85u8) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(229u8) ;
jit . emitb(0xe8) ; jit . save_reloc(fac, 4) ; jit . emitl(0) ;
jit . emitb(73u8) ; jit . emitb(137u8) ; jit . emitb(199u8) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(198u8) ;
let imm = (fmt . as_ptr() as u64) as u64 ; if imm <= 0xffff_ffff
{
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(199u8) ; jit .
    emitl(imm as u32) ;
} else { jit . emitb(72u8) ; jit . emitb(191u8) ; jit . emitq(imm) ; }
let imm = (0) as u64 ; if imm <= 0xffff_ffff
{
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(192u8) ; jit .
    emitl(imm as u32) ;
} else { jit . emitb(72u8) ; jit . emitb(184u8) ; jit . emitq(imm) ; }
let imm = (printf_addr) as u64 ; if imm <= 0xffff_ffff
{
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(193u8) ; jit .
    emitl(imm as u32) ;
} else { jit . emitb(72u8) ; jit . emitb(185u8) ; jit . emitq(imm) ; }
jit . emitb(0xff) ; jit . emitb(209u8) ;
jit . emitb(93u8) ;
jit . emitb(76u8) ; jit . emitb(137u8) ; jit . emitb(248u8) ;
jit . emitb(0xc3) ;
jit . bind_label(fac) ;
jit . emitb(85u8) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(229u8) ;
let imm = (16) as u64 ; if imm > 0xffff_ffff
{ panic ! ("'XXX {}, imm64' does not exists.", "Rsp") ; } jit . emitb(72u8) ;
jit . emitb(129u8) ; jit . emitb(236u8) ; jit . emitl(imm as u32) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(189u8) ; jit .
emitl((- ((i))) as u32) ;
let imm = (1) as u64 ; if imm > 0xffff_ffff
{ panic ! ("'XXX {}, imm64' does not exists.", "Expr(- ((i)))[Rbp]") ; } jit .
emitb(72u8) ; jit . emitb(129u8) ; jit . emitb(189u8) ; jit .
emitl((- ((i))) as u32) ; jit . emitl(imm as u32) ;
jit . emitb(0x0f) ; jit . emitb(0x85) ; jit . save_reloc(l2, 4) ; jit .
emitl(0) ;
let imm = (1) as u64 ; if imm <= 0xffff_ffff
{
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(192u8) ; jit .
    emitl(imm as u32) ;
} else { jit . emitb(72u8) ; jit . emitb(184u8) ; jit . emitq(imm) ; }
jit . emitb(0xe9) ; jit . save_reloc(l3, 4) ; jit . emitl(0) ;
jit . bind_label(l2) ;
jit . emitb(72u8) ; jit . emitb(139u8) ; jit . emitb(189u8) ; jit .
emitl((- ((i))) as u32) ;
let imm = (1) as u64 ; if imm > 0xffff_ffff
{ panic ! ("'XXX {}, imm64' does not exists.", "Rdi") ; } jit . emitb(72u8) ;
jit . emitb(129u8) ; jit . emitb(239u8) ; jit . emitl(imm as u32) ;
jit . emitb(0xe8) ; jit . save_reloc(fac, 4) ; jit . emitl(0) ;
jit . emitb(0x0f) ; jit . emitb(0xaf) ; jit . emitb(133u8) ; jit .
emitl((- ((i))) as u32) ;
jit . bind_label(l3) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(236u8) ;
jit . emitb(93u8) ;
jit . emitb(0xc3) ;
```
