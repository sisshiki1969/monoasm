#![feature(proc_macro_hygiene)]
#![feature(asm)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::JitMemory;
use monoasm_macro::monoasm;

// Utility functions
pub extern "C" fn putc(ch: u8) {
    eprint!("{}", ch as char);
}

pub extern "C" fn putint(i: u64) {
    eprintln!("{:?}", i);
}

pub extern "C" fn dump() {
    #[allow(unused_assignments)]
    let (mut rax, mut rdi, mut rsi, mut rdx, mut rcx, mut r8) =
        (0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    macro_rules! reg_save {
        ($reg:ident) => {
            unsafe {
                asm!(
                    concat!("mov {}, ", stringify!($reg)),
                    out(reg) $reg,
                );
            }
        };
    }
    macro_rules! reg_restore {
        ($reg:ident) => {
            unsafe {
                asm!(
                    concat!("mov ", stringify!($reg),", {}"),
                    in(reg) $reg,
                );
            }
        };
    }
    macro_rules! reg_print {
        ($reg:ident) => {
            eprint!(" {:3}: {:016x}", stringify!($reg), $reg);
        };
    }
    reg_save!(rax);
    reg_save!(rdi);
    reg_save!(rsi);
    reg_save!(rdx);
    reg_save!(rcx);
    reg_save!(r8);

    reg_print!(rax);
    reg_print!(rdi);
    reg_print!(rsi);
    reg_print!(rdx);
    reg_print!(rcx);
    eprintln!();
    reg_print!(r8);
    eprintln!();

    reg_restore!(rax);
    reg_restore!(rdi);
    reg_restore!(rsi);
    reg_restore!(rdx);
    reg_restore!(rcx);
    reg_restore!(r8);
}

fn syscall() -> fn(()) -> u64 {
    let hello = "Hello World! Are you angry?\n\0";
    let mut jit: JitMemory = JitMemory::new();
    monoasm!(
        movq rdi, 1;
        movq rsi, (hello.as_ptr() as u64);
        movq rdx, (hello.len() as u64);
        movq rax, 1;
        syscall;
        ret;
    );

    jit.finalize()
}

fn hello() -> fn(()) -> () {
    let hello = "Hello World! Are you angry?\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let label = jit.label();
    let putc_addr: u64 = putc as *const fn() as u64;
    let dump_addr: u64 = dump as *const fn() as u64;

    monoasm!(
        // prologue
        pushq rbp;
        movq rbp, rsp;
        movq rax, 0x10;
        movq rsi, 0x12;
        movq rdx, 0x13;
        movq rcx, 0x14;
        movq r8, 0x15;
        movq rdi, (dump_addr);
        call rdi;
        // global variable
        //movq r15, (jit.get_mem_addr() + 1024);
        movq r15, (hello.as_ptr() as u64);
    label:
        movq rdi, [r15];
        movq rax, (putc_addr);
        call rax;
        addq r15, 1;
        cmpq [r15], 0x00;
        jne label;
        movq rdi, ('\n' as u64);
        movq rax, (putc_addr);
        call rax;

        // epilogue
        movq rsp, rbp;
        popq rbp;
        ret;
    );
    jit.finalize()
    //jit.p();
}

fn fibo() -> fn(u64) -> u64 {
    let mut jit: JitMemory = JitMemory::new();
    let putint_addr = putint as *const fn() as u64;
    let fibo = jit.label();
    let fibo_ep = jit.label();
    let l0 = jit.label();
    let l1 = jit.label();

    // main()
    monoasm!(
        // prologue
        pushq rbp;
        movq rbp, rsp;
        // local variables
        // 0:result
        subq rsp, 16;

        //movq rdi, 30;
        // fibo(rdi) -> rax
        call fibo;

        movq rdi, rax;
        movq [rbp-8], rax;
        movq rax, (putint_addr);
        call rax;
        movq rax, [rbp-8];

        // epilogue
        movq rsp, rbp;
        popq rbp;
        ret;

    // fibo(rdi:i64) -> rax
    fibo:
        // prologue
        pushq rbp;
        movq rbp, rsp;
        // local variables
        subq rsp, 16;
        // if arg == 0 { return 0 }
        cmpq rdi, 0;
        jne l0;
        movq rax, rdi;
        jmp fibo_ep;
        // else if arg == 1 { return 1 }
    l0:
        cmpq rdi, 1;
        jne l1;
        movq rax, rdi;
        jmp fibo_ep;
        // else { return f(arg - 1) + f(arg - 2) }
    l1:
        movq [rbp-8], rdi;
        subq rdi, 1;
        call fibo;
        movq rdi, [rbp-8];
        movq [rbp-8], rax;
        subq rdi, 2;
        call fibo;
        addq rax, [rbp-8];
        // epilogue
    fibo_ep:
        movq rsp, rbp;
        popq rbp;
        ret;
    );

    jit.finalize()
}

fn fac() -> fn(u64) -> u64 {
    let fmt = "%d\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let printf_addr = libc::printf as u64;
    let fac = jit.label();
    let l2 = jit.label();
    let l3 = jit.label();
    let i: i32 = 8;

    // main()
    monoasm!(
        // prologue
        pushq rbp;
        movq rbp, rsp;

        //movq rdi, 10;
        // fac(rdi) -> rax
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

    // fac(arg:i64) -> rax
    fac:
        // prologue
        pushq rbp;
        movq rbp, rsp;
        // local variables
        // 0:arg
        // 8:
        subq rsp, 16;

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
        call fac;
        imull rax, [rbp-(i)];
    l3:
        // epilogue
        movq rsp, rbp;
        popq rbp;
        ret;
    );

    jit.finalize()
}

#[test]
fn hungry() {
    let func = syscall();
    let ret = func(());
    assert_eq!(29, ret);
}

#[test]
fn hello_world() {
    let func = hello();
    func(());
}

#[test]
fn fibonacci() {
    let func = fibo();
    let ret = func(35);
    assert_eq!(9227465, ret);
}

#[test]
fn factorial() {
    let func = fac();
    let ret = func(10);
    assert_eq!(3628800, ret);
}
