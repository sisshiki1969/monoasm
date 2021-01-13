#![feature(proc_macro_hygiene)]
#![feature(asm)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::JitMemory;
use monoasm_macro::monoasm;

fn syscall() -> fn() -> i64 {
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

fn hello() -> fn() -> u64 {
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

pub extern "C" fn putc(ch: u8) {
    eprint!("{}", ch as char);
}

pub extern "C" fn dump() {
    #[allow(unused_assignments)]
    let mut data: u64 = 0;
    macro_rules! reg_move {
        ($reg:expr, $i:expr) => {
            unsafe {
                asm!(
                    concat!("mov {}, ", $i, "[rsp]"),
                    out(reg) data,
                );
            }
            eprintln!("{}: {:016x}", $reg, data);
        };
    }
    unsafe {
        asm!(
            "push rax",
            "push rdi",
            "push rsi",
            "push rdx",
            "push rcx",
            "push r8",
            "sub rsp, 16"
        );
    }
    // data := (rsp)
    reg_move!("rax", 56);
    reg_move!("rdi", 48);
    reg_move!("rsi", 40);
    reg_move!("rdx", 32);
    reg_move!("rcx", 24);
    reg_move!("r8 ", 16);
    unsafe {
        asm!(
            "add rsp, 16",
            "pop r8",
            "pop rcx",
            "pop rdx",
            "pop rsi",
            "pop rdi",
            "pop rax",
        );
    }
}

fn fac() -> fn() -> i64 {
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

        movq rdi, 10;
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
        // if arg == 1 then goto l2
        jne l2;
        // else return 1
        movq rax, 1;
        jmp l3;
    l2:
        // fac(arg - 1) * [rbp - 8]->
        movq rax, [rbp-(i)];
        subq rax, 1;
        movq rdi, rax;
        call fac;
        imull rax, [rbp-8];
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
    let ret = func();
    assert_eq!(29, ret);
}

#[test]
fn hello_world() {
    let func = hello();
    let ret = func();
    println!("returned value:{:x}", ret);
}

#[test]
fn factorial() {
    let func = fac();
    let ret = func();
    assert_eq!(3628800, ret);
}
