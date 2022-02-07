extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_macro::monoasm;
mod fact;
mod fibo;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn mul() {
        let mut jit: JitMemory = JitMemory::new();
        let i = 3;
        monoasm!(jit,
            movq rax, 0;
            movq rax, 0xffff;
            movq rax, 0xffff_ffff;
            movq rax, [rdi -4];
            movq rax, [rdi +1024];
            movq [R(0)], R(i);
            movq [R(1)], R(i);
            movq [R(2)], R(i);
            movq [R(3)], R(i);
            movq [R(4)], R(i);
            movq [R(5)], R(i);
            movq [R(6)], R(i);
            movq [R(7)], R(i);
            movq [R(8)], R(i);
            movq [R(9)], R(i);
            movq [R(10)], R(i);
            movq [R(11)], R(i);
            movq [R(12)], R(i);
            movq [R(13)], R(i);
            movq [R(14)], R(i);
            movq [R(15)], R(i);
            imul rax, rax;
            imul rax, rcx;
            imul rax, rdx;
            imul rax, rbx;
            imul rax, rsp;
            imul rax, rbp;
            imul rax, rsi;
            imul rax, rdi;
            imul rax, r8;
            imul rax, r9;
            imul rax, r10;
            imul rax, r11;
            imul rax, r12;
            imul rax, r13;
            imul rax, r14;
            imul rax, r15;
            imul rax, [rax];
            imul rax, [rcx];
            imul rax, [rdx];
            imul rax, [rbx];
            imul rax, [rsp];
            imul rax, [rbp];
            imul rax, [rsi];
            imul rax, [rdi];
            imul rax, [r8];
            imul rax, [r9];
            imul rax, [r10];
            imul rax, [r11];
            imul rax, [r12];
            imul rax, [r13];
            imul rax, [r14];
            imul rax, [r15];

            imul R(0), [rax];
            imul R(0), [rcx];
            imul R(0), [rdx];
            imul R(0), [rbx];
            imul R(0), [rsp];
            imul R(0), [rbp];
            imul R(0), [rsi];
            imul R(0), [rdi];
            imul R(0), [r8];
            imul R(0), [r9];
            imul R(0), [r10];
            imul R(0), [r11];
            imul R(0), [r12];
            imul R(0), [r13];
            imul R(0), [r14];
            imul R(0), [r15];

            ret;
        );
        let func = jit.finalize::<u64, u64>();
        func(0);
    }
}

fn syscall() -> fn(()) -> u64 {
    let hello = "こんにちは世界\n\0";
    let mut jit: JitMemory = JitMemory::new();
    monoasm!(jit,
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
    let hello = "hello world!\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let label = jit.label();
    monoasm!(jit,
        // prologue
        pushq rbp;
        movq rbp, rsp;
        pushq r15;
        pushq r14;
        movq r15, (hello.as_ptr() as u64);
        movq r14, 13;
    label:
        movq rdi, [r15];
        movq rax, (test::PUTC as u64);
        call rax;
        addq r15, 1;
        subq r14, 1;
        cmpq r14, 0;
        jne label;
        // epilogue
        popq r14;
        popq r15;
        movq rsp, rbp;
        popq rbp;
        ret;
    );
    jit.finalize()
}

fn div1() -> fn(()) -> u64 {
    let mut jit: JitMemory = JitMemory::new();
    monoasm!(jit,
        movq rax, 63;
        movq rdx, 0;
        movq rdi, 9;
        idiv rdi;
        ret;
    );
    jit.finalize()
}

fn div2() -> fn(()) -> u64 {
    let mut jit: JitMemory = JitMemory::new();
    let divider = 7i64;
    let divider_ptr = &divider as *const i64;
    monoasm!(jit,
        movq rax, 63;
        movq rdx, 0;
        movq rdi, (divider_ptr);
        movq rdi, [rdi];
        idiv rdi;
        ret;
    );
    jit.finalize()
}

#[test]
fn div_test() {
    let func = div1();
    assert_eq!(7, func(()));
    let func = div2();
    assert_eq!(9, func(()));
}

#[test]
fn system_call() {
    let func = syscall();
    let ret = func(());
    assert_eq!(23, ret);
}

#[test]
fn hello_world() {
    let func = hello();
    func(());
}
