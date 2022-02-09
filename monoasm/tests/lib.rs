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
        let i = 0;
        monoasm!(jit,
            movq rax, rdi;
            movsd xmm0, xmm0;
            movsd xmm0, xmm13;
            movsd xmm0, [rax];
            movsd [rax], xmm13;

            addsd xmm(0), xmm(11);
            addsd xmm(0), [rax + 4];
            subsd xmm(0), xmm(11);
            subsd xmm(0), [rax + 4];
            mulsd xmm(0), xmm(11);
            mulsd xmm(0), [rax + 4];
            divsd xmm(0), xmm(11);
            divsd xmm(0), [rax + 4];
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

fn div1() -> fn(u64) -> u64 {
    let mut jit: JitMemory = JitMemory::new();
    monoasm!(jit,
        movq rax, 63;
        movq rdx, 0;
        //movq rdi, 9;
        idiv rdi;
        ret;
    );
    jit.finalize()
}

fn div2() -> fn(u64) -> u64 {
    let mut jit: JitMemory = JitMemory::new();
    let divider = 7i64;
    let divider_ptr = &divider as *const i64;
    monoasm!(jit,
        movq rax, rdi;
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
    assert_eq!(7, func(9));
    let func = div2();
    assert_eq!(9, func(63));
}

fn float() -> fn(f64) -> f64 {
    let mut jit: JitMemory = JitMemory::new();
    monoasm!(jit,
        ret;
    );
    jit.finalize()
}

#[test]
fn float_test() {
    let func = float();
    assert_eq!(9.88, func(9.88));
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
