extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_macro::monoasm;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn mul() {
        let mut jit: JitMemory = JitMemory::new();
        let func = jit.label();

        let cont1 = jit.label();
        let cont2 = jit.label();
        let loop1 = jit.label();
        let loop2 = jit.label();
        let imm = 7;
        monoasm! { &mut jit,
        func:
            cmovneq rdx, [rax + r15 * 8 + 16];
            shlq rdi, (imm);
            jmp [rax];
            call [rax];
            xchgq rax, r15;
            subq r14, 16;  // r14 <- dfp
        loop1:
            cmpq [r14], 0;
            je   cont1;
            movq r14, [r14];
            jp   loop1;
        cont1:
            addq r14, 16;  // r14 <- outermost lfp
            movq rax, [rbx];        // rdi <- cfp
        loop2:
            cmpq [rax - 32], r14;
            je   cont2;
            movq rax, [rax];
            jp   loop2;
        cont2:
            ret;
        }
        jit.finalize();
        let func = jit.get_label_addr(func);
        assert_eq!(3.5 * 2.3 * 100f64, func(2.3));
    }
}

#[test]
fn div_test() {
    fn div1() -> extern "C" fn(u64) -> u64 {
        let mut jit: JitMemory = JitMemory::new();
        let func = jit.label();
        monoasm!(&mut jit,
        func:
            movq rax, 63;
            movq rdx, 0;
            //movq rdi, 9;
            idiv rdi;
            ret;
        );
        jit.finalize();
        jit.get_label_addr(func)
    }

    fn div2() -> extern "C" fn(u64) -> u64 {
        let mut jit: JitMemory = JitMemory::new();
        let divider = 7i64;
        let divider_ptr = &divider as *const i64;
        let func = jit.label();
        monoasm!(&mut jit,
        func:
            movq rax, rdi;
            movq rdx, 0;
            movq rdi, (divider_ptr);
            movq rdi, [rdi];
            idiv rdi;
            ret;
        );
        jit.finalize();
        jit.get_label_addr(func)
    }
    let func = div1();
    assert_eq!(7, func(9));
    let func = div2();
    assert_eq!(9, func(63));
}

#[test]
fn float_test() {
    fn float_sqrt() -> extern "C" fn(f64) -> f64 {
        let mut jit: JitMemory = JitMemory::new();
        let func = jit.label();
        monoasm!(&mut jit,
        func:
            movq   xmm15, xmm0;
            sqrtsd xmm0, xmm15;
            ret;
        );
        jit.finalize();
        jit.get_label_addr(func)
    }
    let func = float_sqrt();
    assert_eq!(3.1432467291003423, func(9.88));
}

#[test]
fn cmove() {
    fn func1() -> extern "C" fn(()) -> i64 {
        let mut jit: JitMemory = JitMemory::new();
        let func = jit.label();
        monoasm!(&mut jit,
        func:
            movq rsi, 42;
            movq rdx, 100;
            movq rax, 10;
            movq rdi, 10;
            cmpq rax, rdi;
            cmoveqq rax, rsi;
            cmovneq rax, rdx;
            ret;
        );
        jit.finalize();
        jit.get_label_addr(func)
    }
    fn func2() -> extern "C" fn(()) -> i64 {
        let mut jit: JitMemory = JitMemory::new();
        let func = jit.label();
        monoasm!(&mut jit,
        func:
            movq rsi, 42;
            movq rdx, 100;
            movq rax, 11;
            movq rdi, 10;
            cmpq rax, rdi;
            cmovgtq rax, rsi;
            cmovleq rax, rdx;
            ret;
        );
        jit.finalize();
        jit.get_label_addr(func)
    }
    let func = func1();
    assert_eq!(42, func(()));
    let func = func2();
    assert_eq!(42, func(()));
}

#[test]
fn system_call() {
    fn syscall() -> extern "C" fn(()) -> u64 {
        let hello = "こんにちは世界\n\0";
        let mut mem: JitMemory = JitMemory::new();
        let mut jit = &mut mem;
        let func = jit.label();
        monoasm!(&mut jit,
        func:
            movq rdi, 1;
            movq rsi, (hello.as_ptr() as u64);
            movq rdx, (hello.len() as u64);
            movq rax, 1;
            syscall;
            ret;
        );

        jit.finalize();
        jit.get_label_addr(func)
    }
    let func = syscall();
    let ret = func(());
    assert_eq!(23, ret);
}

#[test]
fn hello_world() {
    fn hello() -> extern "C" fn(()) -> () {
        let hello = "hello world!\n\0";
        let mut jit: JitMemory = JitMemory::new();
        let label = jit.label();
        let func = jit.label();
        monoasm!(&mut jit,
        func:
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
        jit.finalize();
        jit.get_label_addr(func)
    }
    let func = hello();
    func(());
}

#[test]
fn absolute_reloc() {
    fn test() -> extern "C" fn(()) -> u64 {
        let mut jit = JitMemory::new();
        let func = jit.label();
        let label = jit.abs_address(func);
        monoasm!(&mut jit,
        func:
            movq rax, [rip + label];
            lea  rdi, [rip + func];
            subq rax, rdi;
            ret;
        );

        jit.finalize();
        jit.get_label_addr(func)
    }
    let func = test();
    let ret = func(());
    assert_eq!(0, ret);
}
