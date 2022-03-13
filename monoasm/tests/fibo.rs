extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn fibonacci() {
    let func = fibo();
    let ret = func(35);
    assert_eq!(9227465, ret);
}

fn fibo() -> extern "C" fn(u64) -> u64 {
    let mut jit: JitMemory = JitMemory::new();
    let putint_addr = test::PUTINT as u64;
    let fibo = jit.label();
    let fibo_ep = jit.label();
    let l0 = jit.label();
    let l1 = jit.label();

    // main()
    monoasm!(jit,
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

    jit.finalize();
    jit.get_label_addr(fibo)
}
