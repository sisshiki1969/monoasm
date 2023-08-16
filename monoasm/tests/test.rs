extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_macro::monoasm;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fib_stack() {
        let mut jit: JitMemory = JitMemory::new();
        let func = jit.label();
        let fib = jit.label();
        let nozero = jit.label();
        let noone = jit.label();
        monoasm! { &mut jit,
        func:
        fib:
            cmpq rdi, 0;
            jne  nozero;
            movq rax, 0;
            ret;
        nozero:
            cmpq rdi, 2;
            jgt  noone;
            movq rax, 1;
            ret;
        noone:
            pushq rbp;
            movq rbp, rsp;
            subq rsp, 16;
            movq [rsp], rdi;
            subq rdi, 1;
            call fib;
            movq [rsp + 8], rax;
            movq rdi, [rsp];
            subq rdi, 2;
            call fib;
            addq rax, [rsp + 8];
            movq rsp, rbp;
            popq rbp;
            ret;
        }
        jit.finalize();
        let func: extern "C" fn(i64) -> i64 = jit.get_label_addr(func);
        let now = std::time::Instant::now();
        let answer = func(30);
        let elapsed = now.elapsed();
        println!(
            "{answer} elapsed: {} ms",
            elapsed.as_micros() as f64 / 1000.0
        );
    }
}
