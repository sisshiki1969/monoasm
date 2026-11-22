use monoasm::*;
use monoasm_macro::monoasm;

/// A branch to a label that is never bound is still caught at `finalize`.
#[test]
#[should_panic(expected = "never-bound label")]
fn branch_to_unbound_label_panics_at_finalize() {
    let mut jit: JitMemory = JitMemory::new();
    let nowhere = jit.label();
    monoasm!(&mut jit,
        jmp nowhere;
    );
    jit.finalize();
}

/// Labels that are created but never used cost nothing at later
/// `finalize`s (they used to be kept, and walked, for good), and a label
/// bound after its first reference passes the check.
#[test]
fn unused_labels_do_not_block_finalize() {
    let mut jit: JitMemory = JitMemory::new();
    for _ in 0..1000 {
        let _unused = jit.label();
        let begin = jit.label();
        let exit = jit.label();
        monoasm!(&mut jit,
            begin:
                movq rax, (42);
                jmp exit;
            exit:
                ret;
        );
        jit.finalize();
        let f = jit.get_label_addr::<(), u64>(&begin);
        assert_eq!(f(()), 42);
    }
}
