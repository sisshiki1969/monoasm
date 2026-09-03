use monoasm::*;
use monoasm_macro::monoasm;

/// A 16-byte round trip through memory: load a pair of words with
/// `movups xmm, [mem]` and store it back with `movups [mem], xmm`. Both
/// operands are deliberately *not* 16-byte aligned — that is the whole
/// point of `movups` over `movaps`.
#[test]
fn movups_round_trip_through_unaligned_memory() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            // rdi: *const u64 (2 words), rsi: *mut u64 (2 words).
            movups xmm3, [rdi];
            movups [rsi], xmm3;
            ret;
    );
    jit.finalize();

    let src = [0xdead_beef_0000_0001u64, 0xfeed_face_0000_0002u64];
    let mut dst = [0u64; 3];
    let f = jit.get_label_addr2::<*const u64, *mut u64, ()>(&begin);
    for off in 0..2 {
        // SAFETY: `dst` has room for two words at either offset.
        let out = unsafe { dst.as_mut_ptr().add(off) };
        f(src.as_ptr(), out);
        // SAFETY: the two words just written.
        assert_eq!(unsafe { std::slice::from_raw_parts(out, 2) }, &src);
        dst = [0u64; 3];
    }
}

/// The register-to-register form, on a register that needs the REX.R bit
/// (`xmm8`–`xmm15`) in both operand positions.
#[test]
fn movups_reg_to_reg_with_high_registers() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            movups xmm9, [rdi];
            movups xmm2, xmm9;
            movups xmm10, xmm2;
            movups [rsi], xmm10;
            ret;
    );
    jit.finalize();

    let src = [0x1111_2222_3333_4444u64, 0x5555_6666_7777_8888u64];
    let mut dst = [0u64; 2];
    let f = jit.get_label_addr2::<*const u64, *mut u64, ()>(&begin);
    f(src.as_ptr(), dst.as_mut_ptr());
    assert_eq!(dst, src);
}

/// Filling a run of adjacent 8-byte slots two at a time: seed the first
/// pair with the scalar form, read it back as one 16-byte value, then
/// broadcast. This is how a JIT frame's local slots get initialised.
#[test]
fn movups_broadcasts_a_seeded_pair() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            // rdi: *mut u64 (8 words), rsi: the fill value.
            movq [rdi + 0], rsi;
            movq [rdi + 8], rsi;
            movups xmm0, [rdi];
            movups [rdi + 16], xmm0;
            movups [rdi + 32], xmm0;
            movups [rdi + 48], xmm0;
            ret;
    );
    jit.finalize();

    let mut buf = [0u64; 8];
    let f = jit.get_label_addr2::<*mut u64, u64, ()>(&begin);
    f(buf.as_mut_ptr(), 4);
    assert_eq!(buf, [4u64; 8]);
}

/// The rip-relative form, loading a 16-byte pattern out of the constant
/// pool. Each `const_i64` is individually 16-byte aligned, so a pair of
/// them is *not* contiguous; four `const_i32` are (they are only 4-byte
/// aligned), which is how a 16-byte pattern is laid down today.
#[test]
fn movups_loads_a_rip_relative_constant_pair() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let pair = jit.const_align8();
    for _ in 0..2 {
        jit.const_i32(4); // low half of the word
        jit.const_i32(0); // high half
    }
    monoasm!(&mut jit,
        begin:
            movups xmm0, [rip + pair];
            movups [rdi], xmm0;
            movups [rdi + 16], xmm0;
            ret;
    );
    jit.finalize();

    let mut buf = [0u64; 4];
    let f = jit.get_label_addr::<*mut u64, ()>(&begin);
    f(buf.as_mut_ptr());
    assert_eq!(buf, [4u64; 4], "buf={buf:#x?}");
}
