//! Synthetic reproduction of the monoruby workload: many small
//! `finalize()` calls against a code region that keeps growing.
//!
//! Run with `cargo run --release --example icache_bench` (AArch64 shows the
//! real cache-maintenance cost; on x86-64 the flush is a no-op).
use monoasm::*;

fn main() {
    let rounds: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(20_000);
    let mut jit = JitMemory::new();
    let start = std::time::Instant::now();
    for _ in 0..rounds {
        for _ in 0..16 {
            jit.emitl(0xd503_201f); // nop
        }
        jit.finalize();
    }
    let elapsed = start.elapsed();
    println!(
        "{} finalize() calls, {} bytes of code: {:?}",
        rounds,
        jit.get_current(),
        elapsed
    );
}
