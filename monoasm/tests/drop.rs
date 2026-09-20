//! Dropping a `JitMemory` gives its pages back.
//!
//! Its own test target, and deliberately so: the only thing that can see
//! an address-space reservation is a process-wide figure, and the lib
//! test binary runs its cases in threads that hold `JitMemory`s of their
//! own. Measured here, nothing else in the process allocates one.

#![cfg(target_os = "linux")]

use monoasm::JitMemory;

/// Virtual size of this process, in bytes.
///
/// Linux-only: `/proc/self/statm` is where a reservation is visible, and
/// the reservation is what is being asserted about — most of a
/// `JitMemory`'s pages are never touched, so resident size would not move
/// whether or not they are released.
fn vsize() -> usize {
    let statm = std::fs::read_to_string("/proc/self/statm").unwrap();
    let pages: usize = statm.split_whitespace().next().unwrap().parse().unwrap();
    pages * 4096
}

/// Without a `Drop`, a host that makes one `JitMemory` per unit of work —
/// per interpreter, per test — leaks its whole reservation every time.
///
/// Written as a round trip: what a batch takes is compared against the
/// same process once the batch is gone, never against an absolute figure.
#[test]
fn dropping_gives_the_pages_back() {
    const N: usize = 8;
    // A lower bound on one `JitMemory`, loose because the real figure is
    // `PAGE_SIZE * 3`, which is monoasm's own business and not public.
    const AT_LEAST_EACH: usize = 1 << 30;

    let before = vsize();
    let live: Vec<_> = (0..N).map(|_| JitMemory::new()).collect();
    let held = vsize() - before;
    assert!(
        held >= N * AT_LEAST_EACH,
        "{N} JitMemory hold {held} bytes — too few for the measurement below to mean anything"
    );

    drop(live);
    let after = vsize();
    assert!(
        after < before + AT_LEAST_EACH,
        "{} bytes still reserved after dropping {N} JitMemory",
        after - before
    );
}
