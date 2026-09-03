//! x86-64 backend tests (one module per instruction group).
#![cfg(target_arch = "x86_64")]

mod andpd;
mod cdq;
mod counts;
mod cvttsd2si;
mod div;
mod divl;
mod idivl;
mod minmaxsd;
mod movups;
mod roundpd;
mod shift_dword;
mod xorpd;
