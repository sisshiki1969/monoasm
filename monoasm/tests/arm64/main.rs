//! AArch64 backend tests: host-portable encoding checks plus emulated
//! execution. Build for the aarch64 target to run them, e.g.
//! `cargo test --target aarch64-unknown-linux-gnu --test arm64`.
#![cfg(target_arch = "aarch64")]

mod encoding;
mod exec;
