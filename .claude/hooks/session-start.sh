#!/bin/bash
# SessionStart hook for Claude Code on the web.
#
# monoasm requires the nightly toolchain: monoasm_macro/src/lib.rs uses
# `#![feature(proc_macro_hygiene)]`, which the stable channel rejects (E0554).
#
# This installs nightly (with the rustfmt + clippy components CI relies on),
# warms the host build/test caches, and best-effort sets up AArch64
# cross-compilation + qemu so the arm64 backend's tests can run from this
# x86-64 container (see .cargo/config.toml for the linker/runner wiring).
set -euo pipefail

# Only run setup in the remote (Claude Code on the web) environment.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Run asynchronously so the session starts without waiting for the toolchain
# install and full build to finish.
echo '{"async": true, "asyncTimeout": 600000}'

cd "$CLAUDE_PROJECT_DIR"

# Verbose toolchain/build output goes to stderr to keep it out of session context.
{
  # --- Essential: nightly toolchain + host (x86-64) build cache ---
  rustup toolchain install nightly --profile minimal -c rustfmt -c clippy
  rustup default nightly
  cargo fetch
  cargo build --workspace --tests

  # --- Best-effort: AArch64 cross-build + emulated execution ---
  # The arm64 backend is only compiled for the aarch64 target, so to test it
  # from this x86-64 host we need the aarch64 std, the GNU cross linker and
  # qemu-user. Any failure here is non-fatal: the host setup above is enough
  # to work on the x86-64 side.
  if rustup target add aarch64-unknown-linux-gnu; then
    sudo apt-get update || true
    if sudo apt-get install -y --no-install-recommends \
        qemu-user gcc-aarch64-linux-gnu libc6-dev-arm64-cross; then
      cargo build --workspace --tests --target aarch64-unknown-linux-gnu || true
    else
      echo "warning: aarch64 cross toolchain install failed; skipping arm64 cross-build." >&2
    fi
  else
    echo "warning: could not add aarch64 rust target; skipping arm64 cross-build." >&2
  fi

  echo "monoasm web environment ready."
} 1>&2
