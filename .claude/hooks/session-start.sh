#!/bin/bash
# SessionStart hook for Claude Code on the web.
# monoasm requires the nightly toolchain: monoasm_macro/src/lib.rs uses
# `#![feature(proc_macro_hygiene)]`, which the stable channel rejects (E0554).
# Install nightly (with the rustfmt + clippy components CI relies on) and warm
# the build/test caches so linters and tests run immediately in the session.
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
  rustup toolchain install nightly --profile minimal -c rustfmt -c clippy
  rustup default nightly
  cargo fetch
  cargo build --workspace --tests
  echo "monoasm web environment ready: nightly toolchain installed, workspace built."
} 1>&2
