//! Integration tests for the Jink standard library
//!
//! Each tests/jk/<name>.jk is a self-asserting program (it prints [PASS]/[FAIL]
//! and exit(<failures>)). This runner compiles each one with the jink binary
//! and asserts the resulting program exits 0. Kept out of the compiler sources so
//! builder.rs stays focused on the compiler itself
//!
//! Requires clang on PATH (the compiler shells out to it), same as the existing
//! builder tests

use std::path::Path;
use std::process::Command;

const JK_TESTS: &[&str] = &[
  "sha256", "hkdf", "chacha20", "poly1305", "aead", "x25519", "bigint", "rsa",
  "ecdsa", "x509", "x509_chain", "tls13_kdf", "random", "tls_record", "sha384",
  "ecdsa_p384", "time", "rootstore", "functional", "http_parse", "json", "yaml",
];

fn run_one(name: &str) {
  let binary = env!("CARGO_BIN_EXE_jink");
  let src = format!("tests/jk/{name}.jk");
  let exe = if cfg!(windows) { "output.exe" } else { "output" };
  let _ = std::fs::remove_file(exe);
  let compile = Command::new(binary)
    .arg(&src)
    .arg("-o")
    .output()
    .expect("failed to launch the jink compiler");
  assert!(
    Path::new(exe).exists(),
    "[{name}] compilation failed\nstdout: {}\nstderr: {}",
    String::from_utf8_lossy(&compile.stdout),
    String::from_utf8_lossy(&compile.stderr),
  );

  // Program exits with its failure count (0 means every assertion passed)
  let exe_path = if cfg!(windows) { ".\\output.exe" } else { "./output" };
  let run = Command::new(exe_path)
    .output()
    .expect("failed to run the compiled test");
  assert!(
    run.status.success(),
    "[{name}] assertions failed (exit {:?}):\n{}",
    run.status.code(),
    String::from_utf8_lossy(&run.stdout),
  );
}

#[test]
fn std_library_suite() {
  for name in JK_TESTS {
    run_one(name);
  }
}
