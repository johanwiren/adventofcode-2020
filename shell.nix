{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc
    cargo
    rust.packages.stable.rustPlatform.rustLibSrc
    rust-analyzer
    rustfmt
    clippy
  ];
  RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
  RUST_BACKTRACE = 1;
  shellHook = ''
    export RUST_SRC_PATH RUST_BACKTRACE
  '';
}
