{
  description = "Yet another Game Boy emulator in rust";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, rust-overlay, ... }:
    let
      rustChannel = "stable";
    in
    utils.lib.eachDefaultSystem
      (system:
        let
          # Imports
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (import rust-overlay)
            ];
          };

          # Configuration for the non-Rust dependencies
          buildInputs = with pkgs; [ openssl.dev SDL2 ];
          nativeBuildInputs = with pkgs; [ pkgconfig ];
        in
         {
          # `nix develop`
          devShell = pkgs.mkShell
            {
              buildInputs = buildInputs ++ (with pkgs;
                # Tools you need for development go here.
                [
                  rust-bin.stable.latest.default
                  nixpkgs-fmt
                  cargo-watch
                  cargo-outdated
                  cargo-edit
                ]);
              RUST_SRC_PATH = "${pkgs.rust-bin.stable.latest.rust-src}/lib/rustlib/src/rust/library";
            };
        }
      );
}
