# This file is pretty general, and you can adapt it in your project replacing
# only `name` and `description` below.

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
      # If you change the name here, you must also do it in Cargo.toml
      name = "rusty-feed-reader";
      rustChannel = "stable";
    in
    utils.lib.eachDefaultSystem
      (system:
        let
          # Imports
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              rust-overlay.overlay
              (self: super: {
                rustc = self.rust-bin.${rustChannel}.latest.default;
                cargo = self.rust-bin.${rustChannel}.latest.default;
                rustfmt = self.rust-bin.${rustChannel}.latest.default;
              })
            ];
          };

          # Configuration for the non-Rust dependencies
          buildInputs = with pkgs; [ openssl.dev ];
          nativeBuildInputs = with pkgs; [ rustc cargo pkgconfig ];
        in
        rec {
          # `nix develop`
          devShell = pkgs.mkShell
            {
              buildInputs = buildInputs ++ (with pkgs;
                # Tools you need for development go here.
                [
                  pkgs.cargo
                  pkgs.rustc
                  pkgs.rustfmt
                  nixpkgs-fmt
                  cargo-watch
                ]);
              RUST_SRC_PATH = "${pkgs.rust-bin.${rustChannel}.latest.rust-src}/lib/rustlib/src/rust/library";
            };
        }
      );
}
