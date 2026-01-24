{
  description = "TPT C compiler toolchain, VM runtime, and R3 tooling";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        lua = pkgs.lua5_4.withPackages (ps: with ps; [
          lpeg
        ]);

        python = pkgs.python312.withPackages (ps: with ps; [
          matplotlib
          numpy
        ]);

        r3emu = pkgs.stdenv.mkDerivation {
          pname = "r3emu";
          version = "2026-01-24";
          src = pkgs.fetchgit {
            url = "https://git.l--n.de/justuswolff/R3emu";
            rev = "a2a36312daba7d7aac89b4f9e12f6b124407111c";
            hash = "sha256-cBq4EAoQYxz21EthSvE4XZK/jQ9wRv8lVucbJAgvEOs=";
          };
          nativeBuildInputs = [
            pkgs.meson
            pkgs.ninja
            pkgs.pkg-config
          ];
          buildInputs = [
            pkgs.SDL2
          ];
          configurePhase = ''
            meson setup build
          '';
          buildPhase = ''
            meson compile -C build
          '';
          installPhase = ''
            install -Dm755 build/R3emu $out/bin/R3emu
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            lua
            python
            pkgs.clang
            pkgs.gcc
            pkgs.gnumake
            pkgs.SDL2
            pkgs.the-powder-toy
            r3emu
          ];

          shellHook = ''
            echo "tpt-c-compiler dev shell"
            echo "Lua: $(lua -v | head -n1)"
            echo "Python: $(python3 --version)"
            echo ""
            echo "Example: lua cli.lua examples/hello.c --output examples/hello.asm"
          '';
        };

        packages.r3emu = r3emu;
        packages.default = r3emu;
        packages.python-env = python;
        packages.lua-env = lua;
      }
    );
}
