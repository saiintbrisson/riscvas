{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain;
      in
      {
        devShells.default = with pkgs; mkShell {
          buildInputs = [
            pkg-config
            just
            git
            dtc
            python313
            python313Packages.pip
            glib
            glib.dev
            ninja
            gawk
            gnused
            gmp.dev
            mpfr.dev
            libmpc
            isl
            flock
            zlib.dev
            libgpg-error
            libgpg-error.dev
            libgcrypt
            libgcrypt.dev
            expat.dev
            texinfo

            llvmPackages_19.stdenv
            llvmPackages_19.libcxx
            llvmPackages_19.libcxxStdenv
            llvmPackages_19.clang-unwrapped

            apple-sdk_15
          ];

          shellHook = ''
            export C_INCLUDE_PATH="${pkgs.llvmPackages_19.clang-unwrapped.lib}/lib/clang/19/include:${pkgs.gmp.dev}/include"
            export CPLUS_INCLUDE_PATH="${pkgs.llvmPackages_19.libcxx.dev}/include/c++/v1:${pkgs.llvmPackages_19.clang-unwrapped.lib}/lib/clang/19/include:${pkgs.gmp.dev}/include"

            export PATH="${pkgs.llvmPackages_19.clang-unwrapped}/bin:$PATH"
            echo "Using clang version: $(clang --version | head -n1)"
          '';
        };
      }
    );
}
