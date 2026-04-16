{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=ebe4301cbd8f81c4f8d3244b3632338bbeb6d49c";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    {
      nixpkgs,
      rust-overlay,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

      in
      with pkgs;
      let
        haskell925 = haskell.packages.ghc925;
      in
      {
        devShells.default = mkShell {
          buildInputs = with haskellPackages; [
            haskell.compiler.ghc925
            haskell925.haskell-language-server
            haskell925.ghcid
            stack
            cabal-install
            hlint
            hpack
            (rust-bin.selectLatestNightlyWith (
              toolchain: toolchain.default.override { extensions = [ "rust-src" ]; }
            ))
          ];
        };
      }
    );
}
