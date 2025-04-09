{
  pkgs ? import <nixpkgs> { },
  lib,
  ghc-wasm-meta
}:
let
  packages = with pkgs; [
    # haskell part
    haskell-language-server
    haskell.compiler.ghc910
    haskellPackages.Cabal_3_14_0_0
    cabal-install

    # C++ part
    termcap
    ncurses

    # JS/wasm part
    wasmtime
    ghc-wasm-meta
    typescript
  ];
in
pkgs.mkShell {
  # Get dependencies from the main package
  nativeBuildInputs = packages;
  buildInputs = packages;
  env = {
    LIBCLANG_PATH = "${pkgs.libclang.lib}/lib";
    LD_LIBRARY_PATH = "${lib.makeLibraryPath packages}";
  };
}
