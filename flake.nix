{
  description = "A tiny APL dialect and interpreter in Haskell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs =
    { self, nixpkgs, ghc-wasm-meta }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = (import nixpkgs { system = "x86_64-linux"; });
    in
    {
      devShells = forAllSystems (system: {
        default = pkgs.callPackage ./shell.nix {
          inherit pkgs;
          ghc-wasm-meta = ghc-wasm-meta.packages.${pkgs.system}.all_9_10;
        };
      });
    };
}
