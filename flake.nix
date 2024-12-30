{
  inputs = {
    nixpkgs.follows = "ghc-wasm-meta/nixpkgs";
    flake-utils.follows = "ghc-wasm-meta/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs) lib;
      devShells = lib.genAttrs [ "9_10" "9_12" ] (ghc: pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}."all_${ghc}"
        ];
      });
    in
    {
      devShells = devShells // { default = devShells."9_10"; };
    });
}
