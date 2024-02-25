{
  inputs = {
    nixpkgs.follows = "ghc-wasm-meta/nixpkgs";
    flake-utils.follows = "ghc-wasm-meta/flake-utils";
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let pkgs = inputs.nixpkgs.legacyPackages.${system};
    in {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_gmp
        ];
      };
    });
}
