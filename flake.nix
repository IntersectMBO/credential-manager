# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#flakenix
{
  description = "Change the description field in your flake.nix";


  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.CHaP.follows = "cardano-node/CHaP";
      inputs.hackage.follows = "cardano-node/hackageNix";
      # inputs.haskell-nix.follows = "haskell-nix";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl.url = "github:nix-community/nixGL";

    nixpkgs.follows = "iogx/nixpkgs";

    # hackage = {
    #   url = "github:input-output-hk/hackage.nix";
    #   flake = false;
    # };

    # CHaP = {
    #   url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    #   flake = false;
    # };

    # haskell-nix = {
    #   url = "github:input-output-hk/haskell.nix";
    #   inputs.hackage.follows = "hackage";
    # };
    cardano-node.url = "github:IntersectMBO/cardano-node/9.1.0";
  };


  # Docs for mkFlake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkflake
  outputs = inputs: inputs.iogx.lib.mkFlake {

    inherit inputs;

    repoRoot = ./.;

    outputs = import ./nix/outputs.nix;

    # systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];

    # debug = false;

    # nixpkgsArgs = {
    #   config = {};
    #   overlays = [];
    # };

    # flake = { repoRoot, inputs }: {};
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.garnix.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    allow-import-from-derivation = true;
  };
}
