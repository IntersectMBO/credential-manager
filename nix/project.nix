{ repoRoot, inputs, pkgs, system, lib }:

let

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ pkgs, config, ... }:
    let
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      name = "credential-manager";

      src = ../.;

      shell.withHoogle = false;

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.iogx.inputs.CHaP;
      };

      compiler-nix-name = lib.mkDefault "ghc96";

      flake.variants = {
        ghc96 = { };
        ghc98.compiler-nix-name = "ghc98";
        ghc910.compiler-nix-name = "ghc910";
      };

      modules =
        [
          {
            dontStrip = false;
            packages = { };
          }
        ];
    });


  cabalProject = cabalProject'.appendOverlays [ ];


  # Docs for mkHaskellProject: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellproject
  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;

    shellArgs = repoRoot.nix.shell;

    includeMingwW64HydraJobs = true;

    readTheDocs = {
      enable = true;
      siteFolder = "doc/read-the-docs-site";
    };
    combinedHaddock = {
      enable = true;
    };
  };

in

project
