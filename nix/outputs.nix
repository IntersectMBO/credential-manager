{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

  staticPkgs = project.cabalProject.projectCross.musl64.hsPkgs;

  static = staticPkgs.credential-manager.components.exes;

in

[
  (
    # Docs for project.flake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectoutflake
    lib.recursiveUpdate project.flake rec {
      packages.signing-tool =
        project.cabalProject.hsPkgs.credential-manager.components.exes.signing-tool.overrideAttrs
          (final: prev: {
            nativeBuildInputs = prev.nativeBuildInputs ++ [ pkgs.wrapGAppsHook4 ];
          });
    }
  )
  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.static.orchestrator-cli = static.orchestrator-cli;
  })
]
