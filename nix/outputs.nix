{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

  staticPkgs = project.cabalProject.projectCross.musl64.hsPkgs;

  static = staticPkgs.credential-manager.components.exes;

in

[
  (
    # Docs for project.flake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectoutflake
    project.flake
  )
  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.static.orchestrator-cli = static.orchestrator-cli;
  })
]
