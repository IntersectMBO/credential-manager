{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

  projectCross = project.cabalProject.projectCross;

  staticPkgs = project.cabalProject.projectCross.musl64.hsPkgs;

  static = staticPkgs.credential-manager.components.exes;

in

[
  project.flake
  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.static.orchestrator-cli = static.orchestrator-cli;
  })
]
