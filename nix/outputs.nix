{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

  staticPkgs =
    if pkgs.stdenv.isLinux
    then project.cabalProject.projectCross.musl64.hsPkgs
    else if system == "x86_64-darwin"
    then project.cabalProject.projectCross.x86_64-darwin.hsPkgs
    else if system == "aarch64-darwin"
    then project.cabalProject.projectCross.aarch64-darwin.hsPkgs
    else { };

  static = staticPkgs.credential-manager.components.exes;

in

[
  (
    # Docs for project.flake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectoutflake
    project.flake
  )
  {
    hydraJobs.static.orchestrator-cli = static.orchestrator-cli;
  }
]
