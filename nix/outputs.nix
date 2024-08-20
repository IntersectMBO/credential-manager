{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

in

[
  project.flake
  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.musl64 = project.cross.musl64.hydraJobs;
  })
]
