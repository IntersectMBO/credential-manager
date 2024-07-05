{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

  staticPkgs = project.cabalProject.projectCross.musl64.hsPkgs;

  static = staticPkgs.credential-manager.components.exes;

  allStatic = pkgs.runCommand "all-statics" { } ''
    mkdir -p $out
    ${lib.concatMapStringsSep "\n" (drv: "cp ${drv}/bin/* $out") (lib.attrValues static)}
  '';


in

[
  (
    # Docs for project.flake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectoutflake
    project.flake
  )
  (lib.optionalAttrs pkgs.stdenv.isLinux
    {
      packages.allStatic = allStatic;
      hydraJobs.allStatic = allStatic;
      hydraJobs.static = static;
    }
  )
]
