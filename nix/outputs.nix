{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;
  cc-sign-native = pkgs.runCommand "cc-sign"
    {
      nativeBuildInputs = [
        pkgs.macdylibbundler
        pkgs.darwin.autoSignDarwinBinariesHook
      ];
    } ''
    mkdir -p $out/bin
    cp ${project.packages.cc-sign}/bin/cc-sign $out/bin/cc-sign
    chmod 755 $out/bin/cc-sign
    dylibbundler -b --no-codesign \
      -x $out/bin/cc-sign \
      -d $out/bin \
      -p '@executable_path'
    signDarwinBinariesInAllOutputs
  '';

in

[
  project.flake
  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.musl64 = project.cross.musl64.hydraJobs;
  })
  (lib.optionalAttrs pkgs.stdenv.isDarwin {
    packages = { inherit cc-sign-native; };
    hydraJobs = { inherit cc-sign-native; };
  })
]
