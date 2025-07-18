{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;
  cc-sign-native = pkgs.stdenv.mkDerivation {
    name = "cc-sign-native";

    nativeBuildInputs = lib.optional pkgs.stdenv.isDarwin [
      pkgs.macdylibbundler
      pkgs.darwin.autoSignDarwinBinariesHook
    ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp ${project.packages.cc-sign}/bin/cc-sign $out/bin/cc-sign
      chmod 755 $out/bin/cc-sign

      ${lib.optionalString pkgs.stdenv.isDarwin ''
        # on any Darwin host (Intel or ARM)
        dylibbundler -b --no-codesign \
          -x $out/bin/cc-sign \
          -d $out/bin \
          -p '@executable_path'
      signDarwinBinariesInAllOutputs
      ''}
    '';

  };

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
