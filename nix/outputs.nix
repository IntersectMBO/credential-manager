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

  outputs = lib.foldl lib.recursiveUpdate project.flake [
    (lib.optionalAttrs pkgs.stdenv.isLinux {
      hydraJobs.musl64 = project.cross.musl64.hydraJobs;
    })
    (lib.optionalAttrs pkgs.stdenv.isDarwin {
      packages = { inherit cc-sign-native; };
      hydraJobs = { inherit cc-sign-native; };
    })
  ];
in

[
  (removeAttrs outputs [ "checks" ])
  {
    checks =
      let
        # https://github.com/numtide/flake-utils/issues/121#issuecomment-2589899217
        recurseIntoDeepAttrs = attrs:
          lib.recurseIntoAttrs
            (lib.mapAttrs
              (_: v:
                if builtins.typeOf v == "set" && !lib.isDerivation v
                then recurseIntoDeepAttrs v
                else v
              )
              attrs
            );
      in
      inputs.iogx.inputs.flake-utils.lib.flattenTree (recurseIntoDeepAttrs outputs.hydraJobs);
  }
]
