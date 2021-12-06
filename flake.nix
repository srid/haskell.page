{
  description = "Ema documentation source";
  inputs = {
    ema.url = "github:srid/ema/master";
    # Use the nixpkgs used by the pinned ema.
    nixpkgs.follows = "ema/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    {
      ciNix = inputs.flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;

        # Optional. Systems for which to perform CI.
        # By default, every system attr in the flake will be built.
        # Example: [ "x86_64-darwin" "aarch64-linux" ];
        systems = [ "x86_64-linux" ];
      };

      effects.helloEffect = self.helloEffect.x86_64-linux;
    } //
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        name = "haskellpage";
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
        # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-976899227
        m1MacHsBuildTools =
          pkgs.haskellPackages.override {
            overrides = self: super:
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
                ormolu = workaround140774 super.ormolu;
              };
          };
        # Based on https://github.com/input-output-hk/daedalus/blob/develop/yarn2nix.nix#L58-L71
        filter = name: type:
          let
            baseName = baseNameOf (toString name);
            sansPrefix = pkgs.lib.removePrefix (toString ./.) name;
          in
          # Ignore these files when building source package
            !(
              baseName == "README.md" ||
              sansPrefix == "/bin" ||
              sansPrefix == "/.github" ||
              sansPrefix == "/.vscode" ||
              sansPrefix == "/.ghcid"
            );
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv name;
            root = pkgs.lib.cleanSourceWith { inherit filter name; src = ./.; };
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = disableCabalFlag inputs.ema.defaultPackage.${system} "with-examples";
              # lvar = self.callCabal2nix "lvar" inputs.ema.inputs.lvar { }; # Until lvar gets into nixpkgs
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with (if system == "aarch64-darwin"
                then m1MacHsBuildTools
                else pkgs.haskellPackages); [
                  # Specify your build/dev dependencies here. 
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.nixpkgs-fmt
                ]);
          };
        effects = (import inputs.nixpkgs {
          overlays = [
            inputs.hercules-ci-effects
          ];
        }).effects;
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project false;

        # Used by `nix develop`
        devShell = project true;

        helloEffect = effects.mkEffect {
          effectScript = ''
            echo Hello World
          '';
        };
      });
}
