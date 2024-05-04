{
  inputs = {
    nixpkgs.url = "nixpkgs-unstable";
    nix-github-actions = {
      url = "github:nix-community/nix-github-actions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, nix-github-actions }:
    let
      ghcVersion = "98";
      mkHsPackage = pkgs:
        pkgs.haskell.packages."ghc${ghcVersion}".override ({
          overrides = hself: hsuper: {
            hourglass = pkgs.haskell.lib.dontCheck hsuper.hourglass;
            bsb-http-chunked =
              pkgs.haskell.lib.dontCheck hsuper.bsb-http-chunked;
            tasty-hedgehog =
              pkgs.haskell.lib.overrideCabal hsuper.tasty-hedgehog_1_4_0_2 {
                revision = "3";
                editedCabalFile =
                  "5c74496db551c8f1d783036aa8b99a39d89ec60a2925ea466a8bbcd5e68141c6";
              };
            http-conduit = hsuper.http-conduit_2_3_8_3;
            finite-typelits = hsuper.finite-typelits_0_2_0_0;
          };
        });
    in {

      packages = builtins.mapAttrs (system: pkgs:
        let hsPackages = mkHsPackage pkgs;
        in {
          default = hsPackages.developPackage {
            root = ./.;
            modifier = drv: pkgs.haskell.lib.appendConfigureFlag drv "-O2";
          };
        }) nixpkgs.legacyPackages;

      devShells = builtins.mapAttrs (system: pkgs:
        let hsPackage = (mkHsPackage pkgs);
        in {
          default = hsPackage.shellFor {
            packages = _: [ self.packages.${system}.default ];
            nativeBuildInputs = with pkgs; [
              (haskell-language-server.override {
                supportedGhcVersions = [ ghcVersion ];
                supportedFormatters = [ "ormolu" ];
              })
              haskellPackages.cabal-fmt
              cabal-install
              ghcid
            ];
            withHoogle = true;
          };
        }) nixpkgs.legacyPackages;

      checks = builtins.mapAttrs (system: pkgs:
        with pkgs; {
          default = self.packages.${system}.default;
          shell = self.devShells.${system}.default;
        }) nixpkgs.legacyPackages;

      githubActions = nix-github-actions.lib.mkGithubMatrix {
        checks =
          builtins.mapAttrs (_: checks: { inherit (checks) default shell; }) {
            inherit (self.checks) x86_64-linux x86_64-darwin;
          };
        platforms = {
          x86_64-linux = "ubuntu-22.04";
          x86_64-darwin = "macos-13";
        };
      };
    };

  nixConfig = {
    extra-substituters = [ "https://fanshi1028-personal.cachix.org" ];
    extra-trusted-public-keys = [
      "fanshi1028-personal.cachix.org-1:XoynOisskxlhrHM+m5ytvodedJdAo8gKpam/L6/AmBI="
    ];
  };
}
