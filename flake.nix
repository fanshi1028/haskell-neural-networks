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
            finite-typelits = hsuper.finite-typelits_0_2_0_0;
            microlens-th = pkgs.haskell.lib.callHackageDirect {
              pkg = "microlens-th";
              version = "0.4.3.15";
              sha256 = "00000000";
            };
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
