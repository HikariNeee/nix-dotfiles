{
  description = "flake setup :D";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rbl.url = "github:areif-dev/river-bsp-layout";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    impermanence.url = "github:nix-community/impermanence";
    wrapper-manager.url = "github:ViperML/wrapper-manager";
  };

  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
      rbl,
      impermanence,
      wrapper-manager,
    }@inputs:
    let
      system = "x86_64-linux";
      overlays = [ (import self.inputs.emacs-overlay) ];
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowUnfree = true;
      };
      inherit (nixpkgs) lib;
    in
    {
      formatter.x86_64-linux = pkgs.nixfmt-rfc-style;
      nixosConfigurations = {
        Tsu = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs self;
          };
          modules = [
            ./modules/base/config.nix
            impermanence.nixosModules.impermanence
          ];
        };
      };
    };
}
