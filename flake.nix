{
  description = "flake setup :D";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    arkenfox.url = "github:dwarfmaster/arkenfox-nixos";
    rbl.url = "github:areif-dev/river-bsp-layout";
    catppuccin.url = "github:catppuccin/nix";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      arkenfox,
      emacs-overlay,
      rbl,
      catppuccin,
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
            ./config.nix
            catppuccin.nixosModules.catppuccin            
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { rootPath = ./.; };
              home-manager.users.hikari = {
                imports = [
                  ./home.nix
                  catppuccin.homeManagerModules.catppuccin
                  arkenfox.hmModules.arkenfox
                ];
              };
            }
          ];
        };
      };
    };
}
