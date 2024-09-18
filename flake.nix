{
 description = "flake setup :D";
 inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  home-manager.inputs.nixpkgs.follows = "nixpkgs";
  home-manager.url = "github:nix-community/home-manager";
  arkenfox.url = "github:dwarfmaster/arkenfox-nixos";
  rbl.url = "github:areif-dev/river-bsp-layout";
 };

 outputs = { self, nixpkgs, home-manager, arkenfox, rbl }@inputs:
  let 
   system = "x86_64-linux";
   pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
   inherit (nixpkgs) lib;
  in {
   nixosConfigurations = {
    Tsu = lib.nixosSystem {
     inherit system;
     specialArgs = { inherit inputs self; };
     modules = [ 
      ./config.nix 
      home-manager.nixosModules.home-manager {
       home-manager.useGlobalPkgs = true;
       home-manager.useUserPackages = true;
       home-manager.users.hikari = { imports = [ ./home.nix arkenfox.hmModules.arkenfox ]; };
     }
    ];
   };
  };
 };
}
