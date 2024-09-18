# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ 
 self,
 config, 
 lib, 
 pkgs, 
 inputs,
 ... 
}:

{
  imports = [
   ./hardware.nix
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.gc.randomizedDelaySec = "10m";
  nix.package = pkgs.nixVersions.latest;
  nix.gc = {
   automatic = true;
   dates = "weekly";
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.zfsSupport = true;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.loader.grub.splashImage = null;
  boot.tmp.cleanOnBoot = true;
  
  boot.kernelParams = [ 
   "cgroup_no_v1=all" 
   "systemd.unified_cgroup_hierarchy=yes" 
   "console=tty1" 
   "mitigations=off" 
   "nowatchdog" 
   "tsc=reliable"
   "loglevel=3"
  ];


  networking.hostName = "Tsu";
  networking.hostId = "abcd1234";
  networking.networkmanager.enable = true;  

  time.timeZone = "Asia/Kolkata";

  i18n.defaultLocale = "en_GB.UTF-8";

  services.pipewire = {
   enable = true;
   pulse.enable = true;
  };
  services.dbus.implementation = "broker";
  services.openssh.enable = true;
  services.emacs.enable = true;

  users.users.hikari = {
   isNormalUser = true;
   extraGroups = [ "wheel" ];
   };

  hardware.graphics.enable = true;
  
  environment.systemPackages = with pkgs; [
   wget
   chezmoi
   git
   fuzzel
   swaybg 
   dunst
   i3bar-river
   river
   fish
   foot
   haskell.compiler.ghc910
   cabal-install
   python311Packages.py3status
   grim
   slurp
   wl-clipboard
   libnotify
   inputs.rbl.packages.${pkgs.stdenv.hostPlatform.system}.river-bsp-layout
  ];

  fonts.packages = with pkgs; [
   comic-neue
   noto-fonts
   noto-fonts-cjk
   noto-fonts-emoji
  ];

  system.stateVersion = "24.05"; # Did you read the comment?

  security.doas.enable = true;
  security.sudo.enable = false;
  
  security.doas.extraRules = [{
   users = [ "hikari" ];
   keepEnv = true;
   noPass = true;
  }];
  security.protectKernelImage = true;
}

