{
  config,
  pkgs,
  lib,
  ...
}:
{
  home.username = "hikari";
  home.homeDirectory = "/home/hikari";
  home.stateVersion = "24.05";

  catppuccin.flavor = "mocha";
  catppuccin.pointerCursor.enable = true;
 
  imports = [ ./modules ];

  programs.home-manager.enable = true;
  programs.git = {
    userName = "Hikari";
    userEmail = "quelln@protonmail.com";
  };


  programs.fish = {
    enable = true;
    preferAbbrs = true;
    catppuccin.enable = true;
  };

  
  home.packages = with pkgs; [
    gcc
    htop
    sbcl
    hunspell
    hunspellDicts.en_GB-ize
    enchant
    pkg-config
    nurl
  ];
}
