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

  imports = [ ../. ];

  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "Hikari";
    userEmail = "quelln@protonmail.com";
  };

  programs.fish = {
    enable = true;
    preferAbbrs = true;
    catppuccin.enable = true;
  };

  services.dunst = {
    enable = true;
    catppuccin.enable = true;
    settings = {
      global = {
        width = 400;
        offset = "5x5";
        progress_bar_min_width = 380;
        progress_bar_max_width = 380;
        progress_bar_corner_radius = 2;
        padding = 10;
        horizontal_padding = 10;
        frame_width = 1;
        gap_size = 3;
        font = "Comic Shanns Mono 12";
      };
    };
  };

  home.packages = with pkgs; [
    gcc
    ripgrep
    fd
    htop
    sbcl
    hunspell
    hunspellDicts.en_GB-ize
    enchant
    pkg-config
    nurl
    lagrange
  ];
}
