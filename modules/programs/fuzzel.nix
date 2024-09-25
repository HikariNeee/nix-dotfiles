{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "Comic Shanns Mono-12";
        terminal = "foot -e";
        tabs = 2;
        width = 40;
      };
    catppuccin.enable = true;
    };
  };
}
