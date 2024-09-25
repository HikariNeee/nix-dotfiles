{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.foot = {
    enable = true;
    catppuccin.enable = true;
    settings = {
      main = {
        font = "Comic Shanns Mono:size=12";
        shell = "fish";
      };
    };
  };
}

