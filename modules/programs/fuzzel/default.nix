{
  config,
  pkgs,
  lib,
  ...
}:
{
  wrappers.fuzzel = {
    basePackage = pkgs.fuzzel;
    flags = [
      "--config"
      ../../conf/fuzzel.ini
    ];
  };
}
