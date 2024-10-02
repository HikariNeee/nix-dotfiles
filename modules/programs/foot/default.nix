{
  config,
  pkgs,
  lib,
  ...
}:
{
  wrappers.foot = {
    basePackage = pkgs.foot;
    flags = [
      "-c"
      ../../conf/foot.ini
    ];
  };
}
