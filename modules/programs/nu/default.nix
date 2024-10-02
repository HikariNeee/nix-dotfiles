{
  pkgs,
  ...
}: {
  wrappers.nushell = {
    basePackage = pkgs.nushell;
    flags = ["--config" ../../conf/config.nu];
  };
}
