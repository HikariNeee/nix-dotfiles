{ 
  pkgs,
  ...
}: {
  wrappers.i3bar-river = {
    basePackage = pkgs.i3bar-river;
    flags = [
      "--config"
      ../../conf/i3bar-river.toml
    ];
  };
}
