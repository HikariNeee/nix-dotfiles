{ 
  pkgs,
  ...
}: {
  wrappers.i3status-rust = {
    basePackage = pkgs.i3status-rust;
    flags = [
      ../../conf/i3status-rust.toml
    ];
  };
}
