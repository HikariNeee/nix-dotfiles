{
  config,
  lib,
  ...
}:
{

  imports = [
    ./i3bar-river/i3bar-river.nix
    ./programs/firefox.nix
    ./programs/foot.nix
    ./programs/fuzzel.nix
    ./programs/i3bar-river.nix
    ./base/config.nix
    ./base/home.nix
  ];

}
