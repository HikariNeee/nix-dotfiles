{
  config,
  lib,
  ...
}:
{

  imports = [
    ./i3bar-river/i3bar-river.nix
    ./browsers/browsers.nix
    ./programs/foot.nix
    ./programs/fuzzel.nix
    ./programs/i3bar-river.nix
  ];

}
