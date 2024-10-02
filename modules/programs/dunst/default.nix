{
  pkgs,
  ...
}: {
  wrappers.dunst = {
    basePackage = pkgs.dunst;
    flags = [
      "-config"
      ../../conf/dunstrc
    ];
  };
}
