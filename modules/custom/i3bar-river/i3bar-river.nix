{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.i3bar-river;
  format = pkgs.formats.toml { };
in
{
  options.programs.i3bar-river = {
    enable = mkEnableOption "i3bar-river";

    package = mkOption {
      type = types.package;
      default = pkgs.i3bar-river;
      defaultText = literalExpression "pkgs.i3bar-river";
      description = "the i3bar-river package to install";
    };

    settings = mkOption {
      type = format.type;
      default = { };
      description = ''
        Configuration written to {file}`$XDG_CONFIG_HOME/i3bar-river/config.toml`.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."i3bar-river/config.toml" = mkIf (cfg.settings != { }) {
      source = format.generate "config.toml" cfg.settings;
    };
  };
}
