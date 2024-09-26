{
  config,
  pkgs,
  lib,
  ...
}:
{
  programs.firefox = {
    enable = true;
    arkenfox.enable = true;
    arkenfox.version = "128.0";
    profiles.hikari.name = "hikari";
    profiles.hikari.search.default = "DuckDuckGo";
    profiles.hikari.search.privateDefault = "DuckDuckGo";
    profiles.hikari.search.force = true;
    profiles.hikari.isDefault = true;
    profiles.hikari.settings = {
      "browser.compactmode.show" = true;
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };
    profiles.hikari.userChrome = ''
      * {
      font-family: "Comic Neue" !important;
      font-size: 12pt !important;
      }
    '';
    profiles.hikari.arkenfox = {
      enable = true;
      enableAllSections = true;
      "4500" = {
        enable = true;
        "4504"."privacy.resistFingerprinting.letterboxing".value = false;
      };
    };
  };
}
