{
  config,
  pkgs,
  lib,
  ...
}:
{
  programs.i3status-rust = {
    enable = true;
    bars.top = {
      theme = "ctp-mocha";
      icons = "awesome6";
      blocks = [
             {
               block = "uptime";
             }
             {
               block = "cpu";
               interval = 5;
             }
             {
               block = "time";
               interval = 60;
               format = " $timestamp.datetime(f:'%a %d/%m %R') ";
             }
          ];      
    };
  };

  programs.i3bar-river = {
    enable = true;
    settings = {
      command = "i3status-rs ~/.config/i3status-rust/config-top.toml";
      background = "#1e1e2eff";
      color = "#cdd6f4ff";
      separator = "#9a8a62ff";
      tag_fg = "#b4637aff";
      tag_bg = "#1e1e2eff";
      tag_focused_fg = "#04a5e5ff";
      tag_focused_bg = "#313244ff";
      tag_urgent_fg = "#282828ff";
      tag_urgent_bg = "#dc8a78ff";
      tag_inactive_fg = "#d79921ff";
      tag_inactive_bg = "#282828ff";

      font = "Comic Shanns Mono 12";
      height = 20;
      margin_top = 4;
      margin_bottom = 0;
      margin_left = 8;
      margin_right = 8;
      separator_width = 0;
      tags_r = 6;
      tags_padding = 15.0;
      blocks_r = 6;
      blocks_overlap = 0.0;
      position = "top";
      hide_inactive_tags = true;
      invert_touchpad_scrolling = true;
      blend = true;
      show_mode = true;

      wm.river = {
        max_tag = 9;
      };
    };
  };
}
