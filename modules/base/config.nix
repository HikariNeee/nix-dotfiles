# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  self,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    ./hardware.nix
  ];
  nixpkgs.overlays = [
    (import inputs.emacs-overlay)
  ];

  services.emacs = {
    package = (
      pkgs.emacsWithPackagesFromUsePackage {
        config = ./conf/emacs.el;
        defaultInitFile = true;
        package = pkgs.emacs-pgtk;
        extraEmacsPackages = (
          epkgs:
          (with epkgs; [
            (treesit-grammars.with-grammars (
              grammars: with grammars; [
                tree-sitter-nix
                tree-sitter-commonlisp
                tree-sitter-haskell
                tree-sitter-python
                tree-sitter-clojure
              ]
            ))
          ])
        );
      }
    );

    enable = true;
  };

  nix = {
    package = pkgs.nixVersions.latest;
    gc = {
      automatic = true;
      dates = "weekly";
      randomizedDelaySec = "10m";
    };
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";

    settings = {
      auto-optimise-store = true;
      keep-outputs = true;
      warn-dirty = false;
      keep-derivations = true;
      sandbox = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.zfsSupport = true;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.loader.grub.splashImage = null;
  boot.tmp.cleanOnBoot = true;

  boot.kernelParams = [
    "cgroup_no_v1=all"
    "systemd.unified_cgroup_hierarchy=yes"
    "console=tty1"
    "mitigations=off"
    "nowatchdog"
    "tsc=reliable"
    "loglevel=3"
    "intel_pstate=disable"
    "page_alloc.shuffle=1"
  ];

  console.catppuccin.enable = true;

  networking = {
    hostName = "Tsu";
    hostId = "abcd1234";

    networkmanager = {
      enable = true;
      dhcp = "internal";
    };

    nameservers = [
      "9.9.9.9"
      "149.112.112.112"
    ];
  };

  time.timeZone = "Asia/Kolkata";

  i18n.defaultLocale = "en_GB.UTF-8";

  users.users.hikari = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  hardware.graphics.enable = true;

  environment = {
    variables = {
      NIXOS_OZONE_WL = "1";
    };
    systemPackages = with pkgs; [
      wget
      swaybg
      i3bar-river
      river
      fish
      ghc
      haskellPackages.haskell-language-server
      haskellPackages.cabal-install
      grim
      slurp
      wl-clipboard
      libnotify
      inputs.rbl.packages.${pkgs.stdenv.hostPlatform.system}.river-bsp-layout
      emacs-lsp-booster
    ];
  };

  fonts.packages = with pkgs; [
    comic-neue
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    font-awesome
  ];

  system.stateVersion = "24.05"; # Did you read the comment?

  security.doas.enable = true;
  security.sudo.enable = false;
  security.polkit.enable = true;

  security.doas.extraRules = [
    {
      users = [ "hikari" ];
      keepEnv = true;
      noPass = true;
    }
  ];
  security.protectKernelImage = true;

  services = {
    dbus.implementation = "broker";
    thermald.enable = true;
    pipewire = {
      enable = true;
      pulse.enable = true;
    };
    dnscrypt-proxy2 = {
      enable = true;
      settings = {
        ipv6_servers = true;
        require_dnssec = true;

        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };
      };
    };
  };

  systemd = {
    coredump.enable = false;
    services = {
      NetworkManager-wait-online.enable = false;
      dnscrypt-proxy2.serviceConfig = {
        StateDirectory = "dnscrypt-proxy";
      };
    };
  };
}
