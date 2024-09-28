# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
let
  emacsWithTreeSitter =
    (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
    ]);
in
{
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })

      (import inputs.emacs-overlay)
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  nix = let
    flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
  in {
    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Opinionated: disable global registry
      flake-registry = "";
      # Workaround for https://github.com/NixOS/nix/issues/9574
      nix-path = config.nix.nixPath;
      trusted-users = ["root" "chahak"];
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
    };
    # Opinionated: disable channels
    channel.enable = false;

    # Opinionated: make flake registry and nix path match flake inputs
    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
    nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
  };

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "boticelli";
  networking.networkmanager.enable = true;

  # Time zone
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  users.users = {
    chahak = {
      # Be sure to change it (using passwd) after rebooting!
      isNormalUser = true;
      description = "Chahak Mehta";
      extraGroups = ["wheel" "networkmanager"];
    };
  };

  # OpenSSh Daemon
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
    };
  };

  # X11 windowing system
  # services.xserver = {
  #   enable = true;

  #   # Display Manager
  #   displayManager = {
  #     gdm.enable = true;
  #   };

  #   # Desktop Manager
  #   desktopManager = {
  #     gnome.enable = true;
  #   };

  #   # Keymaps
  #   xkb = {
  #     layout = "us";
  #     variant = "";
  #     options = "ctrl:nocaps";
  #   };
  # };
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
  };

  # Enable CUPS to print docs
  services.printing.enable = true;

  # Enable sound with pipewire
  # sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
  };


  # Enable bluetooth
  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  environment.systemPackages = with pkgs; [
    neovim
    git
    cachix
    unzip
    killall
    mariadb
    emacsWithTreeSitter
    mako                        # Notification display system
    libnotify                   # To use `notify-send` to send notifications
    upower
    poweralertd
    greetd.tuigreet
  ];
  environment = {
    variables.EDITOR = "nvim";
  };

  # Fonts
  fonts.packages = with pkgs; [
    # etBook
    jetbrains-mono
  ];

  programs.dconf.enable = true;

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };

  services.emacs = {
    enable = true;
    package = emacsWithTreeSitter;
  };

  services.upower = {
    enable = true;
    percentageLow = 20;
  };


    # services.greetd = let
    #   tuigreet = "${pkgs.greetd.tuigreet}/bin/tuigreet";
    #   hyprland-session = "${inputs.hyprland.packages.${pkgs.system}.hyprland}/share/wayland-sessions";
    # in {
    #   enable = true;
    #   settings = {
    #     default_session = {
    #       command = "${tuigreet} --time --remember --sessions ${hyprland-session} --cmd ~/.local/bin/wrappedhl";
    #       user = "chahaĸ";
    #     };
    #   };
    # };

  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal";
    TTYReset = true;
    TTYHangup = true;
    TTYVTDisallocate = true;
  };

  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };
  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.11";
}
