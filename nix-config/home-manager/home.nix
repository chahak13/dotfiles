# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
{
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  home = {
    username = "chahak";
    homeDirectory = "/home/chahak";
  };

  # Add stuff for your user as you see fit:
  home.packages = with pkgs; [
    alacritty
    bat
    chromium
    direnv
    dive
    eza
    firefox
    fish
    glib
    glibc
    htop
    jq
    jujutsu
    nil
    pyenv
    python312
    python312Packages.pipx
    (poetry.override { python3 = python312; })
    ripgrep
    steam
    tmux
    zoxide
    fd
    tealdeer
    dconf
    graphviz                    # Mainly used in roam graphs
    # Hyprland packages
    bemenu
    waybar
    swaybg
    brightnessctl
    font-awesome
    bluetuith                   # Nice TUI for bluetooth management
    vlc
    mpv
    hledger
  ];

  programs.home-manager.enable = true;
  programs.git.enable = true;
  programs.neovim.enable = true;
  programs.alacritty.enable = true;
  programs.fd.enable = true;
  # programs.steam.enable = true;

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
    zoxide init fish | source
    alias ls eza

    function fish_hybrid_key_bindings --description \
    "Vi-style bindings that inherit emacs-style bindings in all modes"
        for mode in default insert visual
            fish_default_key_bindings -M $mode
        end
        fish_vi_key_bindings --no-erase
    end
    set -g fish_key_bindings fish_hybrid_key_bindings
    '';
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  # programs.emacs = {
  #   enable = true;
  #   package = emacsWithTreeSitter;
  # };
  # services.emacs = {
  #   enable = true;
  #   package = emacsWithTreeSitter;
  # };

  # home.file.".tmux.conf".source = "/home/chahak/.tmux.conf";
  programs.tmux = {
    enable = true;
    mouse = true;
    sensibleOnTop = false;
    shell = "/etc/profiles/per-user/chahak/bin/fish";
    baseIndex = 1;
    terminal = "tmux-256color";
    historyLimit = 100000;
    keyMode = "vi";
    escapeTime = 0;
    prefix = "C-f";
    extraConfig = ''
    # Set default shell as bash
    # set-option -g default-shell /usr/bin/fish

    # Set to 256 colors
    set -g default-terminal "tmux-256color"
    # Source gruvbox theme for tmux
    source-file ~/.dotfiles/tmux/tmux_gruvbox.conf

    # Reload tmux configuration file with C-r
    # bind C-r source-file ~/.tmux.conf \; display-message "configuration reloaded"

    # Split panes vertically (Horizontal split is by " (default))
    bind | split-window -h

    # Set scroll history to 1,00,000 lines
    set-option -g history-limit 100000

    # Renumber windows on moving/deleting
    set-option -g renumber-windows on

    # Change prefix
    unbind C-b
    set -g prefix C-f

    # Start window indexing from 1 to match keyboard layout
    set -g base-index 1

    # Start pane indexing from 1 to match keyboard layout
    set -g pane-base-index 1

    # Fix titlebar
    set -g set-titles on
    set -g set-titles-string "#T"

    # Vim mode
    set -g mode-keys vi

    # Enable mouse
    set -g mouse on

    # Use vim bindings to move between panes
    bind h select-pane -L
    bind j select-pane -D
    bind k select-pane -U
    bind l select-pane -R

    # reenable automatic renaming for the current window
    bind N setw automatic-rename on \; display-message "automatic rename enabled"

    # No waiting for escape sequences
    set -sg escape-time 0

    # Display pane numbers for longer time
    set -g display-panes-time 2000
    '';
  };

  # GTK Settings
  gtk = {
    enable = true;
    gtk3.extraConfig = {
      "gtk-application-prefer-dark-theme" = true;
    };
  };

  services.mako = {
    enable = true;
    sort = "-time";
    layer = "overlay";
    backgroundColor = "#2e3440";
    width = 300;
    height = 110;
    borderSize = 2;
    borderColor = "#88c0d0";
    borderRadius = 15;
    icons = true;
    maxIconSize = 64;
    defaultTimeout = 5000;
    ignoreTimeout = true;
    font = "monospace 10";
    extraConfig = ''
    [urgency=low]
    border-color=#cccccc

    [urgency=normal]
    border-color=#d08770

    [urgency=high]
    border-color=#bf616a
    default-timeout=0

    [category=mpd]
    default-timeout=2000
    group-by=category
    '';
  };

  services.poweralertd.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.11";
}
