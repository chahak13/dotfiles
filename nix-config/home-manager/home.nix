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
    extraConfig = ''
    # Source gruvbox theme for tmux
    source-file ~/.dotfiles/tmux/tmux_gruvbox.conf

    # Fix titlebar
    set -g set-titles on
    set -g set-titles-string "#T"
    '';
  };

  # GTK Settings
  gtk = {
    enable = true;
    gtk3.extraConfig = {
      "gtk-application-prefer-dark-theme" = true;
    };
  };

  services.mako.enable = true;
  services.poweralertd.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.11";
}
