# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
export PATH=/opt/local/bin:$PATH
if [[ -d $HOME/.cargo ]]; then
    export PATH=$HOME/.cargo/bin:$PATH
fi
# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting osx)

ZSH_DISABLE_COMPFIX=true
source $ZSH/oh-my-zsh.sh
# eval "$(fasd --init auto)"

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8


export ZSH_LOCAL=$HOME/.dotfiles/shell/zshrc.local
if [[ -f $ZSH_LOCAL ]]; then
    source $ZSH_LOCAL
fi

if [[ -f ~/.fzf.zsh ]]; then
    source ~/.fzf.zsh
else
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
    source ~/.fzf.zsh
fi

if command -v fzf > /dev/null; then
    export FZF_DEFAULT_OPTS="--height=40% --min-height=20"
    export FZF_COMPLETION_TRIGGER='~~'
    export FZF_COMPLETION_OPTS='+c -x'
fi

if command -v bat > /dev/null; then
    export MANPAGER=bat
fi

if [[ -f ~/.dotfiles/shell/aliases.sh ]]; then
    source ~/.dotfiles/shell/aliases.sh
fi

export XDG_CONFIG_HOME="$HOME/.config/"
export PATH="$XDG_CONFIG_HOME/bin":$PATH
source ~/.zprofile

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh