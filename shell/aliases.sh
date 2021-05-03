# Standard aliases
alias zshconfig='vim ~/.zshrc'
alias ohmyzshconfig='vim ~/.oh-my-zsh'

# Open dotfiles directory
alias dotfiles="cd ~/.dotfiles"

# Protect against overwriting
alias cp='cp -i'
alias mv='mv -i'

# Change tmux directory to save server info
if ! [[ -d $HOME/.tmux_sessions ]];then
    mkdir $HOME/.tmux_sessions
fi
export TMUX_TMPDIR=$HOME/.tmux_sessions

# Mysql aliases
alias mysql='/usr/local/mysql/bin/mysql'
alias mysqladmin='/usr/local/mysql/bin/mysqladmin'

# Lazygit
if [[ $TERM == "tmux-256color" ]]; then
    alias lag='TERM=xterm-kitty lazygit'
else
    alias lag='lazygit'
fi

# Ncmpcpp
alias np='ncmpcpp -b ~/.ncmpcpp/bindings'

# If available, use nvim as editor. Vim otherwise
if command -v nvim > /dev/null; then
    alias vim="nvim"
fi

# If available, use exa for listing directories
if command -v exa > /dev/null; then
  alias ls=exa
  alias l="exa -1"
  alias la="exa -a"
fi

# File search functions
f() { find . -iname "*$1*" ${@:2} }
r() { grep "$1" ${@:2} -R . }

# List top `n` pacman packages by 
pactop() {
    pacman -Qi | awk '/^Name/{name=$3} /^Installed Size/{print $4$5, name}' | sort -hr | head -"$1"
}

# Count files in a folder
lwc() {
    local cdir="${1}"
    if [[ "${1}" == "" ]];then
        cdir="$(pwd)"
    fi
    local count=$(ls $1 | wc -l)
    echo "Total files in ${cdir}: ${count}"
}
# Update dotfiles
dfu() {
    {
        cd ~/.dotfiles && git pull && ./install
    }
}

# Create a folder and move into it in one command
mkcd() { mkdir -p "$@" && cd "$_"; }

# Go up [n] directories
up()
{
    local cdir="$(pwd)"
    if [[ "${1}" == "" ]]; then
        cdir="$(dirname "${cdir}")"
    elif ! [[ "${1}" =~ ^[0-9]+$ ]]; then
        echo "Error: argument must be a number"
    elif ! [[ "${1}" -gt "0" ]]; then
        echo "Error: argument must be positive"
    else
        for ((i=0; i<${1}; i++)); do
            local ncdir="$(dirname "${cdir}")"
            if [[ "${cdir}" == "${ncdir}" ]]; then
                break
            else
                cdir="${ncdir}"
            fi
        done
    fi
    cd "${cdir}"
}

# fzf
if command -v fzf > /dev/null; then

    function fzf-history() {
      local tac
      if which tac > /dev/null; then
          tac="tac"
      else
          tac="tail -r"
      fi
      BUFFER=$(\history -n 1 | fzf)
      CURSOR=$#BUFFER
    }

    fh() {
    print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
    }

    # fkill - kill process
    fkill() {
    local pid
    pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

    if [ "x$pid" != "x" ]
    then
      echo $pid | xargs kill -${1:-9}
    fi
    }

    # v() {
    #   local file
    #   file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && vim "${file}" || return 1
    # }

    #less on fuzzy find
    fl() {
    less $(fzf)
    }

    fv() {
    nvim $(fzf)
    }
fi
