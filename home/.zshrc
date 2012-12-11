export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="dpoggi"
export DISABLE_AUTO_UPDATE="true"
export LC_ALL="C"
export LANG="en_US.UTF-8"
export EDITOR="/usr/local/bin/vim"

if [ `uname` = "Darwin" ]; then
    alias ls='ls -G'
else
    alias ls='ls --color'
fi

alias ll='ls -lh'
alias la='ls -a'
alias tmux="TERM=screen-256color-bce tmux -u"

# Don't correct filenames with these commands
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'

PATH=""
if [ -d $HOME/Scripts ]; then
    PATH="$HOME/Scripts"
fi
if [ -d $HOME/bin ]; then
    PATH="$HOME/bin"
fi

HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000
setopt appendhistory
setopt sharehistory
setopt incappendhistory

zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

if [ -d $HOME/.rbenv ]; then
    PATH="$PATH:$HOME/.rbenv/shims:$HOME/.rbenv/bin"
    HAS_RBENV=true
fi

SYSTEM_PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/usr/X11/bin:/bin:/sbin"
[[ $PATH == "" ]] && PATH=$SYSTEM_PATH || PATH="$PATH:$SYSTEM_PATH"
export PATH

[[ ! -z $HAS_RBENV ]] && eval "$(rbenv init -)"

ZSH_CUSTOM=$HOME/.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
plugins=(history-substring-search zsh-syntax-highlighting git vi-mode)
source $ZSH/oh-my-zsh.sh

ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
ZSH_HIGHLIGHT_PATTERNS+=('sudo ' 'fg=white,bold,bg=red')

if [ -f $HOME/.zsh_aliases ]; then
    source $HOME/.zsh_aliases
fi
if [ -f $HOME/.zsh_local ]; then
    source $HOME/.zsh_local
fi
if [ -f $HOME/.aliases ]; then
    source $HOME/.aliases
fi
if [ -f $HOME/.local ]; then
    source $HOME/.local
fi

# Autojump
export AUTOJUMP_DATA_DIR=$HOME/.local/share/autojump

function autojump_preexec() {
    { (autojump -a "$(pwd -P)"&)>/dev/null 2>>|${AUTOJUMP_DATA_DIR}/.autojump_errors ; } 2>/dev/null
}

typeset -ga preexec_functions
preexec_functions+=autojump_preexec

alias jumpstat="autojump --stat"
function j { local new_path="$(autojump $@)";if [ -n "$new_path" ]; then echo -e "\\033[31m${new_path}\\033[0m"; cd "$new_path";else false; fi }

# Set shell colors
. ~/.base16-shell/base16-default.dark.sh

# Stop annoying correcting all the time
unsetopt correct_all

# Git fastness
__git_files () {
  _wanted files expl 'local files' _files
}

# aliases -kb
alias be="bundle exec"
alias brake="be rake"
alias bspec="be rspec"

alias dotfiles="cd ~/.homesick/repos/.files/home"
alias rc="source ~/.zshrc"

