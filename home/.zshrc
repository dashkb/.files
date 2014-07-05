#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

if [[ $TMUX != "" ]]; then
  # Snag current tmux window title if any
  WINDOW_TITLE=$(tmux list-window -F "#{window_name}#{window_active}" | sed '/0$/d' | sed 's/.$//')
  if [[ $WINDOW_TITLE == 'reattach-to-user-namespace' ]]; then
    WINDOW_TITLE=$(pwd)
  fi
fi

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Set shell colors
. ~/.base16-shell/base16-default.dark.sh

zstyle ':prezto:module:terminal' auto-title 'no'


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
alias tr="tmux rename-window"

alias z="zeus"

# environment
export EDITOR=vim
export PATH="${PATH}:${HOME}/.rvm/bin:./node_modules/.bin:./bin"

export ANDROID_HOME=/usr/local/opt/android-sdk

if [[ $TMUX != "" && $WINDOW_TITLE != "" ]]; then
  tr $WINDOW_TITLE
fi

[[ -s ~/.awsrc ]] && source ~/.awsrc

set -o vi

# I can type, bro
unsetopt CORRECT
setopt NOCORRECT

# No seriously, I know what I am doing
unalias rm

setopt autocd
