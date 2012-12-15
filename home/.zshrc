#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi


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

export EDITOR=vim
export TERM=xterm-256color

. ~/.rvm/scripts/rvm

# Unset stupid keybindings
bindkey -r "jk"
bindkey -r "kj"
