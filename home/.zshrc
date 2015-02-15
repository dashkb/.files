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

# Set shell colors
. ~/.base16-shell/base16-default.dark.sh

# environment
export EDITOR="vim"
export VISUAL=${EDITOR}
export GIT_EDITOR=${EDITOR}

export PATH="${HOME}/.rvm/bin:./node_modules/.bin:./bin:${HOME}/bin:${PATH}"
eval "$(shy init)"

for plugin in ~/.zplugin/*; do
  shy load $plugin
done

export CLICOLOR=1
export ANDROID_HOME=/usr/local/opt/android-sdk

if [[ $TMUX != "" && $WINDOW_TITLE != "" ]]; then
  tr $WINDOW_TITLE
fi

[[ -s ~/.awsrc ]] && source ~/.awsrc

set -o vi

# I can type, bro
unsetopt CORRECT
setopt NOCORRECT

# So I can send !! from vim
export HISTFILE=~/.zhistory
export SAVEHIST=10000
export HISTSIZE=10000
setopt NO_HIST_VERIFY
setopt SHARE_HISTORY

setopt autocd

bindkey '^v' down-line-or-history
bindkey '^b' up-line-or-history
bindkey -M 'viins' 'jk' vi-cmd-mode

