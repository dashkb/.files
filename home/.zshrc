if [[ $TMUX != "" ]]; then
  # Snag current tmux window title if any
  WINDOW_TITLE=$(tmux list-window -F "#{window_name}#{window_active}" | sed '/0$/d' | sed 's/.$//')
  if [[ $WINDOW_TITLE == 'reattach-to-user-namespace' || $WINDOW_TITLE == 'tmux-default-command' ]]; then
    WINDOW_TITLE=$(pwd)
  fi
  export TERM='xterm-256color'
else
  export TERM='screen-256color'
fi

# Set shell colors
. ~/.base16-shell/base16-default.dark.sh

# environment
export EDITOR="nvim"
export VISUAL=${EDITOR}
export GIT_EDITOR=${EDITOR}

export PATH="${HOME}/.rvm/bin:./bin:node_modules/.bin:${HOME}/bin:/Applications/Racket v6.2.1/bin:${PATH}"
eval "$(shy init)"

export PATH="$HOME/.linuxbrew/bin:$PATH"
export MANPATH="$HOME/.linuxbrew/share/man:$MANPATH"
export INFOPATH="$HOME/.linuxbrew/share/info:$INFOPATH"

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

bindkey '^p' up-line-or-history
bindkey '^n' down-line-or-history

if [[ -n ${EMACS} ]]; then
  zstyle ':prezto:module:terminal' auto-title 'no'
fi

export NVM_DIR="/home/kyle/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

export FZF_DEFAULT_COMMAND='pt -g ""'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
