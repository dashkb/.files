# zmodload zsh/zprof

if [[ -z "$TMUX" || $INSIDE_EMACS ]]; then
  export TERM='xterm-256color'
else
  export TERM='screen-256color'
fi

# environment
export EDITOR="nvim"
export VISUAL=${EDITOR}
export GIT_EDITOR=${EDITOR}

export PATH="$HOME/.linuxbrew/bin:$PATH"
export MANPATH="$HOME/.linuxbrew/share/man:$MANPATH"
export INFOPATH="$HOME/.linuxbrew/share/info:$INFOPATH"

export PATH="./bin:node_modules/.bin:${HOME}/bin:${PATH}"

# eval "$(shy init)"

for plugin in ~/.zplugin/*; do
  . $plugin
done

export CLICOLOR=1
export ANDROID_HOME=/usr/local/opt/android-sdk

if [[ -z $INSIDE_EMACS ]]; then
  set -o vi
  bindkey -M viins 'jk' vi-cmd-mode

  # Set shell colors
  . ~/.base16-shell/base16-default.dark.sh

fi

[[ -s ~/.awsrc ]] && source ~/.awsrc

set -o vi
bindkey -M viins 'jk' vi-cmd-mode
#
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

export FZF_DEFAULT_COMMAND='pt -g ""'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export CHEF_USER=dashkb

