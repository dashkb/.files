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
bindkey -M viins 'fd' vi-cmd-mode
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

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/kyle/code/voting/backend/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/kyle/code/voting/backend/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/kyle/code/voting/backend/node_modules/tabtab/.completions/sls.zsh ]] && . /home/kyle/code/voting/backend/node_modules/tabtab/.completions/sls.zsh