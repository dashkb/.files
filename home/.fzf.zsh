# Setup fzf
# ---------
if [[ ! "$PATH" == */home/kyle/.fzf/bin* ]]; then
  export PATH="$PATH:/home/kyle/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/kyle/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/kyle/.fzf/shell/key-bindings.zsh"

