fpath=(/home/kyle/.linuxbrew/share/zsh-completions /home/kyle/.zsh/completions $fpath)

autoload -zU compinit && compinit

# Git fastness
__git_index_files () {
  _wanted files expl 'local files' _files
}

