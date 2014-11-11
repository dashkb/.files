fpath=(/usr/local/share/zsh/site-functions /usr/local/share/zsh-completions $fpath)

autoload -U compinit
compinit

# Git fastness
__git_index_files () {
  _wanted files expl 'local files' _files
}

