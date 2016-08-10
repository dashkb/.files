alias vimwiki="vim ~/vimwiki/index.wiki"

alias be="bundle exec"
alias brake="be rake"
alias bspec="be rspec"

alias dotfiles="cd ~/.homesick/repos/.files/home"
alias rc="source ~/.zshrc"
alias tr="tmux rename-window"

alias z="zeus"
alias ccat="pygmentize -g"

# alias git=hub
alias g=hub
alias gxx="git add . && git commit --amend --no-edit && git push -f"
alias gyy="git add . && git commit -m 'fixup' && g rbm && git push -f"
alias fup="g add -p && g commit -m 'fixup' && g rbm"
alias hamster="g co master && g reset --hard origin/master"

alias "delete-merged-git-branches"='git branch --merged master | grep -v "master" | xargs -n 1 git branch -d'

alias fixtures="rake spec:fixture_builder:clean"
alias prepare-tests="RAILS_ENV=test rake db:schema:load && fixtures"

alias rs="spring rails server -b 0.0.0.0"
alias rs5="rs -p 5000"
alias rs4="rs -p 4000"
alias rs3="rs -p 3000"

alias fuck='$(thefuck $(fc -ln -1))'

alias json='jsonpp | pygmentize -l json'
alias ec=$EDITOR
alias ed="emacs --daemon"
alias redodb="spring stop && rake db:drop && rake db:create db:schema:load db:seed"

alias pn="apt-cache pkgnames"
alias pi="sudo apt-get install -y"

alias ls="ls --color"

alias slideshow="feh --cycle-once -D 3 -Z -."

alias vim=nvim
alias emacs="emacs -nw"
# alias emacs=edit-with-emacs

alias fs="foreman start -f Procfile.dev"
alias ft="foreman start -f Procfile.test"
alias pr="pry-remote"
alias dc="sudo mount -t ecryptfs ~/safe ~/safe -o key=passphrase,ecryptfs_cipher=aes,ecryptfs_key_bytes=16,ecryptfs_passthrough=no,ecryptfs_enable_filename_crypto=yes"
alias bcd="boot cider dev"

