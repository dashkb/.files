alias vim="mvim -v"

alias be="bundle exec"
alias brake="be rake"
alias bspec="be rspec"

alias dotfiles="cd ~/.homesick/repos/.files/home"
alias rc="source ~/.zshrc"
alias tr="tmux rename-window"
alias t=todo.sh

alias z="zeus"
alias ccat="pygmentize -g"

alias git=hub
alias g=git

alias "delete-merged-git-branches"='git branch --merged master | grep -v "master" | xargs -n 1 git branch -d'

alias sk="z r script/sidekiq.rb -q default,1 -q mailer,1 -c 1"
