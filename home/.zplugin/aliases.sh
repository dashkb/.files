alias vimwiki="vim ~/vimwiki/index.wiki"

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
alias gxx="git add . && git commit --amend --no-edit && git push -f"
alias gyy="git add . && git commit -m 'fixup' && g rbm && git push -f"
alias fup="g add -p && g commit -m 'fixup' && g rbm"
alias hamster="g co master && g reset --hard origin/master"

alias "delete-merged-git-branches"='git branch --merged master | grep -v "master" | xargs -n 1 git branch -d'

alias clear-sidekiq="redis-cli KEYS 'resque:*' | xargs redis-cli DEL"
alias sk="z r script/sidekiq.rb -q default,1 -q mailer,1 -c 1"

alias fixtures="rake spec:fixture_builder:clean"

alias rs="spring rails server -b 0.0.0.0"
alias rs5="rs -p 5000"
alias rs4="rs -p 4000"
alias rs3="rs -p 3000"

alias fuck='$(thefuck $(fc -ln -1))'

alias json='jsonpp | pygmentize -l json'
