alias z=zeus
alias sk="redis-cli flushall; z r script/sidekiq.rb -q default,1 -q mailer,1 -c 1"
