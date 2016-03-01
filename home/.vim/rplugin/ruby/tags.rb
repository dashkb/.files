Neovim.plugin do |plug|
  plug.autocmd(:BufEnter, :pattern => "*.rb") do |nvim|
    nvim.command("echom 'Ruby file, eh?'")
  end
end

