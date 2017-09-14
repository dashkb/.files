" Basics {
  set enc=utf-8
  set nocompatible " explicitly get out of vi-compatible mode
  set exrc " don't use local version of .(g)vimrc, .exrc
  set cpoptions=aABceFsmq
  "             |||||||||
  "             ||||||||+-- When joining lines, leave the cursor
  "             |||||||      between joined lines
  "             |||||||+-- When a new match is created (showmatch)
  "             ||||||      pause for .5
  "             ||||||+-- Set buffer options when entering the
  "             |||||      buffer
  "             |||||+-- :write command updates current file name
  "             ||||+-- Automatically add <CR> to the last line
  "             |||      when using :@r
  "             |||+-- Searching continues at the end of the match
  "             ||      at the cursor position
  "             ||+-- A backslash has no special meaning in mappings
  "             |+-- :write updates alternative file name
  "             +-- :read updates alternative file name
  syntax on " syntax highlighting on
  let mapleader = " "
" }

" Plugins {
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall
  endif
  call plug#begin()

  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-repeat'
  Plug 'mattn/webapi-vim'
  Plug 'mattn/gist-vim'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-scripts/Align'
  Plug 'vim-ruby/vim-ruby'
  Plug 'vim-scripts/SyntaxRange'
  Plug 'wlangstroth/vim-racket'
  Plug 'edkolev/tmuxline.vim'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'tpope/vim-dispatch'
  Plug 'tpope/vim-vinegar'
  Plug 'tpope/vim-surround'
  Plug 'vimwiki/vimwiki'
  Plug 'schickling/vim-bufonly'
  Plug 'luochen1990/rainbow'
  Plug 'nelstrom/vim-qargs'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'Shougo/deoplete.nvim'
  Plug 'Shougo/neosnippet.vim'
  Plug 'Shougo/neosnippet-snippets'
  Plug 'honza/vim-snippets'
  Plug 'benmills/vimux'
  Plug 'skalnik/vim-vroom'
  Plug 'cazador481/fakeclip.neovim'
  Plug 'airblade/vim-rooter'
  Plug 'elzr/vim-json'
  Plug 'chriskempson/base16-vim'
  Plug 'nazo/pt.vim'
  Plug 'neovim/node-host', { 'do': 'npm install -g neovim' }
  " Plug 'snoe/nvim-parinfer.js'
  Plug 'kchmck/vim-coffee-script'
  Plug 'powerman/vim-plugin-AnsiEsc'
  Plug 'Raimondi/delimitMate'
  Plug 'tpope/vim-markdown'
  Plug 'pangloss/vim-javascript'
  Plug 'isRuslan/vim-es6'
  Plug 'w0rp/ale'
  Plug 'godlygeek/tabular'
  Plug 'gabrielelana/vim-markdown'
  call plug#end()

  let g:airline_powerline_fonts = 1
  let g:airline_section_b = ''

  au BufEnter * set mouse=
  au BufRead,BufNewFile *.service set ft=systemd

  au FileType racket setl isk-=^
  au FileType racket setl isk+=/
  au FileType racket set commentstring=;%s

  set lispwords+=get,put,post,patch,delete,class,class*
  set lispwords+=define-for-syntax,define-syntax-rule
  set lispwords+=for/syntax,thunk,place,define-syntax-parameter,syntax-parse
  set lispwords+=for/list
" }

" General {
  set noswapfile
  set backspace=indent,eol,start " make backspace a more flexible
  set nobackup " make backup files
  set clipboard+=unnamed " share windows clipboard
  set fileformats=unix,dos,mac " support all three, in this order
  set hidden " you can change buffers without saving
  set noerrorbells " don't make noise
  set whichwrap=b,s,h,l,<,>,~,[,] " everything wraps
  "             | | | | | | | | |
  "             | | | | | | | | +-- "]" Insert and Replace
  "             | | | | | | | +-- "[" Insert and Replace
  "             | | | | | | +-- "~" Normal
  "             | | | | | +-- <Right> Normal and Visual
  "             | | | | +-- <Left> Normal and Visual
  "             | | | +-- "l" Normal and Visual (not recommended)
  "             | | +-- "h" Normal and Visual (not recommended)
  "             | +-- <Space> Normal and Visual
  "             +-- <BS> Normal and Visual
  set wildmenu " turn on command line completion wild style
  " ignore these list file extensions
  set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,
                  \*.jpg,*.gif,*.png
  set wildmode=list:longest " turn on wild mode huge list
  set autoindent

  nmap <Leader>wo :only<CR>

  inoremap jk <C-C>:stopi<CR>
  inoremap Jk <C-C>:stopi<CR>
  inoremap JK <C-C>:stopi<CR>

  " Kill trailing whitespace
  nmap <Leader>ktw :%s/\s\+$<CR>
  " Vim reload
  nmap <Leader>vr :silent so ~/.vimrc<CR>

  nmap <Leader>ff :FZF!<CR>
  nmap <Leader>fb :History<CR>
  nmap <Leader>bb :bprevious<CR>
  nmap <Leader>bn :bnext<CR>

  nmap <Leader>sc :nohl<CR>

  " If text is selected, save it in the v buffer and send that buffer it to tmux
  vmap <Leader>e "vy :call VimuxSlime()<CR>
  nmap <Leader>eb gg"vyG'' :call VimuxSlime()<CR>
  nmap <Leader>el "vyy :call VimuxSlime()<CR>

  nmap <Leader>rc :e ~/.vimrc<CR>
  nmap <Leader>pry :call OpenPry()<CR>
  nmap <Leader>railsc :call OpenRailsConsole()<CR>

  " Window stuff
  nmap <Leader>x :bd<CR>
  nmap <Leader>w- :split<CR>
  nmap <Leader>w/ :vsplit<CR>

  " Toggles
  nmap <Leader>tn :set number!<CR>
  nmap <Leader>tcc :set cursorcolumn!<CR>

  " Quickfix
  nmap <Leader>cn :cnext<CR>
  nmap <Leader>cx :cclose<CR>
  nmap <Leader>co :copen<CR>

  nmap <Leader>sf :w<CR>

  " function! SwapParinferMode()
  "   if g:parinfer_mode ==? 'indent'
  "     let g:parinfer_mode = 'paren'
  "   elseif g:parinfer_mode ==? 'paren'
  "     let g:parinfer_mode = 'indent'
  "   endif
  " endfunction

  " nmap <Leader>pm :call SwapParinferMode()<CR>

  " imap <C-k>     <Plug>(neosnippet_expand_or_jump)
  " smap <C-k>     <Plug>(neosnippet_expand_or_jump)
  " xmap <C-k>     <Plug>(neosnippet_expand_target)

  " imap <M-m> <C-v>u03A9

  smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" }

" Vim UI {
  let g:base16_shell_path='~/.base16-shell'
  let base16colorspace=256
  colorscheme base16-default-dark
  let g:airline_theme = 'base16'
  set background=dark
  set cursorcolumn " highlight the current column
  set cursorline " highlight current line
  set incsearch " BUT do highlight as you type you
                  " search phrase
  set laststatus=2 " always show the status line
  set lazyredraw " do not redraw while running macros
  set linespace=2 " don't insert any extra pixel lines
                    " betweens rows
  set list " we do what to show tabs, to ensure we get them
            " out of my files
  set listchars=tab:>-,trail:- " show tabs and trailing
  set matchtime=0 " how many tenths of a second to blink
                    " matching brackets for
  set nostartofline " leave my cursor where it was
  set novisualbell " don't blink
  set numberwidth=1 " We are good up to 99999 lines
  set report=0 " tell us when anything is changed via :...
  set ruler " Always show current positions along the bottom
  set scrolloff=20 " Keep 10 lines (top/bottom) for scope
  set shortmess=aOstT " shortens messages to avoid
                        " 'press a key' prompt
  set showcmd " show the command being typed
  set showmatch " show matching brackets
  set sidescrolloff=10 " Keep 5 lines at the size
  set wmh=0 " Set the minimum window height to 0 (hides split panes)
" }

" Text Formatting/Layout {
  set expandtab " no real tabs please!
  set formatoptions=rq " Automatically insert comment leader on return,
                        " and let gq format comments
  set formatoptions-=o
  set ignorecase " case insensitive by default
  set infercase " case inferred by default
  set nowrap " do not wrap line
  set shiftround " when at 3 spaces, and I hit > ... go to 4, not 5
  set smartcase " if there are caps, go case-sensitive
  set shiftwidth=2 " auto-indent amount when using cindent,
                    " >>, << and stuff like that
  set softtabstop=2 " when hitting tab or backspace, how many spaces
                      "should a tab be (see expandtab)
  set tabstop=8 " real tabs should be 8, and they will show with
                  " set list on

  " fun! OmgFold(lnum)
  "   return empty(getline(a:lnum))?'-1':indent(a:lnum)/2
  " endfun

  " set foldexpr=OmgFold(v:lnum)
  " set fdm=syntax
  " au BufWinEnter * let &foldlevel = max(map(range(1, line('$')), 'foldlevel(v:val)'))
  " au BufWinEnter *.rb,Gemfile,Berksfile,*.coffee setlocal fdm=indent
" }

" Plug Settings {
  let b:match_ignorecase = 1 " case is stupid

  " Tagbar Settings

  " PowerLine Settings {
    let g:Powerline_symbols = 'unicode'
  " }

  let g:gist_clip_command = 'pbcopy'
  let g:gist_post_private = 1

  function! ColorOf(thing)
    let color = synIDattr(synIDtrans(hlID(a:thing)), 'fg')
    let val = [color, color]
    return val
  endfunction

  let g:rainbow_active = 1

  let g:deoplete#enable_at_startup = 1
  let g:deoplete#delimiters = ['/', '.', '::', ':', '#' ]
  let g:neosnippet#enable_snipmate_compatibility = 1
  let g:neosnippet#snippets_director = '~/.vim/snippets'

  " For conceal markers.
  if has('conceal')
    set conceallevel=2 concealcursor=niv
  endif

  let g:vroom_use_vimux = 1
  let g:vroom_use_bundle_exec = 0

  function! VimuxSlime()
    call VimuxOpenRunner()
    call VimuxSendText(@v)
    call VimuxSendKeys("Enter")
  endfun

  function! OpenPry()
    let @v = "pry"
    call VimuxSlime()
  endfun

  function! OpenRailsConsole()
    let @v = "rails c"
    call VimuxSlime()
  endfun

  let vim_markdown_preview_github=1

  Plug 'prettier/vim-prettier', {
    \ 'do': 'yarn install',
      \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql']
      \ }
  " }
