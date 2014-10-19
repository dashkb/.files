" Modeline and Notes {
"   vim: set foldmarker={,} foldlevel=0 spell:
"
"   This is my personal .vimrc, I don't recommend you copy it, just
"   use the "   pieces you want(and understand!).  When you copy a
"   .vimrc in its entirety, weird and unexpected things can happen.
"
"   If you find an obvious mistake hit me up at:
"   http://robertmelton.com/contact (many forms of communication)
" }

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
    let mapleader = ","
" }

" Vundle {
    filetype off
    set rtp+=~/.vim/vundle/
    call vundle#rc()

    Bundle 'kien/ctrlp.vim'
    Bundle 'scrooloose/nerdcommenter'
    Bundle 'bling/vim-airline'
    Bundle 'myusuf3/numbers.vim'
    Bundle 'chriskempson/base16-vim'
    Bundle 'rking/ag.vim'
    Bundle 'jeetsukumaran/vim-buffergator'
    Bundle 'vim-scripts/vimwiki'
    Bundle 'kchmck/vim-coffee-script'
    Bundle 'mattn/webapi-vim'
    Bundle 'mattn/gist-vim'
    Bundle 'tpope/vim-fugitive'
    Bundle 'vim-scripts/Align'
    Bundle 'nono/vim-handlebars'
    Bundle 'digitaltoad/vim-jade'
    Bundle 'tpope/vim-haml'
    Bundle 'itspriddle/vim-marked'
    Bundle 'tpope/vim-markdown'
    Bundle 'jtratner/vim-flavored-markdown'
    Bundle 'jnwhiteh/vim-golang'
    Bundle 'jayferd/eco.vim'
    Bundle 'vim-ruby/vim-ruby'
    Bundle 'tpope/gem-browse'
    Bundle 'tpope/vim-bundler'
    Bundle 'tpope/vim-rake'
    Bundle 'majutsushi/tagbar'
    Bundle 'vim-scripts/SyntaxRange'
    Bundle 'slim-template/vim-slim'
    Bundle 'groenewege/vim-less'
    Bundle 'ekalinin/Dockerfile.vim'
    Bundle "Matt-Deacalion/vim-systemd-syntax"
    Bundle 'SirVer/ultisnips'
    Bundle 'honza/vim-snippets'
    Bundle 'wlangstroth/vim-racket'
    Bundle 'jpalardy/vim-slime'
    Bundle 'vim-scripts/paredit.vim'
    Bundle 'edkolev/tmuxline.vim'

    filetype plugin indent on " load filetype plugins/indent settings
    let g:airline_powerline_fonts = 1
    au BufNewFile,BufRead *.hamlc set ft=haml
    au BufRead,BufNewFile *.service set filetype=systemd
    au BufRead,BufNewFile *.rkt setl isk-=^
    au BufRead,BufNewFile *.rkt setl isk+=/
    set lispwords+=get,put,post,patch,delete
" }

" General {
    "set noautochdir " always switch to the current file directory
    set backspace=indent,eol,start " make backspace a more flexible
    set nobackup " make backup files
    "set backupdir=~/.vim/backup " where to put backup files
    set clipboard+=unnamed " share windows clipboard
    set directory=~/.vim/swap " directory to place swap files in
    set fileformats=unix,dos,mac " support all three, in this order
    set hidden " you can change buffers without saving
    "set mouse=a " use mouse everywhere
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

    " Split pane movement and management
    nmap <Leader>o :only<CR>
    nmap <Leader>s :split<CR>
    nmap <Leader>v :vsplit<CR>

    inoremap jk <C-C>:stopi<CR>
" }

" Vim UI {
    let base16colorspace=256
    colorscheme base16-railscasts
    set background=dark
    set cursorcolumn " highlight the current column
    set cursorline " highlight current line
    set incsearch " BUT do highlight as you type you
                   " search phrase
    set laststatus=2 " always show the status line
    set lazyredraw " do not redraw while running macros
    set linespace=0 " don't insert any extra pixel lines
                     " betweens rows
    set list " we do what to show tabs, to ensure we get them
              " out of my files
    set listchars=tab:>-,trail:- " show tabs and trailing
    set matchtime=0 " how many tenths of a second to blink
                     " matching brackets for
    set nostartofline " leave my cursor where it was
    set novisualbell " don't blink
    set number " turn on line numbers
    set numberwidth=5 " We are good up to 99999 lines
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
    set completeopt= " don't use a pop up menu for completions
    set expandtab " no real tabs please!
    set formatoptions=rq " Automatically insert comment leader on return,
                          " and let gq format comments
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

    fun! OmgFold(lnum)
      return empty(getline(a:lnum))?'-1':indent(a:lnum)/2
    endfun

    set foldexpr=OmgFold(v:lnum)
    set fdm=expr

" }

" Plugin Settings {
  let b:match_ignorecase = 1 " case is stupid

  " Tagbar Settings
  nmap <Leader>c :TagbarOpenAutoClose<CR>

  " PowerLine Settings {
    let g:Powerline_symbols = 'unicode'
  " }

  let g:gist_clip_command = 'pbcopy'
  let g:gist_post_private = 1

  " Refresh ctrlp
  nmap <Leader>cp :CtrlPClearAllCaches<CR>
  let g:ctrlp_cmd = 'CtrlPMixed'
  let g:ctrlp_show_hidden = 1
  let g:ctrlp_custom_ignore = {
        \  'dir': '\v[\/]node_modules$'
        \  }

  let g:slime_target = 'tmux'
  nmap <Leader>spec :execute ':SlimeSend1 z spec ' . @% . ':' . line(".")<CR>
  nmap <Leader>!! :execute ':SlimeSend1 !!'<CR>
  nmap <Leader>par :call PareditToggle()<CR>
" }

" Coffeescript tags {
  let g:tagbar_type_coffee = {
      \ 'ctagstype' : 'coffee',
      \ 'kinds'     : [
          \ 'c:classes',
          \ 'm:methods',
          \ 'f:functions',
          \ 'v:variables',
          \ 'u:suite',
          \ 's:specs'
      \ ]
  \ }

  " Posix regular expressions for matching interesting items. Since this will
  " be passed as an environment variable, no whitespace can exist in the options
  " so [:space:] is used instead of normal whitespaces.
  " Adapted from: https://gist.github.com/2901844
  let s:ctags_opts = '
    \ --langdef=coffee
    \ --langmap=coffee:.coffee
    \ --regex-coffee=/(^|=[[:space:]])*class[[:space:]]([A-Za-z]+\.)*([A-Za-z]+)([[:space:]]extends[[:space:]][A-Za-z.]+)?$/\3/c,class/
    \ --regex-coffee=/^[[:space:]]*(module\.)?(exports\.)?@?([A-Za-z.]+):.*[-=]>.*$/\3/m,method/
    \ --regex-coffee=/^[[:space:]]*(module\.)?(exports\.)?([A-Za-z.]+)[[:space:]]+=.*[-=]>.*$/\3/f,function/
    \ --regex-coffee=/^[[:space:]]*([A-Za-z.]+)[[:space:]]+=[^->\n]*$/\1/v,variable/,
    \ --regex-coffee=/^[[:space:]]*it[[:space:][:punct:]]+([^[:punct:]]+).+$/\1/s,spec/'

  let $CTAGS = substitute(s:ctags_opts, '\v\([nst]\)', '\\', 'g')
" }

