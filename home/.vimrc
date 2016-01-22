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
  let mapleader = " "
" }

" Plugins {
  set rtp+=~/.fzf
  call plug#begin('~/.vim/bundle')

  let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
  Plugin 'bling/vim-airline'
  Plugin 'tpope/vim-commentary'
  Plugin 'myusuf3/numbers.vim'
  Plugin 'chriskempson/base16-vim'
  Plugin 'rking/ag.vim'
  Plugin 'jeetsukumaran/vim-buffergator'
  Plugin 'kchmck/vim-coffee-script'
  Plugin 'mattn/webapi-vim'
  Plugin 'mattn/gist-vim'
  Plugin 'tpope/vim-fugitive'
  Plugin 'vim-scripts/Align'
  Plugin 'tpope/vim-haml'
  Plugin 'itspriddle/vim-marked'
  Plugin 'tpope/vim-markdown'
  Plugin 'jtratner/vim-flavored-markdown'
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'tpope/gem-browse'
  Plugin 'tpope/vim-bundler'
  Plugin 'tpope/vim-rake'
  Plugin 'tpope/vim-rails'
  Plugin 'majutsushi/tagbar'
  Plugin 'vim-scripts/SyntaxRange'
  Plugin 'ekalinin/Dockerfile.vim'
  Plugin 'Matt-Deacalion/vim-systemd-syntax'
  Plugin 'honza/vim-snippets'
  Plugin 'wlangstroth/vim-racket'
  Plugin 'jpalardy/vim-slime'
  Plugin 'edkolev/tmuxline.vim'
  Plugin 'christoomey/vim-tmux-navigator'
  Plugin 'tpope/vim-dispatch'
  Plugin 'tpope/vim-vinegar'
  Plugin 'groenewege/vim-less'
  Plugin 'lmeijvogel/vim-yaml-helper'
  Plugin 'vimwiki/vimwiki'
  Plugin 'schickling/vim-bufonly'
  Plugin 'vim-scripts/paredit.vim'
  Plugin 'vim-scripts/vim-misc'
  Plugin 'xolox/vim-lua-ftplugin'
  Plugin 'vim-scripts/lua.vim'
  Plugin 'vim-scripts/terra.vim'
  Plugin 'luochen1990/rainbow'
  Plugin 'kien/ctrlp.vim'
  Plugin 'slim-template/vim-slim'
  Plugin 'nelstrom/vim-qargs'
  Plugin 'LucHermitte/lh-vim-lib'
  Plugin 'LucHermitte/local_vimrc'
  call plug()

  let g:airline_powerline_fonts = 1
  let g:airline_section_b = ''

  nmap <leader>sm <Plug>SlimeMotionSend
  nmap <leader>sl <Plug>SlimeLineSend

  au BufNewFile,BufRead *.hamlc set ft=haml
  au BufRead,BufNewFile *.service set filetype=systemd
  au BufRead,BufNewFile *.slum set ft=lisp
  au Bufread,BufNewFile *.t set ft=terra

  au FileType racket setl isk-=^
  au FileType racket setl isk+=/
  au FileType racket set commentstring=;%s

  set lispwords+=get,put,post,patch,delete,class,class*
  set lispwords+=define-for-syntax,define-syntax-rule
  set lispwords+=for/syntax,thunk,place,define-syntax-parameter,syntax-parse

  if executable('ag')
    " Use Ag over Grep
    set grepprg=ag\ --nogroup\ --nocolor

    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  endif

  nmap <Leader>cp :CtrlPClearAllCaches<CR>
  nmap <Leader>ct :CtrlPTag<CR>
  nmap <Leader>ut :Dispatch ctags -R . >/dev/null 2>&1<CR>

  let g:ctrlp_map = ''
  nmap <Leader>ut :Dispatch ctags --exclude=.git --exclude=node_modules -R . >/dev/null 2>&1<CR>
  nmap <C-p> :FZF!<CR>

  let g:UltiSnipsExpandTrigger = '<c-s>'
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

  " Split pane movement and management
  nmap <Leader>o :only<CR>

  inoremap jk <C-C>:stopi<CR>

  " Kill trailing whitespace
  nmap <Leader>ktw :%s/\s\+$<CR>
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
  set fdm=indent
  set foldlevelstart=2
" }

" Plugin Settings {
  let b:match_ignorecase = 1 " case is stupid

  " Tagbar Settings
  nmap <Leader>tb :TagbarOpenAutoClose<CR>

  " PowerLine Settings {
    let g:Powerline_symbols = 'unicode'
  " }

  let g:gist_clip_command = 'pbcopy'
  let g:gist_post_private = 1

  let g:slime_target = 'tmux'
  nmap <Leader>zspec :execute ':SlimeSend1 z spec ' . @% . ':' . line(".")<CR>
  nmap <Leader>rspec :execute ':SlimeSend1 rspec ' . @% . ':' . line(".")<CR>
  nmap <Leader>!! :execute ':SlimeSend1 !!'<CR>
  nmap <Leader>par :call PareditToggle()<CR>

  let g:paredit_leader = "<space>"

  function! ColorOf(thing)
    let color = synIDattr(synIDtrans(hlID(a:thing)), 'fg')
    let val = [color, color]
    return val
  endfunction

  let g:rainbow_active = 1


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
