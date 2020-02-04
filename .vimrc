" Startup {{{

" Fetch Vundle before anything else if it's not installed
if !filereadable(expand('~/.vim/bundle/Vundle.vim/README.md'))
  echo 'Installing Vundle...'
  silent !mkdir -p ~/.vim/bundle/Vundle.vim
  silent !git clone https://github.com/gmarik/Vundle.vim $HOME/.vim/bundle/Vundle.vim
endif

if has('vim_starting')
  " Disable filetype detection while initializing
  filetype off

  " Runtime path modification for certain plugins
  set runtimepath+=~/.vim/bundle/Vundle.vim
endif

" }}}
" Plugins {{{
call vundle#rc(expand('~/.vim/bundle'))

" Core {{{

Plugin 'gmarik/Vundle.vim' " Let Vundle manage itself

" }}}
" Editor {{{

Plugin 'Lokaltog/vim-easymotion' " Extended motion commands
Plugin 'godlygeek/tabular' " Align search patterns for pretty tabular data
Plugin 'kana/vim-smartinput' " Matches character pairs, e.g. (), [], {}
Plugin 'kshenoy/vim-signature' " Shows mark symbols in the signs column
Plugin 'scrooloose/syntastic' " Unified syntax checking
Plugin 'tpope/vim-endwise' " Ends certain language's blocks intelligently
Plugin 'tpope/vim-sleuth' " Guesses indentation settings
Plugin 'tpope/vim-surround' " Work with surrounding characters
Plugin 'tpope/vim-commentary' " Comment toggling
Plugin 'chrisbra/vim-diff-enhanced' " Better diffing with patience alg

" Autocompletion
if ! has('gui_running')
  " MacVim is terrible
  " Plugin 'Valloric/YouCompleteMe'
endif
" Plugin 'marijnh/tern_for_vim'

" }}}
" Syntaxes {{{

Plugin 'dag/vim2hs'
Plugin 'digitaltoad/vim-jade'
Plugin 'guns/vim-clojure-static'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'hdima/python-syntax'
Plugin 'kchmck/vim-coffee-script'
Plugin 'mitsuhiko/vim-jinja'
Plugin 'othree/html5.vim'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-markdown'
Plugin 'vim-ruby/vim-ruby'
Plugin 'wavded/vim-stylus'
Plugin 'wting/rust.vim'
Plugin 'applescript.vim'
Plugin 'dag/vim-fish'
Plugin 'groenewege/vim-less'
Plugin 'groovy.vim'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'nginx.vim'
Plugin 'saltstack/salt-vim'
Plugin 'b4winckler/vim-objc'
Plugin 'Keithbsmiley/swift.vim'
Plugin 'othree/yajs.vim'
Plugin 'joukevandermaas/vim-ember-hbs'
Plugin 'cakebaker/scss-syntax.vim'

" }}}
" Colorschemes {{{

" Plugin 'godlygeek/csapprox'
Plugin 'altercation/vim-colors-solarized'
Plugin 'chriskempson/base16-vim'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'tomasr/molokai'
Plugin 'jnurmine/Zenburn'
Plugin 'baskerville/bubblegum'
Plugin 'nanotech/jellybeans.vim'
Plugin 'moria'
Plugin 'w0ng/vim-hybrid'
Plugin 'cocopon/iceberg.vim'
Plugin 'ciaranm/inkpot'
Plugin 'zenorocha/dracula-theme', {'rtp': 'vim/'}
Plugin 'morhetz/gruvbox'
Plugin 'arcticicestudio/nord-vim'

" }}}
" Extras {{{

Plugin 'vim-airline/vim-airline' " Powerline-a-like, Vimscript only
Plugin 'vim-airline/vim-airline-themes' " Powerline-a-like, Vimscript only
Plugin 'jmcantrell/vim-virtualenv' " Make vim virtualenv aware
Plugin 'juanpabloaj/help.vim' " Eases help navigation
Plugin 'junegunn/goyo.vim' " Distraction free writing
Plugin 'ctrlpvim/ctrlp.vim' " Fuzzy file finder
Plugin 'kien/rainbow_parentheses.vim' " Colour matching parentheses
Plugin 'mattn/emmet-vim' " Eases HTML creation
Plugin 'rking/ag.vim' " Easy access to ag from vim
Plugin 'tpope/vim-abolish' " Abolish typos by making them abbrevs
Plugin 'tpope/vim-dispatch' " Dispatches compiler commands
Plugin 'tpope/vim-eunuch' " Adds UNIX integration
Plugin 'tpope/vim-fireplace' " Adds a quasi REPL
Plugin 'tpope/vim-fugitive' " The best Git plugin
Plugin 'tpope/vim-projectionist' " Easily navigate project layouts
Plugin 'tpope/vim-repeat' " Repeat commands added by plugins
Plugin 'tpope/vim-unimpaired' " Mappings for quickfix/location list nav
Plugin 'tpope/vim-vinegar' " Improvements for netrw
Plugin 'guns/xterm-color-table.vim' " Show the terminal colour table
Plugin 'editorconfig/editorconfig-vim' " Nice project wide config for editors
" Plugin 'eraserhd/vim-ios' " Utilities for developing iOS apps in vim
Plugin 'alexlafroscia/vim-ember-cli' " Ember-cli utilities for vim

" Text objects
Plugin 'kana/vim-textobj-user'
Plugin 'kana/vim-textobj-indent'

" }}}
" }}}
" Settings {{{
" Core {{{

" if $TERM == 'xterm-256color'
"   set t_Co=256
" endif

let mapleader = ','
let maplocalleader = ' '

set title
set ttyfast
set laststatus=2 " always show the statusline
set noswapfile
set modelines=0
set hidden
set shortmess=at " skip all 'Press Enter' messages
set shortmess+=I " skip the intro page
set mouse=a " listen to mouse in all modes
set autoread " read files as they change (branch switches, etc)

" }}}
" Editor {{{

" Set encoding to UTF-8 for new files
if !strlen(&fileencoding)
  set fileencoding=utf-8
endif

set colorcolumn=80
set cursorline
set relativenumber
set numberwidth=4
set linebreak
let &showbreak = '↳'
set backspace=indent,eol,start
set autoindent
set shiftround
set smarttab
set textwidth=0
set nowrap
set linespace=1 " nice for readability
set listchars=tab:→\ ,eol:↵,trail:·,nbsp:·
set gdefault
set expandtab
set completeopt=menuone
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
set iskeyword-=_
set diffopt+=vertical

" }}}
" Search {{{

set hlsearch
set incsearch
set ignorecase
set smartcase

" }}}
" Wildmenu {{{

set wildignore+=*.swp,*.pyc,*.dmg,.DS_Store
set wildignore+=*.png,*.gif,*.jpg
set wildignore+=node_modules/*
set wildignore+=bundle/*
set wildignore+=*.woff,*.woff2,*.ttf,*.eot
set wildchar=<Tab>
set wildmenu
" Expand to the longest common sequence first and list all matches
" Next press expands to first match, then second, etc
set wildmode=list:longest,full

" }}}
" GUI {{{

if has('gui_running')
  set guioptions=aegi
endif

" }}}
" Grep {{{

if executable('ag')
  let &grepprg = 'ag --nocolor --nogroup'
endif

" }}}
" }}}
" Commands {{{

command! -nargs=0 Marked :!open -a 'Marked' %

function! QuickfixFilenames()
  " Building a hash ensures we get each buffer only once
  let buffer_numbers = {}
  for quickfix_item in getqflist()
    let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
  endfor
  return join(map(values(buffer_numbers), 'fnameescape(v:val)'))
endfunction
command! -nargs=0 -bar Qargs execute 'args' QuickfixFilenames()

" }}}
" Key mappings {{{

" The worst default key mapping in the world
nnoremap K <nop>

" Quick little chord for exiting insert mode
inoremap jk <Esc>

" Edit vimrc, vimlocal
nnoremap <Leader><Leader>rc :e ~/.vimrc<CR>
nnoremap <Leader><Leader>lc :e ~/.vimlocal<CR>

" Un-highlight searches
nmap <Leader><Space> :noh<CR>

" Moves the cursor back to where it started after '.'
nmap . .`[

" Move normally over wrapped lines
function! ScreenMovement(movement)
  if &wrap
    return "g" . a:movement
  else
    return a:movement
  endif
endfunction

onoremap <silent> <expr> j ScreenMovement("j")
onoremap <silent> <expr> k ScreenMovement("k")
onoremap <silent> <expr> 0 ScreenMovement("0")
onoremap <silent> <expr> ^ ScreenMovement("^")
onoremap <silent> <expr> $ ScreenMovement("$")
nnoremap <silent> <expr> j ScreenMovement("j")
nnoremap <silent> <expr> k ScreenMovement("k")
nnoremap <silent> <expr> 0 ScreenMovement("0")
nnoremap <silent> <expr> ^ ScreenMovement("^")
nnoremap <silent> <expr> $ ScreenMovement("$")

" }}}
" Autocommands {{{

" Clear any existing autocommands when sourcing this file
autocmd!

" Write all files when GVIM loses focus
" au FocusLost * :silent! wall

" Only show cursorline in current window
au WinEnter * setlocal cursorline
au WinLeave * setlocal nocursorline

" My vimrc uses fold markers
au BufRead .vimrc setl foldmethod=marker

" Let splits remain even on resize
au VimResized * exe 'normal \<c-w>='

" Strip whitespace on write
function! StripWhitespace ()
  let old_search = @/
  keepjumps %substitute/\s\+$//
  let @/ = old_search
endfunction
" au BufWritePre * :silent! call StripWhitespace()
" No longer used, editorconfig lets this be smarter

" Ensure parent directories exist before write, useful for new files
function! EnsureDirExists ()
  let required_dir = expand("%:h")
  if !isdirectory(required_dir)
    call mkdir(required_dir, 'p')
  endif
endfunction
au BufWritePre * :silent! call EnsureDirExists()

" Undetected filetypes
au BufNewFile,BufRead Vagrantfile set ft=ruby
au BufNewFile,BufRead .vimlocal set ft=vim
au BufNewFile,BufRead *.ft set ft=markdown
au BufNewFile,BufRead *.hbs,*.handlebars set ft=html syntax=handlebars
au BufNewFile,BufRead .envrc set ft=sh
au BufNewFile,BufRead *.gradle set ft=groovy

" Filetype settings
au FileType html setl sw=2
au FileType coffee setl sw=2
au FileType yaml setl sw=2
au FileType json setl sw=4
au FileType javascript setl sw=2
au FileType rust set sw=4
au FileType json set sw=2

" }}}
" Plugin settings {{{
" Read plugins settings from .vim/config
runtime! config/**/*.vim
" }}}
" Finally {{{

" Enable syntax, filetype detection, plugins, and indentation settings
filetype on
filetype plugin indent on
syntax on

" Don't highlight searches just because this is being sourced
nohlsearch

" Source the local settings file if it exists
if filereadable(expand('~/.vimlocal'))
  source ~/.vimlocal
endif

" }}}
