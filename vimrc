" Startup {{{

" VIM, not vi
set nocompatible

" Fetch NeoBundle before anything else if it's not installed
if !filereadable(expand('~/.vim/bundle/neobundle.vim/README.md'))
  echo 'Installing NeoBundle...'
  silent !mkdir -p ~/.vim/bundle/neobundle.vim
  silent !git clone https://github.com/Shougo/neobundle.vim $HOME/.vim/bundle/neobundle.vim
endif

if has('vim_starting')
  " Disable filetype detection while initializing
  filetype off

  " Runtime path modification for certain plugins
  set runtimepath+=~/.vim/bundle/neobundle.vim
  set runtimepath+=~/.vim/bundle/powerline/powerline/bindings/vim
endif

" }}}
" Plugins {{{
call neobundle#rc(expand('~/.vim/bundle'))

" Core {{{

NeoBundleFetch 'Shougo/neobundle.vim' " Lets NeoBundle manage itself
NeoBundle 'Shougo/vimproc', {
        \   'build' : {
        \     'mac' : 'make -f make_mac.mak',
        \     'unix' : 'make -f make_unix.mak',
        \   },
        \ }

" }}}
" Editor {{{

NeoBundle 'Lokaltog/vim-easymotion' " Extended motion commands
NeoBundle 'godlygeek/tabular' " Align search patterns for pretty tabular data
NeoBundle 'kana/vim-smartinput' " Matches character pairs, e.g. (), [], {}
NeoBundle 'kshenoy/vim-signature' " Shows mark symbols in the signs column
NeoBundle 'scrooloose/syntastic' " Unified syntax checking
NeoBundle 'tomtom/tcomment_vim' " Toggle comments
NeoBundle 'tpope/vim-endwise' " Ends certain language's blocks intelligently
NeoBundle 'tpope/vim-sleuth' " Guesses indentation settings
NeoBundle 'tpope/vim-surround' " Work with surrounding characters

" Autocompletion
NeoBundle 'Valloric/YouCompleteMe', {
        \   'build': {
        \     'mac': 'sh install.sh --clang-completer',
        \     'unix': 'sh install.sh --clang-completer'
        \   },
        \ }
NeoBundleLazy 'marijnh/tern_for_vim', {
        \   'autoload': {
        \     'filetypes': 'javascript'
        \   },
        \   'build': {
        \     'mac': 'npm install',
        \     'unix': 'npm install',
        \   },
        \ }

" }}}
" Syntaxes {{{

NeoBundle 'atourino/jinja.vim'
NeoBundle 'dag/vim2hs'
NeoBundle 'guns/vim-clojure-static'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'hdima/python-syntax'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'mitsuhiko/vim-jinja'
NeoBundle 'othree/html5.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'tpope/vim-git'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'wavded/vim-stylus'

" }}}
" Colorschemes {{{

NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'jonathanfilip/vim-lucius'
NeoBundle 'tomasr/molokai'

" }}}
" Extras {{{

NeoBundle 'ervandew/supertab' " Gives <Tab> superpowers
NeoBundle 'bling/vim-airline' " Powerline-a-like, Vimscript only
NeoBundle 'bling/vim-bufferline' " List buffers in the statusline
NeoBundle 'h1mesuke/unite-outline' " Creates a file outline Unite source
NeoBundle 'jmcantrell/vim-virtualenv' " Make vim virtualenv aware
NeoBundle 'juanpabloaj/help.vim' " Eases help navigation
NeoBundle 'kien/rainbow_parentheses.vim' " Colour matching parentheses
NeoBundle 'mattn/emmet-vim' " Eases HTML creation
NeoBundle 'Shougo/unite.vim' " Unifies, completely
NeoBundle 'Shougo/unite-help' " Adds help source for Unite
NeoBundle 'tpope/vim-abolish' " Abolish typos by making them abbrevs
NeoBundle 'tpope/vim-dispatch' " Dispatches compiler commands
NeoBundle 'tpope/vim-eunuch' " Adds UNIX integration
NeoBundle 'tpope/vim-fireplace' " Adds a quasi REPL
NeoBundle 'tpope/vim-fugitive' " The best Git plugin
NeoBundle 'tpope/vim-repeat' " Repeat commands added by plugins
NeoBundle 'ujihisa/unite-colorscheme' " Lists available colorschemes in Unite

" Text objects
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-textobj-fold'
NeoBundle 'kana/vim-textobj-indent'

" }}}

NeoBundleCheck
" }}}
" Settings {{{
" Core {{{

if $TERM == 'xterm-256color'
  set t_Co=256
endif

let mapleader = ','
let maplocalleader = ' '

set title
set ttyfast
set laststatus=2 " always show the statusline
set noswapfile
set nobackup
set nowritebackup
set noerrorbells
set novisualbell
set modelines=0
set hidden
set shortmess=at " skip all 'Press Enter' messages
set shortmess+=I " skip the intro page
set mouse=n " only listen to the mouse in normal mode

" }}}
" Editor {{{

" Set encoding to UTF-8 for new files
if !strlen(&fileencoding)
  set fileencoding=utf-8
endif

set colorcolumn=80
set cursorline
set relativenumber
set numberwidth=5
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

" Quick little chord for exiting insert mode
inoremap jk <Esc>

" Edit vimrc, vimlocal
nnoremap <Leader><Leader>rc :e ~/.vimrc<CR>
nnoremap <Leader><Leader>lc :e ~/.vimlocal<CR>

" The worst default key mapping in the world
nmap K <Nop>

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
au BufWritePre * :silent! call StripWhitespace()

" Ensure parent directories exist before write, useful for new files
function! EnsureDirExists ()
  let required_dir = expand("%:h")
  if !isdirectory(required_dir)
    call mkdir(required_dir, 'p')
  endif
endfunction
au BufWritePre * :silent! call EnsureDirExists()

" Undetected filetypes
au BufNewFile,BufRead Vagrantfile setl filetype=ruby
au BufNewFile,BufRead .vimlocal setl filetype=vim

" Filetype settings
au Filetype html setl sw=2
au Filetype coffee setl et

" }}}
" Plugin settings {{{
" Abolish {{{

Abolish funciton function
Abolish dbeugger debugger
Abolish tableay tableau

" }}}
" airline {{{

let g:airline_powerline_fonts = 1
let g:airline_theme = 'solarized'

" }}}
" Powerline {{{

let g:powerline_config_overrides = {}
let g:powerline_config_overrides.ext = {}
let g:powerline_config_overrides.ext.vim = {'colorscheme': 'solarized'}

" }}}
" python-syntax {{{

let python_version_2 = 1

" }}}
" Syntastic {{{

let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_javascript_checkers = ['jshint']

" }}}
" Unite {{{

let g:unite_source_grep_max_candidates = 200
let s:file_rec_ignore = '\.\%(gif\|png\|jpg\|jpeg\|ico\)$'
call unite#custom#source('file_rec', 'ignore_pattern', s:file_rec_ignore)
let g:unite_enable_ignore_case = 1

" Use ag or ack for Unite's grep if available
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nocolor --nogroup --hidden -i'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack-grep')
  let g:unite_source_rec_async_command = 'ack-grep -f --no-filter'
  let g:unite_source_grep_command = 'ack-grep'
  let g:unite_source_grep_default_opts = '--no-heading --no-color -a -i'
  let g:unite_source_grep_recursive_opt = ''
endif

nnoremap <C-p> :Unite -start-insert file_rec/async<CR>
nnoremap <Leader>b :Unite -start-insert buffer<CR>
nnoremap <Leader>/ :Unite -auto-preview grep:.<CR>
nnoremap <Leader>h :Unite help<CR>
nnoremap <LocalLeader><LocalLeader> :Unite -quick-match buffer<CR>
nnoremap <LocalLeader>o :Unite outline<CR>

" }}}
" VimFiler {{{

" let g:vimfiler_as_default_explorer = 1
" let g:vimfiler_force_overwrite_statusline = 0
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_marked_file_icon = '✓'
let g:vimfiler_safe_mode_by_default = 0

" }}}
" }}}
" Finally {{{

" Enable syntax, filetype detection, plugins, and indentation settings
filetype on
filetype plugin indent on

" Don't highlight searches just because this is being sourced
nohlsearch

" Source the local settings file if it exists
if filereadable(expand('~/.vimlocal'))
  source ~/.vimlocal
endif

" }}}
