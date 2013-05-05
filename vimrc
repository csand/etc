" VIM options {{{
" =============================================================================

" First up, this isn't vi
set nocompatible

" Clear existing autocomands
au!

" Startup tweaks
filetype off

" Pathogen paths
call pathogen#infect()
call pathogen#infect('unmanaged')
call pathogen#helptags()

" Vundle
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

source ~/.vim/plugins.vim

syntax on
filetype plugin indent on
syntax enable

let mapleader="," " Cause \ is a bitch, yo
let maplocalleader="\\"

call togglebg#map("<F5>")

" Terminal Settings
if $TERM=='xterm-256color'
  set t_Co=256
endif

set title   " Makes the terminal title reflect current buffer
set ttyfast " Mark this as a fast terminal

color lucius
LuciusDark

" GUI settings
if has("gui_running")
  set guioptions=aegi
endif

" OS GUI Settings
if has("gui_macvim")
  set guifont=Source\ Code\ Pro:h12
  set linespace=2 " bump this up a little bit for looks
  set fuoptions=maxvert,maxhorz
  set shell=/usr/local/bin/zsh
elseif has("gui_win32")
  set guifont=Droid\ Sans\ Mono:h12
elseif has("gui_gtk")
  set guifont=Ubuntu\ Mono\ 12
endif

" Core VIM Settings
set hidden        " Hides buffers rather than require they be written out
set laststatus=2  " Defines when VIM draws the status line (2=always)
set modelines=0   " Never read any modelines (security weakness)
set nobackup
set noerrorbells
set noswapfile    " Swap files cause more annoyance than they're worth
set novisualbell
set nowritebackup
set autoread      " Automatically read files into the buffer if they've changed
                  " on disk. Relatively safe given the `FocusLost wall`
                  " autocommand
set shortmess=atI " Skip those annoying 'Press Enter' messages
                  " http://items.sjbach.com/319/configuring-vim-right

" Wildcard matching
set wildignore=*.swp,*.bak,*.pyc,*.class,node_modules/*,_sgbak,.DS_Store
set wildmenu
set wildmode=longest,list:longest

" Search Optimizations
set hlsearch
set ignorecase
set incsearch " Incremental search as you type
set smartcase " Use case sensitivity if search includes a capital letter

" Editor Settings

" Sets file encoding to UTF-8 for new files
if !strlen(&fileencoding)
  set fileencoding=utf-8
endif

set autoindent
set backspace=indent,eol,start " I don't use Windows much anymore, but this fixes the backspace key for it
set clipboard=unnamed          " Use the system clipboard as default register
set colorcolumn=80             " Show a line at col 80, in case you *want* to wrap
set cursorline                 " Highlights the line the cursor is presently on
set encoding=utf-8             " No reason not to write in utf-8 as a default now
set gdefault                   " Replace all occurrences in line by default
set linebreak                  " VIM wraps at `breakat` instead of last character that fits
set numberwidth=5              " Column width for line numbers, will never exceed 5 because of relnums
set relativenumber             " 7.3 introduced this; relative line numbers
set ruler                      " Makes navigating TraceBacks just a bit easier
set scrolloff=7                " Changes when VIM starts scrolling file (i.e. cursor two lines from bottom)
set shiftround
set shiftwidth=0
let &showbreak = '++'
set smarttab
set tabstop=8                  " Number of spaces a tab is displayed as
set textwidth=0                " Somehow getting set to 78, which is weird
set nowrap                     " Wrapping just looks odd on top of being a nuisance

set listchars=tab:→\ ,eol:¬,trail:·,nbsp:· " Used with `set list`

" }}}

" Plugin specific settings {{{
" =============================================================================

" Syntastic
let g:syntastic_enable_signs=1
let g:syntastic_javascript_checkers=['jshint']
let g:syntastic_javascript_jshint_conf="sub:true"

" delimitMate
let delimitMate_expand_cr=1
let delimitMate_expand_space=1
let delimitMate_balance_matchpairs=1

" SuperTab
let g:SuperTabDefaultCompletionType = "context"

" CtrlP
let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_persistent_input = -1
let g:ctrlp_custom_ignore = {}
let g:ctrlp_custom_ignore.dir = '\v\.(git|hg)\|lib\/(dojo|dijit|dgrid)$'
let g:ctrlp_custom_ignore.file = '\v\.so$'
let g:ctrlp_use_caching = 0
let g:ctrlp_show_hidden = 1

" EasyMotion highlighting
hi link EasyMotionTarget ErrorMsg
hi link EasyMotionShade Comment

" Zencoding

let g:user_zen_leader_key = '<c-e>'
let g:user_zen_settings = { 'indentation': '    ' }

" NERDTree
let NERDTreeHijackNetrw=1

" }}}

" Functions {{{
" =============================================================================

" Does what it says
function! ToggleSpellCheck ()
  if !&spell
    setlocal spell spelllang=en_ca
  else
    setlocal nospell
  endif
endfunction

imap <silent> <F4> <Esc>:call ToggleSpellCheck()<CR>i
nmap <silent> <F4> :call ToggleSpellCheck()<CR>

function! EnsureDirExists ()
  let required_dir = expand("%:h")
  if !isdirectory(required_dir)
          call mkdir(required_dir, 'p')
  endif
endfunction

function! Scratch ()
  split +e nofile
  set buftype=nofile
  bufhidden=hide
  setlocal noswapfile
endfunction

command! -nargs=0 Scratch call Scratch()

function! ToggleReadOnlyBit ()
  let fname = fnameescape(substitute(expand("%:p"), "\\", "/", "g"))
  checktime
  execute "au FileChangedShell " . fname . " :echo"
  if &readonly
    silent !chmod u-r %
  else
    silent !chmod u+r %
  endif
  checktime
  set invreadonly
  execute "au! FileChangedShell " . fname
endfunction

command! -nargs=0 ToggleReadOnly call ToggleReadOnlyBit()

function! SynStack()
  echo join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), " > ")
endfunction

nnoremap <leader>ss :call SynStack()<CR>

" Common fixes which need to be made to CSS files
function! FixCssSmell ()
  " Add newlines between rules
  %substitute/\}\(\n\n\)\@!/}/g
  " Add a space after a colon, if it isn't a pseudo-class/filter
  %substitute/:\( \|DXImage\|hover\|active\|focus\)\@!/: /gi
endfunction

command! -nargs=0 CSSfix :call FixCssSmell()<CR>

" }}}

" Commands {{{
" =============================================================================

command! -nargs=0 StripWhitespace %s/\s\+$//|let @/=''
nmap <silent> <leader>sw :StripWhitespace<CR>

command! -nargs=0 TabIndents %s/        /<tab>/
command! -nargs=0 SpaceIndents %s/<tab>/        /

" Python makes JSON formatting easy
command! -nargs=0 FormatJSON %!python -m json.tool

command! -nargs=0 JsBeautify call g:Jsbeautify()

" Re-open the current file with dos line endings
command! -nargs=0 Dos e ++ff=dos

" Show the next incident of mixed indentation
command! -nargs=0 MixedLine /^\( \+\t\|\t\+ \+\(\*\@!\)\)

" Show Ri documentation using Clam
command! -nargs=1 Ri Clam ri <args>

" }}}

" Keymaps {{{
" =============================================================================

map Y y$

inoremap <C-space> <C-x><C-o>
nnoremap <C-space> <C-x><C-o>

" De-highlight search when you're done
nnoremap <silent> <leader><space> :noh<cr>:match none<cr>:2match none<cr>:3match none<cr>

" Create a new split and switch to it
nnoremap <leader>vsp <C-w>v<C-w>l<C-w>=
nnoremap <leader>sp <C-w>s<C-w>j<C-w>=

" Switch to previous buffer
nnoremap <leader>q :b#<CR>

" Save yourself some time
inoremap jk <Esc>

" Sort CSS properties
nnoremap <leader>sortcss ?{<CR>jV/^\s*\}?$<CR>k:sort<CR>:nohlsearch<CR>

" Forgot to "sudo vim" and stuck in read-only? :w!!
cmap w!! w !sudo tee % >/dev/null

" <- -> to change buffer, ^ for a list
nmap <Left> :bp<CR>
nmap <Right> :bn<CR>
nmap <Up> :BufExplorer<CR>
nmap <leader>be :BufExplorer<CR>

nnoremap <S-h> :bp<CR>
nnoremap <S-l> :bn<CR>

" Moves the cursor back to where it started after '.'
nmap . .`[

" Toggle folds with space
nnoremap <space> za
vnoremap <space> za

" Quick new line
inoremap <C-cr> <esc>A:<cr>
inoremap <S-cr> <esc>A<cr>

" Never use it anyways
nnoremap H 0
nnoremap L $

" Just put me at the end of the line
inoremap <C-e> <ESC>A

" Edit vimrc
nmap <leader><leader>rc :e ~/.vimrc<CR>

" Who hates F1?
function! HateF1()
  if has("gui_macvim")
    if &fu
      set nofu
    else
      set fu
    endif
  endif
endfunction

nnoremap <F1> call HateF1()<CR>
inoremap <F1> <ESC>call HateF1()<CR>

" Mapping to make movements operate on 1 screen line in wrap mode
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

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>? :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Toggle "keep current line centered" mode
nnoremap <leader>C :let &scrolloff=999-&scrolloff<cr>

" Fuck off, shift+k
nnoremap K <nop>

" }}}

" Autocommands {{{
" =============================================================================

" editor conveniences {{{
augroup editor_conveniences
  au!

  " Write all files when GVIM loses focus
  au FocusLost * :silent! wall

  " Only show cursorline in current window
  au WinEnter * setl cursorline
  au WinLeave * setl nocursorline

  " My vimrc is set up for the marker fold method
  au BufRead .vimrc setl foldmethod=marker

  " Strip whitespace on write
  au BufWritePre * :silent! StripWhitespace

  " Make all windows equal size on Gvim window resize
  au VimResized * exe "normal \<c-w>="

  " Create parent directories for file if they don't exist when writing
  au BufWritePre * :call EnsureDirExists()

augroup END
" }}}

" filetype_settings {{{
augroup filetype_settings
  au!
  au FileType coffee     setl sw=2 ts=2 et
  au FileType css        setl ts=4 sw=4 et
  au FileType css        setl omnifunc=csscomplete#CompleteCSS
  au FileType cucumber   setl ts=2 sw=2 et
  au FileType eruby      setl ts=2 sw=2 et
  au FileType haskell    setl et
  au FileType html       setl ts=4 sw=4 et
  au FileType html       setl omnifunc=htmlcomplete#CompleteTags
  au FileType htmldjango setl ts=4 sw=4 et
  au FileType javascript setl foldmethod=syntax omnifunc=javascriptcomplete#CompleteJS
  au FileType javascript setl ts=4 sw=4 et
  au FileType less       setl ts=4 sw=4 et
  au FileType python     setl et omnifunc=pythoncomplete#Complete
  au FileType python     setl ts=4 sw=4 et
  au FileType qf         setl nolist nocursorline nowrap colorcolumn=0
  au FileType ruby       setl ts=2 sw=2 et foldmethod=syntax
  au FileType scss       setl ts=2 sw=2 et
  au FileType stylus     setl sw=4 ts=4 et
  au FileType vim        setl sw=2 ts=2 et
  au FileType zsh        setl sw=2 ts=2 et
augroup END
" }}}

" undetected_filetypes {{{
augroup undetected_filetypes
  au!
  au BufNewFile,BufRead *.coffee setl ft=coffee foldmethod=indent nofoldenable
  au BufNewFile,BufRead *.jade   setl ft=jade   foldmethod=indent nofoldenable
  au BufNewFile,BufRead *.json   setl ft=json
  au BufNewFile,BufRead *.pp     setl ft=puppet
  au BufNewFile,BufRead *.md     setl ft=markdown
augroup END
" }}}

" markdown {{{
augroup markdown
  au!
  au Filetype markdown syntax region markdownFold start="^\z(#\+\) " end="\(^#\(\z1#*\)\@!#*[^#]\)\@=" transparent fold
  au FileType markdown syn sync fromstart
  au FileType markdown set foldmethod=syntax
augroup END
" }}}

" }}}
