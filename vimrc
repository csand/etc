" Startup {{{

" First up, this isn't vi
set nocompatible

" Clear existing autocomands
au!

" Startup tweaks
filetype off

if has('vim_starting')
  set rtp+=~/.vim/bundle/neobundle.vim
end

call neobundle#rc()

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc', {
        \   'build' : {
        \     'windows' : 'make -f make_mingw32.mak',
        \     'cygwin' : 'make -f make_cygwin.mak',
        \     'mac' : 'make -f make_mac.mak',
        \     'unix' : 'make -f make_unix.mak',
        \   },
        \ }

syntax on
filetype plugin indent on
syntax enable

" }}}

" Plugins {{{

" Core improvements {{{
NeoBundle 'argtextobj.vim'
NeoBundle 'juanpabloaj/help.vim'
NeoBundle 'nelstrom/vim-visual-star-search'
NeoBundle 'Raimondi/delimitMate'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'surround.vim'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'Valloric/YouCompleteMe', {
        \   'build': {
        \     'mac': './install.sh --clang-completer'
        \   },
        \ }
" }}}

" Additional functionality {{{
NeoBundle 'EasyMotion'
NeoBundle 'SearchComplete'
NeoBundle 'ShowMarks'
NeoBundle 'godlygeek/tabular'
NeoBundle 'thinca/vim-ref'
NeoBundle 'zhaocai/linepower.vim'
NeoBundle 'tyru/restart.vim'
NeoBundle 'hrsh7th/vim-versions'
NeoBundle 'nathanaelkane/vim-indent-guides'
" }}}

" Textobjs {{{
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-textobj-line'
NeoBundle 'kana/vim-textobj-fold'
NeoBundle 'coderifous/textobj-word-column.vim'
" }}}

" Unite plugins {{{
NeoBundle 'h1mesuke/unite-outline'
NeoBundle 'Shougo/unite-help'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'ujihisa/unite-font'
" }}}

" UI and colour schemes {{{
NeoBundle 'Lokaltog/vim-powerline'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'jonathanfilip/vim-lucius'
NeoBundle 'moria'
NeoBundle 'w0ng/vim-hybrid'
" }}}

" Core syntax improvements {{{
NeoBundle 'JSON.vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'othree/html5.vim'
NeoBundle 'python.vim--Vasiliev'
NeoBundle 'vim-ruby/vim-ruby'
" }}}

" Uncommon syntaxes, lazy-loaded {{{
NeoBundleLazy 'Haskell-Highlight-Enhanced'
NeoBundleLazy 'Puppet-Syntax-Highlighting'
NeoBundleLazy 'bryanjswift/vim-rust'
NeoBundleLazy 'cakebaker/scss-syntax.vim'
NeoBundleLazy 'depuracao/vim-rdoc'
NeoBundleLazy 'digitaltoad/vim-jade'
NeoBundleLazy 'groenewege/vim-less'
NeoBundleLazy 'jnwhiteh/vim-golang'
NeoBundleLazy 'nginx.vim'
NeoBundleLazy 'nono/vim-handlebars'
NeoBundleLazy 'pangloss/vim-javascript'
NeoBundleLazy 'wavded/vim-stylus'
NeoBundleLazy 'kchmck/vim-coffee-script'

augroup lazy_loaded_syntaxes
  au!
  au FileType coffee     NeoBundleSource vim-coffee-script
  au FileType golang     NeoBundleSource vim-golang
  au FileType handlebars NeoBundleSource vim-handlebars
  au FileType haskell    NeoBundleSource Haskell-Highlight-Enhanced
  au FileType jade       NeoBundleSource vim-jade
  au FileType javascript NeoBundleSource vim-javascript
  au FileType less       NeoBundleSource vim-less
  au FileType nginx      NeoBundleSource nginx.vim
  au FileType puppet     NeoBundleSource Puppet-Syntax-Highlighting
  au FileType rdoc       NeoBundleSource vim-rdoc
  au FileType rust       NeoBundleSource vim-rust
  au FileType scss       NeoBundleSource scss-syntax.vim
  au FileType stylus     NeoBundleSource vim-stylus
augroup END
" }}}

" tpope {{{
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-bundler'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-sleuth'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tpope/vim-unimpaired'
" }}}

" Shougo {{{
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'Shougo/vimshell.vim'
" }}}

NeoBundleCheck

"}}}

" Options {{{
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
LuciusLight

" GUI settings
if has("gui_running")
  set guioptions=aegi
endif

" OS GUI Settings
if has("gui_macvim")
  set guifont=Source\ Code\ Pro:h11
  set linespace=3 " bump this up a little bit for looks
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
set wildignore+=*.dmg
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

set iskeyword+=<,>,[,],:,`,!
set iskeyword-=_,-

" }}}

" Plugin specific settings {{{

" Syntastic
let g:syntastic_enable_signs=1
let g:syntastic_javascript_checkers=['jshint']
let g:syntastic_javascript_jshint_conf="sub:true"

" delimitMate
let delimitMate_expand_cr=1
let delimitMate_expand_space=1
let delimitMate_balance_matchpairs=1

" EasyMotion highlighting
hi link EasyMotionTarget ErrorMsg
hi link EasyMotionShade Comment

" Zencoding
let g:user_zen_leader_key = '<c-e>'
let g:user_zen_settings = { 'indentation': '    ' }

" Show Marks
let g:showmarks_enable = 0

" Unite
let g:unite_source_grep_max_candidates = 200

if executable('ag')
  " Use ag in unite grep source.
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nocolor --nogroup --hidden -i'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack-grep')
  " Use ack in unite grep source.
  let g:unite_source_grep_command = 'ack-grep'
  let g:unite_source_grep_default_opts = '--no-heading --no-color -a -i'
  let g:unite_source_grep_recursive_opt = ''
endif

" VimFiler
let g:vimfiler_as_default_explorer=1
let g:vimfiler_force_overwrite_statusline=0
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_marked_file_icon = '✓'
let g:vimfiler_safe_mode_by_default = 0

" }}}

" Functions {{{

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
nnoremap <Leader>ss :call SynStack()<CR>

" Common fixes which need to be made to CSS files
function! FixCssSmell ()
  " Add newlines between rules
  %substitute/\}\(\n\n\)\@!/}/g
  " Add a space after a colon, if it isn't a pseudo-class/filter
  %substitute/:\( \|DXImage\|hover\|active\|focus\)\@!/: /gi
endfunction
command! -nargs=0 CSSfix :call FixCssSmell()<CR>

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

" Commands {{{

command! -nargs=0 StripWhitespace %s/\s\+$//|let @/=''
nmap <silent> <Leader>sw :StripWhitespace<CR>

command! -nargs=0 TabIndents %s/        /<tab>/
command! -nargs=0 SpaceIndents %s/<tab>/        /

" Python makes JSON formatting easy
command! -nargs=0 FormatJSON %!python -m json.tool

command! -nargs=0 JsBeautify call g:Jsbeautify()

" Re-open the current file with dos line endings
command! -nargs=0 Dos e ++ff=dos

" Show the next incident of mixed indentation
command! -nargs=0 MixedLine /^\( \+\t\|\t\+ \+\(\*\@!\)\)

command! -nargs=0 Search Unite -auto-preview grep:.

command! -nargs=0 -bar Reconfig source ~/.vimrc

" }}}

" Keymaps {{{

map Y y$

inoremap <C-space> <C-x><C-o>
nnoremap <C-space> <C-x><C-o>

" De-highlight search when you're done
nnoremap <silent> <Leader><Space> :noh<cr>:match none<cr>:2match none<cr>:3match none<cr>

" Save yourself some time
inoremap jk <Esc>

" Sort CSS properties
nnoremap <Leader>sortcss ?{<CR>jV/^\s*\}?$<CR>k:sort<CR>:nohlsearch<CR>

" Forgot to "sudo vim" and stuck in read-only? :w!!
cmap w!! w !sudo tee % >/dev/null

" Moves the cursor back to where it started after '.'
nmap . .`[

" Quick new line
inoremap <C-cr> <esc>A:<cr>
inoremap <S-cr> <esc>A<cr>

" Never use it anyways
nnoremap H 0
nnoremap L $

" Just put me at the end of the line
inoremap <C-e> <ESC>A

" Edit vimrc
nmap <Leader><Leader>rc :e ~/.vimrc<CR>

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
nnoremap <silent> <Leader>? :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Toggle 'keep current line centered' mode
nnoremap <Leader>C :let &scrolloff=999-&scrolloff<cr>

" Fuck off, shift+k
nnoremap K <nop>

" Get around buffers like tabs
nnoremap gb :bn<CR>
nnoremap gB :bp<CR>

" Unite mappings
nnoremap <C-p> :Unite -start-insert -auto-preview file_rec/async<CR>
nnoremap <Space><Space> :Unite -quick-match buffer<CR>

" }}}

" Autocommands {{{

" Editor Conveniences {{{
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

  " Open VimFiler is no files given as arguments
  " au VimEnter * if !argc() | VimFiler . | endif

augroup END
" }}}

" Filetype Settings {{{
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
  au FileType htmldjango let b:delimitMate_matchpairs="(:),[:],<:>,{:},%:%"
  au FileType javascript setl ts=4 sw=4 et
  au FileType javascript setl foldmethod=syntax omnifunc=javascriptcomplete#CompleteJS
  au FileType less       setl ts=4 sw=4 et
  au FileType python     setl ts=4 sw=4 et
  au FileType python     setl omnifunc=pythoncomplete#Complete
  au FileType qf         setl nolist nocursorline nowrap colorcolumn=0
  au FileType ruby       setl ts=2 sw=2 et foldmethod=syntax
  au FileType scss       setl ts=2 sw=2 et
  au FileType stylus     setl sw=4 ts=4 et
  au FileType vim        setl sw=2 ts=2 et
  au FileType zsh        setl sw=2 ts=2 et
  au FileType yaml       setl sw=2 ts=2 et
augroup END
" }}}

" Undetected Filetypes {{{
augroup undetected_filetypes
  au!
  au BufNewFile,BufRead *.coffee setl ft=coffee foldmethod=indent nofoldenable
  au BufNewFile,BufRead *.jade   setl ft=jade   foldmethod=indent nofoldenable
  au BufNewFile,BufRead *.json   setl ft=json
  au BufNewFile,BufRead *.pp     setl ft=puppet
  au BufNewFile,BufRead *.md     setl ft=markdown
  au BufNewFile,BufRead *.hbs,*.handlebars setl ft=handlebars
  au BufNewFile,BufRead */Views/* setl ft=htmldjango
augroup END
" }}}

" Markdown {{{
augroup markdown
  au!
  au Filetype markdown syntax
    \ region markdownFold
    \ start="^\z(#\+\) "
    \ end="\(^#\(\z1#*\)\@!#*[^#]\)\@="
    \ transparent fold
  au FileType markdown syn sync fromstart
  au FileType markdown set foldmethod=syntax
augroup END
" }}}

" }}}
