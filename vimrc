" Startup {{{
" First up, this isn't vi
set nocompatible
" Clear existing autocomands
au!
" Startup tweaks
filetype off

if has('vim_starting')
  set rtp+=~/.vim/bundle/neobundle.vim
  set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
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
" VIMprovements {{{
NeoBundle 'EasyMotion'
NeoBundle 'Raimondi/delimitMate'
NeoBundle 'SearchComplete'
NeoBundle 'ShowMarks'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundleLazy 'godlygeek/tabular', {'autoload': {'commands': 'Tabularize'}}
NeoBundle 'hrsh7th/vim-versions'
NeoBundle 'juanpabloaj/help.vim'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'nelstrom/vim-visual-star-search'
NeoBundleLazy 'rking/ag.vim', {'autoload': {'commands': 'Ag'}}
NeoBundle 'surround.vim'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'thinca/vim-ref'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tyru/restart.vim'
NeoBundle 'zhaocai/linepower.vim'
NeoBundle 'Lokaltog/powerline'
" }}}
" Missing syntaxes {{{
NeoBundleLazy 'elzr/vim-json', {'autoload': {'filetypes': 'json'}}
NeoBundleLazy 'hail2u/vim-css3-syntax', {'autoload': {'filetypes': 'css'}}
NeoBundleLazy 'othree/html5.vim', {'autoload': {'filetypes': 'html'}}
NeoBundleLazy 'python.vim--Vasiliev', {'autoload': {'filetypes': 'python'}}
NeoBundleLazy 'vim-ruby/vim-ruby', {'autoload': {'filetypes': 'ruby'}}
NeoBundleLazy 'atourino/jinja.vim', {'autoload': {'filetypes': 'jinja'}}
NeoBundleLazy 'Haskell-Highlight-Enhanced', {
      \ 'autoload': {
      \   'filetypes': 'haskell'
      \ }}
NeoBundleLazy 'Puppet-Syntax-Highlighting', {
      \ 'autoload': {
      \   'filetypes': 'puppet'
      \ }}
NeoBundleLazy 'cakebaker/scss-syntax.vim', {'autoload': {'filetypes': 'scss'}}
NeoBundleLazy 'depuracao/vim-rdoc', {'autoload': {'filetypes': 'rdoc'}}
NeoBundleLazy 'digitaltoad/vim-jade', {'autoload': {'filetypes': 'jade'}}
NeoBundleLazy 'groenewege/vim-less', {'autoload': {'filetypes': 'less'}}
NeoBundleLazy 'jnwhiteh/vim-golang', {'autoload': {'filetypes': 'go'}}
NeoBundleLazy 'nginx.vim', {'autoload': {'filetypes': 'nginx'}}
NeoBundleLazy 'nono/vim-handlebars', {'autoload': {'filetypes': 'handlebars'}}
NeoBundleLazy 'pangloss/vim-javascript', {
      \ 'autoload': {
      \   'filetypes': 'javascript'
      \ }}
NeoBundleLazy 'tpope/vim-markdown', {'autoload': {'filetypes': 'markdown'}}
NeoBundleLazy 'wavded/vim-stylus', {'autoload': {'filetypes': 'stylus'}}
NeoBundleLazy 'kchmck/vim-coffee-script', {'autoload': {'filetypes': 'coffee'}}
" }}}
" Syntax checking and auto-completion {{{
NeoBundle 'scrooloose/syntastic'
NeoBundle 'Valloric/YouCompleteMe', {
        \   'build': {
        \     'mac': './install.sh --clang-completer'
        \   },
        \ }
NeoBundle 'marijnh/tern_for_vim', {
        \   'build': {
        \     'mac': 'npm install',
        \     'unix': 'npm install',
        \   },
        \ }
" }}}
" Text objects {{{
NeoBundle 'argtextobj.vim'
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-textobj-line'
NeoBundle 'kana/vim-textobj-fold'
NeoBundle 'kana/vim-textobj-indent'
NeoBundle 'coderifous/textobj-word-column.vim'
" }}}
" Unite {{{
NeoBundle 'Shougo/unite.vim'
NeoBundle 'h1mesuke/unite-outline'
NeoBundle 'Shougo/unite-help'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'ujihisa/unite-font'
" }}}
" Colour schemes {{{
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'jonathanfilip/vim-lucius'
NeoBundle 'moria'
NeoBundle 'w0ng/vim-hybrid'
NeoBundle 'tomasr/molokai'
NeoBundle 'Liquid-Carbon'
NeoBundle 'pyte'
NeoBundle 'proton'
NeoBundle 'endel/vim-github-colorscheme'
NeoBundle 'candy.vim'
NeoBundle 'zefei/cake16'
" }}}
" tpope {{{
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-bundler'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-sleuth'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tpope/vim-unimpaired'
" }}}
" Shougo {{{
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'Shougo/vimshell.vim'
" }}}
" Python {{{
NeoBundle 'jmcantrell/vim-virtualenv'
NeoBundle 'lambdalisue/nose.vim'
" }}}
NeoBundleCheck
" }}}
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

" GUI settings
if has("gui_running")
  set guioptions=aegi
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
set mouse=n       " Only listen to mouse events in normal mode

" Wildcard matching
set wildignore=*.swp,*.bak,*.pyc,*.class,node_modules/*,_sgbak,.DS_Store
set wildignore+=*.dmg
set wildmenu
set wildmode=list:longest,full

" Search optimizations
set hlsearch
set ignorecase
set incsearch " Incremental search as you type
set smartcase " Use case sensitivity if search includes a capital letter

" Editor settings

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
set scrolloff=14               " Changes when VIM starts scrolling file (i.e. cursor two lines from bottom)
set shiftround
set shiftwidth=4
let &showbreak = '↳'
set smarttab
set tabstop=4                  " Number of spaces a tab is displayed as
set textwidth=0                " Somehow getting set to 78, which is weird
set nowrap                     " Wrapping just looks odd on top of being a nuisance
set linespace=3                " Bump this up a little bit for looks

set listchars=tab:→\ ,eol:↵,trail:·,nbsp:· " Used with `set list`

" }}}
" Plugin specific settings {{{

" Syntastic
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_javascript_checkers = ['jshint']

" delimitMate
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1
let delimitMate_balance_matchpairs = 1

" EasyMotion highlighting
hi link EasyMotionTarget ErrorMsg
hi link EasyMotionShade Comment

" Show Marks
let g:showmarks_enable = 0

" Unite
let g:unite_source_grep_max_candidates = 200
let s:file_rec_ignore = '\.\%(gif\|png\|jpg\|jpeg\|ico\)$'
call unite#custom#source('file_rec', 'ignore_pattern', s:file_rec_ignore)
let g:unite_enable_ignore_case = 1

if executable('ag')
  " Use ag in unite grep source.
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nocolor --nogroup --hidden -i'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack-grep')
  " Use ack in unite grep source.
  let g:unite_source_rec_async_command = 'ack-grep -f --no-filter'
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

" Tube
let g:tube_terminal = 'iterm'

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

command! -nargs=0 Marked !open -a 'Marked' %

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
" Editor conveniences {{{
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
" Filetype settings {{{
augroup filetype_settings
  au!
  au FileType coffee     setl sw=2 ts=2 et
  au FileType css        setl ts=4 sw=4 et
  au FileType cucumber   setl ts=2 sw=2 et
  au FileType eruby      setl ts=2 sw=2 et
  au FileType haskell    setl et
  au FileType html       setl ts=4 sw=4 et
  au FileType htmldjango setl ts=4 sw=4 et
  au FileType htmldjango let b:delimitMate_matchpairs="(:),[:],<:>,{:},%:%"
  au FileType javascript setl ts=4 sw=4 et
  au FileType less       setl ts=4 sw=4 et
  au FileType python     setl ts=4 sw=4 et
  au FileType qf         setl nolist nocursorline nowrap colorcolumn=0
  au FileType ruby       setl ts=2 sw=2 et foldmethod=syntax
  au FileType scss       setl ts=2 sw=2 et
  au FileType stylus     setl sw=4 ts=4 et
  au FileType vim        setl sw=2 ts=2 et
  au FileType zsh        setl sw=2 ts=2 et
  au FileType yaml       setl sw=2 ts=2 et
augroup END
" }}}
" Undetected filetypes {{{
augroup undetected_filetypes
  au!
  au BufNewFile,BufRead *.coffee set ft=coffee foldmethod=indent nofoldenable
  au BufNewFile,BufRead *.hbs,*.handlebars setl ft=handlebars
  au BufNewFile,BufRead *.j2 setl ft=jinja
  au BufNewFile,BufRead *.jade setl ft=jade foldmethod=indent nofoldenable
  au BufNewFile,BufRead *.json setl ft=json
  au BufNewFile,BufRead *.md setl ft=markdown
  au BufNewFile,BufRead *.pp setl ft=puppet
  au BufNewFile,BufRead *.sls setl ft=yaml
  au BufNewFile,BufRead */Views/*.html setl ft=htmldjango
  au BufNewFile,BufRead .vimlocal setl ft=vim
  au BufNewFile,BufRead Vagrantfile setl ft=ruby
  au BufNewFile,BufRead *.sls setl ft=yaml
  au BufNewFile,BufRead .vimlocal setl ft=vim
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
" Cursorline {{{
augroup cursor_line
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
augroup END
" }}}
" Dispatch compilers {{{
augroup dispatch_compilers
    au!
    au FileType python
augroup END
" }}}
" }}}
" Source local settings {{{
if filereadable(glob('~/.vimlocal'))
  source ~/.vimlocal
endif
" }}}
