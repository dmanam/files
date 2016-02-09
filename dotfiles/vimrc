"if &shell =~# 'fish$'
"  set shell=dash
"endif

syntax on
set nocompatible
set encoding=utf-8
set showcmd

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'VundleVim/Vundle.vim'

"set background=dark

Bundle 'sjl/gundo.vim'

"Bundle 'altercation/vim-colors-solarized'
"set background=dark
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1

Bundle 'jonathanfilip/vim-lucius'

"Bundle 'dag/vim-fish'

Bundle 'scrooloose/syntastic'
let g:syntastic_mode_map = {'mode': 'active', 'passive_filetypes': ['fish', 'tex']}

Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-notes'
let g:notes_directories = ['~/Documents/class/notes']

Bundle 'tpope/vim-eunuch'

Bundle 'sbl/scvim'
let g:sclangTerm = "urxvt -e"

Bundle 'def-lkb/vimbufsync'
Bundle 'the-lambda-church/coquille'
au FileType coq call coquille#FNMapping()
au FileType coq map <F8> :CoqLaunch<CR>
au FileType coq map <F9> :CoqKill<CR>

"Bundle 'vim-scripts/CoqIDE'
"let g:CoqIDE_coqtop = '/usr/bin/coqtop'
"let g:CoqIDEDefaultMap = 1

call vundle#end()

filetype plugin indent on

colorscheme lucius
let g:lucius_no_term_bg=1
LuciusBlack

set tabstop=2 shiftwidth=2
set expandtab
set ai
set si

set autoread

set backspace=indent,eol,start

set hlsearch
set incsearch
set ignorecase
set smartcase

set noerrorbells
set novisualbell
set t_vb=
set tm=500

set number
set lazyredraw
set nocursorline
set showmatch

set ttyfast
set mouse=a

set foldenable
nnoremap <space> za
set foldmethod=syntax
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview

au BufEnter *.hs compiler ghc

let mapleader=","

noremap <silent> <leader>w :call ToggleWrap()<CR>
function ToggleWrap()
  if &wrap
    echo "Wrap OFF"
    setlocal nowrap
  else
    echo "Wrap ON"
    setlocal wrap linebreak nolist
  endif
endfunction

set wrap linebreak nolist
noremap  <buffer> <silent> <Up>   gk
noremap  <buffer> <silent> <Down> gj
noremap  <buffer> <silent> k      gk
noremap  <buffer> <silent> j      gj
noremap  <buffer> <silent> 0      g0
noremap  <buffer> <silent> ^      g^
noremap  <buffer> <silent> $      g$
noremap  <buffer> <silent> <Home> g<Home>
noremap  <buffer> <silent> <End>  g<End>
inoremap <buffer> <silent> <Up>   <C-o>gk
inoremap <buffer> <silent> <Down> <C-o>gj
inoremap <buffer> <silent> <Home> <C-o>g<Home>
inoremap <buffer> <silent> <End>  <C-o>g<End>

"nnoremap B ^
"vnoremap B ^
"nnoremap E $
"vnoremap E $
"nnoremap ^ <nop>
"vnoremap ^ <nop>
"nnoremap $ <nop>
"vnoremap $ <nop>

nnoremap <leader>u :GundoToggle<CR>
nnoremap <leader>s :mksession<CR>

"if exists('$TMUX')
"  let &t_SI = "\<Esc>Ptmux;\<Exc>\<Exc>]50;CursorShape=1\x7<Esc>\\"
"  let &t_EI = "\<Esc>Ptmux;\<Exc>\<Exc>]50;CursorShape=0\x7<Esc>\\"
"else
"  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
"  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
"endif

au BufEnter *.hs compiler ghc
let g:haddock_browser = "xdg-open"
let g:haddock_browser_callformat="sh -c '%s file://%s >/dev/null 2>&1 &'"

nnoremap ; :
vnoremap ; :

ca W SudoWrite
ca w!! SudoWrite

dig (t 9115 (m 9116 (b 9117 )t 9118 )m 9119 )b 9120 It 8992 Im 9134 Ib 8993 <( 10216 )> 10217 ns 8345 /E 8713 /C 8836 ~= 8773 TO 8868 BO 8869 TS 8866 DT 8872 NN 8469 RR 8477 -. 8230
