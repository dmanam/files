" standard things
	syntax on
	filetype plugin indent on
	set nocompatible
	set encoding=utf-8
	set showcmd
	set backspace=indent,eol,start
	let g:netrw_dirhistmax=0
	nnoremap Y y$

" more convenient leader
	let mapleader=","

call plug#begin('~/.local/share/nvim/site/plugged')

" from vim-plug docs
	function! Cond(cond, ...)
		let opts = get(a:000, 0, {})
		return a:cond ? opts : extend(opts, { 'on': [], 'for': [] })
	endfunction

" undo tree
	Plug 'sjl/gundo.vim'
	nnoremap <leader>u :GundoToggle<CR>

" color scheme
	Plug 'jonathanfilip/vim-lucius'
	let g:lucius_no_term_bg=1

" syntax checker
	Plug 'neomake/neomake'
	autocmd! BufWritePost * Neomake

" general utilities
	Plug 'tpope/vim-repeat'
	Plug 'tpope/vim-endwise'
	Plug 'tpope/vim-commentary'
	Plug 'tpope/vim-surround'
	Plug 'tpope/vim-eunuch'
	ca W SudoWrite
	ca w!! SudoWrite
	ca ww SudoWrite

" haskell
	Plug 'lukerandall/haskellmode-vim', { 'for': 'haskell' }
	autocmd FileType *.hs compiler ghc
	let g:haddock_browser = "xdg-open"
	let g:haddock_browser_callformat="sh -c '%s file://%s >/dev/null 2>&1 &'"
	"autocmd FileType haskell setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
	"autocmd FileType cabal setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
	autocmd BufRead,BufNewFile *.hsc setlocal filetype=haskell
	autocmd BufRead,BufNewFile *.chs setlocal filetype=haskell

" tex
	Plug 'lervag/vimtex', { 'for': 'tex' }
	let g:vimtex_view_method = "zathura"
	let g:vimtex_latexmk_progname = "nvr"
	let g:vimtex_quickfix_open_on_warning = 0
	let g:vimtex_indent_enabled = 0
	let g:vimtex_latexmk_continuous = 0
	let g:vimtex_latexmk_background = 1
	let g:vimtex_imaps_enabled = 0
	augroup vimtex_config
		au!
		au User VimtexEventQuit VimtexClean
	augroup END
	au FileType tex map <leader>c :VimtexCompile<CR>
	au FileType tex map <leader>s :VimtexStatus<CR>
	au FileType tex map <leader>v :VimtexView<CR>
	autocmd BufWritePost *.tex VimtexCompile
	au FileType tex set indentkeys=
	autocmd BufRead,BufNewFile *.cls setlocal filetype=tex
	autocmd FileType tex setlocal indentexpr&
	"autocmd FileType tex setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
	let g:tex_flavor = "latex"
	let g:neomake_tex_enabled_makers = ['chktex'] ", 'rubberinfo', 'proselint']
	function BibHighlight()
		unlet b:current_syntax
		syntax include @BIB syntax/bib.vim
		syntax region bibZone
		      \ start="\\begin{filecontents}{[^}]*\.bib}"rs=s
		      \ end="\\end{filecontents}"re=e
		      \ keepend
		      \ transparent
		      \ contains=texBeginEnd,@BIB
		let b:current_syntax = 'tex'
	endfunction
	autocmd FileType tex call BibHighlight()

" coq
        Plug 'def-lkb/vimbufsync', { 'for': 'coq' }
        Plug 'the-lambda-church/coquille', { 'for': 'coq' }
        au FileType coq call coquille#FNMapping()
        au FileType coq map <F8> :CoqLaunch<CR>
        au FileType coq map <F9> :CoqKill<CR>

" easily delete view files
	Plug 'vim-scripts/delview'
	ca delview Delview

call plug#end()

" enable color scheme
	colorscheme lucius
	LuciusBlack

" indentation
	set expandtab tabstop=2 softtabstop=2 shiftwidth=2
	set ai
	set si

" hidden characters
	set showbreak=↪\ 
	set list
	set listchars=tab:»\ ,eol:↲,nbsp:␣,trail:█,extends:⟩,precedes:⟨

" searching
	set hlsearch
	set incsearch
	set ignorecase
	set smartcase

" remove bells
	set noerrorbells
	set novisualbell
	set t_vb=
	set tm=500

" show line numbers
	set number

" show matching brackets
	set showmatch

" increase speed
	set ttyfast
	set lazyredraw

" enable mouse
	set mouse=a

" folds
	set foldenable
	nnoremap <space> za
	set foldmethod=syntax

" automatically save and load views
	autocmd BufWinLeave *.* mkview
	autocmd BufWinEnter *.* silent! loadview

" persistent undo
	set undofile
	set undolevels=1000
	set undoreload=10000

" wrapping
	set colorcolumn=80
	highlight ColorColumn ctermbg=8
	noremap <silent> <leader>w :call ToggleWrap()<CR>
	function ToggleWrap()
		if &wrap
			echo "Wrap OFF"
			setlocal nowrap
		else
			echo "Wrap ON"
			setlocal wrap linebreak
		endif
	endfunction
	set wrap linebreak
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

" title
	set title titlestring=%F

" easier to press ; than :
	nnoremap ; :
	vnoremap ; :

" digraphs
	dig (t 9115  " ⎛
	dig (m 9116  " ⎜
	dig (b 9117  " ⎝
	dig )t 9118  " ⎞
	dig )m 9119  " ⎟
	dig )b 9120  " ⎠
	dig It 8992  " ⌠
	dig Im 9134  " ⎮
	dig Ib 8993  " ⌡
	dig <( 10216 " ⟨
	dig )> 10217 " ⟩
	dig ns 8345  " ₙ
	dig /E 8713  " ∉
	dig /C 8836  " ⊄
	dig ~= 8773  " ≅
	dig TO 8868  " ⊤
	dig BO 8869  " ⊥
	dig TS 8866  " ⊢
	dig NN 8469  " ℕ
	dig RR 8477  " ℝ
	dig -. 8230  " …
