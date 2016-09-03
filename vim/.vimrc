"SETUP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible "uses Vim settings as opposed to Vi
execute pathogen#infect() 

"TEXT FORMATTING
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"enables filetype detection, also loading the custom plugin and indent files of the filetype detected
filetype plugin indent on
set tabstop=4 "how many spaces the tab key counts for in insert mode
set shiftwidth=4 "how many spaces of text is indented when using the << and >> operations in normal mode
set expandtab "tabs converted into spaces for all files
set softtabstop=4 "how many spaces the backspace key can delete insert mode (and how many spaces the tab key counts for, as well)
set autoindent
set encoding=utf-8 "allows non-ASCII characters to be displayed

autocmd BufRead,BufNewFile *.es set filetype=javascript "Emcascript syntax as javascript (ES6)
autocmd Bufread,BufNewFile *.hss set filetype=haskell.script "for vim-slime
autocmd Bufread,BufNewFile *.as set filetype=actionscript "overriding .as extension from Atlas to ActionScript3
autocmd Bufread,BufNewFile *.md set filetype=markdown "overriding .as extension from Modula to Markdown

"BEHAVIOUR
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set backspace=indent,eol,start "allow backspacing over everything in insert mode
set incsearch "searches while typing
set hlsearch "highlights all matched words
set vb t_vb= "disable that annoying bell noise

set ignorecase "ignore case when searching...
set smartcase "unless uppercase character used (toggle with \c)
set wildmenu "opens up completion list for vim commands
set hidden "allows buffers to be hidden that contain unwritten changes
if has("unix")
	set directory=~/.vim/tmpfiles//
else
	set directory=~/vimfiles/tmpfiles//
endif

"KEY MAPPINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = "," "leader key to , (global plugins)
let maplocalleader = "\\" "localleader to \ (filetype specific plugins)
"remap ,, to , so you can still search backwards with ,,
nnoremap <leader><leader> ,

"anchor here (useful with Ctrl-P)
nnoremap <leader>ah :cd %:p:h<CR>

nnoremap <C-q> :x<CR>
nnoremap <C-t> :tabnew<CR>

"consistent <C-S> mappings
noremap <C-S> :update<CR>
vnoremap <C-S> <ESC>:update<CR>
inoremap <C-S> <C-O>:update<CR><ESC>

"copies path of current buffer to clipboard
nnoremap <leader>cp :let @+ = expand("%:p")<CR>

"searching
nnoremap <CR> :noh <CR><CR>

"Tab navigation
nnoremap <A-1> 1gt
nnoremap <A-2> 2gt
nnoremap <A-3> 3gt
nnoremap <A-4> 4gt
nnoremap <A-5> 5gt
nnoremap <A-6> 6gt
nnoremap <A-7> 7gt
nnoremap <A-8> 8gt
nnoremap <A-9> 9gt
nnoremap <A-0> 10gt

"intuitive previous tab
nnoremap tg gT

"moving between windows easily
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

"sourcing VIMRC
nnoremap <leader>sv :source $MYVIMRC<CR><CR>

"lists open buffers
nnoremap <leader>ls :ls<CR>:b
"toggling spell
nnoremap <leader>ts :set spell!<CR>

"Sourcing scratchpad
nnoremap <leader>ss :source C:\Users\Eddie\code\vimscript\scratch.vim<CR><CR>

function! VimGrep()

	if has("unix")
		let separator = '/'
	else
		let separator = '\'
	endif

	call inputsave()
	let pwd = getcwd()
	let files = input('Files to search: ', pwd . separator . '**' . separator .'*')
	call inputrestore()

	if empty(files)
		return
	endif

	call inputsave()
	let pattern = input('Pattern: ')
	call inputrestore()

	"use very magic search
	if !empty(pattern)
		execute ':vimgrep /\v' . pattern . '/gj' . ' ' . files
		execute ':cw'
	endif

endfunction

"Mnemonic: search globally
nnoremap <leader>sg :call VimGrep()<CR>

" Misc gimmicks
map <F3> mzHVLg?`z

" For markdown syntax highlightings:
" http://vim.wikia.com/wiki/Fix_syntax_highlighting
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

"File-type specitic key mappings (merge/put these into an existing plugin later?)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! MarkdownApplyListStrikethrough()
	let start_pos = getpos('.')
	exec 'normal! F*'
	if getpos('.')[2] == start_pos[2]
		exec 'normal! f*'
	endif
	exec 'normal! la~~'
	exec 'normal! A~~'
	call setpos('.', start_pos)
endfunction

"Mnemonic: toggle todo
au FileType markdown nnoremap <LocalLeader>tt :call MarkdownApplyListStrikethrough()<CR>

"AESTHETICS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on "turns on the syntax
set relativenumber "set relative number to begin with
set number "set number as well, so you get both
set guioptions= "disable all guioptions
set wrap "don't wrap lines
set linebreak "break at whitespace if convenient
if has("unix")
	set guifont=Monospace\ 10
else "has windows
	set guifont=DejaVu\ Sans\ Mono:h11 "For Math
endif

set foldcolumn=1 "shows where folds are
set display=lastline "Partially displays any long lines
set cursorline "highlights the whole active line
set laststatus=2 "always show the status line (even if there is only one buffer in the window)

"Colorscheming
"----------------------------
set background=dark "default background
colorscheme badwolf "preferred dark colorscheme

"ABBREVIATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"PLUGIN SPECIFIC BINDINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"CtrlP
"----------------------------
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe,*.fla,*.swf,*.o,*.hi
let g:ctrlp_custom_ignore = {
	\ 'dir': '\v[\/](node_modules|dist|lib|libs|db|env|bourbon|\.git)'
	\}
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlPMixed' "find mru and cwd
"https://coderwall.com/p/hk_bwg/how-to-speed-up-ctrlp
" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
    " Use Ag over Grep
    set grepprg=ag\ --nogroup\ --nocolor
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

"NERDtree
"----------------------------
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>d :NERDTreeFromBookmark 
"Visual Studio-esque binding
nnoremap <M-L> :NERDTreeFind<CR>
"ignore compiled filetypes
let NERDTreeIgnore = ['\.hi$', '\.o$', '\.exe$', '\.fla$', '\.swf$']

"Recommended from the NERDTree docs 
augroup nerdtree
	autocmd!
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif "closes tree if it is the last window
augroup END

"Emmet-vim
"----------------------------
let g:user_emmet_mode='iv'  "Only enable for visual and normal (ctrl-y conflicts in normal mode with incremental scroll-up)

"Airline
"----------------------------
let g:airline#extensions#whitespace#enabled = 0

"Fugitive
"----------------------------
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>ga :Git add %:p<CR><CR>
nnoremap <leader>gp :Git push origin master<CR><CR>
nnoremap <leader>gu :Git pull origin master<CR><CR>
nnoremap <leader>gd :Gvdiff<CR><CR>
nnoremap <leader>gl :Git log --oneline --decorate --graph<CR><CR>
nnoremap <leader>gb :Gblame<CR>

"Syntastic
"----------------------------
"when check is run, location list is populated without running :Errors
let g:syntastic_always_populate_loc_list = 1
"automatically open loc list with check
let g:syntastic_auto_loc_list = 1
"use manual checking
nnoremap <leader>sc :SyntasticCheck<CR>
nnoremap <leader>so :SyntasticReset<CR>
let g:syntastic_mode_map = {
	\ "mode": "passive"
	\}

"vim-go
"----------------------------
autocmd FileType go nmap <LocalLeader>r <Plug>(go-run)
autocmd FileType go nmap <LocalLeader>b <Plug>(go-build)
autocmd FileType go nmap <LocalLeader>t <Plug>(go-test)
autocmd FileType go nmap <LocalLeader>f <Plug>(go-test-func)
autocmd FileType go nmap <LocalLeader>c <Plug>(go-coverage)
autocmd FileType go nmap <LocalLeader>i <Plug>(go-info)
autocmd FileType go nmap <LocalLeader>n <Plug>(go-rename)
autocmd FileType go nmap <LocalLeader>s <Plug>(go-install)
"gd goes to definition (default)

"vim-replr
"----------------------------
"example:
let g:replr_build_instructions = {}
let g:replr_build_instructions['C:\Users\Eddie\vimfiles\bundle\vim-replr\plugin'] = "go.bat"
"Mnemonic: build script
nnoremap <leader>bs :Replr<CR>

"Thesaurus feature
"----------------------------
set thesaurus+=C:\Users\Eddie\Documents\ref\mthesaur.txt

"Terminal hackery
"----------------------------
if has("unix") && !has("gui_running")
"Getting the alt key to work in Cygwin
"http://vim.wikia.com/wiki/Get_Alt_key_to_work_in_terminal
"http://t73257.editors-vim.editortalk.us/alt-key-is-not-working-on-linux-or-cygwin-console-t73257.html
	set <M-1>=1
	set <M-2>=2
	set <M-3>=3
	set <M-4>=4
	set <M-5>=5
	set <M-6>=6
	set <M-7>=7
	set <M-8>=8
	set <M-9>=9
	set <M-L>=L

"Getting the background colors to work in tmux
"http://superuser.com/questions/399296/256-color-support-for-vim-background-in-tmux
	set t_ut=

"For terminal in Ubuntu bash
set t_Co=256

"Another key binding for incrementing numbers while in tmux
nnoremap <C-i> <C-a>

endif

"Vim-slime
"----------------------------
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane": "0.1", "difference_trim": 1}
let g:slime_python_ipython = 1
let g:slime_take_snapshot = 1

"vim-hdevtools
"----------------------------
au FileType haskell nnoremap <buffer> <LocalLeader>t :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <LocalLeader>c :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <LocalLeader>i :HdevtoolsInfo<CR>
au FileType haskell.script nnoremap <buffer> <LocalLeader>t :HdevtoolsType<CR>
au FileType haskell.script nnoremap <buffer> <silent> <LocalLeader>c :HdevtoolsClear<CR>
au FileType haskell.script nnoremap <buffer> <silent> <LocalLeader>i :HdevtoolsInfo<CR>

"vim-markdown
"----------------------------
"contents
nnoremap <LocalLeader>c :Toc<CR>

"don't autofold
autocmd Syntax markdown normal zR

"YouCompleteMe
"----------------------------
nnoremap <leader>ct :YcmCompleter GoTo<CR>
nnoremap <leader>cd :YcmCompleter GetDoc<CR>
""UltiSnips clash
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"

function! TestFunction(config)
    call system("tmux -L " . shellescape(a:config["socket_name"]) . " capture-pane -S - -t " . shellescape(a:config["target_pane"]))
    call system("tmux -L " . shellescape(a:config["socket_name"]) . " save-buffer " . g:slime_current_file)
endfunction

nnoremap <leader>tf :call TestFunction({"difference_trim": 1, "socket_name": "default", "target_pane": "0.1"})<CR>
