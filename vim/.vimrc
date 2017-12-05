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
autocmd Filetype javascript setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype json setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype html setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype haxe setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype yaml setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype scss setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype css setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype c setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Filetype python setlocal tabstop=2 softtabstop=2 shiftwidth=2
autocmd Bufread,BufNewFile *.hss set filetype=haskell.script "for vim-slime
autocmd Bufread,BufNewFile *.as set filetype=actionscript "overriding .as extension from Atlas to ActionScript3
autocmd Bufread,BufNewFile *.md set filetype=markdown "overriding .as extension from Modula to Markdown
autocmd Bufread,BufNewFile *.tsv set noexpandtab "don't expandtab on tsv files for obvious reasons

"BEHAVIOUR
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set backspace=indent,eol,start "allow backspacing over everything in insert mode
set incsearch "searches while typing
set hlsearch "highlights all matched words
set visualbell t_vb= "disable that annoying bell noise
set lazyredraw "don't update screen when executing macros etc.

set ignorecase "ignore case when searching...
set smartcase "unless uppercase character used (toggle with \c)
set wildmenu "opens up completion list for vim commands
set hidden "allows buffers to be hidden that contain unwritten changes
runtime macros/matchit.vim "enchanced use of % to jump around matching constructs
if has("unix")
	set directory=~/.vim/tmpfiles//
else
	set directory=~/vimfiles/tmpfiles//
endif

"CONVENIENT KEY MAPPINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = "," "leader key to , (global plugins)
let maplocalleader = "\\" "localleader to \ (filetype specific plugins)
"remap ,, to , so you can still search backwards with ,,
nnoremap <leader><leader> ,

"anchor here (useful with Ctrl-P)
nnoremap <leader>ah :cd %:p:h<CR>

"windows-esque quitting
nnoremap <C-q> :x<CR>
nnoremap <C-t> :tabnew<CR>

"consistent <C-S> mappings
noremap <C-S> :update<CR>
vnoremap <C-S> <ESC>:update<CR>
inoremap <C-S> <C-O>:update<CR><ESC>

"escaping normal mode
inoremap jk <Esc>

"copies path of current buffer to clipboard
nnoremap <leader>cp :let @+ = expand("%:p")<CR>

"searching
nnoremap <CR> :noh<CR>

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
nnoremap <leader>sv :source $MYVIMRC<CR>

"lists open buffers
nnoremap <leader>ls :ls<CR>:b
"toggle paste
nnoremap <leader>tp :set paste!<CR>
"Sourcing scratchpad
nnoremap <leader>ss :source $HOME/.vim_scratch.vim<CR>

"Visually repeating operations
vnoremap . :normal .<CR>
vnoremap q :normal @q<CR>

function! VimGrep(pattern, files)
	if has("unix")
		let separator = '/'
	else
		let separator = '\'
	endif
    if executable('ag')
        execute ':silent grep! --case-sensitive "' . a:pattern . '" ' . a:files
    else
        execute ':noautocmd vimgrep /\v' . a:pattern . '/gj' . ' ' . a:files \
        . separator . '**' . separator . '*'
    endif
    execute ':copen'
    execute ':redraw!'
endfunction

function! VimGrepFromCursor()
	if has("unix")
		let separator = '/'
	else
		let separator = '\'
	endif
    let l:searchterm = getreg("z")
    let pwd = getcwd()
    let files = pwd
    call VimGrep(l:searchterm, files)
endfunction

function! VimGrepFromManual()
	if has("unix")
		let separator = '/'
	else
		let separator = '\'
	endif

	call inputsave()
	let pwd = getcwd()
    let files = input('Files to search: ', pwd)
	call inputrestore()

	if empty(files)
		return
	endif

	call inputsave()
	let pattern = input('Pattern: ')
	call inputrestore()

	"use very magic search
	if !empty(pattern)
        call VimGrep(pattern, files)
	endif
endfunction

"Mnemonic: search files
nnoremap <leader>sf :call VimGrepFromManual()<CR>
" Search word in pwd
vnoremap <F4> "zy<Esc>:call VimGrepFromCursor()<CR>
nnoremap <F4> viw"zy<Esc>:call VimGrepFromCursor()<CR>

function! ReplaceWordUnderCursor()
    let word_to_replace  = getreg("z")
    let replacement = input('Replace word under cursor with: ', word_to_replace )
    execute ':%s/' . word_to_replace  . '/' . replacement . '/gc'
endfunction
"Mnemonic: search files
vnoremap <leader>rb "zy<Esc>:call ReplaceWordUnderCursor()<CR>
nnoremap <leader>rb viw"zy<Esc>:call ReplaceWordUnderCursor()<CR>

"Copy buffer contents to system clipboard
"http://vi.stackexchange.com/questions/7761/how-to-restore-the-position-of-the-cursor-after-executing-a-normal-command
function! CopyToSystemClipboard()
    let save_pos = getpos(".")
    :normal! ggVG"+y
    call setpos('.', save_pos)
endfunction
function! CopySelectionToSystemClipboard()
    echo "Selection copied to clipboard"
endfunction
"TODO: more elegant way of doing this?
nnoremap <F2> :call CopyToSystemClipboard()<CR>
vnoremap <F2> "+y<Esc>:call CopySelectionToSystemClipboard()<CR>

function! PasteSystemClipboardToBuffer()
    :normal! "+p
    echo "Clipboard contents pasted"
endfunction
nnoremap <F3> :call PasteSystemClipboardToBuffer()<CR>
inoremap <F3> <C-R>+
vnoremap <F3> "+p

" Mnemonic: jump to
" Only in the QuickFix buffer
au FileType qf nnoremap <buffer> <LocalLeader>jt :.cc<CR>

" For fixing markdown syntax highlightings:
" http://vim.wikia.com/wiki/Fix_syntax_highlighting
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

" Looking things up
"http://stackoverflow.com/questions/4316193/check-if-there-is-a-current-active-visual-selection-in-vim-from-a-function-invok
function! GoogleSearch()
    let l:searchterm = getreg("z")
    call netrw#BrowseX('https://google.com'."/search?q=".l:searchterm, 0)
endfunction
vnoremap <leader>sg "zy<Esc>:call GoogleSearch()<CR>
nnoremap <leader>sg viw"zy<Esc>:call GoogleSearch()<CR>

function! DocsSearch()
    let l:searchprefix = {
                \'javascript': 'https://developer.mozilla.org/en-US/search?q=',
                \'python': 'https://docs.python.org/2/search.html?q='
                \}
    let l:filetype = &filetype
    if has_key(l:searchprefix, l:filetype)
        let l:searchterm = getreg("z")
        call netrw#BrowseX(l:searchprefix[l:filetype].l:searchterm, 0)
    else
        call GoogleSearch()
    endif
endfunction
vnoremap <leader>sd "zy<Esc>:call DocsSearch()<CR>
nnoremap <leader>sd viw"zy<Esc>:call DocsSearch()<CR>

" For vim lookups: http://stackoverflow.com/questions/15867323/search-vim-help-for-subject-under-cursor
set keywordprg=:help

"File-type specitic key mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Markdown
"----------------------------
"Mnemonic: toggle todo
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
au FileType markdown nnoremap <LocalLeader>tt :call MarkdownApplyListStrikethrough()<CR>

"Mnemonic: insert timestamp
function! MarkdownInsertTimestamp()
    let @l = '* **' . system('printf `date +%H:%M`') . '**: '
    put l
endfunction
au FileType markdown nnoremap <LocalLeader>it :call MarkdownInsertTimestamp()<CR>

"Mnemonic: insert date
function! MarkdownInsertDate()
    let @l = '## ' . system('printf `date +%d/%m/%y`')
    put l
endfunction
au FileType markdown nnoremap <LocalLeader>id :call MarkdownInsertDate()<CR>

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
colorscheme gruvbox "preferred dark colorscheme

"ABBREVIATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"PLUGIN SPECIFIC BINDINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"CtrlP
"----------------------------
set wildignore+=*.map
set wildignore+=*.zip
set wildignore+=*.png
set wildignore+=*.fla
set wildignore+=*.swf
set wildignore+=*.log
set wildignore+=*.o
set wildignore+=*.hi
set wildignore+=*.dump
set wildignore+=*/.git/*
set wildignore+=*/node_modules/*
set wildignore+=*/third_party/*
set wildignore+=*/Assets/*
set wildignore+=*/Exports/*
set wildignore+=*/dst/*
set wildignore+=*/dist/*

" Only applicable when search with native vim
let g:ctrlp_custom_ignore = {
	\ 'dir': '\v[\/](Applications|Library|Downloads|third_party|node_modules|libs|db|env|bourbon|Assets|Exports|\.git)$'
	\}
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlPMixed' "find mru and cwd
let g:ctrlp_switch_buffer = 'et' "only switch to an opened buffer if it's open in the current tab

"https://coderwall.com/p/hk_bwg/how-to-speed-up-ctrlp
" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
    " Use Ag over Grep
    set grepprg=ag\ --nogroup\ --nocolor
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor
                \ --ignore ''Applications'' 
                \ --ignore ''Library'' 
                \ --ignore ''Downloads'' 
                \ --ignore ''third_party'' 
                \ --ignore ''notes_public'' 
                \ --ignore ''node_modules'' 
                \ --ignore ''Assets'' 
                \ --ignore ''Exports'' 
                \ --ignore ''.git''
                \ -g ""'
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
nnoremap <leader>g<Space> :Git<Space>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gd :Gvdiff<CR>
nnoremap <leader>g0 :Gvdiff :0<CR>
nnoremap <leader>g1 :Gvdiff ~1<CR>
nnoremap <leader>g2 :Gvdiff ~2<CR>
nnoremap <leader>g3 :Gvdiff ~3<CR>
nnoremap <leader>g4 :Gvdiff ~4<CR>
nnoremap <leader>g5 :Gvdiff ~5<CR>
nnoremap <leader>g6 :Gvdiff ~6<CR>
nnoremap <leader>g7 :Gvdiff ~7<CR>
nnoremap <leader>ga :Git add %:p<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gb :Gblame<CR>

"Syntastic
"----------------------------
"when check is run, location list is populated without running :Errors
let g:syntastic_always_populate_loc_list = 1
"automatically open loc list with check (disabling for the moment)
let g:syntastic_auto_loc_list = 1
"javascript file checking
let g:syntastic_javascript_checkers = ['eslint', 'jshint']
"use manual checking
nnoremap <leader>sc :SyntasticCheck<CR>
nnoremap <leader>so :SyntasticReset<CR>
let g:syntastic_mode_map = {
	\ "mode": "passive"
	\}

"vim JSX
"----------------------------
"Enable on all .js files
let g:jsx_ext_required = 0

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
let g:replr_build_instructions = {}
"Windows example:
"let g:replr_build_instructions['C:\Users\Eddie\vimfiles\bundle\vim-replr\plugin'] = "go.bat"
"Mnemonic: build script
nnoremap <leader>bs :Replr<CR>

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
"This is interfering with jumping to previous
"nnoremap <C-i> <C-a>

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

"vim-markdown (vanilla)
"----------------------------
let g:markdown_folding = 1

"vim-markdown (PlasticBoy plugin)
"----------------------------
"contents
nnoremap <LocalLeader>c :Toc<CR>

"don't autofold
autocmd Syntax markdown normal zR

"YouCompleteMe
"----------------------------
nnoremap <leader>ct :YcmCompleter GoTo<CR>
nnoremap <leader>cd :YcmCompleter GetDoc<CR>
"UltiSnips clash
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
"Make sure you compile with:
"git submodule update --int --recursive
"./install.py --clang-completer --js-completer

"SyntaxRange
"----------------------------
"Embedding some scripting syntax
autocmd FileType markdown :silent! call SyntaxRange#Include('```javascript', '```', 'javascript', 'SpecialComment')
autocmd FileType markdown :silent! call SyntaxRange#Include('```python', '```', 'python', 'SpecialComment')
autocmd FileType markdown :silent! call SyntaxRange#Include('```powershell', '```', 'ps1', 'SpecialComment')
autocmd FileType markdown :silent! call SyntaxRange#Include('```html', '```', 'html', 'SpecialComment')
autocmd FileType markdown :silent! call SyntaxRange#Include('```sh', '```', 'sh', 'SpecialComment')
" DON'T enable this one; it messes up word text objects for some reason
"autocmd FileType markdown call SyntaxRange#Include('```scheme', '```', 'scheme', 'SpecialComment')

"VlcDrill
""----------------------------
let g:vlcdrill#annotation#path = "~/dev/ketones/songs/rihanna/good_girl_gone_bad_youtube.json"
nnoremap <leader>vds :VlcDrillShow<CR>
nnoremap <leader>vdl :VlcDrillLoadAnnotation<CR>

"Screenshot location
function! ChangeScreenshotLocation()
    let script_path = "$HOME/dotfiles/scripts/screenshot_location.sh"
    let current_directory = expand("<sfile>:p:h")
    let screenshot_location = input(current_directory . '/')
    let screenshot_path = current_directory . '/' . screenshot_location
    call system(script_path . " -s " . screenshot_path)
    echo "Screenshot location set to " . screenshot_path
endfunction

function! GetScreenshotLocation()
    let script_path = "$HOME/dotfiles/scripts/screenshot_location.sh"
    call system(script_path)
endfunction

nnoremap <leader>sls :call ChangeScreenshotLocation()<CR>
nnoremap <leader>slg :call GetScreenshotLocation()<CR>

"Vaxe
""----------------------------
set autowrite
au FileType haxe nnoremap <buffer> <LocalLeader>m :make<CR>
"Trying to speed it up:
let g:vaxe_completion_disable_optimizations = 0
let g:vaxe_completeopt_menuone = 1
let g:vaxe_enable_ycm_defaults = 0
let g:vaxe_cache_server = 1
