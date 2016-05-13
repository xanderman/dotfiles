set nocompatible

" Set mapleader to spacebar
nnoremap <space> <nop>
let mapleader = " "
let maplocalleader = "\\"

" filetype/syntax should be off before loading plugins and enabled afterward or
" new stuff won't be noticed.
filetype off
syntax off

" At work I don't use vimdiff directly, but instead use a script to handle a
" multi-file diff with tabs. Conveniently the SCM passes ':' as the first
" argument when multi-file diff is enabled.
let s:diff_mode = 0
if &diff || argv(0) == ':'
  let s:diff_mode = 1
endif

" Vundle management
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

Plugin 'altercation/vim-colors-solarized'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'ciaranm/securemodelines'
" Plugin 'ConradIrwin/vim-bracketed-paste'
Plugin 'drmikehenry/vim-fixkey'
Plugin 'Julian/vim-textobj-variable-segment'
Plugin 'kana/vim-textobj-function'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-lastpat'
Plugin 'kana/vim-textobj-line'
Plugin 'kana/vim-textobj-user'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'michaeljsmith/vim-indent-object'
Plugin 'nacitar/terminalkeys.vim'
Plugin 'sjl/gundo.vim'
Plugin 'sjl/splice.vim'
Plugin 'theevocater/vim-perforce'
Plugin 'tmux-plugins/vim-tmux'
Plugin 'tommcdo/vim-exchange'
Plugin 'tommcdo/vim-lion'
Plugin 'tomtom/quickfixsigns_vim'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-capslock'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'Valloric/MatchTagAlways'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'vim-scripts/argtextobj.vim'
Plugin 'google/vim-searchindex'
Plugin 'google/vim-syncopate'
Plugin 'chrisbra/vim-diff-enhanced'

" Plugins with settings
Plugin 'mhinz/vim-signify'
let g:signify_vcs_list = ['perforce', 'git', 'hg', 'svn']

Plugin 'scrooloose/syntastic'
let g:syntastic_mode_map = {
  \ 'mode': 'passive',
  \ 'active_filetypes': ['java', 'python'],
  \ 'passive_filetypes': [],
  \ }
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0

Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsListSnippets = "<c-l>"
let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips/"

" Enable any local modifications
if filereadable($HOME . '/.local_config/local.vim')
  source ~/.local_config/local.vim
endif

source $VIMRUNTIME/macros/matchit.vim

" Now that we're done loading and sourcing, turn on filetype/syntax.
call vundle#end()
filetype plugin indent on
syntax on

" Make sure my configs have precedence over any plugins
" This really shouldn't be necessary, but sometimes plugin managers get it wrong
set runtimepath-=~/.vim
set runtimepath^=~/.vim
set runtimepath-=~/.vim/after
set runtimepath+=~/.vim/after

" Put all my autocmds in the same group
" DO NOT source anything after this point!
augroup myvimrc
autocmd!

set autoindent          " copy previous indent on new lines
set background=dark     " Use brighter text color
set backspace=indent,eol,start  " more powerful backspacing
set clipboard=exclude:.*      " never connect to the X server
set colorcolumn+=+1,+2  " poor man's print margin
set cursorline          " highlight the row with the cursor
set diffopt+=iwhite     " ignore trailing whitespace in diffs
set diffopt+=vertical   " always split diff windows vertically
set display+=lastline   " show full last line when it's long
set esckeys             " use arrow keys in insert mode
set expandtab           " don't replace my spaces with tab characters
set foldcolumn=2        " show me where the folds are in the gutter
set foldlevelstart=1    " start with most folds closed
set gdefault            " always do global search/replace
set history=1000        " remember ALL the commands!
set hlsearch            " Search term highlighting
set ignorecase          " ignore case in searches
set incsearch           " incremental search
set laststatus=2        " always show status line
set magic               " extended regex
set matchpairs+=<:>     " % jumps between <> too
set mouse=a             " let me use the mouse!
set nodigraph           " don't bother me with two-byte chars
set noerrorbells        " I hate the bell!
set noicon              " this is a terminal, who needs icons?
set nostartofline       " keep cursor in current column when paging
set nowrap              " No line wrapping
set number              " Show line numbers
set numberwidth=3       " give just enough space for relative line numbers
set relativenumber      " show line numbers relative to cursor position
set ruler               " show cursor position on bottom right
set scrolloff=5         " always keep a few lines above/below cursor visible
set shiftround          " round indent to nearest shiftwidth
set shortmess=atToOsI   " use abbreviated forms for most messages
set showcmd             " Show partial command in status line
set showmatch           " show opening bracket for just typed closing bracket
set sidescrolloff=5     " always keep a few columns left/right of cursor visible
set smartcase           " match case when I put a capital letter in the search
set smarttab            " use shiftwidth for tabs at BOL
set splitbelow          " new horizontal splits on the bottom
set splitright          " new vsplit window on right
set tabpagemax=100      " let me have lots of tabs
set title               " update the window title
set tpm=100             " open as many tabs as I want
set ttimeout            " short timeout for terminal characters
set ttimeoutlen=50
set undofile            " save undo across sessions
set wildchar=<Tab>      " tab-completion the way I like it
set wildmenu
set wildmode=longest:full,full
set ww+=<,>,[,]         " allow arrow keys to move across line boundaries
nnoremap Y y$

" Put swap/undo/backup files in /tmp
let s:local_swap_dir_root = '/tmp/vim_tmp/'
if !isdirectory(s:local_swap_dir_root)
  call mkdir(s:local_swap_dir_root, '', 01777)
endif
if filewritable(s:local_swap_dir_root) == 2  " If it's a directory
  let s:swap_dir = s:local_swap_dir_root . $LOGNAME . '/swap'
  if !isdirectory(s:swap_dir)
    call mkdir(s:swap_dir, 'p', 0700)
  endif
  execute 'set directory^=' . s:swap_dir . '//'

  let s:backup_dir = s:local_swap_dir_root . $LOGNAME . '/backup'
  if !isdirectory(s:backup_dir)
    call mkdir(s:backup_dir, 'p', 0700)
  endif
  execute 'set backupdir^=' . s:backup_dir

  let s:undo_dir = s:local_swap_dir_root . $LOGNAME . '/undo'
  if !isdirectory(s:undo_dir)
    call mkdir(s:undo_dir, 'p', 0700)
  endif
  execute 'set undodir^=' . s:undo_dir
endif

" TODO(bobgardner): remove when new regexp engine doesn't suck
if exists("&regexpengine")
  set regexpengine=1
endif

" Set my preferred colors
let g:solarized_termcolors=16
colorscheme solarized
highlight TabLineSel ctermfg=Green

" Airline settings
" powerline_fonts needs to come first
let g:airline_powerline_fonts = 1
if exists('g:local_vimrc_airline_section_y')
  let g:airline_section_y = airline#section#create(g:local_vimrc_airline_section_y)
endif

" Show tabs, trailing whitespace, etc
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
if &termencoding ==# 'utf-8' || &encoding ==# 'utf-8'
  let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
endif
set list

" highlight the row with the cursor only in the active window
autocmd WinEnter * set cursorline
autocmd WinLeave * set nocursorline

" Restore cursor across sessions
set viminfo='50,<1000,s100,/50,:50,h,n~/.vim/viminfo
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
        \|   exe("norm `\"")
        \| endif

" Formatting options by filetype
autocmd FileType *
      \ setlocal formatoptions-=o fo+=lj
      \| if index(['text', 'gitcommit', 'p4-spec'], &ft) == -1
      \|   setlocal formatoptions-=t fo+=crq
      \| endif

" Temporary hack to use theevocater/vim-perforce for p4 client specs
autocmd FileType p4-spec set filetype=p4client

" Experimental remove trailing whitespace on edited lines
" autocmd InsertLeave * keepjumps '[,']s/\s\+$//e
" autocmd InsertLeave * normal! g`^

" 'write' with sudo hack
cmap w!! w !sudo tee > /dev/null %

" Quickly remove search highlight without turning off the option permanently
nnoremap <silent> <F2> :nohlsearch<CR>
nnoremap <silent> <CR> :nohlsearch<CR>

" Make for nicer window/tab management
nnoremap <silent> <M-h> <C-w>h
nnoremap <silent> <M-j> <C-w>j
nnoremap <silent> <M-k> <C-w>k
nnoremap <silent> <M-l> <C-w>l
nnoremap <silent> <M-n> gt
nnoremap <silent> <M-p> gT
nnoremap <silent> <Leader><M-n> :exec tabpagenr() % tabpagenr('$') . "tabm"<CR>
nnoremap <silent> <Leader><M-p> :exec (tabpagenr() == 1 ? "" : tabpagenr() - 2) . "tabm"<CR>
nnoremap <silent> <Leader>gt :exec tabpagenr() % tabpagenr('$') . "tabm"<CR>
nnoremap <silent> <Leader>gT :exec (tabpagenr() == 1 ? "" : tabpagenr() - 2) . "tabm"<CR>
nnoremap ZA :qa<CR>

" <leader>b to geta  quick list of buffers and a prompt to choose one
nnoremap <leader>b :ls<CR>:b<SPACE>

" Close quickfix, location, and preview windows quickly
nnoremap <silent> <F3> :cclose<CR>:lclose<CR>:pclose<CR>

if s:diff_mode
  " TODO: rethink this for multidiff, where the buffer numbers aren't
  " predictable
  " special settings for vimdiff
  " nnoremap <Leader>m :diffget 1<CR>
  " nnoremap <Leader>y :diffget 3<CR>
  nnoremap <Leader>r :diffupdate<CR>
else
  " special setup for non-diff mode

  " Easy find/replace word under cursor
  noremap <leader>r :%s/\<<C-R><C-W>\>/
  " In visual mode find/replace last yank
  vnoremap <leader>r :s/\<<C-R>0\>/

  " Auto-change directories
  autocmd BufEnter * silent! lcd %:p:h
endif

" F5 toggles paste in command and edit modes
noremap <F5> :set invpaste paste?<return>
set pastetoggle=<F5>
autocmd InsertLeave * set nopaste

" Use +/- for increment/decrement
" intentionally allow remap because vim-speeddating does awesome things
nmap + <C-A>
nmap - <C-X>

" Enable spell checking, even in program source files. Hit <F4> to
" highlight spelling errors. Hit it again to turn highlighting off. Type
"     :help spell
" to read the manual, here is a brief reminder:
" ]s Next misspelled word
" [s Previous misspelled word
" z= Make suggestions for current word
" zg Add to good words list
" Turn spelling on by default for US English, so, Center is correctly spelled.
" Centre is not, and shows with spell local colors. Completely misspelled words
" show like soo.
set dictionary=/usr/share/dict/american-english
set spelllang^=en_us  " American English spelling first
set spellfile=~/.words.utf8.add " My own word list is saved here.
" Toggle spelling with F4 key.
noremap <F4> :set spell!<CR><Bar>:echo "Spell check: " .
  \ strpart("OffOn", 3 * &spell, 3)<CR>
" Change the default highlighting colors and terminal attributes
highlight SpellBad cterm=underline ctermfg=red ctermbg=blue
" Limit list of suggestions to the top 10 items
set spellsuggest=best,10

" Instead of suspending ViM, center the cursor
noremap <C-Z> zvzz

" Quickly edit this file
nnoremap <leader>ev :tabe $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
" Execute the line under the cursor as an Ex command
nnoremap <silent> <leader>ex :yank<CR>:@"<CR>

command! -nargs=1 VC  call ExecuteVimCommandAndViewOutput(<q-args>)

function! ExecuteVimCommandAndViewOutput(cmd)
  let @v = maktaba#command#GetOutput(a:cmd)
  new
  set buftype=nofile
  put v
endfunction

nnoremap U <C-r>
nnoremap <F6> :GundoToggle<CR>

" Always open readonly when a swap file exists, and warn me gently
autocmd SwapExists * let v:swapchoice = 'o'
autocmd VimEnter * call WarnSwapFile()

function! WarnSwapFile() abort
  if v:swapchoice isnot ''
    call maktaba#error#Warn('Swap file exists; opened readonly')
  endif
endfunction

" Make sure this remains the last line!
augroup END
