set nocompatible

" Set mapleader to spacebar
nnoremap <space> <nop>
let mapleader = " "
let maplocalleader = "\\"

" filetype/syntax should be off before loading plugins and enabled afterward or
" new stuff won't be noticed.
filetype off
syntax off

" Vundle management
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims = 1

Bundle 'ciaranm/securemodelines'
Bundle 'Julian/vim-textobj-variable-segment'
Bundle 'kana/vim-textobj-user'
Bundle 'michaeljsmith/vim-indent-object'
Bundle 'sjl/gundo.vim'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-capslock'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-unimpaired'
Bundle 'Valloric/MatchTagAlways'

" Only load some bundles if diff mode is off
if !&diff
  Bundle 'tomtom/quickfixsigns_vim'
  Bundle 'scrooloose/syntastic'
  let g:syntastic_mode_map = {
    \ 'mode': 'passive',
    \ 'active_filetypes': ['java', 'python'],
    \ 'passive_filetypes': [],
    \ }
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_wq = 0

  if has('python')
    Bundle 'SirVer/ultisnips'
    let g:UltiSnipsEditSplit = "vertical"
    let g:UltiSnipsListSnippets = "<c-l>"
  endif

  Bundle 'terryma/vim-multiple-cursors'
  let g:multi_cursor_start_key = '<F6>'
endif

" Enable any local modifications
if filereadable($HOME . '/.local_config/local.vim')
  source ~/.local_config/local.vim
endif

source $VIMRUNTIME/macros/matchit.vim

" Now that we're done loading and sourcing, turn on filetype/syntax.
filetype plugin indent on
syntax on

" Make sure my configs have precedence over any plugins
set runtimepath-=~/.vim
set runtimepath^=~/.vim
set runtimepath-=~/.vim/after
set runtimepath+=~/.vim/after

set autoindent          " copy previous indent on new lines
set background=dark     " Use brighter text color
set backspace=indent,eol,start  " more powerful backspacing
set cb="exclude:.*"     " never connect to the X server
set colorcolumn+=+1,+2  " poor man's print margin
set cursorline          " highlight the row with the cursor
set display+=lastline   " show full last line when it's long
set esckeys             " use arrow keys in insert mode
set expandtab           " don't replace my spaces with tab characters
set foldlevelstart=1    " start with most folds closed
set gdefault            " always do global search/replace
set history=1000        " remember ALL the commands!
set hlsearch            " Search term highlighting
set ignorecase          " ignore case in searches
set incsearch           " incremental search
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
set regexpengine=1      " TODO(bobgardner): remove when new regexp engine doesn't suck
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
set tabpagemax=100      " let me have lots of tabs
set tildeop             " treat ~ as an operator
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

" Set my preferred colors
colorscheme elflord
highlight TabLineSel ctermfg=Green

" Status line madness
highlight StatusLine ctermfg=Cyan
set laststatus=2
set statusline=%F%m%r%w\ %y\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ %{CapsLockStatusline()}%=%l/%L,%v[%p%%]

set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
if &termencoding ==# 'utf-8' || &encoding ==# 'utf-8'
  let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
endif

" highlight the row with the cursor only in the active window
augroup highlightcursor
  autocmd!
  autocmd WinEnter * set cursorline
  autocmd WinLeave * set nocursorline
augroup END

" Restore cursor across sessions
set viminfo='50,<1000,s100,/50,:50,h,n~/.vim/viminfo
augroup restorecursor
  autocmd!
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
         \|   exe("norm `\"")
         \| endif
augroup END

" Formatting options by filetype
augroup formatting
  autocmd!
  autocmd FileType *
        \ setlocal formatoptions-=o fo+=lj
        \| if index(['text', 'gitcommit', 'p4-spec'], &ft) == -1
        \|   setlocal formatoptions-=t fo+=crq
        \| endif
augroup END

" Experimental remove trailing whitespace on edited lines
" augroup removewhitespace
  " autocmd!
  " autocmd InsertLeave * keepjumps '[,']s/\s\+$//e
  " autocmd InsertLeave * normal! g`^
" augroup END

" 'write' with sudo hack
cmap w!! w !sudo tee > /dev/null %

" Quickly remove search highlight without turning off the option permanently
nnoremap <silent> <F2> :nohlsearch<CR>

" Make for nicer window/tab management
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-j> <C-w>j
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-l> <C-w>l
nnoremap <silent> <C-n> gt
nnoremap <silent> <C-p> gT
nnoremap <silent> <Leader><C-n> :exec tabpagenr() % tabpagenr('$') . "tabm"<CR>
nnoremap <silent> <Leader><C-p> :exec (tabpagenr() == 1 ? "" : tabpagenr() - 2) . "tabm"<CR>
nnoremap ZA :qa<CR>

" Close quickfix, location, and preview windows quickly
nnoremap <silent> <F3> :cclose<CR>:lclose<CR>:pclose<CR>

if &diff
  " special settings for vimdiff
  nnoremap <Leader>m :diffget 1<CR>
  nnoremap <Leader>y :diffget 3<CR>
  nnoremap <Leader>r :diffupdate<CR>
  set diffopt+=iwhite
else
  " special setup for non-diff mode

  " Easy find/replace word under cursor
  noremap <leader>r :%s/\<<C-R><C-W>\>/
  " In visual mode find/replace last yank
  vnoremap <leader>r :s/\<<C-R>0\>/

  " Auto-change directories
  augroup chdirs
    autocmd!
    autocmd BufEnter * silent! lcd %:p:h
  augroup END
endif

" F5 toggles paste in command and edit modes
noremap <F5> :se invpaste paste?<return>
set pastetoggle=<F5>

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
setlocal spell spelllang=en_us  " American English spelling.
set spellfile=~/.words.utf8.add " My own word list is saved here.
" Toggle spelling with F4 key.
noremap <F4> :set spell!<CR><Bar>:echo "Spell check: " .
  \ strpart("OffOn", 3 * &spell, 3)<CR>
" Change the default highlighting colors and terminal attributes
highlight SpellBad cterm=underline ctermfg=red ctermbg=blue
" Limit list of suggestions to the top 10 items
set spellsuggest=best,10
" Uncomment the next line, if you wish spelling to be off by default.
set nospell

" Instead of suspending ViM, center the cursor
noremap <C-Z> zvzz

" Quickly edit this file
nnoremap <leader>ev :tabe $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
" Execute the line under the cursor as an Ex command
nnoremap <silent> <leader>ex :yank<CR>:@"<CR>

" Make a simple "search" text object.
vnoremap <silent> s //e<C-r>=&selection=='exclusive'?'+1':''<CR><CR>
    \:<C-u>call histdel('search',-1)<Bar>let @/=histget('search',-1)<CR>gv
onoremap s :normal vs<CR>

command! -nargs=1 VC  call ExecuteVimCommandAndViewOutput(<q-args>)

function! ExecuteVimCommandAndViewOutput(cmd)
  let @v = maktaba#command#GetOutput(a:cmd)
  new
  set buftype=nofile
  put v
endfunction

nnoremap <F6> :GundoToggle<CR>
