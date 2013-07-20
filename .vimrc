set nocompatible
let mapleader = ","

" Vundle management
filetype off            " Must be set temporarily to load bundles
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle 'michaeljsmith/vim-indent-object'

Bundle 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims = 1

Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-capslock'
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

  if has('python')
    Bundle 'SirVer/ultisnips'
    let g:UltiSnipsEditSplit = "vertical"
    let g:UltiSnipsListSnippets = "<c-t>"
  endif

  if (v:version >= 703) && has('patch584') && has('python')
    Bundle 'Valloric/YouCompleteMe'
  endif
endif

filetype plugin indent on
syntax on
set background=dark     " Use brighter text color
colorscheme elflord
hi TabLineSel ctermfg=Green

" TODO local swap files

" Make sure my configs have precedence over any plugins
set runtimepath-=~/.vim
set runtimepath^=~/.vim
set runtimepath-=~/.vim/after
set runtimepath+=~/.vim/after

source /usr/share/vim/vim73/macros/matchit.vim

set backspace=indent,eol,start  " more powerful backspacing
set cb="exclude:.*"     " never connect to the X server
set colorcolumn+=+1,+2  " poor man's print margin
set cursorline          " clearly markthe row with the cursor
set esckeys             " use arrow keys in insert mode
set expandtab           " don't replace my spaces with tab characters
set gdefault            " always do global search/replace
set ignorecase          " ignore case in searches
set incsearch           " incremental search
set magic               " extended regex
set matchpairs+=<:>     " % jumps between <> too
set modelines=2         " Scan first and last 2 lines for modelines
set mouse=a             " let me use the mouse!
set nodigraph           " don't bother me with two-byte chars
set noerrorbells        " I hate the bell!
set noicon              " this is a terminal, who needs icons?
set nostartofline       " keep cursor in current column when paging
set numberwidth=3       " give just enough space for relative line numbers
set relativenumber      " show line numbers relative to cursor position
set shortmess=atToOsI   " use abbreviated forms for most messages
set shiftwidth=2        " Seriously, why would I want it to be 8?
set showcmd             " Show partial command in status line
set showmatch           " show opening bracket for just typed closing bracket
set smartcase           " match case when I put a capital letter in the search
set title               " update the window title
set tpm=100             " open as many tabs as I want
set undofile            " save undo across sessions
set wildchar=<Tab>      " tab-completion the way I like it
set wildmenu
set wildmode=longest:full,full
set ww+=<,>,[,]         " allow arrow keys to move across line boundaries
nnoremap Y y$

" Status line madness
hi StatusLine ctermfg=Cyan
set laststatus=2
set statusline=%F%m%r%w\ %y\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ %{CapsLockStatusline()}%=%l/%L,%v[%p%%]

" Experimental remove trailing whitespace on edited lines
" TODO try using the google#substitute thing
" au InsertLeave * '[,']s/\s\+$//e
" au InsertLeave * normal! ``

" Make for nicer window/tab management
nmap <silent> <C-h> <C-w>h
nmap <silent> <C-j> <C-w>j
nmap <silent> <C-k> <C-w>k
nmap <silent> <C-l> <C-w>l
nmap <silent> <C-n> gt
nmap <silent> <C-p> gT
nmap <silent> <Leader><C-n> :exec tabpagenr() % tabpagenr('$') . "tabm"<CR>
nmap <silent> <Leader><C-p> :exec (tabpagenr() == 1 ? "" : tabpagenr() - 2) . "tabm"<CR>
nmap ZA :qa<CR>

" Easy find/replace word under cursor
noremap <leader>r :%s/\<<C-R><C-W>\>/
" In visual mode find/replace last yank
vnoremap <leader>r :s/\<<C-R>0\>/

" Restore cursor across sessions
set viminfo='50,<1000,s100,/50,:50,h,n~/.vim/viminfo
au BufReadPost * if line("'0") > 0 && line("'0") <= line("$")
              \|   exe("norm '\"")
              \| endif

" Auto-change directories
au BufEnter * silent! lcd %:p:h
cabbrev cdg lcd %:p:h:s?google3/.*$?google3?

" Search term highlighting
set hlsearch
nnoremap <silent> <F2> :nohlsearch<CR><C-l>

" No line wrapping by default, but easy to toggle
set nowrap
map <F3> :set wrap! wrap?<CR>

" Recognize gitcommit files (so as to use my nifty syntax file!)
autocmd BufNewFile,BufRead COMMIT_EDITMSG set filetype=gitcommit

autocmd BufNewFile,BufRead *.go set filetype=go

" F5 toggles paste in command and edit modes
map <F5> :se invpaste paste?<return>
set pastetoggle=<F5>

" vimdiff tools
if &diff
  nmap <Leader>m :diffget 1<CR>
  nmap <Leader>y :diffget 3<CR>
  nmap <Leader>r :diffupdate<CR>
  set diffopt+=iwhite
endif

" Enable spell checking, even in program source files. Hit <F4> to
" highlight spelling errors. Hit it again to turn highlighting off. Type
"     :help spell
" to read the manual, here is a brief reminder:
" ]s Next misspelled word
" [s Previous misspelled word
" z= Make suggestions for current word
" zg Add to good words list
" Turn spelling on by default for US English (which is common among
" Googlers), so, Center is correctly spelled. Centre is not, and shows
" with spell local colors. Completely misspelled words show like soo.
set dictionary=/usr/share/dict/american-english
setlocal spell spelllang=en_us  " American English spelling.
set spellfile=~/.words.utf8.add " My own word list is saved here.
" Toggle spelling with F4 key.
map <F4> :set spell!<CR><Bar>:echo "Spell check: " .
  \ strpart("OffOn", 3 * &spell, 3)<CR>
" Change the default highlighting colors and terminal attributes
highlight SpellBad cterm=underline ctermfg=red ctermbg=blue
" Limit list of suggestions to the top 10 items
set spellsuggest=best,10
" Uncomment the next line, if you wish spelling to be off by default.
set nospell

" Don't let me accidentally stop vim
map <C-Z> <C-Y>

" Make a simple "search" text object.
vnoremap <silent> s //e<C-r>=&selection=='exclusive'?'+1':''<CR><CR>
    \:<C-u>call histdel('search',-1)<Bar>let @/=histget('search',-1)<CR>gv
omap s :normal vs<CR>
