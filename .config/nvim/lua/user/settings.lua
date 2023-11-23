local set = vim.opt
set.diffopt:append('internal,algorithm:patience')
set.autoindent = true
set.background = 'dark'
set.backspace = 'indent,eol,start'
set.colorcolumn:append('+1,+2')
set.cursorline = true
set.display:append('lastline')
set.expandtab = true
set.foldcolumn = '2'
set.foldlevelstart = 1
set.gdefault = true
set.hidden = true
set.history = 1000
set.hlsearch = true
set.ignorecase = true
set.inccommand = 'nosplit'
set.incsearch = true
set.laststatus = 2
set.list = true
set.listchars = 'tab:⇥ ,trail:␣,extends:⇉,precedes:⇇,nbsp:·'
set.magic = true
set.mouse = ''
set.digraph = false
set.errorbells = false
set.icon = false
set.startofline = false
set.wrap = false
set.number = true
set.numberwidth = 3
set.relativenumber = true
set.scrolloff = 5
set.shada = "'50,<1000,s100,/50,:50,h"
set.shiftround = true
set.shiftwidth = 2
set.shortmess = 'atToOsI'
set.showcmd = true
set.showmatch = true
set.sidescrolloff = 5
set.smartcase = true
set.smarttab = true
set.softtabstop = 2
set.splitbelow = true
set.splitright = true
set.tabpagemax = 100
set.title = true
set.ttimeout = true
set.ttimeoutlen = 50
set.undofile = true
set.whichwrap:append('<,>,[,]')
set.wildchar = ('\t'):byte()
set.wildmenu = true
set.wildmode = 'longest:full,full'

vim.g.solarized_termcolors=256
vim.cmd.colorscheme('solarized')
