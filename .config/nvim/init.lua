vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

-- vim-plug
local Plugin = vim.fn['plug#']
vim.call('plug#begin')

Plugin('nvim-lua/plenary.nvim')

-- Treesitter and friends are experimental
Plugin('nvim-treesitter/nvim-treesitter', {['do'] = ':TSUpdate'})
Plugin('RRethy/nvim-treesitter-endwise')
Plugin('lukas-reineke/indent-blankline.nvim')
Plugin('stevearc/aerial.nvim')

Plugin('nvim-telescope/telescope.nvim', {branch = '0.1.x'})

Plugin('ycm-core/YouCompleteMe', {['do'] = 'python3 install.py --all'})
vim.g.ycm_language_server = {
	{
		name = 'ruby',
		cmdline = { 'solargraph', 'stdio' },
		filetypes = { 'ruby' },
		project_root_files = { 'Gemfile' },
	},
}

Plugin('github/copilot.vim')

Plugin('fatih/vim-go', {['do'] = ':GoInstallBinaries'})
Plugin('towolf/vim-helm')

Plugin('AndrewRadev/splitjoin.vim')
Plugin('ciaranm/securemodelines')

Plugin('ConradIrwin/vim-comment-object')
Plugin('michaeljsmith/vim-indent-object')
Plugin('Julian/vim-textobj-variable-segment')
Plugin('kana/vim-textobj-user')
Plugin('kana/vim-textobj-function')
Plugin('kana/vim-textobj-indent')
Plugin('kana/vim-textobj-lastpat')
Plugin('kana/vim-textobj-line')
Plugin('sgur/vim-textobj-parameter')
vim.g.vim_textobj_parameter_mapping = 'a'

-- Plugin('Lokaltog/vim-easymotion')
Plugin('tomtom/quickfixsigns_vim')
Plugin('tpope/vim-abolish')
Plugin('tpope/vim-capslock')
Plugin('tpope/vim-commentary')
Plugin('tpope/vim-markdown')
Plugin('tpope/vim-repeat')
Plugin('tpope/vim-speeddating')
Plugin('tpope/vim-surround')
Plugin('tpope/vim-unimpaired')
Plugin('tpope/vim-rails')
Plugin('tpope/vim-bundler')
Plugin('Valloric/MatchTagAlways')
Plugin('Raimondi/delimitMate')

Plugin('vim-airline/vim-airline')
Plugin('vim-airline/vim-airline-themes')

Plugin('rlue/vim-fold-rspec')
Plugin('tmux-plugins/vim-tmux')

Plugin('mhinz/vim-signify')
vim.g.signify_vcs_list = {'perforce', 'git', 'hg', 'svn'}

Plugin('google/vim-searchindex')
Plugin('google/vim-syncopate')
Plugin('google/vim-maktaba')
Plugin('google/vim-glaive')
Plugin('google/vim-codefmt')
Plugin('google/vim-selector')

-- Plugin 'prabirshrestha/async.vim'
-- Plugin 'prabirshrestha/vim-lsp'
-- Plugin 'natebosch/vim-lsc'
Plugin 'altercation/vim-colors-solarized'
-- Plugin 'nacitar/terminalkeys.vim'
-- Plugin 'sjl/gundo.vim'
-- Plugin 'sjl/splice.vim'
-- Plugin 'nfvs/vim-perforce'
-- Plugin 'tommcdo/vim-exchange'
-- Plugin 'tommcdo/vim-lion'
-- Plugin 'scrooloose/syntastic'
-- let g:syntastic_mode_map = {
--   \ 'mode': 'passive',
--   \ 'active_filetypes': ['java', 'python'],
--   \ 'passive_filetypes': [],
--   \ }
-- let g:syntastic_auto_loc_list = 1
-- let g:syntastic_check_on_wq = 0

-- Plugin 'SirVer/ultisnips'
-- Plugin 'honza/vim-snippets'
-- let g:UltiSnipsEditSplit = "vertical"
-- let g:UltiSnipsListSnippets = "<c-l>"
-- let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips/"

vim.call('plug#end')

vim.call('glaive#Install')
vim.cmd 'Glaive codefmt google_java_executable=\'/usr/local/bin/google-java-format\' prettier_executable=\'/usr/local/bin/prettier\''

require('user')
