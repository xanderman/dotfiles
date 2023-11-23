local map = vim.keymap.set

-- 'write' with sudo hack
map('c', 'w!!', 'w !sudo tee > /dev/null %')

-- Quickly remove search highlight without turning off the option permanently
map('n', '<CR>', ':nohlsearch<CR>:syntax sync fromstart<CR>', { silent = true })

-- Make for nicer window/tab management
map('n', '<M-h>', '<C-w>h', { silent = true })
map('n', '<M-j>', '<C-w>j', { silent = true })
map('n', '<M-k>', '<C-w>k', { silent = true })
map('n', '<M-l>', '<C-w>l', { silent = true })
map('n', '<M-n>', 'gt', { silent = true })
map('n', '<M-p>', 'gT', { silent = true })
map('n', '<leader><M-n>', ':+tabmove<CR>', { silent = true })
map('n', '<leader><M-p>', ':-tabmove<CR>', { silent = true })
map('n', '<leader>gt', ':+tabmove<CR>', { silent = true })
map('n', '<leader>gT', ':-tabmove<CR>', { silent = true })
map('n', 'ZA', ':qa<CR>')

-- <leader>b to get a quick list of buffers and a prompt to choose one
-- I'm trying telescope for this for now
-- map('n', '<leader>b', ':ls<CR>:b<SPACE>')

-- Close quickfix, location, and preview windows quickly
map('n', '<F3>', ':cclose<CR>:lclose<CR>:pclose<CR>', { silent = true })

-- Use +/- for increment/decrement
-- intentionally allow remap because vim-speeddating does awesome things
map('n', '+', '<C-A>', { silent = true, remap = true })
map('n', '-', '<C-X>', { silent = true, remap = true })

-- Instead of suspending ViM, center the cursor
map({'n', 'v', 'o', 's'}, '<C-Z>', 'zvzz', { silent = true })

-- Quickly edit init.lua
map('n', '<leader>ev', ':tabe $MYVIMRC<CR>')
-- map('n', '<leader>sv', ':source $MYVIMRC<CR>')

-- Execute the line under the cursor as an Ex command
map('n', '<leader>ex', ':yank<CR>:@"<CR>', { silent = true })
