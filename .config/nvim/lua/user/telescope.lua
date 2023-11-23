local map = vim.keymap.set

map("n", "<leader>ff", "<cmd>Telescope find_files<CR>")
map("n", "<leader>fg", "<cmd>Telescope live_grep<CR>")
map("n", "<leader>fs", "<cmd>Telescope git_status<CR>")
map("n", "<leader>fc", "<cmd>Telescope commands<CR>")
map("n", "<leader>fm", "<cmd>Telescope keymaps<CR>")
map("n", "<leader>fh", "<cmd>Telescope help_tags<CR>")
map("n", "<leader>fb", "<cmd>Telescope git_branches<CR>")
map('n', '<leader>b', '<cmd>Telescope buffers<CR>')

-- From Matt's, I'm not using but might in future
-- map("n", "<leader>fy", "<cmd>Telescope neoclip<CR>")
-- map("n", "<leader>fw", "<cmd>Telescope lsp_workspace_symbols query=")
