vim.g.airline_powerline_fonts = 1
if vim.g.local_vimrc_airline_section_y ~= nil then
        vim.g.airline_section_y = vim.call('airline#section#create', vim.g.local_vimrc_airline_section_y)
end
