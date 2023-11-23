local augroup = vim.api.nvim_create_augroup('myvimrc', {clear = true})
local au = vim.api.nvim_create_autocmd

-- Highlight the row with the cursor only in the active window
au('WinEnter', {
        desc = 'Highlight the row with the cursor',
        group = augroup,
        command = 'set cursorline',
})
au('WinLeave', {
        desc = 'but only in the active window',
        group = augroup,
        command = 'set nocursorline',
})

-- " Formatting options by filetype
-- autocmd FileType *
--   \  setlocal formatoptions-=o fo+=lj
--   \| if index(['text', 'gitcommit', 'p4-spec'], &ft) == -1
--   \|    setlocal formatoptions-=t fo+=crq
--   \| endif

-- keep cwd in the folder I'm looking at
-- au('BufEnter', {
--         desc = 'Set the cwd to the current file',
--         group = augroup,
--         command = 'silent! lcd %:p:h',
-- })

-- F5 toggles paste in normal and insert modes
vim.opt.pastetoggle = '<F5>'
au('InsertLeave', {
        desc = 'Disable paste mode when leaving insert mode',
        group = augroup,
        command = 'set nopaste',
})

-- Always open readonly when a swap file exists, and warn me gently
-- This only runs once, so use ':e' to deal with the file after recovery
au('SwapExists', {
        desc = 'Open in readonly when a swap file exists',
        group = augroup,
        command = 'let v:swapchoice = "o"',
        once = true,
})
au('VimEnter', {
        desc = 'Warn me gently when a swap file exists',
        group = augroup,
        callback = function(ev)
                if (vim.v.swapchoice == 'o') then
                        vim.call('maktaba#error#Warn', 'Swap file exists; opened readonly')
                end
        end
})
