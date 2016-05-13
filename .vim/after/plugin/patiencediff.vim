" This can't go in .vimrc, because :PatienceDiff isn't available
if !exists(":PatienceDiff")
" This block is optional, but will protect you from errors if you
" uninstall vim-diff-enhanced or share your config across machines
  finish
endif
PatienceDiff
