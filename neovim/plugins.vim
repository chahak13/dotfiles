" ================ Plugins for vim-plug ========================== "
" Vim-plug installations:
call plug#begin('~/.neovim/pluggins')

Plug 'junegunn/fzf', { 'do': './install --bin' }    | " Fzf

Plug 'tpope/vim-commentary'             | " Block commenting
Plug 'scrooloose/nerdTree'              | " NerdTree
Plug 'machakann/vim-sandwich'           | " Add/delete/replace text surroundings
Plug 'junegunn/goyo.vim'                | " Goyo for zen mode
Plug 'junegunn/vim-peekaboo'            | " Peak at registers
Plug 'junegunn/fzf.vim'                 | " FZF wrapper for vim
Plug 'neovim/nvim-lsp'                  | " Neovim LSP
Plug 'srcery-colors/srcery-vim'         | " Srcery colorscheme

call plug#end()

" lsp specific config
lua << EOF
  local nvim_lsp = require('nvim_lsp')
  nvim_lsp.pyls.setup{}
EOF
