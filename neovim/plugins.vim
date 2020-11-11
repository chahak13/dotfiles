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
Plug 'mhinz/vim-startify'               | " Vim start screen
Plug 'arcticicestudio/nord-vim'         | " Nord colorscheme
Plug 'nvim-treesitter/nvim-treesitter'  | " Neovim Treesitter

call plug#end()

" lsp specific config
lua << EOF
    local nvim_lsp = require('nvim_lsp')
    nvim_lsp.pyls.setup{}
EOF

" Treesitter config
lua << EOF
    require'nvim-treesitter.configs'.setup {
        ensure_installed = 'python',
        highlight = {
            enable = true,
            disable = { "markdown" },
        },
    }
EOF
