" ================ Plugins for vim-plug ========================== "
" Vim-plug installations:
call plug#begin('~/.neovim/pluggins')

Plug 'neoclide/coc.nvim', {'branch': 'release'}     | " Code Completion
Plug 'junegunn/fzf', { 'do': './install --bin' }    | " Fzf

Plug 'tpope/vim-commentary'             | " Block commenting
Plug 'scrooloose/nerdTree'              | " NerdTree
Plug 'machakann/vim-sandwich'           | " Add/delete/replace text surroundings
Plug 'tpope/vim-fugitive'               | " Git plugin
Plug 'junegunn/goyo.vim'                | " Goyo for zen mode
Plug 'jacob-ogre/vim-syncr'             | " Vim-syncr
Plug 'wincent/terminus'                 | " Terminal integration improvements
Plug 'AndrewRadev/splitjoin.vim'        | " Split and join programming lines
Plug 'junegunn/vim-peekaboo'            | " Peak at registers
Plug 'morhetz/gruvbox'                  | " Gruvbox theme
Plug 'vimwiki/vimwiki'                  | " Vim wiki

call plug#end()


