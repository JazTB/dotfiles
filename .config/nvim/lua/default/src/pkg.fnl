(local lazypath (.. (vim.fn.stdpath "data") "/lazy/lazy.nvim"))
(when (not ((. (or vim.uv vim.loop) :fs_stat) lazypath))
  (vim.fn.system
    ["git"
     "clone"
     "--filter=blob:none"
     "https://github.com/folke/lazy.nvim.git"
     "--branch=stable"
     lazypath]))
(vim.opt.rtp:prepend lazypath)
(set vim.g.mapleader " ")
(set vim.g.maplocalleader "\\")

((. (require :lazy) :setup)
 ["xiyaowong/transparent.nvim"
  "leafo/moonscript-vim"
  ;;"vim-airline/vim-airline"
  ;;"preservim/nerdtree"
  "svermeulen/nvim-teal-maker" ;; exposes vim api to teal?
  "nvim-tree/nvim-tree.lua"
  "nvim-tree/nvim-web-devicons"
  ;; THEMES
  "sainnhe/everforest" ;; low-key grey
  "folke/tokyonight.nvim" ;; purple
  "catppuccin/nvim" ;; customizable theme
  "nvim-lualine/lualine.nvim"
  ;;"vim-airline/vim-airline-themes"
  "preservim/tagbar"
  "norcalli/nvim-colorizer.lua"
  "NLKNguyen/vim-lisp-syntax"
  "ollykel/v-vim"
  "leafgarland/typescript-vim"
  "yuezk/vim-js"
  "MaxMEllon/vim-jsx-pretty"
  "sheerun/vim-polyglot"
  "ap/vim-css-color"
  "neovim/nvim-lspconfig"
  "hrsh7th/nvim-cmp"
  "hrsh7th/cmp-nvim-lsp"
  "hrsh7th/cmp-buffer"
  "hrsh7th/cmp-path"
  "hrsh7th/cmp-cmdline"
  "L3MON4D3/LuaSnip"
  "saadparwaiz1/cmp_luasnip"
  "nvim-treesitter/nvim-treesitter"
  ;;"HiPhish/nvim-ts-rainbow2"
  {1 "HiPhish/rainbow-delimiters.nvim"
   :submodules false}
  "wlangstroth/vim-racket"
  "kien/rainbow_parentheses.vim"
  "lluchs/vim-wren"
  "jaawerth/fennel.vim"
  "Olical/conjure"
  "tpope/vim-dispatch"
  "clojure-vim/vim-jack-in"
  "radenling/vim-dispatch-neovim"
  "Olical/aniseed"
  "janet-lang/janet.vim"
  "vim-ruby/vim-ruby"
  "hylang/vim-hy"
  ;;"monkoose/parsley"
  ;;"monkoose/nvlime"
  "vlime/vlime"
  ;;"HiPhish/nvim-cmp-vlime"
  "krischik/vim-ada"
  "udalov/kotlin-vim"
  "stefanos82/nelua.vim"
  ;;"mfussenegger/nvim-jdtls"
  ;;"alaviss/nim.nvim"
  {1 "scalameta/nvim-metals"
   :dependencies ["nvim-lua/plenary.nvim"]
   :ft [:scala :sbt]
   :opts
   (lambda []
     (local metals-config ((. (require :metals) :bare_config)))
     (set metals-config.on_attach (lambda [_ _] nil))
     metals-config)
   :config 
   (lambda [self metals-config]
     (local nvim-metals-group
       (vim.api.nvim_create_augroup :nvim-metals {:clear true}))
     (vim.api.nvim_create_autocmd
       :FileType
       {:pattern self.ft
        :callback
        (lambda []
          ((. (require :metals) :initialize_or_attach) metals-config))
       :group nvim-metals-group}))}
  {1 "lukas-reineke/indent-blankline.nvim"
   :main :ibl
   :opts {}}])
