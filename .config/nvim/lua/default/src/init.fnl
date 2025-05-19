(vim.cmd "set nocompatible")
(set vim.g.polyglot_disabled ["ada"])

(local config-name (require :current_config))

(require (.. config-name ".lua.pkg"))
(require (.. config-name ".lua.key"))
(require (.. config-name ".lua.completion"))
;;(require (.. config-name ".lua.rainbow"))
(require (.. config-name ".lua.conjure"))
(require (.. config-name ".lua.treesitter"))

(fn setn [tbl kvs]
	(each [k v (pairs kvs)]
		(tset tbl k v)))

(local s vim.opt)
(setn s
  {:relativenumber true
   :modifiable     true
   :autoindent     true
   :tabstop        2
   :shiftwidth     2
   :expandtab      true
   :smarttab       true
   :softtabstop    2
   :mouse          :a
   :background     :dark
   :termguicolors  true
   :compatible     false})
;; TODO: fix indentation in fennel
;; it seems to ignore the above config
;; due to some plugin and use regular tabs

;; Java will error with built-in syntax hl
(vim.cmd "syntax off")
(vim.api.nvim_create_autocmd
	[:BufNewFile :BufRead]
	{:callback (lambda []
		(when (~= vim.bo.filetype :java)
			(vim.cmd "syntax on")))})

(fn recognise_filetype [pattern filetype]
	(vim.api.nvim_create_autocmd
		[:BufnewFile :BufRead]
		{:pattern pattern
		 :command (.. "set filetype=" filetype)}))

(recognise_filetype "*.janet" "janet")
(recognise_filetype "*.hy"    "hy")
(recognise_filetype "*.asd"   "lisp")
(recognise_filetype "*.v"     "v")

;; theme setup
(require (.. config-name ".lua.theme"))

;; nvim-tree
(set vim.g.loaded_netrw 1)
(set vim.g.loaded_netrwPlugin 1)

;; thanks to https://github.com/MarioCarrion/videos/blob/269956e913b76e6bb4ed790e4b5d25255cb1db4f/2023/01/nvim/lua/plugins/nvim-tree.lua 
(local WIDTH-RATIO 0.8)
(local HEIGHT-RATIO 0.8)
(fn open-win-config []
  (local screen-w (vim.opt.columns:get))
  (local screen-h (- (vim.opt.lines:get)
                     (vim.opt.cmdheight:get)))
  (local window-w (* screen-w WIDTH-RATIO))
  (local window-h (* screen-h HEIGHT-RATIO))
  (local window-w-int (math.floor window-w))
  (local window-h-int (math.floor window-h))
  (local center-x (/ (- screen-w window-w) 2))
  (local center-y (- (/ (- (vim.opt.lines:get) window-h) 2)
                     (vim.opt.cmdheight:get)))
  {:relative :editor
   :border :rounded
   :row center-y
   :col center-x
   :width window-w-int
   :height window-h-int})

((. (require :nvim-tree) :setup)
 {:sort     {:sorter :case_sensitive}
	:view     {:relativenumber true
             :centralize_selection true
             :cursorline true
             :width 30
             :float {:enable true
                     :open_win_config open-win-config
                     :quit_on_focus_loss true}}
	:renderer {:group_empty false}
	:filters  {:dotfiles false}
  :update_cwd true})
