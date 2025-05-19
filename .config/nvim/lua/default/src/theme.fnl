;; theme.fnl -> theme.lua
;; Handles colours, theming, and lualine

(vim.cmd "set colorcolumn=50,80,100")
(vim.cmd "set cursorline")
(vim.cmd "set cursorcolumn")

((. (require :transparent) :setup) {})
;;(vim.cmd "TransparentEnable")

((. (require :colorizer) :setup))

(local coloursets
	{:white
	 {:grad			 ["#e1e1e1"
								"#c4c4c4"
								"#9a9a9a"
								"#737373"
								"#6d6d6d"
								"#505050"
								"#444444"
								"#353535"
								"#2d2d2d"
								"#242424"
								"#1d1d1d"
								"#121212"]
		:error      "#ffad94"
		:errorlight "#e6ff94"
		:warn       "#94ffe3"
		:warnlight  "#94b1ff"
		:transparent :off}
	 :pink
	 {:grad			 ["#e18bbe"
								"#c474a4"
								"#aa598a"
								"#934674"
								"#7d3560"
								"#702e55"
								"#5c2144"
								"#4d1736"
								"#41112c"
								"#2d1523"
								"#1d0c16"
								"#12090f"]
		:error      "#eb5a8a"
		:errorlight "#eb735a"
		:warn       "#3d598a"
		:warnlight  "#6e3d8a"
		:transparent :on}})
	
(var c (. coloursets :pink))

;; setup command
(fn setup-colorscheme [cset]
	(when (not (. coloursets cset))
		(print "Colourset doesn't exist")
		(lua "return"))

	(set c (. coloursets cset))

	;; catppuccin
	((. (require :catppuccin) :setup)
	 {:integrations {:ts_rainbow true}
		:color_overrides
		{:all
		 {:text		  (. c.grad 1)
			:subtext1 (. c.grad 2)
			:subtext0 (. c.grad 3)
			:overlay2 (. c.grad 4)
			:overlay1 (. c.grad 5)
			:overlay0 (. c.grad 6)
			:surface2 (. c.grad 7)
			:surface1 (. c.grad 8)
			:surface0 (. c.grad 9)
			:base		  (. c.grad 10)
			:mantle   (. c.grad 11)
			:crust    (. c.grad 12)}}})

	;; ibl
	(local iblhighlight
		[:RainbowLight :RainbowMid :RainbowDark])
	
	(local iblhooks (require "ibl.hooks"))
	
	(iblhooks.register
		iblhooks.type.HIGHLIGHT_SETUP
		(lambda []
			(vim.api.nvim_set_hl 0 :RainbowLight {:fg (. c.grad 5)})
			(vim.api.nvim_set_hl 0 :RainbowMid   {:fg (. c.grad 6)})
			(vim.api.nvim_set_hl 0 :RainbowDark  {:fg (. c.grad 7)})))

	((. (require :ibl) :setup)
	 {:scope {:enabled false}
		:indent {:highlight iblhighlight}})
	
	(vim.cmd "colorscheme catppuccin")
	
	(local theme
		{:normal
		 {:a {:fg (. c.grad 1)  :bg (. c.grad 10)}
			:b {:fg (. c.grad 1)  :bg (. c.grad 9)}
			:c {:fg (. c.grad 10) :bg (. c.grad 1)}
			:z {:fg (. c.grad 1)  :bg (. c.grad 10)}}
		 :insert  {:a {:fg (. c.grad 1) :bg (. c.grad 10)}}
		 :visual  {:a {:fg (. c.grad 1) :bg (. c.grad 10)}}
		 :replace {:a {:fg (. c.grad 1) :bg (. c.grad 10)}}})
	
	(fn search-result []
		(when (= vim.v.hlsearch 0)
			(lua "return ''"))
		(local last-search (vim.fn.getreg "/"))
		(when (not (or last-search
									 (= last-search "")))
			(lua "return ''"))
		(local search-count
			(vim.fn.searchcount {:maxcount 9999}))
		(.. last-search
				"("
				search-count.current
				"/"
				search-count.total))
	
	(fn modified []
		(if
			vim.bo.modified
			"MODIFIED"
			;;elseif
			(or (= vim.bo.modifiable false)
					(= vim.bo.readonly   true))
			"-"
			""))

	;; lualine
	((. (require :lualine) :setup)
	 {:options
		{:theme							   theme
		 :component_separators ""
		 :section_separators   ""
		 :disabled_filetypes   {:statusline []
														:winbar []}
		 :ignore_focus         {}
		 :always_divide_middle true
		 :always_show_tabline  true
		 :globalstatus				 false
		 :refresh							 {:statusline 100
														:tabline 100
														:winbar 100}
		 }
		:sections
		{:lualine_a [:mode]
		 :lualine_b
			[:branch
			 :diff
			 {1 :diagnostics
				:source            [:nvim]
				:sections          [:error]
				:diagnostics_color {:error {:bg c.errorlight
																		:fg (. c.grad 9)}}}
			 {1 :diagnostics
				:source            [:nvim]
				:sections          [:warn]
				:diagnostics_color {:warn {:bg c.warnlight
																		:fg c.errorlight}}}
			 {1 :filename
				:file_status false
				:path 1}
			 {1 modified
				:color {:fg (. c.grad 9) :bg c.errorlight}}
			 {1 "%s"
				:cond (lambda [] vim.wo.previewwindow)}
			 {1 "%r"
				:cond (lambda [] vim.bo.readonly)}
			 {1 "%q"
				:cond (lambda [] (= vim.bo.buftype :quickfix))}]
		 :lualine_c []
		 :lualine_x []
		 :lualine_y [search-result :filetype]
		 :lualine_z ["%l:%c" "%p%%/%L"]}
		:inactive_sections {:lualine_c ["%f %y %m"]
												:lualine_x []}
		:tabline         []
		:winbar          []
		:inactive_winbar []
		:extensions      []})

	;; NOTE: to self:
	;; transparent.nvim uses the following groups
	;; to set as transparent:
	;; 'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier',
	;; 'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function',
	;; 'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText',
	;; 'SignColumn', 'CursorLine', 'CursorLineNr', 'StatusLine', 'StatusLineNC',
	;; 'EndOfBuffer',
	(if (= c.transparent :on)
				(do ;; see note above
					(vim.cmd ":highlight Normal guibg=none")
					(vim.cmd ":highlight NonText guibg=none"))
			(= c.transparent :plugin)
				(vim.cmd "TransparentEnable")
		  (= c.transparent :off)
				(vim.cmd "TransparentDisable")
			nil)

	;; rainbow
	(local rainbow-delimiters (require :rainbow-delimiters))

	(vim.api.nvim_set_hl
	  0	"RDelimOne"
		{:fg   c.error
		 :ctermfg "White"})
	(vim.api.nvim_set_hl
	  0	"RDelimTwo"
		{:fg   c.warn
		 :ctermfg "White"})
	(vim.api.nvim_set_hl
	  0	"RDelimThree"
		{:fg   c.errorlight
		 :ctermfg "White"})
	(vim.api.nvim_set_hl
		0 "RDelimFour"
		{:fg   c.warnlight
		 :ctermfg "White"})

	((. (require "rainbow-delimiters.setup") :setup)
	 {:ensure_installed [:all]
		:strategy  {""   (. rainbow-delimiters.strategy :global)
							  :vim (. rainbow-delimiters.strategy :local)}
		:query     {""   :rainbow-delimiters
							  :lua :rainbow-blocks}
		:highlight [:RDelimOne
								:RDelimTwo
								:RDelimThree
								:RDelimFour]})

	;; HACK:
	;; ensures delimiters are highlighted in real-time
	;; temporary fix? (this has been here for a while)
	(vim.api.nvim_create_autocmd
		"BufRead"
		{:desc "Ensure treesitter is initialized?"
		 :callback (lambda [] (pcall vim.treesitter.start))})
	
	(vim.api.nvim_create_autocmd
		[:BufNewFile :BufRead]
		{:pattern "*.hx"
		 :callback (lambda []
								 (vim.cmd "RainbowParenthesesToggleAll"))})
	
	(vim.api.nvim_create_autocmd
		[:BufNewFile :BufRead]
		{:pattern "*.wren"
		 :callback (lambda []
								 (vim.cmd "RainbowParenthesesToggleAll"))})
	
	(set vim.g.rbpt_colorpairs
		[["brown"       "cyan"]
	   ["Darkblue"    "darkmagenta"]
	   ["darkgray"    "magenta"]
	   ["darkgreen"   "darkcyan"]
	   ["darkcyan"    "cyan"]
	   ["darkred"     "darkmagenta"]
	   ["darkmagenta" "magenta"]
	   ["brown"       "darkcyan"]
	   ["gray"        "cyan"]
	   ["black"       "darkmagenta"]
	   ["darkmagenta" "magenta"]
	   ["Darkblue"    "darkcyan"]
	   ["darkgreen"   "cyan"]
	   ["darkcyan"    "darkmagenta"]
	   ["darkred"     "magenta"]
	   ["red"         "darkcyan"]]))

(setup-colorscheme :pink)

(vim.api.nvim_create_user_command "Colourset"
	(lambda [opts]
		(setup-colorscheme opts.args))
	{:nargs 1
	 :complete (lambda [_ _]
		(local l [])
		(each [k _ (pairs coloursets)]
			(table.insert l k))
		l)})

(set vim.opt.termguicolors true)
