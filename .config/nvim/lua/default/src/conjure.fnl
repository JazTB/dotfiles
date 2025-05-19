(fn set_keybindings []
    (vim.api.nvim_set_keymap "n" "k" ":ConjureEval<CR>" {:noremap true})
    (vim.api.nvim_set_keymap "v" "k" ":ConjureEval<CR>" {:noremap true}) ;; visual mode
    (vim.api.nvim_set_keymap "x" "k" ":ConjureEval<CR>" {:noremap true})) ;; visual block/line mode
;; clojure (requires vim-jack-in and its deps for opening the REPL)
(vim.api.nvim_create_autocmd
    ["FileType"]
    {:pattern :clojure
     :callback (lambda []
       (vim.cmd "tabnew") ;; when opening :Clj in an autocmd it splits instead of tabbing.
       (vim.cmd "Clj") ;; open our clojure repl
       (vim.cmd "tabfirst") ;; return to the file
       (set_keybindings)
       (vim.cmd "tabonly") ;; close the tabs opened by :Clj
       (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes "<esc>" true false true) "x" true))}) ;; escape insert mode
;; fennel (requires aniseed, works seamlessly)
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :fennel
     :callback set_keybindings})
;; racket (requires vim-racket for filetype)
;; make sure you have XREPL (extended repl, a racket library) by running ,? in a racket repl (you should have it)
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :racket
     :callback set_keybindings})
;; lua (requires nvim-treesitter - make sure to run :TSInstall lua)
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :lua
     :callback (lambda []
        (tset vim.g "conjure#extract#tree_sitter#enabled" true)
        (set_keybindings))})
;; python (requires nvim-treesitter - make sure to run :TSInstall python)
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :python
    :callback set_keybindings})
;; rust
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :rust
     :callback (lambda []
        (set_keybindings)
        (tset vim.g "conjure#extract#tree_sitter#enabled" true))})
(fn async_till_close [cmd] ;; run a shell command, but ensure it exits when nvim exits
    (local h (vim.fn.jobstart cmd {:detach true}))
    (vim.api.nvim_create_autocmd
        :VimLeave
        {:callback (lambda [] (when h (vim.fn.jobstop h)))}))
;; janet
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :janet
     :callback (lambda [] 
        (async_till_close "janet -e '(import spork/netrepl) (netrepl/server)'")
        (set_keybindings)
        (tset vim.g "conjure#extract#tree_sitter#enabled" true))})
;; hy
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :hy
     :callback (lambda []
        (set_keybindings)
        (tset vim.g "conjure#extract#tree_sitter#enabled" true))})
;; commonlisp
(vim.api.nvim_create_autocmd
    [:FileType]
    {:pattern :lisp
     :callback (lambda []
        (async_till_close "sbcl --eval \"(ql:quickload :swank)\"  --eval \"(swank:create-server :dont-close t)\"")
        (set_keybindings)
        (tset vim.g "conjure#extract#tree_sitter#enabled" true))})
