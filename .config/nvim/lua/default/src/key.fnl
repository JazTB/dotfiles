(fn normal_key [key fun]
    (vim.api.nvim_set_keymap "n" key fun {:noremap true}))

(fn terminal_key [key fun]
    (vim.api.nvim_set_keymap "t" key fun {:noremap true}))

(fn map [fun stuff]
    (each [_ v (ipairs stuff)]
        (fun (. v 1) (. v 2))))

(map normal_key
    [["<M-n>" ":NvimTreeToggle<CR>"]
     ["<C-n>" ":NvimTreeFocus<CR>"]
     ["<C-M-t>" ":TagbarToggle<CR>"]
     
     ["<C-t>" ":tabnew<CR>"]
     ["<M-t>" ":tabclose<CR>"]
     ;;["<C-d>" ":tab split<CR> <C-]>"]
     ["<C-d>" ":cd %:h<CR>"]
     
     ["<M-Right>" ":tabnext<CR>"]
     ["<M-Left>" ":tabprevious<CR>"]
     
     ["<C-x>" ":terminal<CR>i"]
     ["<C-q>" ":q<CR>"]
     ["<C-M-Q>" ":qa<CR>"]
     ["<C-Right>" "<C-w>l"]
     ["<C-Left>" "<C-w>h"]
     ["<C-Up>" "<C-w>k"]
     ["<C-Down>" "<C-w>j"]
     ["<C-M-Right>" ":vertical resize +1<CR>"]
     ["<C-M-Left>" ":vertical resize -1<CR>"]
     ["<C-M-Up>" ":resize -1<CR>"]
     ["<C-M-Down>" ":resize +1<CR>"]
     ["<M-=>" "<C-w>="]
     ["<C-s>" ":split<CR>"]
     ["<C-M-s>" ":vsplit<CR>"]])

(map terminal_key [["<Esc>" "<C-\\><C-n>"]])
