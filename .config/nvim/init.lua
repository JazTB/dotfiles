vim.api.nvim_create_user_command('ReloadConfig', function()
  vim.cmd('source $MYVIMRC')
  print('Config reloaded')
end,{})

vim.api.nvim_create_user_command("Config", function(opts)
  local conf_name = opts.args
  local file_path = vim.fn.stdpath('config') .. '/lua/current_config.lua'

  local new_conf_dir = vim.fn.stdpath('config') .. '/lua/' .. conf_name
  if vim.fn.isdirectory(new_conf_dir) ~= 1 and vim.fn.filereadable(new_conf_dir..'.lua') ~= 1 then
    print('Config dir `'.. new_conf_dir ..'` doesn\'t exist')
    return
  end

  local f = io.open(file_path, 'w')
  if f then
    f:write('return "' .. conf_name .. '"')
    f:close()
    print('Success')
    vim.cmd('ReloadConfig')
  else
    print('Failure opening: ' .. file_path)
  end
end, {
  nargs = 1,
  complete = function(_,_)
    return vim.fn.getcompletion('', 'command')
  end
})

local current_config = require('current_config') -- for example "default"
require(current_config) -- Would require "default/init.lua"
