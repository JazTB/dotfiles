local inspect = require("inspect");
local p = function(...) print(inspect(...)); end
local function sep() print("---"); end

local ms = require("moonscript");

---@class Vector2
---@field x      number
---@field y      number
---@field format fun(): string
---Supports all possible operators via metamethods

---@class Array
---@field data any[]
---
---@field push       fun(item: any)
---@field pop        fun(): any
---@field push_slice fun(slice: any[])
---@field pop_slice  fun(n: integer): any[]
---
---@field push_front       fun(item:any)
---@field pop_front        fun(): any
---@field push_slice_front fun(slice: any[])
---@field pop_slice_front  fun(): any[]
---
---@field map     fun(cb: fun(i: any): any)
---@field foreach fun(cb: fun(i: any)): any[] returns a table and doesn't modify
---@field filter  fun(cb: fun(i:any): boolean)
---@field fold    fun(initial: any, cb: fun(accum: any, i: any): any): any

---@class Colour
---@field r      number u8
---@field g      number u8
---@field b      number u8
---@field a      number u8
---@field format fun(no_a: boolean): string #rrggbbaa, `no_a` = no alpha

---@class types
---@field Vector2 Vector2
---@field Array   Array
---@field Colour  Colour
--- this does return Vector2, Array, Colour as namespaced
--- in a table, but they're also set as globals because
--- of `export` in moonscript :)
local types = ms.dofile("../types.moon");
local Vector2 = types.Vector2;
local Array   = types.Array;
local Colour  = types.Colour;

-- Vector2
local a = Vector2(69,420);
local b = Vector2(42069, 69420);
p((b + a):format());
p((b - a):format());
p((b * a):format());
p((b / a):format());
p((b ^ a):format());
p((b % a):format());

-- Array
local nums = Array();
nums:push_slice{3,4,5,6,7};
nums:foreach(function(i) p(i) end);
sep();
-- 3 4 5 6 7
nums:push_slice_front{1,2};
nums:foreach(function(i) p(i) end);
sep();
-- 1 2 3 4 5 6 7
nums:push(8);
nums:foreach(function(i) p(i) end);
sep();
-- 1 2 3 4 5 6 7 8
nums:push_front(0);
nums:foreach(function(i) p(i) end);
sep();
-- 0 1 2 3 4 5 6 7 8
nums:map(function(i) return i + 1; end);
nums:foreach(function(i) p(i) end);
sep();
-- 1 2 3 4 5 6 7 8 9
nums:filter(function(i) return (i % 2) == 0; end);
nums:foreach(function(i) p(i) end);
sep();
-- 2 4 6 8
local res = nums:fold(0, function(accum, item)
  return accum + item;
end);
p(res);
sep();
-- 20

-- Colour
local white = Colour(255,255,255,255);
local grey  = Colour(127,127,127,255);
local c     = Colour(31, 63, 127,255);
p(white:format());
p(white:format(true));
p(grey:format());
p(grey:format(true));
p(c:format());
p(c:format(true));
