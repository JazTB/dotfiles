export class Vector2
  new: (x,y) =>
    @x = x
    @y = y
  format: =>
    "(#{@x}, #{@y})"
  __add: (other) =>
    @@(@x + other.x, @y + other.y)
  __sub: (other) =>
    @@(@x - other.x, @y - other.y)
  __div: (other) =>
    @@(@x / other.x, @y / other.y)
  __mul: (other) =>
    @@(@x * other.x, @y * other.y)
  __mod: (other) =>
    @@(@x % other.x, @y % other.y)

  __unm: =>
    @@(0 - @x, 0 - @y)
  __pow: (other) =>
    @@(@x ^ other.x, @y ^ other.y)
  __idiv: (other) =>
    @@(@x // other.x, @y // other.y)

  __eq: (other) =>
    (@x == other.x) and (@y == other.y)
  __lt: (other) =>
    (@x < other.x) and (@y < other.y)
  __le: (other) =>
    (@x <= other.x) and (@y <= other.y)

  __band: (other) =>
    @@(@x & other.x, @y & other.y)
  __bor: (other) =>
    @@(@x | other.x, @y | other.y)
  __bxor: (other) =>
    @@(@x ~ other.x, @y ~ other.y)
  __bnot: =>
    @@(~@x, ~@y)
  __shl: (other) =>
    @@(@x << other.x, @y << other.y)
  __shr: (other) =>
    @@(@x >> other.x, @y >> other.y)

export class Array
  new: =>
    @data = {}
  
  push: (item) =>
    table.insert(@data, item)
  pop: =>
    table.remove(@data, #@data)
  push_slice: (slice) =>
    for _,item in ipairs slice
      table.insert(@data, item)
  pop_slice: (n) =>
    out = {}
    for i = 1, n
      table.insert(out, table.remove(@data, #@data))
    out

  push_front: (item) =>
    table.insert(@data, 1, item)
  pop_front: =>
    table.remove(@data, 1)
  push_slice_front: (slice) =>
    i = #slice
    while i >= 1
      table.insert(@data, 1, slice[i])
      i -= 1
  pop_slice_front: (n) =>
    out = {}
    for i = 1, n
      table.insert(out, table.remove(@data, 1))
    out

  map: (cb) =>
    for k,v in ipairs @data
      @data[k] = cb(v)
  foreach: (cb) =>
    out = @@!
    for k,v in ipairs @data
      out[k] = cb(v)
    out
  filter: (cb) =>
    i = 1
    while i <= #@data
      if cb(@data[i])
        i += 1
      else
        table.remove(@data, i)
  fold: (initial, cb) =>
    accum = initial
    for _,v in ipairs @data
      accum = cb(accum, v)
    accum

export class Colour
  new: (r,g,b,a) =>
    @r = r
    @g = g
    @b = b
    @a = a
  format: (no_a) =>
    if no_a
      string.format "#%02x%02x%02x", @r, @g, @b
    else -- default
      string.format "#%02x%02x%02x%02x", @r, @g, @b, @a

return { :Vector2, :Array, :Colour }
