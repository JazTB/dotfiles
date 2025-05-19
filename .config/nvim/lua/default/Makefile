ALL_LUA=lua/completion.lua \
        lua/conjure.lua \
        lua/key.lua \
        lua/pkg.lua \
        lua/theme.lua \
				lua/treesitter.lua \
        lua/init.lua 

.PHONY: all
all: test lua $(ALL_LUA)

lua:
	mkdir -p ./lua

.PHONY: test
test:
	@pushd src/tests/ && lua ./init.lua && popd

.PHONY: clean
clean:
	rm ./lua/*.lua
	rm -r ./lua/

lua/%.lua: src/%.fnl
	fennel --compile $< > $@
lua/%.lua: src/%.moon
	moonc $< -o $@
lua/%.lua: src/%.tl
	tl gen $< -o $@
lua/%.lua: src/%.lua
	cp $< $@
