ALL_LUA=config_util.lua

.PHONY: all
all: $(ALL_LUA)
	pushd default && make && popd

.PHONY: clean
clean:
	rm ./*.lua

%.lua: src/%.fnl
	fennel --compile $< > $@
%.lua: src/%.lua
	cp $< $@
