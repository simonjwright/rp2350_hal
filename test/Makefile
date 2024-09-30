STYLE = --development
test.uf2: bin/test
	picotool uf2 convert -t elf $< $@

bin/test: force
	alr build $(STYLE) -- -XRP2350_LOADER=ROM

.PHONY: force
