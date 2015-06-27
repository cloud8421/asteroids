.PHONY: dist

BUILD_ROOT = ./build
MAIN_FILE  = ./Main.elm
BUILD_FILE = $(BUILD_ROOT)/index.html

$(BUILD_ROOT):
	mkdir -p $@

dist: $(BUILD_ROOT)
	elm-make $(MAIN_FILE) --output=$(BUILD_FILE)
