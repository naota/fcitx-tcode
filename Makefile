FCITX_DIR=$(HOME)/.config/fcitx
ADDON_DIR=$(FCITX_DIR)/addon
IM_DIR=$(FCITX_DIR)/inputmethod
FCITX_LIB_DIR=$(FCITX_DIR)/lib

all: build

build:
	cargo build --release

install: build
	mkdir -p "$(ADDON_DIR)"
	mkdir -p "$(IM_DIR)"
	mkdir -p "$(FCITX_LIB_DIR)"
	cp files/fcitx-tcode.conf $(ADDON_DIR)
	cp files/tcode.conf $(IM_DIR)
	cp target/release/libfcitx_tcode.so "$(FCITX_LIB_DIR)"/fcitx-tcode.so

test:
	cargo test

clean:
	cargo clean
