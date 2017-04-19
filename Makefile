CLIENT_SOURCES = $(wildcard src/*.elm) 
CLIENT_TARGET = public/scripts/app.js 
CLIENT_DEV_TARGET = public/scripts/app-debug.js 
DIST_SOURCES = $(wildcard public/*) $(wildcard public/*/*)

dev: $(CLIENT_DEV_TARGET)

build: $(CLIENT_TARGET)

$(CLIENT_TARGET): $(CLIENT_SOURCES)
	elm make src/Main.elm --output $@

$(CLIENT_DEV_TARGET): $(CLIENT_SOURCES)
	elm make src/Main.elm --debug --output $@

.PHONY: dev build

