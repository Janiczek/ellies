.PHONY: dev
dev:
	elm make src/Main.elm
	firefox index.html

.PHONY: build
build:
	elm make --optimize src/Main.elm --output elm.js
	npx elm-minify elm.js
	rm elm.js
	mv elm.min.js dist/elm.js

.PHONY: deploy
deploy:
	dark --canvas janiczek-ellies dist
