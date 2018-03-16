build:
	elm-make --yes --output dist/main.js src/Main.elm
	node main.js
