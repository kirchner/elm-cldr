generate:
	elm-make --output dist/elm.js src/Generator.elm
	node generate.js
