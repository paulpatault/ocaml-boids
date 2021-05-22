all:
	dune build
	rm -rf boids.js
	cp _build/default/bin/main.bc.js boids.js

clean:
	dune clean
	rm -rf boids.js

run:
	open index.html


