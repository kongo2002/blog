all: rebuild publish

site: site.hs
	@if [ -d ".cabal-sandbox" ]; then \
		ghc -package-db $(wildcard .cabal-sandbox/*packages.conf.d) --make $<; \
	else \
		ghc --make $<; \
	fi

rebuild: site
	./site rebuild

build: site
	./site build

serve: site
	./site watch

publish:
	rsync -a _site/ web2544@494627.server.adminflex.de:/home/web2544/www/
	@echo "Done."
