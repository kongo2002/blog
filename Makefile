all: rebuild publish

site: site.hs
	ghc --make site.hs

rebuild: site
	./site rebuild

build: site
	./site build

serve: site
	./site preview

publish:
	rsync -a _site/ web2544@494627.server.adminflex.de:/home/web2544/www/
	@echo "Done."
