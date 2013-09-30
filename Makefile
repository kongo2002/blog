all: rebuild publish

rebuild:
	./site rebuild

build:
	./site build

serve:
	./site preview

publish:
	rsync -a _site/ web2544@494627.server.adminflex.de:/home/web2544/www/
	@echo "Done."
