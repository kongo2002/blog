all: rebuild publish

rebuild:
	./hakyll rebuild

build:
	./hakyll build

serve:
	./hakyll preview

publish:
	rsync -a _site/ web2544@494627.server.adminflex.de:/home/web2544/www/
	@echo "Done."
