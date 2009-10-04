TOOLS_DIR=$(HOME)/tools

TOOLSET=sb ci co locks sbdif sbmkdir sbcreat sbnote

all:: sb sbgw

sb: client.c
	cc -O2 client.c -o sb

sbgw: sbcgi.c util.c
	cc -O2 sbcgi.c util.c -o sbgw

clean::
	rm -f sbgw sb

tools:: $(TOOLSET)
	install -m755 $(TOOLSET) $(TOOLS_DIR)

install::
	cp -p sb $(HOME)/bin/sb
	cp -p sbgw /usr/local/www/cgi-bin/sbgw
