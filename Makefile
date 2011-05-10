OCAMLMAKEFILE=OCamlMakefile

SOURCES=castlecli.ml

RESULT=castle-cli

LIBS=
PACKS=acunu_utils castle

THREADS=threads
TRASH=*~ \\\#*
CFLAGS=-D_FILE_OFFSET_BITS=64 -I. -pthread
OCAMLFLAGS=-g -w Aez -warn-error Aez
OCAMLLDFLAGS=-g

all: target

target: native-code

install: target
	install -d                  $(BUILD_ROOT)/usr/bin
	install castle-cli          $(BUILD_ROOT)/usr/bin
	ln -sfT /usr/bin/castle-cli $(BUILD_ROOT)/opt/acunu/castle/bin/objClient


-include $(OCAMLMAKEFILE)
