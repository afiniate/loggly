# Copyright 2013 Afiniate All Rights Reserved.
#
# You can control some aspects of the build with next variables (e.g.
# make VARIABLE_NAME="some-value")
#
#  * PARALLEL_JOBS=N let ocamlbuild run N jobs in parallel. The recommended
#    value is the number of cores of your machine. Note that we don't want to
#    use make's own parallel flag (-j), as it will spawn several ocamlbuild jobs
#    competing with each other
#  * BUILD_FLAGS flags used to compile non production ocaml code (e.g tools,
#    tests ...)
#  * BUILD_PROD_FLAGS flags used to compile production ocaml code
#
# =============================================================================
# VARS
# =============================================================================
BUILD_DIR := $(CURDIR)/_build
LIB_DIR := $(BUILD_DIR)/src

PREFIX := /usr

### Knobs
PARALLEL_JOBS ?= 2
BUILD_FLAGS ?= -use-ocamlfind -cflags -bin-annot -lflags -g

# =============================================================================
# Descriptive Vars
# =============================================================================
NAME=loggly
LICENSE="OSI Approved :: Apache Software License v2.0"
AUTHOR="Afiniate, Inc."
HOMEPAGE="https://github.com/afiniate/loggly"

DEV_REPO="git@github.com:afiniate/loggly.git"
BUG_REPORTS="https://github.com/afiniate/loggly/issues"

DESC="Loggly is a path resolution library for ocaml"

BUILD_DEPS=vrt

# =============================================================================
# COMMANDS
# =============================================================================

BUILD := ocamlbuild -j $(PARALLEL_JOBS) -build-dir $(BUILD_DIR) $(BUILD_FLAGS)

# =============================================================================
# Rules to build the system
# =============================================================================

.PHONY: all build rebuild opam install

.PRECIOUS: %/.d

%/.d:
	mkdir -p $(@D)
	touch $@

all: build

rebuild: clean all

build:
	$(BUILD) $(NAME).cma $(NAME).cmx $(NAME).cmxa $(NAME).a $(NAME).cmxs

metadata:
	vrt prj make-opam \
	--homepage $(HOMEPAGE) \
	--dev-repo $(DEV_REPO) \
	--lib-dir $(LIB_DIR) \
	--license $(LICENSE) \
	--name $(NAME) \
	--author $(AUTHOR) \
	--maintainer $(AUTHOR) \
	--bug-reports $(BUG_REPORTS) \
	--build-cmd "make" \
	--install-cmd "make \"install\" \"PREFIX=%{prefix}%\"" \
	--remove-cmd "make \"remove\" \"PREFIX=%{prefix}%\"" \
	--build-depends $(BUILD_DEPS) \
	--desc $(DESC)

install:
	cd $(LIB_DIR); ocamlfind install $(NAME) META $(NAME).cmi $(NAME).cmo $(NAME).o \
	                                           $(NAME).cmx $(NAME).mli $(NAME).a \
	                                           $(NAME).cmxa

remove:
	ocamlfind remove $(NAME)

clean:
	rm -rf $(BUILD_DIR)