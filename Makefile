# revision-hexdump --- an editable hex editor window for the revision framework.
#
# Assumes the sibling checkout ../revision exists.  The test suite uses FiveAM,
# which revision vendors under ../revision/systems (populate it with `ocicl
# install' in ../revision if it is missing -- see the CI workflow).

SBCL ?= sbcl
LOAD := --load setup.lisp

.PHONY: all build test run clean

all: build

# Compile + load the whole system (a build check).
build:
	$(SBCL) --non-interactive $(LOAD) \
	  --eval '(asdf:load-system :revision-hexdump)' \
	  --eval '(format t "~&BUILD OK~%")'

# Headless FiveAM suite: column geometry, buffer load/edit/save, movement,
# scrolling, and the window wiring.  No terminal required.
test:
	$(SBCL) --script tests/tests.lisp

# Open a file in the hex editor full-screen:  make run FILE=/bin/ls
FILE ?=
run:
	$(SBCL) $(LOAD) \
	  --eval '(asdf:load-system :revision-hexdump)' \
	  --eval '(revision-hexdump:run-hexdump $(if $(FILE),"$(FILE)",nil))'

clean:
	rm -rf ~/.cache/common-lisp/*revision-hexdump* 2>/dev/null || true
