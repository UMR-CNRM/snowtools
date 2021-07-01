# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build-3
SOURCEDIR     = source
BUILDDIR      = build
PYHTON        = python3


# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Librarie indexes to be built...
SCRIPTHELPFILE  = ./scripts_list.txt
SCRIPTHELP_LIST = $(shell cut -d ' ' -f 1 $(SCRIPTHELPFILE))
GENSCRIPTHELP   = bash scripts/genscripthelp.sh
SCRIPTSHELPDESTDIR = source/code/autoscripts/
SCRIPTHELPSRCDIR   = ../

clean-scripthelp:
	rm -rf $(SCRIPTSHELPDESTDIR)/*

# Standard build for library indexes
gen-scripthelp:
	$(GENSCRIPTHELP) $(SCRIPTHELPFILE) $(SCRIPTHELPSRCDIR) $(SCRIPTSHELPDESTDIR)


# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#%: Makefile
#	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

html: gen-scripthelp
	$(SPHINXBUILD) -b html $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/html

pdf: gen-scripthelp
	$(SPHINXBUILD) -b pdf $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/pdf

clean: clean-scripthelp
	rm -rf $(BUILDDIR)
