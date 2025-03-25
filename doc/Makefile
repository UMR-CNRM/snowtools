## Makefile for Sphinx documentation
##

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = python3 -m sphinx
SOURCEDIR     = source
BUILDDIR      = build
PYTHON        = python3


# Put it first so that "make" without argument is like "make help".
#help:
#	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

help:  ## Show this help message
	@egrep -h '^##\s' $(MAKEFILE_LIST) | awk 'BEGIN {FS = "## "}; {printf "%s\n", $$2}'
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m %-20s\033[0m %s\n", $$1, $$2}'

.PHONY: help Makefile

#######################################################################
# Auto-generation of documentation of scripts listed in scripts_list  #
#######################################################################
SCRIPTHELPFILE  = ./scripts_list.txt
SCRIPTHELP_LIST = $(shell cut -d ' ' -f 1 $(SCRIPTHELPFILE))
SCRIPTHELP_DONE = $(patsubst %.py, $(SCRIPTSHELPDESTDIR)%.rst,$(subst /,--,$(SCRIPTHELP_LIST)))
GENSCRIPTHELP   = bash scripts/genscripthelp.sh
SCRIPTSHELPDESTDIR = source/code/autoscripts/
SCRIPTHELPSRCDIR   = ../snowtools/

clean-scripthelp:
	rm -rf $(SCRIPTSHELPDESTDIR)/*

scripthelp: $(SCRIPTHELP_DONE)

$(SCRIPTSHELPDESTDIR)%.rst: $(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, $(SCRIPTHELPSRCDIR)%.py, $@))
	$(info Generate documentation for script $(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, %.py, $@)))
	@# Create directory if not exist
	@mkdir -p $(SCRIPTSHELPDESTDIR)
	@# Introduction
	@echo "Script \`\`$(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst,%.py,$@))\`\` help:\n\n" > $@
	@echo ".. code-block:: none\n" >> $@
	@# Get -h, remove vortex import messages and indent correctly for ResT
	@$(PYTHON) $(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, $(SCRIPTHELPSRCDIR)%.py, $@)) -h | \
	    sed -e 's/^ *Vortex.*\(loaded\|completed\) (.*, at [0-9:]* )$$//' | sed -e 's/^/   /' >> $@
	@echo "\n\n" >> $@

s2mresearch: $(SCRIPTHELPSRCDIR)/tasks/s2m_command.py
	$(info Generate documentation for s2m research command)
	@# Create directory if not exist
	@mkdir -p $(SCRIPTSHELPDESTDIR)
	@# Introduction
	@echo "s2m command help:\n\n" > $(SCRIPTSHELPDESTDIR)/s2mresearch.rst
	@echo ".. code-block:: none\n" >> $(SCRIPTSHELPDESTDIR)/s2mresearch.rst
	@# Get -h, remove vortex import messages and indent correctly for ResT
	@$(PYTHON) $< research -h | \
	    sed -e 's/^ *Vortex.*\(loaded\|completed\) (.*, at [0-9:]* )$$//' | \
	    sed -e 's/^SNOWTOOLS Directory .*$$//' | sed -e 's/^Snowtools installation has been successfully checked.$$//' | \
	    sed -e 's/^/   /' >> $(SCRIPTSHELPDESTDIR)/s2mresearch.rst
	@echo "\n\n" >> $(SCRIPTSHELPDESTDIR)/s2mresearch.rst



#######################################################################
#                           Sphinx targets                            #
#######################################################################
# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#%: Makefile
#	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

html: scripthelp s2mresearch  ## Generate HTML documentation
	$(SPHINXBUILD) -b html $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/html
	@echo "Build done in $(BUILDDIR)/html"

pdf: scripthelp s2mresearch   ## Generate a PDF documentation (with pdflatex)
	$(SPHINXBUILD) -M latexpdf $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/pdf
	@echo "Build done in $(BUILDDIR)/pdf"

latex: scripthelp s2mresearch ## Generate a latex documentation (do not compile it)
	$(SPHINXBUILD) -b latex $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/latex
	@echo "Build done in $(BUILDDIR)/latex"

man: scripthelp s2mresearch   ## Generate a unix man format documentation
	$(SPHINXBUILD) -b man $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/man
	@echo "Build done in $(BUILDDIR)/man"

#######################################################################
#                                Clean                                #
#######################################################################
clean: clean-scripthelp  ## Clean the all build directories
	rm -rf $(BUILDDIR)/html
	rm -rf $(BUILDDIR)/latex
	rm -rf $(BUILDDIR)/pdf
	rm -rf $(BUILDDIR)/man
