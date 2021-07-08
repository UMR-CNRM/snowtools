# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = python3 -m sphinx
SOURCEDIR     = source
BUILDDIR      = build
PYTHON        = python3


# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

#######################################################################
# Auto-generation of documentation of scripts listed in scripts_list  #
#######################################################################
SCRIPTHELPFILE  = ./scripts_list.txt
SCRIPTHELP_LIST = $(shell cut -d ' ' -f 1 $(SCRIPTHELPFILE))
SCRIPTHELP_DONE = $(patsubst %.py, $(SCRIPTSHELPDESTDIR)%.rst,$(subst /,--,$(SCRIPTHELP_LIST)))
GENSCRIPTHELP   = bash scripts/genscripthelp.sh
SCRIPTSHELPDESTDIR = source/code/autoscripts/
SCRIPTHELPSRCDIR   = ../

clean-scripthelp:
	rm -rf $(SCRIPTSHELPDESTDIR)/*

scripthelp: $(SCRIPTHELP_DONE)

$(SCRIPTSHELPDESTDIR)%.rst: $(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, $(SCRIPTHELPSRCDIR)%.py, $@))
	$(info Generate documentation for script $(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, %.py, $@)))
	@# Create directory if not exist
	@mkdir -p $(SCRIPTSHELPDESTDIR)
	@# Introduction
	@echo -e "Script \`\`$(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, %.py, $@))\`\` help:\n\n" > $@
	@echo -e ".. code-block:: none\n" >> $@
	@# Get -h, remove vortex import messages and indent correctly for ResT
	@$(PYTHON) $(subst --,/,$(patsubst $(SCRIPTSHELPDESTDIR)%.rst, $(SCRIPTHELPSRCDIR)%.py, $@)) -h | \
	    sed -e 's/^ *Vortex.*\(loaded\|completed\) (.*, at [0-9:]* )$$//' | sed -e 's/^/   /' >> $@
	@echo -e "\n\n" >> $@


#######################################################################
#                           Sphinx targets                            #
#######################################################################
# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#%: Makefile
#	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

html: scripthelp
	$(SPHINXBUILD) -b html $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/html

pdf: scripthelp
	$(SPHINXBUILD) -b latexpdf $(SPHINXOPTS) $(SOURCEDIR) $(BUILDDIR)/pdf

#######################################################################
#                                Clean                                #
#######################################################################
clean: clean-scripthelp
	rm -rf $(BUILDDIR)/html
	rm -rf $(BUILDDIR)/latex
