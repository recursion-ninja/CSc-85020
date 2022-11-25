#######
###
#   Environmental Constants
###
#######

SHELL := /bin/sh
COMMA := ,
EMPTY :=
SPACE := $(EMPTY) $(EMPTY)
define NEWLINE

$(EMPTY)
endef

extension-figure      := png
extension-latex       := tex
extension-markdown    := md
extension-portabledoc := pdf
extension-postscript  := ps

row-delimiter := $(SPACE)\$(NEWLINE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)

#######
###
#   Variables for project build targets
###
#######

project-classname  := CSc-85020-Project
project-cleanup    := Cleaning-CSc-85020-Project
project-prefix     := Washburn-$(project-classname)

project-dir-assets := ./assets/
project-dir-target := ./doc/
project-dir-source := ./txt/

# PDF document filepaths
pitch-name        := $(project-prefix)-Proposal-Slides
pitch-filename    := proposal-slides.$(extension-markdown)
pitch-filepath    := $(project-dir-source)$(pitch-filename)
pitch-output      := $(project-dir-target)$(pitch-name)
pitch-latex       := $(pitch-output).$(extension-latex)
pitch-target      := $(pitch-output).$(extension-portabledoc)

proposal-name     := $(project-prefix)-Proposal
proposal-filename := proposal-paper.$(extension-markdown)
proposal-filepath := $(project-dir-source)$(proposal-filename)
proposal-output   := $(project-dir-target)$(proposal-name)
proposal-latex    := $(proposal-output).$(extension-latex)
proposal-target   := $(proposal-output).$(extension-portabledoc)

report-name       := $(project-prefix)-Report
report-filename   := report-paper.$(extension-markdown)
report-filepath   := $(project-dir-source)$(report-filename)
report-output     := $(project-dir-target)$(report-name)
report-latex      := $(report-output).$(extension-latex)
report-target     := $(report-output).$(extension-portabledoc)

slides-name       := $(project-prefix)-Report-Slides
slides-filename   := report-slides.$(extension-markdown)
slides-filepath   := $(project-dir-source)$(slides-filename)
slides-output     := $(project-dir-target)$(slides-name)
slides-latex      := $(slides-output).$(extension-latex)
slides-target     := $(slides-output).$(extension-portabledoc)

all-named-targets := $(sort $(basename \
  $(pitch-target) \
  $(proposal-target) \
  $(report-target) \
  $(slides-target) \
))

#######
###
#   Variables for pandoc invocations
###
#######

pandoc-flag-list-papers = \
  --dpi=300 \
  --citeproc \
  --from=markdown \
  --listings \
  --pdf-engine=lualatex \
  --shift-heading-level=0 \
  --standalone \
  --to=latex \
  --variable=fontsize:11pt \
  --variable=institute:"Graduate Center, CUNY" \
  --variable=lang:en-US \
  --variable=linkstyle:bold \

pandoc-flag-list-slides = \
  --columns=50 \
  --dpi=300 \
  --citeproc \
  --from=markdown \
  --listings \
  --pdf-engine=lualatex \
  --shift-heading-level=0 \
  --slide-level=2 \
  --standalone \
  --to=beamer \
  --toc \
  --variable=aspectratio:169 \
  --variable=classoption:"aspectratio=169" \
  --variable=colortheme:"orchid" \
  --variable=fontsize:11pt \
  --variable=institute:"Graduate Center, CUNY" \
  --variable=lang:en-US \
  --variable=linkstyle:bold \
  --variable=section-titles:false \
  --variable=theme:"Rochester" \
  --variable=toc:true

pandoc-options-papers := $(subst $(SPACE),$(row-delimiter),$(pandoc-flag-list-papers))
pandoc-options-slides := $(subst $(SPACE),$(row-delimiter),$(pandoc-flag-list-slides))

#######
###
#   Variables for project build artifacts
###
#######
latexmk_artifact_exts = $(sort aux fdb_latexmk fls log toc acn glg loc nav snm soc synctex.gz thm vrb $(extension-latex))
latexmk_cleanall_exts = $(sort \
  $(latexmk_handled_exts) \
  $(latexmk_artifact_exts) \
  $(extension-latex) \
  $(extension-portabledoc) \
  )

latexmk_artifact_list = \
  $(subst $(SPACE),$(row-delimiter),\
  $(sort \
  $(strip \
  $(foreach ext,$(latexmk_artifact_exts),\
  $(foreach name,$(all-named-targets),\
  $(wildcard $(name).$(ext))\
  )))))

latexmk_cleanall_list = \
  $(subst $(SPACE),$(row-delimiter),\
  $(sort \
  $(strip \
  $(foreach ext,$(latexmk_cleanall_exts),\
  $(foreach name,$(all-named-targets),\
  $(wildcard $(name).$(ext))\
  )))))

#######
###
#   Standard Targets
###
#######

.PHONY:: all clean install installdirs pdf

all:: $(pitch-target) $(proposal-target) $(report-target)

clean:: $(project-cleanup)

install:: $(pitch-target)

installdirs:: $(dir $(pitch-target))

pdf:: $(pitch-target)

#######
###
#   Phony targets
###
#######

.PHONY:: $(project-cleanup)

$(project-cleanup):
	-rm -f $(latexmk_cleanall_list)
	$(if $(wildcard    $(pitch-latex) ),$(shell rm -f    $(pitch-latex) ))
	$(if $(wildcard    $(pitch-target)),$(shell rm -f    $(pitch-target)))
	$(if $(wildcard $(proposal-latex) ),$(shell rm -f $(proposal-latex) ))
	$(if $(wildcard $(proposal-target)),$(shell rm -f $(proposal-target)))
	$(if $(wildcard   $(report-latex) ),$(shell rm -f   $(report-latex) ))
	$(if $(wildcard   $(report-latex) ),$(shell rm -f   $(report-target)))

#######
###
#   Build Targets
###
#######
.INTERMEDIATE: $(pitch-latex) $(proposal-latex) $(report-latex)

$(dir $(pitch-target)):
	mkdir -p $(dir $@)

$(pitch-latex): $(pitch-filepath)
	pandoc \
	  $(pandoc-options-slides) \
	  --output=$@ \
	  $<
	sed -i 's/<\.->/<\.->\[frame\]/g' $@

$(pitch-target): $(pitch-latex) $(dir $(pitch-target))
	latexmk    -jobname=$(basename $(@F)) -output-directory=$(@D) -pdf $<
	-rm -f $(latexmk_artifact_list)

$(proposal-latex): $(proposal-filepath)
	pandoc \
	  $(pandoc-options-papers) \
	  --output=$@ \
	  $<

$(proposal-target): $(proposal-latex) $(dir $@)
	latexmk    -jobname=$(basename $(@F)) -output-directory=$(@D) -pdf $<
	-rm -f $(latexmk_artifact_list)

$(report-latex): $(report-filepath)
	pandoc \
	  $(pandoc-options-papers) \
	  --output=$@ \
	  $<

$(report-target): $(report-latex) $(dir $@)
	latexmk    -jobname=$(basename $(@F)) -output-directory=$(@D) -pdf $<
	-rm -f $(latexmk_artifact_list)

