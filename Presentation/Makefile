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

#######
###
#   Conditionl Redefinitions
###
#######

extension-figure      ?= png
extension-latex       ?= tex
extension-markdown    ?= md
extension-portabledoc ?= pdf
extension-postscript  ?= ps
dir-slides-source     ?= ./

#######
###
#   Variables for PRESENTATION
###
#######

row-delimiter := $(SPACE)\$(NEWLINE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)

resource-filepath := $(dir-slides-source)assets/

# Thesis PDF document file path
presentation-name   := Washburn-CSc-85020-Presentation
presentation-latex  := $(presentation-name).$(extension-latex)
presentation-target := $(presentation-name).$(extension-portabledoc)

slides-filename := slides.md
slides-filepath := $(dir-slides-source)$(slides-filename)

pandoc-flag-list = \
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

pandoc-options  := $(subst $(SPACE),$(row-delimiter),$(pandoc-flag-list))

latexmk_accounted_exts := aux fdb_latexmk fls log toc vrb
latexmk_leftover_exts  = $(sort acn glg loc nav snm soc synctex.gz thm $(extension-latex))
latexmk_leftover_list  = $(subst $(SPACE),$(row-delimiter),$(strip $(foreach ext,$(latexmk_leftover_exts),\
  $(wildcard $(presentation-name).$(ext)))))

artifact-exts := $(sort \
  $(latexmk_accounted_exts) \
  $(latexmk_leftover_exts) \
  $(extension-latex) \
  $(extension-portabledoc) \
  )

artifact-list := $(subst $(SPACE),$(row-delimiter),$(strip $(foreach ext,$(artifact-exts),\
  $(wildcard $(presentation-name).$(ext)))))

#######
###
#   Standard Targets
###
#######

.PHONY: all clean install installdirs pdf

all:: $(presentation-target)

clean:: presentation-clean

install:: $(presentation-target)

installdirs:: $(dir $(presentation-target))

pdf:: $(presentation-target)

#######
###
#   Phony targets
###
#######

.PHONY: presentation presentation-clean

presentation: $(presentation-target)

presentation-clean:
	-rm -f $(artifact-list)
	$(if $(wildcard $(presentation-target)),$(shell rm -f $(presentation-target)))

#######
###
#   Build Targets
###
#######

$(presentation-latex): $(slides-filepath)
	pandoc \
	  $(pandoc-options) \
	  --output=$@ \
	  $<
	sed -i 's/<\.->/<\.->\[frame\]/g' $@

$(presentation-target): $(presentation-latex)
	latexmk -pdf $<
	latexmk -c
	-rm -f $(latexmk_leftover_list)
