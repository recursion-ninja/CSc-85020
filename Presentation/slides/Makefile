#######
###
#   Environmental Constants
###
#######

.ONESHELL:
.DEFAULT:;
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

row-delimiter := $(SPACE)\$(NEWLINE)$(SPACE)$(SPACE)$(SPACE)$(SPACE)

resource-filepath := $(dir-slides-source)assets/

# Thesis PDF document file path
presentation-name   := Washburn-CSc-85020-Presentation
presentation-latex  := $(presentation-name).$(extension-latex)
presentation-target := $(presentation-name).$(extension-portabledoc)

slides-filename := slides.md
slides-filepath := $(dir-slides-source)$(slides-filename)

latexmk-flag-list = \
  -output-directory="$(dir $(presentation-target))"

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
  --variable=institute:"Hunter College, CUNY" \
  --variable=lang:en-US \
  --variable=linkstyle:bold \
  --variable=section-titles:false \
  --variable=theme:"Rochester" \
  --variable=toc:true

latexmk-options := $(subst $(SPACE),$(row-delimiter),$(latexmk-flag-list))
pandoc-options  := $(subst $(SPACE),$(row-delimiter),$(pandoc-flag-list))

artifact-list := \
  $(addprefix $(presentation-name).,nav snm) \
  $(presentation-target) \
  $(presentation-latex)
artifact-rows := $(subst $(SPACE),$(row-delimiter),$(artifact-list))


#######
###
#   Standard Targets
###
#######

.PHONY: all clean distclean install installdirs pdf

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
#	( cd $(dir $(presentation-latex)); latexmk -CA; )
	-rm -f$(row-delimiter)$(artifact-rows)

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
	( latexmk \
	    -pdf \
	    $< ; \
	  cp \
	    $(subst $(extension-latex),$(extension-portabledoc),$(presentation-latex)) \
	    $@ ; \
	)

#$(presentation-target): $(slides-filepath) $(dir $(presentation-target))
#	( cd $(dir-slides-source); \
	pandoc \
	  $(pandoc-options) \
	  --output=$@ \
	  $< ; \
	)

