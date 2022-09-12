prefix   = A1

student  = Washburn
ext      = tex
out      = pdf
name     = $(prefix)-$(student)
source   = $(name).$(ext)
target   = $(name).$(out)

all: $(target)

$(target): $(source)
	latexmk $(source)

clean:
	latexmk -CA
	rm -vf $(target) *~
