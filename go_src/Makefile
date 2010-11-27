include $(GOROOT)/src/Make.inc

SOURCES := $(wildcard *.go)
OBJECTS := $(SOURCES:%.go=./%.out)

all: $(OBJECTS)

./%.$(GOCOMPILER): %.go
	$(GOCOMPILER)g $<

./%.out: %.$(GOCOMPILER)
	$(GOCOMPILER)l $<