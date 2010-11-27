include $(GOROOT)/src/Make.inc

SOURCES := $(wildcard *.go)
OBJECTS := $(SOURCES:%.go=./%.out)

all: $(OBJECTS)

./%.6: %.go
	6g $<

./%.out: %.6
	6l $<