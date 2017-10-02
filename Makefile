HS = src/lvish/tests/verified/Bit.hs src/lvish/tests/verified/Sum.hs \
     src/lvish/Data/LVar/PureSet.hs src/lvish/Data/LVar/SLSet.hs

# Dummy target
CHS = $(subst hs,chs,$(HS))

DOCKER ?= true
TIMEIT ?=

ifeq ($(DOCKER),true)
	STACK ?= stack --docker
	ALL = docker build check
else
	STACK ?= stack
	ALL = build check
endif

LIQUID ?= $(STACK) exec liquid --

ifeq ($(TIMEIT),true)
	LIQUID := time $(LIQUID) -q >/dev/null
endif

all: $(ALL)

docker:
	docker build -t liquidhaskell .

build:
	$(STACK) build
	$(STACK) test --no-run-tests
	$(STACK) bench --no-run-benchmarks

check: build $(CHS)

%.chs: %.hs
	$(LIQUID) -i src/lvish -i src/lvish/tests/verified $<

clean:
	find . -type d -name '.liquid' -exec rm -rf {} \+
	$(STACK) clean

.PHONY: docker build check clean
