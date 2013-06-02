default: nat-lang natpair-lang natpair-ivars-lang

all: nat-lang-all natpair-lang-all natpair-ivars-lang-all

nat-lang:
	raco test -s test-fast nat.rkt

nat-lang-all:
	raco test -s test-all nat.rkt 

natpair-lang:
	raco test -s test-fast natpair.rkt

natpair-lang-all:
	raco test -s test-all natpair.rkt

natpair-ivars-lang:
	raco test -s test-fast natpair-ivars.rkt

natpair-ivars-lang-all:
	raco test -s test-all natpair-ivars.rkt