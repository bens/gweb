.PHONY : build
build :
	cabal build exe:gweb

.PHONY : test
test :
	cabal test test:gweb-test

.PHONY : example
example :
	cabal run exe:gweb -- --config example/build.cfg

################################################################################
## DEVELOPMENT
################################################################################

REPL=cabal repl -f live-repl
REPL_EXAMPLE=${REPL} exe:gweb --repl-options="-ghci-script example/.ghci"

.PHONY : repl-example
repl-example :
	${REPL_EXAMPLE}


.PHONY : repl-test
repl-test :
	${REPL} "test:gweb-test"

.PHONY : watch-example
watch-example :
	ghcid --command='${REPL_EXAMPLE}' \
	    --test=main \
	    --lint \
	    --reload=example/doc \
	    --reload=example/templates \
	    --warnings

.PHONY : watch-test
watch-test :
	ghcid --command='${REPL} test:gweb-test' \
	    --test=main \
	    --lint \
	    --warnings
