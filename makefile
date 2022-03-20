help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

todo: ## Things that should be done...
	@grep -nR --color=always  --before-context=2  --after-context=2 TODO .

xxx: ## Things that require attention!
	@grep -nR --color=always --before-context=2  --after-context=2 XXX .

MACHINETYPE=ta6le
WORKSPACE=$(PWD)/racket-chez/$(MACHINETYPE)
SCHEME=$(WORKSPACE)/bin/scheme

racket-chez/ta6le/bin/scheme:
	git submodule init
	git submodule update --depth 1
	cd racket-chez && git submodule init
	cd racket-chez && git submodule update --depth 1
	cd racket-chez && ./configure --pb
	cd racket-chez && make -j $(nproc) $(MACHINETYPE).bootquick
	cd racket-chez && ./configure --kernelobj
	cd racket-chez && make -j $(nproc)

racket: racket-chez/ta6le/bin/scheme

PETITE_BOOT=$(WORKSPACE)/boot/$(MACHINETYPE)/petite.boot
SCHEME_BOOT=$(WORKSPACE)/boot/$(MACHINETYPE)/scheme.boot

src/arew.wpo: src/arew.sls src/arew/matchable.sls src/arew/record.sls $(shell find src/arew/ -name "*.scm")
	cd src && echo "(compile-imported-libraries #t)(generate-wpo-files #t)(compile-file \"arew.sls\")" | $(SCHEME) --boot $(PETITE_BOOT) --boot $(SCHEME_BOOT) --quiet --compile-imported-libraries

src/arew.concatenated.so: src/arew.wpo
	cd src/ && echo '(concatenate-object-files "arew.concatenated.so" "arew/record.so" "arew/matchable.so" "arew.so")' | $(SCHEME) --boot $(PETITE_BOOT) --boot $(SCHEME_BOOT) --quiet


arew: src/arew.boot src/arew.concatenated.so racket ## Build src/arew binary
	rm -rf arew
	echo "(make-boot-file \"arew.boot\" '() \"$(PETITE_BOOT)\" \"$(SCHEME_BOOT)\") (exit)" | $(SCHEME) --boot $(PETITE_BOOT) --boot $(SCHEME_BOOT)
	mv arew.boot src/
	cd src && $(SCHEME) --quiet --boot $(PETITE_BOOT) --boot $(SCHEME_BOOT) --program arew.scm compile . arew.scm ../arew

install: arew ## Install arew!
	mkdir -p $(HOME)/.local/bin/
	mv arew $(HOME)/.local/bin/

clean:
	find . -type f -name "*.so" -exec rm {} \;
	find . -type f -name "*.wpo" -exec rm {} \;
