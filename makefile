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
	cd racket-chez && make -j $(nproc) ta6le.bootquick
	cd racket-chez && ./configure --kernelobj
	cd racket-chez && make -j $(nproc)

racket: racket-chez/ta6le/bin/scheme

PETITE_BOOT=$(WORKSPACE)/boot/$(MACHINETYPE)/petite.boot
SCHEME_BOOT=$(WORKSPACE)/boot/$(MACHINETYPE)/scheme.boot

arew: src/arew.scm
	echo "(make-boot-file \"arew.boot\" '() \"$(PETITE_BOOT)\" \"$(SCHEME_BOOT)\") (exit)" | $(SCHEME) --boot $(PETITE_BOOT) --boot $(SCHEME_BOOT)
	mv arew.boot src/
	cd src && $(SCHEME) --boot $(PETITE_BOOT) --boot $(SCHEME_BOOT) --program arew.scm compile . arew.scm arew
