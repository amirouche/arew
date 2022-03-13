ROOT=$(shell realpath ../..)
MACHINETYPE=$(shell fgrep m= "$(PWD)/../s/Mf-config" | cut -f2 -d=)
SCHEME=$(shell realpath "$(PWD)/../bin/$(MACHINETYPE)/scheme")

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

rootsrc = $(shell cd "$(ROOT)/a/"; find . -type f)
$(rootsrc):
	mkdir -p $(shell dirname $@)
	ln -sf "$(ROOT)/a/$@" $@


source: $(rootsrc)

arew/arew.wpo: $(shell find . -name "*.sld") $(shell find . -name "*.scm") $(shell find . -name "*.md")
	echo "(generate-wpo-files #t)(compile-file \"arew/arew.sld\")" | $(SCHEME) --boot "$(PWD)/../boot/$(MACHINETYPE)/petite.boot" --boot "$(PWD)/../boot/$(MACHINETYPE)/scheme.boot" --quiet --compile-imported-libraries

arew.so: arew/arew.wpo srfi/9.sld arew/arew.sld arew/matchable.scm
	echo '(concatenate-object-files "arew.so" "srfi/9.so" "arew/matchable.so" "arew/arew.so")' | $(SCHEME) --boot "$(PWD)/../boot/$(MACHINETYPE)/petite.boot" --boot "$(PWD)/../boot/$(MACHINETYPE)/scheme.boot"

arew.boot: arew.so
	rm -f arew.boot
	$(SCHEME) --boot "$(PWD)/../boot/$(MACHINETYPE)/petite.boot" --boot "$(PWD)/../boot/$(MACHINETYPE)/scheme.boot" --program make-boot-file.scm $(shell find . -name "*.wpo")
	cp arew.boot $(PWD)/../boot/$(MACHINETYPE)/arew.boot

main: arew.boot
	rm -f main
	$(SCHEME) --boot ./arew.boot --program main.scm compile .so main.scm main
	cp "$(PWD)/main" "$(PWD)/../bin/$(MACHINETYPE)/arew"

clean:
	find . -name "*.so" -delete
	find . -name "*.wpo" -delete
	rm -f arew.boot main

build: main
