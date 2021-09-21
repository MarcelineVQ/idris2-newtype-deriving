# literally just convenience, you'd be better served using a package manager

PKGNAME = newtype-generics

.PHONY: build

build:
	idris2 --build ${PKGNAME}.ipkg

install:
	idris2 --install ${PKGNAME}.ipkg

clean:
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
