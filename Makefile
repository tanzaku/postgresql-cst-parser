ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: clean prepare-source prepare-original-source make-patch

prepare-source: prepare-original-source
	patch -p1 < $(ROOT_DIR)/patches/patch_scan.patch

prepare-original-source: clean
	mkdir -p ./tmp
	cd ./tmp; git clone -b 16-latest --depth=1 git@github.com:pganalyze/libpg_query.git
	sed -i 's/sed -i ""/sed -i/g' ./tmp/libpg_query/Makefile
	cd ./tmp/libpg_query; make $(ROOT_DIR)tmp/libpg_query/tmp/postgres
	mkdir -p ./crates/lexer-generator/resources
	mkdir -p ./crates/parser-generator/resources
	cp $(ROOT_DIR)tmp/libpg_query/tmp/postgres/src/backend/parser/scan.l $(ROOT_DIR)crates/lexer-generator/resources
	cp $(ROOT_DIR)tmp/libpg_query/tmp/postgres/src/include/parser/kwlist.h $(ROOT_DIR)crates/lexer-generator/resources
	cp $(ROOT_DIR)tmp/libpg_query/tmp/postgres/src/backend/parser/gram.y $(ROOT_DIR)crates/parser-generator/resources

make-patch:
	git diff -- ./crates/lexer-generator/resources/scan.l > ./patches/patch_scan.patch

clean:
	-@ rm -rf ./tmp
