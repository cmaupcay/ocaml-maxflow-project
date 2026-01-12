.PHONY: all build format edit test demo clean

SRC=src
BUILD=_build
TARGET=target

# src?=0
# dst?=5
# graph?=graph1.txt

all: clean build test

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	@dune build
	@mkdir -p $(TARGET)
	@mv $(SRC)/*.exe $(TARGET)

format:
	ocp-indent --inplace $(SRC)/*

edit:
	code . -n

test.tools: build
	@echo "\n   ⚡  TESTING - tools  ⚡\n"
	@mkdir -p ./graphs/outfiles
	@$(TARGET)/tools_test.exe
	
test.export: build
	@echo "\n   ⚡  TESTING - export  ⚡\n"
	@mkdir -p ./graphs/svg
	@$(TARGET)/export_test.exe
	
test.algo: build
	@echo "\n   ⚡  TESTING - algo  ⚡\n"
	@mkdir -p ./graphs/svg
	@$(TARGET)/ford_fulkerson_test.exe

test.univ: build
	@echo "\n   ⚡  TESTING - university attribution ⚡\n"
	@mkdir -p ./graphs/svg
	@$(TARGET)/univ_test.exe

test: test.tools test.export test.algo test.univ
	@echo "\n   🥁  TESTS RAN SUCCESSFULLY  🥁\n"

# demo: build
# 	@echo "\n   ⚡  EXECUTING  ⚡\n"
# 	./ftest.exe graphs/${graph} $(src) $(dst) outfile
# 	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
# 	@cat outfile

clean:
	@find -L . -name "*~" -delete
	@rm -rf $(TARGET) $(BUILD) *.exe outfile*
# 	@dune clean
