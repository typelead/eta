.PHONY: compile clean run install

PATH := bin:$(PATH)

compile:
	stack build eta-ide

exec:
	stack exec -- ${EXEC}

clean:
	stack clean eta-ide

run: compile
	# stack exec eta-ide
	$(shell find .stack-work/dist/*/*/build/eta-ide/eta-ide) --interactive

install:
	stack install eta-ide
