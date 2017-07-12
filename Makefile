.PHONY: all clean doc test

all:
	jbuilder build --dev

test:
	jbuilder runtest --dev

doc:
	jbuilder build @doc

clean:
	jbuilder clean
