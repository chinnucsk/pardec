all: clean compile

clean:
	@rm -rf ebin/*.beam

compile:
	@test -d ebin || mkdir ebin
	@erl -make

test:
	@escript test.escript
