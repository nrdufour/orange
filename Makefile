ERL        ?= erl 

#EBIN_DIRS  := $(wildcard deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:%=-I %) $(EBIN_DIRS:%=-pa %)
APP        := orange

all:
	./rebar compile

doc:
	@mkdir -p doc
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all 
	prove t/*.t

cover: all 
	rm -f cover/*.coverdata
	COVER=1 COVER_BIN=./ebin prove t/*.t
	SRC=./src/ \
		erl -noshell \
		-eval 'etap_report:create()' \
		-s init stop  > /dev/null 2>&1

clean:
	./rebar clean

