#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
 
main(_) ->
	etap:plan(2),
	etap:is(application:start(orange), ok, "Make sure the application starts"),
	etap:is(application:stop(orange), ok, "Make sure the application stops"),
	etap:end_tests(),
	ok.
