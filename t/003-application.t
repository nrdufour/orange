#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    etap_application:load_ok(orange, "Application ADTM 0.1 loads"),
    etap_application:start_ok(orange, "Application ADTM 0.1 starts"),
    etap:end_tests(),
    ok.

