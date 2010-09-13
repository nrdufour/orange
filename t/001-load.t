#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(unknown),
    etap:loaded_ok(orange_class, "Module 'orange_class' loaded"),
    etap:loaded_ok(orange_attribute, "Module 'orange_attribute' loaded"),
    etap:loaded_ok(orange_link, "Module 'orange_link' loaded"),
    etap:loaded_ok(orange_object, "Module 'orange_object' loaded"),
    etap:loaded_ok(orange_server, "Module 'orange_server' loaded"),
    etap:loaded_ok(orange_storage_server, "Module 'orange_storage_server' loaded"),
    etap:loaded_ok(orange_util, "Module 'orange_util' loaded"),
    etap:loaded_ok(orange, "Module 'orange' loaded"),
    etap:loaded_ok(orange_sup, "Module 'orange_sup' loaded"),
    etap:end_tests(),
    ok.
