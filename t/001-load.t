#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(unknown),
    etap_can:loaded_ok(orange_class, "Module 'orange_class' loaded"),
    etap_can:loaded_ok(orange_attribute, "Module 'orange_attribute' loaded"),
    etap_can:loaded_ok(orange_link, "Module 'orange_link' loaded"),
    etap_can:loaded_ok(orange_object, "Module 'orange_object' loaded"),
    etap_can:loaded_ok(orange_server, "Module 'orange_server' loaded"),
    etap_can:loaded_ok(orange_storage_server, "Module 'orange_storage_server' loaded"),
    etap_can:loaded_ok(orange_util, "Module 'orange_util' loaded"),
    etap_can:loaded_ok(orange, "Module 'orange' loaded"),
    etap_can:loaded_ok(orange_sup, "Module 'orange_sup' loaded"),
    etap:end_tests(),
    ok.
