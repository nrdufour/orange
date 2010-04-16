#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(unknown),
    etap_can:loaded_ok(orange_class, "Module 'orange_class' loaded."),
    etap_can:has_attrib(orange_class, behavior),
    etap_can:is_attrib(orange_class, behavior, gen_server),
    etap_can:can_ok(orange_class, create, 1),
    etap_can:can_ok(orange_class, hibern, 1),
    etap_can:can_ok(orange_class, awake, 1),
    etap_can:can_ok(orange_class, destroy, 1),
    etap_can:can_ok(orange_class, resur, 1),
    etap_can:can_ok(orange_class, purge, 1),

    application:start(orange),
    orange_storage_server:init_storage(),
    etap:is(orange_class:create("Bridge"), {ok, alive}, "Creating Class Bridge"),
    etap:is(orange_class:create("Bridge"), {error, already_exists}, "Try to create Class Bridge again"),
    etap:is(orange_class:create("River"), {ok, alive}, "Creating Class River"),
    etap:is(orange_class:create("River"), {error, already_exists}, "Try to create Class River again"),

    etap:is(orange_class:hibern("River"), {ok, frozen}, "Hibernate Class River"),
    etap:is(orange_class:hibern("River"), {error, wrong_state}, "Try to hibernate again Class River"),
    etap:is(orange_class:hibern("Car"), {error, not_found}, "Try to hibernate a class which doesn't exist"),
    application:stop(orange),

    etap:end_tests(),
    ok.

