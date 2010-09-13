#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(unknown),
    etap:loaded_ok(orange_object, "Module 'orange_object' loaded."),
    etap:has_attrib(orange_object, behavior),
    etap:is_attrib(orange_object, behavior, gen_server),
    etap:can_ok(orange_object, create),
    etap:can_ok(orange_object, create, 2),
    etap:can_ok(orange_object, hibern),
    etap:can_ok(orange_object, hibern, 2),
    etap:can_ok(orange_object, awake),
    etap:can_ok(orange_object, awake, 2),
    etap:can_ok(orange_object, destroy),
    etap:can_ok(orange_object, destroy, 2),
    etap:can_ok(orange_object, resur),
    etap:can_ok(orange_object, resur, 2),
    etap:can_ok(orange_object, purge),
    etap:can_ok(orange_object, purge, 2),

    application:start(orange),
    orange_storage_server:init_storage(),

    % First create a class
    orange_class:create("Bridge"),

    % Try to create an object
    etap:is(orange_object:create("Bridge", "Alma"), {ok, alive}, "Creating Attribute Bridge.Alma"),

    % Changing the adt state
    etap:is(orange_object:hibern("Bridge", "Alma"), {ok, frozen}, "Freezing Attribute Bridge.Alma"),
    etap:is(orange_object:awake("Bridge", "Alma"), {ok, alive}, "Unfreezing Attribute Bridge.Alma"),
    etap:is(orange_object:destroy("Bridge", "Alma"), {ok, destroyed}, "Destroying Attribute Bridge.Alma"),
    etap:is(orange_object:resur("Bridge", "Alma"), {ok, alive}, "Resurecting Attribute Bridge.Alma"),

    % Try to create an object in a class that doesnt exist
    etap:is(orange_object:create("Nope", "Alma"), {error, invalid_parents}, "Can't create Nope.Alma"),

    % Try to recreate the same object in the same class
    etap:is(orange_object:create("Bridge", "Alma"), {error, already_exists}, "Can't create the same object twice"),

    application:stop(orange),

    etap:end_tests(),
    ok.

