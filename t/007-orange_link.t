#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(unknown),
    etap_can:loaded_ok(orange_link, "Module 'orange_link' loaded."),
    etap_can:has_attrib(orange_link, behavior),
    etap_can:is_attrib(orange_link, behavior, gen_server),
    etap_can:can_ok(orange_link, create),
    etap_can:can_ok(orange_link, create, 3),
    etap_can:can_ok(orange_link, create, 4),
    etap_can:can_ok(orange_link, hibern),
    etap_can:can_ok(orange_link, hibern, 3),
    etap_can:can_ok(orange_link, awake),
    etap_can:can_ok(orange_link, awake, 3),
    etap_can:can_ok(orange_link, destroy),
    etap_can:can_ok(orange_link, destroy, 3),
    etap_can:can_ok(orange_link, resur),
    etap_can:can_ok(orange_link, resur, 3),
    etap_can:can_ok(orange_link, purge),
    etap_can:can_ok(orange_link, purge, 3),

    application:start(orange),
    orange_storage_server:init_storage(),

    % First create two classes
    orange_class:create("Bridge"),
    orange_class:create("River"),

    % Try to create an link
    etap:is(orange_link:create("Bridge", "River", "cross"), {ok, alive}, "Creating Link Bridge-cross-River"),

    % Changing the adt state
    etap:is(orange_link:hibern("Bridge", "River", "cross"), {ok, frozen}, "Freezing Link Bridge-cross-River"),
    etap:is(orange_link:awake("Bridge", "River", "cross"), {ok, alive}, "Unfreezing Link Bridge-cross-River"),
    etap:is(orange_link:destroy("Bridge", "River", "cross"), {ok, destroyed}, "Destroying Link Bridge-cross-River"),
    etap:is(orange_link:resur("Bridge", "River", "cross"), {ok, alive}, "Resurecting Link Bridge-cross-River"),

    % Try to create an link in a class that doesnt exist
    etap:is(orange_link:create("Nope", "River", "cross"), {error, invalid_parents}, "Can't create Nope-cross-River"),
    etap:is(orange_link:create("Bridge", "Nope", "cross"), {error, invalid_parents}, "Can't create Bridge-cross-Nope"),
    etap:is(orange_link:create("Nope", "Nope", "cross"), {error, invalid_parents}, "Can't create Nope-cross-Nope"),

    % Try to recreate the same link in the same class
    etap:is(orange_link:create("Bridge", "River", "cross"), {error, already_exists}, "Can't create the same link twice"),

    application:stop(orange),

    etap:end_tests(),
    ok.

