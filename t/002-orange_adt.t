#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

test_matrix_element({FromState, [{Operation, ToState}|Rest]}) ->
    Message = io_lib:format("\t~-8s[~-10s] -> ~p", [Operation, FromState, ToState]),
    etap:is(orange_adt:new_state_after(Operation, FromState), ToState, Message),
    test_matrix_element({FromState, Rest});
test_matrix_element({_FromState, []}) ->
    ok.

test_matrix([Element|Rest]) ->
    test_matrix_element(Element),
    test_matrix(Rest);
test_matrix([]) ->
    ok.

test_new_state_after() ->
    Base = [
        {none,      [   {create, alive}, {hibern, wrong_state}, {awake, wrong_state},
            {destroy, wrong_state}, {resur, wrong_state}, {purge, wrong_state} ]},
        {alive,     [   {create, wrong_state}, {hibern, frozen}, {awake, wrong_state},
            {destroy, destroyed}, {resur, wrong_state}, {purge, wrong_state} ]},
        {frozen,    [   {create, wrong_state}, {hibern, wrong_state}, {awake, alive},
            {destroy, wrong_state}, {resur, wrong_state}, {purge, wrong_state} ]},
        {destroyed, [   {create, wrong_state}, {hibern, wrong_state}, {awake, wrong_state},
            {destroy, wrong_state}, {resur, alive}, {purge, none} ]}
    ],

    etap:diag("Testing orange_adt:new_state_after."),
    test_matrix(Base).

%% ---------------------------------------------------------------------------

main(_) ->
    etap:plan(unknown),
    
    etap_can:loaded_ok(orange_adt, "Module 'orange_adt' loaded"),

    etap_can:can_ok(orange_adt, new_adt),
    etap_can:can_ok(orange_adt, new_adt, 2),
    etap_can:can_ok(orange_adt, new_state_after),
    etap_can:can_ok(orange_adt, new_state_after, 2),
    etap_can:can_ok(orange_adt, is_ready_for),
    etap_can:can_ok(orange_adt, is_ready_for, 2),

    %%ClassID = { adt_id, class, { 0 } },
    %%ClassName = "Bridge",
    %%ClassADT = { adt, ClassID, ClassName, none },
    %%etap:is( ClassID, orange_adt:new_id(class), "New ID for class" ),
    %%etap:is( ClassADT, orange_adt:new_adt(class, ClassName), "New class ADT"),

    %%AttributeID = { adt_id, attribute, { 0, 0 } },
    %%AttributeName = "height",
    %%AttributeADT = { adt, AttributeID, AttributeName, none },
    %%etap:is( AttributeID, orange_adt:new_id(attribute), "New ID for attribute" ),
    %%etap:is( AttributeADT, orange_adt:new_adt(attribute, AttributeName), "New attribute ADT"),

    %%LinkID = { adt_id, link, { 0, 0, 0 } },
    %%LinkName = "cross",
    %%LinkADT = { adt, LinkID, LinkName, none },
    %%etap:is( LinkID, orange_adt:new_id(link), "New ID for link" ),
    %%etap:is( LinkADT, orange_adt:new_adt(link, LinkName), "New link ADT"),

    %%ObjectID = { adt_id, object, { 0, 0 } },
    %%ObjectName = "Alma",
    %%ObjectADT = { adt, ObjectID, ObjectName, none },
    %%etap:is( ObjectID, orange_adt:new_id(object), "New ID for object" ),
    %%etap:is( ObjectADT, orange_adt:new_adt(object, ObjectName), "New object ADT"),
    
    test_new_state_after(),

    etap:end_tests(),
    ok.

