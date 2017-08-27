
-module(nkchat_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-include("nkchat.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").


-define(WS, "ws://127.0.0.1:9202/_api/ws").
-define(ADMIN_PASS, "1234").
-define(DOM, <<"/chattest">>).



%% Creates:
%%
%% - Domain /chattest
%% - Users /chattest/users/u1, u2, u3
%% - Conversations /chattest/conversations:
%%      - conv1: u1, u2, u3
%%      - conv2: u1, u2
%%      - conv3  u2, u3
%%






test() ->

    clear(),
    Ref1 = make_ref(),
    Pid = login("admin", ?ADMIN_PASS, Ref1),
    ok = create(Pid),
    ok = base_msgs(Pid, Ref1),
    nkapi_client:stop(Pid),
    Ref2 = make_ref(),
    Pid2 = login("/chattest/users/u1", "p1", Ref2),
    session1(Pid2, Ref2),
    lager:notice("Chat test ok!").





clear() ->
    nkdomain:delete_all_childs(?DOM),
    nkdomain_domain_obj:create(root, "/", "chattest", "Chat test"),
    ok.


%% Creates the structure detailed above
create(Pid) ->
    % Create u1
    U1_Data = #{obj_name=>u1, domain=> ?DOM,
                user=>#{name=>u1, surname=>s1, password=>p1, email=><<"u1@chattest">>}},
    {ok, #{<<"obj_id">>:=U1}} = cmd(Pid, user, create, U1_Data),

    % Create u2
    U2_Data = #{obj_name=>u2, domain=>?DOM,
                user=>#{name=>u2, surname=>s2, password=>p2, email=><<"u1@chattest">>}},
    {ok, #{<<"obj_id">>:=U2}} = cmd(Pid, user, create, U2_Data),

    % Create u3
    U3_Data = #{obj_name=>u3, domain=>?DOM,
                user=>#{name=>u3, surname=>s3, password=>p3, email=><<"u3@chattest">>}},
    {ok, #{<<"obj_id">>:=U3}} = cmd(Pid, user, create, U3_Data),

    % Create conv1
    C1_Data = #{obj_name=>conv1, description=><<"Conv 1">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C1}} = cmd_conversation(Pid, create, C1_Data),

    % Create conv2
    C2_Data = #{obj_name=>conv2, description=><<"Conv 2">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C2}} = cmd_conversation(Pid, create, C2_Data),

    % Create conv3
    C3_Data = #{obj_name=>conv3, description=><<"Conv 3">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C3}} = cmd_conversation(Pid, create, C3_Data),

    {error,{<<"missing_field">>, <<"Missing field: 'obj_name'">>}} =
        cmd_conversation(Pid, create, #{domain=><<"/none">>}),
    {error,{<<"missing_field">>, <<"Missing field: 'description'">>}} =
        cmd_conversation(Pid, create, #{obj_name=>c4, domain=><<"/none">>}),
    {error,{<<"could_not_load_parent">>, <<"Object could not load parent '/none'">>}}  =
        cmd_conversation(Pid, create, #{obj_name=>c4, description=>desc, domain=><<"/none">>}),
    {error,{<<"object_already_exists">>, _}} =
        cmd_conversation(Pid, create, #{obj_name=>conv1, description=>desc, domain=>?DOM}),

    % Add members to conversations
    {ok, #{<<"member_obj_id">>:=U1}} =
        cmd_conversation(Pid, add_member, #{id=>C1, member_id=><<"/chattest/users/u1">>}),
    {ok, #{<<"member_obj_id">>:=U2}} =
        cmd_conversation(Pid, add_member, #{id=><<"/chattest/conversations/conv1">>, member_id=>U2}),
    {ok, #{<<"member_obj_id">>:=U3}} =
        cmd_conversation(Pid, add_member, #{id=>C1, member_id=>U3}),
    {ok, #{<<"member_obj_id">>:=U1}} = cmd_conversation(Pid, add_member, #{id=>C2, member_id=>U1}),
    {ok, #{<<"member_obj_id">>:=U2}} = cmd_conversation(Pid, add_member, #{id=>C2, member_id=>U2}),
    {ok, #{<<"member_obj_id">>:=U2}} = cmd_conversation(Pid, add_member, #{id=>C3, member_id=>U2}),
    {ok, #{<<"member_obj_id">>:=U3}} = cmd_conversation(Pid, add_member, #{id=>C3, member_id=>U3}),

    {error, {<<"member_not_found">>, _}} = cmd_conversation(Pid, add_member, #{id=>C2, member_id=><<"a">>}),
    {error, {<<"conversation_not_found">>, _}} = cmd_conversation(Pid, add_member, #{id=><<"a">>, member_id=>U2}),

    {ok, ?DOMAIN_DOMAIN, D, _Path, _Pid} = nkdomain:find(?DOM),
    {ok, #{
        <<"_is_enabled">> := true,
        ?CHAT_CONVERSATION := #{<<"members">> := [#{<<"member_id">>:=C3U1}, #{<<"member_id">>:=C3U2}]},
        <<"description">> := <<"Conv 3">>,
        <<"obj_id">> := C3,
        <<"domain_id">> := D,
        <<"path">> := <<"/chattest/conversations/conv3">>,
        <<"type">> := ?CHAT_CONVERSATION
    }} =
        cmd_conversation(Pid, get, #{}),
    true = lists:sort([U2, U3]) == lists:sort([C3U1, C3U2]),

    {ok, #{
        <<"_is_enabled">> := true,
        ?CHAT_CONVERSATION := #{<<"members">> := [#{<<"member_id">>:=C1U1}, #{<<"member_id">>:=C1U2}, #{<<"member_id">>:=C1U3}]},
        <<"description">> := <<"Conv 1">>,
        <<"obj_id">> := C1,
        <<"domain_id">> := D,
        <<"path">> := <<"/chattest/conversations/conv1">>,
        <<"type">> := ?CHAT_CONVERSATION
    }} =
        cmd_conversation(Pid, get, #{id=>C1}),
    true = lists:sort([U1, U2, U3]) == lists:sort([C1U1, C1U2, C1U3]),

    {ok, #{
        <<"_is_enabled">> := true,
        ?CHAT_CONVERSATION := #{<<"members">> := [#{<<"member_id">>:=C2U1}, #{<<"member_id">>:=C2U2}]},
        <<"description">> := <<"Conv 2">>,
        <<"obj_id">> := C2,
        <<"domain_id">> := D,
        <<"path">> := <<"/chattest/conversations/conv2">>,
        <<"type">> := ?CHAT_CONVERSATION
    }} =
        cmd_conversation(Pid, get, #{id=>C2}),
    true = lists:sort([U1, U2]) == lists:sort([C2U1, C2U2]),
    ok.


%% Send messages to conversations without sessions, notifications are not sent (but events can be used)
base_msgs(Pid, Ref) ->
    {ok, #{}} = cmd(Pid, event, subscribe, #{class=>domain, subclass=>conversation,
                    type=>[message_created, message_deleted]}),

    {ok, _, C1, <<"/chattest/conversations/conv1">>, _} = nkdomain:find(<<"/chattest/conversations/conv1">>),
    {ok, _, C2, <<"/chattest/conversations/conv2">>, _} = nkdomain:find(<<"/chattest/conversations/conv2">>),
    {ok, _, _C3, <<"/chattest/conversations/conv3">>, _} = nkdomain:find(<<"/chattest/conversations/conv3">>),
    BC1 = #{conversation_id=><<"/chattest/conversations/conv1">>},
    BC2 = #{conversation_id=><<"/chattest/conversations/conv2">>},

    % We send a message to a conversation without sessions, only the event is sent
    {ok, #{<<"obj_id">>:=M1}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_1}}),
    #nkevent{subclass = ?CHAT_CONVERSATION, type = <<"message_created">>, obj_id=C1, body=#{<<"message_id">>:=M1}} =
        wait_event(Ref),

    {ok, #{<<"obj_id">>:=M2}} = cmd_message(Pid, create, BC2#{?CHAT_MESSAGE => #{text=>msgC2_1}}),
    #nkevent{subclass = ?CHAT_CONVERSATION, type = <<"message_created">>, obj_id=C2, body=#{<<"message_id">>:=M2}} =
        wait_event(Ref),

    {ok, #{}} = cmd_message(Pid, delete, #{id=>M2}),
    #nkevent{subclass = ?CHAT_CONVERSATION, type = <<"message_deleted">>, obj_id=C2, body=#{<<"message_id">>:=M2}} =
        wait_event(Ref),
    ok.



session1(Pid, _Ref) ->
    % Create the session
    {error, {<<"session_not_found">>, _}} = cmd_session(Pid, find, #{}),
    {ok, #{<<"obj_id">>:=S, <<"conversations">>:=[]}} = cmd_session(Pid, create, #{}),
    {ok, #{<<"sessions">>:=[#{<<"obj_id">>:=S}|_]}} = cmd_session(Pid, find, #{}),
    {ok, #{}} = cmd_session(Pid, stop, #{}),
    {error, {<<"object_not_started">>, _}} = cmd_session(Pid, stop, #{}),
    {error, {<<"session_not_found">>, _}} = cmd_session(Pid, stop, #{id=><<"a">>}),
    {ok, #{<<"obj_id">>:=S, <<"conversations">>:=[]}} = cmd_session(Pid, start, #{id=>S}),
    {ok, _, _, _, _SPid} = nkdomain:find(S),

    % Adds C1 and C3
    {ok, #{<<"conversation_id">>:=_C1}} =
        cmd_session(Pid, add_conversation, #{conversation_id=>"/chattest/conversations/conv1"}),
    {ok, #{<<"conversation_id">>:=_C3}} =
        cmd_session(Pid, add_conversation, #{conversation_id=>"/chattest/conversations/conv3"}),

%%    {ok, _, C2, _, _} = nkdomain:find(root, <<"/chattest/conversations/conv2">>),
%%    BC1 = #{conversation_id=><<"/chattest/conversations/conv1">>},
%%    BC2 = #{conversation_id=><<"/chattest/conversations/conv2">>},
%%    BC3 = #{conversation_id=>C3},
%%    {ok, _, U1, _, _} = nkdomain:find(root, "/chattest/users/u1"),
%%    {ok, _, U2, _, _} = nkdomain:find(root, "/chattest/users/u2"),
%%    {ok, _, U3, _, _} = nkdomain:find(root, "/chattest/users/u3"),
%%
%%    mock_session(Ref),
%%
%%    % Send a message to C1, U1 receives an updated counter, U2 and U3 receive push
%%    {ok, #{<<"obj_id">>:=M1}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_2}}),
%%    {counter, S, C1, 1} = wait_session(Ref),
%%    {A1, C1, M1, _, <<"msgC1_2">>} = wait_push(Ref),
%%    {A2, C1, M1, _, <<"msgC1_2">>} = wait_push(Ref),
%%    true = lists:sort([U2, U3]) == lists:sort([A1, A2]),
%%
%%    {ok, #{<<"obj_id">>:=M2}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_3}}),
%%    {counter, S, C1, 2} = wait_session(Ref),
%%    {A1, C1, M2, _, <<"msgC1_3">>} = wait_push(Ref),
%%    {A2, C1, M2, _, <<"msgC1_3">>} = wait_push(Ref),
%%    true = lists:sort([U2, U3]) == lists:sort([A1, A2]),
%%
%%    {ok, #{}} = cmd_message(Pid, wait_for_save, #{id=>M1}),
%%    {ok, #{}} = cmd_message(Pid, wait_for_save, #{id=>M2}),
%%
%%    {ok, Conv1} = cmd_session(Pid, get_conversation, #{conversation_id=>C1}),
%%    #{
%%        <<"obj_id">> := C1,
%%%%        <<"name">> := <<"conv1">>,
%%        <<"description">> := <<"Conv 1">>,
%%        <<"last_active_time">> := 0,
%%        <<"last_seen_message_id">> := <<>>,
%%        <<"last_seen_message_time">> := _,
%%        <<"unread_count">> := 2,
%%        <<"is_enabled">> := true
%%    } = Conv1,
%%
%%    {error, {<<"conversation_not_found">>, _}} = cmd_session(Pid, get_conversation, #{conversation_id=>C2}),
%%
%%    {ok, Conv2} = cmd_session(Pid, get_conversation, #{conversation_id=>C3}),
%%    #{
%%        <<"obj_id">> := C3,
%%%%        <<"name">> := <<"conv3">>,
%%        <<"description">> := <<"Conv 3">>,
%%        <<"last_active_time">> := 0,
%%        <<"last_seen_message_id">> := <<>>,
%%        <<"last_seen_message_time">> := _,
%%        <<"is_enabled">> := true
%%    } = Conv2,
%%    false = maps:is_key(<<"_unread_count">>, Conv2),
%%    {ok, #{<<"conversations">>:=[_CL1, _CL2]}} = cmd_session(Pid, get_all_conversations, #{}),
%%%%    true = lists:sort([Conv1, Conv2]) == lists:sort([CL1, CL2]),
%%
%%    % Send msg to C2, U1 and U2 receive push
%%    {ok, #{<<"obj_id">>:=M3}} = cmd_message(Pid, create, BC2#{?CHAT_MESSAGE => #{text=>msgC2_2}}),
%%    {A3, C2, M3, _, <<"msgC2_2">>} = wait_push(Ref),
%%    {A4, C2, M3, _, <<"msgC2_2">>} = wait_push(Ref),
%%    true = lists:sort([U1, U2]) == lists:sort([A3, A4]),
%%
%%    % Send msg to C3, U2 receive push, U1 has a session and receives counter event
%%    {ok, #{<<"obj_id">>:=M4}} = cmd_message(Pid, create, BC3#{?CHAT_MESSAGE => #{text=>msgC3_3}}),
%%    {counter, S, C3, 1} = wait_session(Ref),
%%    {A5, C3, M4, _, <<"msgC3_3">>} = wait_push(Ref),
%%    {A6, C3, M4, _, <<"msgC3_3">>} = wait_push(Ref),
%%    true = lists:sort([U2, U3]) == lists:sort([A5, A6]),
%%
%%    % If we stop the session, we receive pushes again. We we start it back, counters are regenerated
%%    {ok, #{}} = cmd_session(Pid, stop, #{}),
%%
%%
%%    session_stopped = wait_session(Ref),
%%    timer:sleep(1000),
%%    false = is_process_alive(SPid),
%%    % Send msg to C3
%%    {ok, #{<<"obj_id">>:=M5}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_4}}),
%%    {A7, C1, M5, _, <<"msgC1_4">>} = wait_push(Ref),
%%    {A8, C1, M5, _, <<"msgC1_4">>} = wait_push(Ref),
%%    {A9, C1, M5, _, <<"msgC1_4">>} = wait_push(Ref),
%%    true = lists:sort([U1, U2, U3]) == lists:sort([A7, A8, A9]),
%%    {ok, #{}} = cmd_message(Pid, wait_for_save, #{id=>M5}),
%%    {ok, #{<<"conversations">>:=[CL3, CL4]}} = cmd_session(Pid, start, #{id=>S}),
%%
%%    {ok, Conv3} = cmd_session(Pid, get_conversation, #{conversation_id=>C1}),
%%    #{
%%        <<"obj_id">> := C1,
%%        <<"last_active_time">> := 0,
%%        <<"last_seen_message_id">> := <<>>,
%%        <<"last_seen_message_time">> := _,
%%        <<"unread_count">> := 3,
%%        <<"is_enabled">> := true
%%    } = Conv3,
%%
%%    {ok, Conv4} = cmd_session(Pid, get_conversation, #{conversation_id=>C3}),
%%    #{
%%        <<"obj_id">> := C3,
%%        <<"last_active_time">> := 0,
%%        <<"last_seen_message_id">> := <<>>,
%%        <<"last_seen_message_time">> := _,
%%        <<"unread_count">> := 1,
%%        <<"is_enabled">> := true
%%    } = Conv4,
%%    {ok, #{<<"conversations">>:=[CL3, CL4]}} = cmd_session(Pid, get_all_conversations, #{}),
%%%%    true = lists:sort([Conv3, Conv4]) == lists:sort([CL3, CL4]),
%%
%%    % Now we activate C1, and send a message, that must be received. Counters should be reset.
%%    {ok, Conv5} = cmd_session(Pid, set_active_conversation, #{conversation_id=>C1}),
%%    #{
%%        <<"obj_id">> := C1,
%%%%        <<"last_active_time">> := 0,
%%        <<"last_seen_message_id">> := <<>>,
%%        <<"last_seen_message_time">> := _,
%%        <<"unread_count">> := 3
%%    } = Conv5,
%%    {ok, #{<<"obj_id">>:=M6}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_5}}),
%%    {created, S, C1, M6, _, <<"msgC1_5">>} = wait_session(Ref),
%%    {A10, C1, M6, T1, <<"msgC1_5">>} = wait_push(Ref),
%%    {A11, C1, M6, _, <<"msgC1_5">>} = wait_push(Ref),
%%    true = lists:sort([U2, U3]) == lists:sort([A10, A11]),
%%
%%    {ok, Conv6} = cmd_session(Pid, get_conversation, #{conversation_id=>C1}),
%%    #{
%%        <<"obj_id">> := C1,
%%        <<"last_active_time">> := _T2,
%%        <<"last_seen_message_id">> := M6,
%%        <<"last_seen_message_time">> := T1,
%%        <<"unread_count">> := 0,
%%        <<"is_enabled">> := true
%%    } = Conv6,
%%    {ok, Conv4} = cmd_session(Pid, get_conversation, #{conversation_id=>C3}),
%%
%%    % Lets update a message in C1, we should receive only session update
%%    {ok, #{}} = cmd_message(Pid, update, #{id=>M5, ?CHAT_MESSAGE => #{text=>msgC1_5B}}),
%%    {updated, S, C1, M5, _, <<"msgC1_5B">>} = wait_session(Ref),
%%
%%    % Lets delete a message in C1, we should receive only session update
%%    {ok, #{}} = cmd_message(Pid, delete, #{id=>M5}),
%%    {deleted, S, C1, M5} = wait_session(Ref),
%%
%%    % Lets add and remove an user to C3, since it is not active nothing happens
%%    {ok, #{<<"member_obj_id">>:=<<"admin">>}} = cmd_conversation(Pid, add_member, #{id=>C3, member_id=>admin}),
%%    {ok, #{}} = cmd_conversation(Pid, remove_member, #{id=>C3, member_id=>admin}),
%%
%%    % Lets activate C3 and repeat
%%    {error, {<<"conversation_not_found">>, _}} = cmd_session(Pid, set_active_conversation, #{conversation_id=>C2}),
%%    {ok, _} = cmd_session(Pid, set_active_conversation, #{conversation_id=>C3}),
%%    {ok, #{<<"member_obj_id">>:=<<"admin">>}} = cmd_conversation(Pid, add_member, #{id=>C3, member_id=>admin}),
%%    {member_added, S, C3, <<"admin">>} = wait_session(Ref),
%%    {ok, #{}} = cmd_conversation(Pid, remove_member, #{id=>C3, member_id=><<"admin">>}),
%%    {member_removed, S, C3, <<"admin">>} = wait_session(Ref),
%%
%%    % Lets add and remove conversations
%%    % Start C1 and C3
%%    {ok, #{<<"conversation_id">>:=C2}} = cmd_session(Pid, add_conversation, #{conversation_id=>C2}),
%%    {error, {<<"conversation_is_already_present">>, _}} = cmd_session(Pid, add_conversation, #{conversation_id=>C2}),
%%    {conversation_added, S, C2} = wait_session(Ref),
%%    {ok, #{}} = cmd_session(Pid, remove_conversation, #{conversation_id=>C2}),
%%    {error, {<<"conversation_not_found">>, _}} = cmd_session(Pid, remove_conversation, #{conversation_id=>C2}),
%%    {conversation_removed, S, C2} = wait_session(Ref),
%%
%%    {ok, #{}} = cmd_session(Pid, stop, #{}),
%%    session_stopped = wait_session(Ref),
%%    flush(),
%%    false = is_process_alive(SPid),
    ok.


%% ===================================================================
%% Client fun
%% ===================================================================


login(User, Pass, Ref) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password => nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    UserData = #{ref=>Ref, pid=>self()},
    {ok, _Reply, Pid} = nkapi_client:start(root, ?WS, Login, Fun, UserData),
    Pid.


api_client_fun(#nkreq{cmd = <<"event">>, data=Event}, #{ref:=Ref, pid:=Pid}=UserData) ->
    lager:warning("Event: ~p", [Event]),
    Pid ! {Ref, Event},
    {ok, UserData};

api_client_fun(_Req, UserData) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.

get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.

cmd_conversation(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, ?CHAT_CONVERSATION, <<>>, Cmd, Data).

cmd_message(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, ?CHAT_MESSAGE, <<>>, Cmd, Data).

cmd_session(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, ?CHAT_SESSION, <<>>, Cmd, Data).


cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).




mock_push(Ref) ->
    Self = self(),
    meck:new(nkdomain_user_obj, [passthrough]),
    meck:expect(nkdomain_user_obj, send_push,
        fun(Srv, Id, Event) -> Self ! {Ref, {Srv, Id, Event}}, ok end),
    Ref.

unmock_push() ->
    catch meck:unload(nkdomain_user_obj),
    code:purge(nkdomain_user_obj),
    {module,nkdomain_user_obj} = code:load_file(nkdomain_user_obj),
    ok.

wait_push(Ref) ->
    receive {Ref, {root, UId, Event}} ->
        #nkevent{
            class = ?CHAT_CONVERSATION,
            subclass = <<"message">>,
            type = <<"created">>,
            obj_id = CId,
            body = #{
                message_id := MsgId,
                text := Text,
                created_time := Time
            }
        } = Event,
        {UId, CId, MsgId, Time, Text}
    after 1000 ->
        error(?LINE)
    end.


wait_event(Ref) ->
    receive {Ref, #nkevent{}=Ev} ->
        Ev
    after 1000 ->
        error(?LINE)
    end.



mock_session(Ref) ->
    Self = self(),
    meck:new(nkchat_session_obj, [passthrough]),
    meck:expect(nkchat_session_obj, send_api_event,
        fun(Event, _Session) -> Self ! {Ref, Event}, ok end),
    Ref.

unmock_session() ->
    catch meck:unload(nkchat_session_obj),
    code:purge(nkchat_session_obj),
    {module,nkchat_session_obj} = code:load_file(nkchat_session_obj),
    ok.


wait_session(Ref) ->
    receive {Ref, #nkevent{}=Event} ->
        #nkevent{
            class = ?CHAT_SESSION,
            subclass = Sub,
            type = Type,
            obj_id = SessId,
            body = Body
        } = Event,
        case {Sub, Type} of
            {?CHAT_CONVERSATION, <<"message_created">>} ->
                #{
                    conversation_id := ConvId,
                    message_id := MsgId,
                    text := Text,
                    created_time := Time,
                    user := _
                } = Body,
                {created, SessId, ConvId, MsgId, Time, Text};
            {?CHAT_CONVERSATION, <<"message_updated">>} ->
                #{
                    conversation_id := ConvId,
                    message_id := MsgId,
                    text := Text,
                    updated_time := Time,
                    user := _
                } = Body,
                {updated, SessId, ConvId, MsgId, Time, Text};
            {?CHAT_CONVERSATION, <<"message_deleted">>} ->
                #{
                    conversation_id := ConvId,
                    message_id := MsgId
                } = Body,
                {deleted, SessId, ConvId, MsgId};
            {?CHAT_CONVERSATION, <<"unread_counter">>} ->
                #{
                    conversation_id := ConvId,
                    counter := Counter
                } = Body,
                {counter, SessId, ConvId, Counter};
            {?CHAT_CONVERSATION, <<"member_added">>} ->
                #{
                    conversation_id := ConvId,
                    member_id := MId,
                    user := _
                } = Body,
                {member_added, SessId, ConvId, MId};
            {?CHAT_CONVERSATION, <<"member_removed">>} ->
                #{
                    conversation_id := ConvId,
                    member_id := MId
                } = Body,
                {member_removed, SessId, ConvId, MId};
            {<<>>, <<"conversation_added">>} ->
                #{
                    conversation := #{obj_id:=ConvId}
                } = Body,
                {conversation_added, SessId, ConvId};
            {<<>>, <<"conversation_removed">>} ->
                #{
                    conversation := #{obj_id:=ConvId}
                } = Body,
                {conversation_removed, SessId, ConvId};
            {<<>>, <<"session_stopped">>} ->
                session_stopped
        end
    after 1000 ->
        error(?LINE)
    end.



flush() ->
    receive Msg ->
        error({received, Msg})
    after 1000 ->
        ok
    end.