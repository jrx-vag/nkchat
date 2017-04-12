
-module(nkchat_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-include("nkchat.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(WS, "ws://127.0.0.1:9202/api/ws").
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






test1() ->

    clear(),
    Pid = login("admin", ?ADMIN_PASS),
    ok = create(Pid),
    Ref = make_ref(),
    ok = base_msgs(Pid, Ref),
    nkapi_client:stop(Pid),

    Pid2 = login("/chattest/users/u1", "p1"),
    session1(Pid2, Ref),


    unmock_push(),
    unmock_session().





clear() ->
    nkdomain_store:delete_all_childs(root, ?DOM),
    nkdomain_domain_obj:create(root, "/", "chattest", "Chat test"),
    ok.


%% Creates the structure detailed above
create(Pid) ->
    U1_Data = #{obj_name=>u1, domain=> ?DOM,
                user=>#{name=>u1, surname=>s1, password=>p1, email=><<"u1@chattest">>}},
    {ok, #{<<"obj_id">>:=U1}} = cmd(Pid, user, create, U1_Data),
    U2_Data = #{obj_name=>u2, domain=>?DOM,
                user=>#{name=>u2, surname=>s2, password=>p2, email=><<"u1@chattest">>}},
    {ok, #{<<"obj_id">>:=U2}} = cmd(Pid, user, create, U2_Data),
    U3_Data = #{obj_name=>u3, domain=>?DOM,
                user=>#{name=>u3, surname=>s3, password=>p3, email=><<"u3@chattest">>}},
    {ok, #{<<"obj_id">>:=U3}} = cmd(Pid, user, create, U3_Data),

    C1_Data = #{obj_name=>conv1, description=><<"Conv 1">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C1}} = cmd_conversation(Pid, create, C1_Data),
    C2_Data = #{obj_name=>conv2, description=><<"Conv 2">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C2}} = cmd_conversation(Pid, create, C2_Data),
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

    {ok, <<"domain">>, D, ?DOM, _Pid} = nkdomain:find(root, ?DOM),
    {ok, #{
        <<"_is_enabled">> := true,
        ?CHAT_CONVERSATION := #{<<"member_ids">> := [C3U1, C3U2]},
        <<"description">> := <<"Conv 3">>,
        <<"obj_id">> := C3,
        <<"parent_id">> := D,
        <<"path">> := <<"/chattest/conversations/conv3">>,
        <<"type">> := ?CHAT_CONVERSATION
    }} =
        cmd_conversation(Pid, get, #{}),
    true = lists:sort([U2, U3]) == lists:sort([C3U1, C3U2]),

    {ok, #{
        <<"_is_enabled">> := true,
        ?CHAT_CONVERSATION := #{<<"member_ids">> := [C1U1, C1U2, C1U3]},
        <<"description">> := <<"Conv 1">>,
        <<"obj_id">> := C1,
        <<"parent_id">> := D,
        <<"path">> := <<"/chattest/conversations/conv1">>,
        <<"type">> := ?CHAT_CONVERSATION
    }} =
        cmd_conversation(Pid, get, #{id=>C1}),
    true = lists:sort([U1, U2, U3]) == lists:sort([C1U1, C1U2, C1U3]),

    {ok, #{
        <<"_is_enabled">> := true,
        ?CHAT_CONVERSATION := #{<<"member_ids">> := [C2U1, C2U2]},
        <<"description">> := <<"Conv 2">>,
        <<"obj_id">> := C2,
        <<"parent_id">> := D,
        <<"path">> := <<"/chattest/conversations/conv2">>,
        <<"type">> := ?CHAT_CONVERSATION
    }} =
        cmd_conversation(Pid, get, #{id=>C2}),
    true = lists:sort([U1, U2]) == lists:sort([C2U1, C2U2]),
    ok.


%% Send messages to conversations without sessions, pushes will be sent
base_msgs(Pid, Ref) ->
    mock_push(Ref),

    {ok, _, U1, _, _} = nkdomain:find(root, "/chattest/users/u1"),
    {ok, _, U2, _, _} = nkdomain:find(root, "/chattest/users/u2"),
    {ok, _, U3, _, _} = nkdomain:find(root, "/chattest/users/u3"),
    {ok, _, C1, _, _} = nkdomain:find(root, <<"/chattest/conversations/conv1">>),
    {ok, _, C2, _, _} = nkdomain:find(root, <<"/chattest/conversations/conv2">>),
    {ok, _, C3, _, _} = nkdomain:find(root, <<"/chattest/conversations/conv3">>),
    BC1 = #{conversation_id=><<"/chattest/conversations/conv1">>},
    BC2 = #{conversation_id=><<"/chattest/conversations/conv2">>},
    BC3 = #{conversation_id=><<"/chattest/conversations/conv3">>},

    {ok, #{<<"obj_id">>:=M1}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_1}}),
    {A1, C1, M1, _, <<"msgC1_1">>} = wait_push(Ref),
    {A2, C1, M1, _, <<"msgC1_1">>} = wait_push(Ref),
    {A3, C1, M1, _, <<"msgC1_1">>} = wait_push(Ref),
    true = lists:sort([U1, U2, U3]) == lists:sort([A1, A2, A3]),

    {ok, #{<<"obj_id">>:=M2}} = cmd_message(Pid, create, BC2#{?CHAT_MESSAGE => #{text=>msgC2_1}}),
    {A4, C2, M2, _, <<"msgC2_1">>} = wait_push(Ref),
    {A5, C2, M2, _, <<"msgC2_1">>} = wait_push(Ref),
    true = lists:sort([U1, U2]) == lists:sort([A4, A5]),

    {ok, #{<<"obj_id">>:=M3}} = cmd_message(Pid, create, BC3#{?CHAT_MESSAGE => #{text=>msgC3_1}}),
    {A6, C3, M3, _, <<"msgC3_1">>} = wait_push(Ref),
    {A7, C3, M3, _, <<"msgC3_1">>} = wait_push(Ref),
    true = lists:sort([U2, U3]) == lists:sort([A6, A7]),
    {ok, #{<<"obj_id">>:=M4}} = cmd_message(Pid, create, BC3#{?CHAT_MESSAGE => #{text=>msgC3_2}}),
    {A6, C3, M4, _, <<"msgC3_2">>} = wait_push(Ref),
    {A7, C3, M4, _, <<"msgC3_2">>} = wait_push(Ref),
    ok.



session1(Pid, Ref) ->
    % Create the session
    {error, {<<"session_not_found">>, _}} = cmd_session(Pid, find, #{}),
    {ok, #{<<"obj_id">>:=S, <<"conversations">>:=[]}} = cmd_session(Pid, create, #{}),
    {ok, #{<<"obj_id">>:=S}} = cmd_session(Pid, find, #{}),
    {ok, #{}} = cmd_session(Pid, stop, #{}),
    {error, {<<"object_not_started">>, _}} = cmd_session(Pid, stop, #{}),
    {error, {<<"session_not_found">>, _}} = cmd_session(Pid, stop, #{id=><<"a">>}),
    {ok, #{<<"obj_id">>:=S, <<"conversations">>:=[]}} = cmd_session(Pid, start, #{id=>S}),
    {ok, _, _, _, SPid} = nkdomain:find(root, S),

    % Start C1 and C3
    {ok, #{<<"conversation_id">>:=C1}} =
        cmd_session(Pid, add_conversation, #{conversation_id=>"/chattest/conversations/conv1"}),
    {ok, #{<<"conversation_id">>:=C3}} =
        cmd_session(Pid, add_conversation, #{conversation_id=>"/chattest/conversations/conv3"}),
    {ok, _, C2, _, _} = nkdomain:find(root, <<"/chattest/conversations/conv2">>),

    BC1 = #{conversation_id=><<"/chattest/conversations/conv1">>},
    BC2 = #{conversation_id=><<"/chattest/conversations/conv2">>},
    BC3 = #{conversation_id=>C3},
    {ok, _, U1, _, _} = nkdomain:find(root, "/chattest/users/u1"),
    {ok, _, U2, _, _} = nkdomain:find(root, "/chattest/users/u2"),
    {ok, _, U3, _, _} = nkdomain:find(root, "/chattest/users/u3"),

    mock_session(Ref),

    % Send a message to C1, U1 receives an updated counter, U2 and U3 receive push
    {ok, #{<<"obj_id">>:=M1}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_2}}),
    {counter, S, C1, 1} = wait_session(Ref),
    {A1, C1, M1, _, <<"msgC1_2">>} = wait_push(Ref),
    {A2, C1, M1, _, <<"msgC1_2">>} = wait_push(Ref),
    true = lists:sort([U2, U3]) == lists:sort([A1, A2]),

    {ok, #{<<"obj_id">>:=M2}} = cmd_message(Pid, create, BC1#{?CHAT_MESSAGE => #{text=>msgC1_3}}),
    {counter, S, C1, 2} = wait_session(Ref),
    {A1, C1, M2, _, <<"msgC1_3">>} = wait_push(Ref),
    {A2, C1, M2, _, <<"msgC1_3">>} = wait_push(Ref),
    true = lists:sort([U2, U3]) == lists:sort([A1, A2]),

    {ok, #{<<"conversations">>:=[CL1, CL2]}} = cmd_session(Pid, get_all_conversations, #{}),
    {ok, Conv1} = cmd_session(Pid, get_conversation, #{conversation_id=>C1}),
    #{
        <<"obj_id">> := C1,
        <<"last_active_time">> := 0,
        <<"last_read_message_id">> := <<>>,
        <<"last_read_message_time">> := _,
        <<"_unread_count">> := 2,
        <<"_enabled">> := true
    } = Conv1,

    {error, {<<"conversation_not_found">>, _}} = cmd_session(Pid, get_conversation, #{conversation_id=>C2}),

    {ok, Conv3} = cmd_session(Pid, get_conversation, #{conversation_id=>C3}),
    #{
        <<"obj_id">> := C3,
        <<"last_active_time">> := 0,
        <<"last_read_message_id">> := <<>>,
        <<"last_read_message_time">> := _,
        <<"_enabled">> := true
    } = Conv3,
    false = maps:is_key(<<"_unread_count">>, Conv3),
    true = lists:sort([Conv1, Conv3]) == lists:sort([CL1, CL2]),

    % Send msg to C2, U1 and U2 receive push
    {ok, #{<<"obj_id">>:=M3}} = cmd_message(Pid, create, BC2#{?CHAT_MESSAGE => #{text=>msgC2_2}}),
    {A3, C2, M3, _, <<"msgC2_2">>} = wait_push(Ref),
    {A4, C2, M3, _, <<"msgC2_2">>} = wait_push(Ref),
    true = lists:sort([U1, U2]) == lists:sort([A3, A4]),

    % Send msg to C3, U2 receive push, U1 has a session and receives counter event
    {ok, #{<<"obj_id">>:=M4}} = cmd_message(Pid, create, BC3#{?CHAT_MESSAGE => #{text=>msgC3_3}}),
    {counter, S, C3, 1} = wait_session(Ref),
    {A5, C3, M4, _, <<"msgC3_3">>} = wait_push(Ref),
    {A6, C3, M4, _, <<"msgC3_3">>} = wait_push(Ref),
    true = lists:sort([U2, U3]) == lists:sort([A5, A6]),
    ok.














%% ===================================================================
%% Client fun
%% ===================================================================


login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, _SessId, Pid, _Reply} = nkapi_client:start(root, ?WS, Login, Fun, #{}),
    Pid.


api_client_fun(#nkapi_req{class=event, data=Event}, UserData) ->
    lager:notice("CLIENT event ~p", [lager:pr(Event, nkservice_events)]),

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
        #event{
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
    receive {Ref, #event{}=Event} ->
        #event{
            class = ?CHAT_SESSION,
            subclass = ?CHAT_CONVERSATION,
            type = Type,
            obj_id = SessId,
            body = Body
        } = Event,
        case Type of
            <<"message_created">> ->
                #{
                    conversation_id := ConvId,
                    message_id := MsgId,
                    text := Text,
                    created_time := Time
                } = Body,
                {created, SessId, ConvId, MsgId, Time, Text};
            <<"unread_counter">> ->
                #{
                    conversation_id := ConvId,
                    counter := Counter
                } = Body,
                {counter, SessId, ConvId, Counter}
        end
    after 1000 ->
        error(?LINE)
    end.
