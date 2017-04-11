
-module(nkchat_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-include("nkchat.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

-define(WS, "ws://127.0.0.1:9202/api/ws").
-define(ADMIN_PASS, "1234").
-define(DOM, <<"/chattest">>).



%% Creates:
%%
%% - Domain /chattest
%% - Users /chattest/users/u1, u2, u3
%% - Conversations /chattest/chat.conversations:
%%      - conv1:
%%      - conv2:
%%      - conv3
%%






test1() ->
    code:purge(nkdomain_user_obj),
    {module,nkdomain_user_obj} = code:load_file(nkdomain_user_obj),

%%    clear(),
    Pid = login("admin", ?ADMIN_PASS),
%%    ok = create(Pid),
    base_msgs(Pid).





clear() ->
    nkdomain_store:delete_all_childs(root, ?DOM),
    nkdomain_domain_obj:create(root, "/", "chattest", "Chat test"),
    ok.


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
    {ok, #{<<"obj_id">>:=C1}} = cmd(Pid, ?CHAT_CONVERSATION, create, C1_Data),
    C2_Data = #{obj_name=>conv2, description=><<"Conv 2">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C2}} = cmd(Pid, ?CHAT_CONVERSATION, create, C2_Data),
    C3_Data = #{obj_name=>conv3, description=><<"Conv 3">>, domain=>?DOM},
    {ok, #{<<"obj_id">>:=C3}} = cmd(Pid, ?CHAT_CONVERSATION, create, C3_Data),

    {ok, #{<<"member_obj_id">>:=U1}} =
        cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=>C1, member_id=><<"/chattest/users/u1">>}),
    {ok, #{<<"member_obj_id">>:=U2}} =
        cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=><<"/chattest/chat.conversations/conv1">>, member_id=>U2}),
    {ok, #{<<"member_obj_id">>:=U3}} =
        cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=>C1, member_id=>U3}),
    {ok, #{<<"member_obj_id">>:=U1}} = cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=>C2, member_id=>U1}),
    {ok, #{<<"member_obj_id">>:=U2}} = cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=>C2, member_id=>U2}),
    {ok, #{<<"member_obj_id">>:=U2}} = cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=>C3, member_id=>U2}),
    {ok, #{<<"member_obj_id">>:=U3}} = cmd(Pid, ?CHAT_CONVERSATION, add_member, #{id=>C3, member_id=>U3}),

    {error,{<<"missing_field">>, <<"Missing field: 'obj_name'">>}} =
        cmd(Pid, ?CHAT_CONVERSATION, create, #{domain=><<"/none">>}),
    {error,{<<"missing_field">>, <<"Missing field: 'description'">>}} =
        cmd(Pid, ?CHAT_CONVERSATION, create, #{obj_name=>c4, domain=><<"/none">>}),
    {error,{<<"could_not_load_parent">>, <<"Object could not load parent '/none'">>}}  =
        cmd(Pid, ?CHAT_CONVERSATION, create, #{obj_name=>c4, description=>desc, domain=><<"/none">>}),
    {error,{<<"object_already_exists">>, _}} =
        cmd(Pid, ?CHAT_CONVERSATION, create, #{obj_name=>conv1, description=>desc, domain=>?DOM}),

    {ok, <<"domain">>, D, ?DOM, _Pid} = nkdomain:find(root, ?DOM),
    {ok, #{
        <<"_is_enabled">> := true,
        <<"chat.conversation">> := #{<<"member_ids">> := [C3U1, C3U2]},
        <<"description">> := <<"Conv 3">>,
        <<"obj_id">> := C3,
        <<"parent_id">> := D,
        <<"path">> := <<"/chattest/chat.conversations/conv3">>,
        <<"type">> := <<"chat.conversation">>
    }} =
        cmd(Pid, ?CHAT_CONVERSATION, get, #{}),
    true = lists:sort([U2, U3]) == lists:sort([C3U1, C3U2]),

    {ok, #{
        <<"_is_enabled">> := true,
        <<"chat.conversation">> := #{<<"member_ids">> := [C1U1, C1U2, C1U3]},
        <<"description">> := <<"Conv 1">>,
        <<"obj_id">> := C1,
        <<"parent_id">> := D,
        <<"path">> := <<"/chattest/chat.conversations/conv1">>,
        <<"type">> := <<"chat.conversation">>
    }} =
        cmd(Pid, ?CHAT_CONVERSATION, get, #{id=>C1}),
    true = lists:sort([U1, U2, U3]) == lists:sort([C1U1, C1U2, C1U3]),

    {ok, #{
        <<"_is_enabled">> := true,
        <<"chat.conversation">> := #{<<"member_ids">> := [C2U1, C2U2]},
        <<"description">> := <<"Conv 2">>,
        <<"obj_id">> := C2,
        <<"parent_id">> := D,
        <<"path">> := <<"/chattest/chat.conversations/conv2">>,
        <<"type">> := <<"chat.conversation">>
    }} =
        cmd(Pid, ?CHAT_CONVERSATION, get, #{id=>C2}),
    true = lists:sort([U1, U2]) == lists:sort([C2U1, C2U2]),
    ok.

base_msgs(Pid) ->
    Ref = make_ref(),
    Self = self(),
    meck:new(nkdomain_user_obj, [passthrough]),
    meck:expect(nkdomain_user_obj, send_push,
        fun(Srv, Id, Type, ObjId, Msg) -> Self ! {Ref, {Srv, Id, Type, ObjId, Msg}}, ok end),


    {ok, _, U1, _, _} = nkdomain:find(root, "/chattest/users/u1"),
    {ok, _, U2, _, _} = nkdomain:find(root, "/chattest/users/u2"),
    {ok, _, U3, _, _} = nkdomain:find(root, "/chattest/users/u3"),
    {ok, _, C1, _, _} = nkdomain:find(root, <<"/chattest/chat.conversations/conv1">>),
    {ok, _, C2, _, _} = nkdomain:find(root, <<"/chattest/chat.conversations/conv2">>),
    {ok, _, C3, _, _} = nkdomain:find(root, <<"/chattest/chat.conversations/conv3">>),
    BC1 = #{conversation_id=><<"/chattest/chat.conversations/conv1">>},
    BC2 = #{conversation_id=><<"/chattest/chat.conversations/conv2">>},
    BC3 = #{conversation_id=><<"/chattest/chat.conversations/conv3">>},

    {ok, #{<<"obj_id">>:=M1}} = cmd(Pid, ?CHAT_MESSAGE, create, BC1#{?CHAT_MESSAGE => #{message=>msgC1_1}}),
    {A1, C1, M1, _, <<"msgC1_1">>} = wait_push(Ref),
    {A2, C1, M1, _, <<"msgC1_1">>} = wait_push(Ref),
    {A3, C1, M1, _, <<"msgC1_1">>} = wait_push(Ref),
    true = lists:sort([U1, U2, U3]) == lists:sort([A1, A2, A3]),

    {ok, #{<<"obj_id">>:=M2}} = cmd(Pid, ?CHAT_MESSAGE, create, BC2#{?CHAT_MESSAGE => #{message=>msgC2_1}}),
    {A4, C2, M2, _, <<"msgC2_1">>} = wait_push(Ref),
    {A5, C2, M2, _, <<"msgC2_1">>} = wait_push(Ref),
    true = lists:sort([U1, U2]) == lists:sort([A4, A5]),

    {ok, #{<<"obj_id">>:=M3}} = cmd(Pid, ?CHAT_MESSAGE, create, BC3#{?CHAT_MESSAGE => #{message=>msgC3_1}}),
    {A6, C3, M3, _, <<"msgC3_1">>} = wait_push(Ref),
    {A7, C3, M3, _, <<"msgC3_1">>} = wait_push(Ref),
    true = lists:sort([U2, U3]) == lists:sort([A6, A7]),




    meck:unload(nkdomain_user_obj).





wait_push(Ref) ->
    receive
        {Ref, {axft4mi, UId, ?CHAT_CONVERSATION, CId, {message, MId, {created, T, #{message:=Msg}}}}} ->
            {UId, CId, MId, T, Msg}
    after 1000 ->
            error(?LINE)
    end.




% f(C1), f(C2), [C1, C2] = nkchat_test2:get_cons().
get_cons() ->
    Search = #{filters => #{type => ?CHAT_CONVERSATION}, sort => [created_time]},
    {ok, 2, List} = nkdomain_domain_obj:find_childs(root, ?DOM, Search),
    [ObjId || {?CHAT_CONVERSATION, ObjId, _Path} <- List].



conv_t1() ->

    % Delete (if exists) and create test data
    nkdomain:delete(root, "/chattest/users/u1"),
    nkdomain:delete(root, "/chattest/users/u2"),
    nkdomain:delete(root, "/chattest/chat.conversations/conv1"),

    nkdomain_user_obj:create(root, "/chattest", u1, #{name=>u1, surname=>s1}),
    nkdomain_user_obj:create(root, "/chattest", u2, #{name=>u2, surname=>s2}),
    {ok, C, _, CP} = nkchat_conversation_obj:create(root, "/chattest", "conv1", "Conv 1"),

    % Add user admin to conv1 and remove it
    {ok, <<"admin">>} = nkchat_conversation_obj:add_member(root, C, admin),
    {error, member_already_present} = nkchat_conversation_obj:add_member(root, C, admin),
    ok = nkchat_conversation_obj:remove_member(root, C, admin),
    {error, member_not_found} = nkchat_conversation_obj:remove_member(root, C, admin),

    %% Ad users u1 and u2
    {ok, U1} = nkchat_conversation_obj:add_member(root, C, "/chattest/users/u1"),
    {ok, U2} = nkchat_conversation_obj:add_member(root, C, "/chattest/users/u2"),
    timer:sleep(100),
    {ok, #{<<"chat.conversation">>:=#{<<"member_ids">>:=Ids1}}} =
        nkdomain_store_es:object_store_read_raw(axft4mi, C),
    Ids1S = lists:sort([U1, U2]),
    Ids1S = lists:sort(Ids1),

    % Check the user monitors are working, disabling u1
    {ok, #obj_session{data=Data1}} = nkdomain_obj:get_session(C),
    Mon1 = element(2, Data1),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon1)),
    [] = nkdomain_monitor:get_disabled(Mon1),
    nkdomain_obj:unload(U1, normal),
    timer:sleep(500),
    {ok, #obj_session{data=Data2}} = nkdomain_obj:get_session(C),
    Mon2 = element(2, Data2),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon2)),
    [{U1, Time}] = nkdomain_monitor:get_disabled(Mon2),
    true = (nklib_util:timestamp() - Time) < 2,
    {ok, _, _, _, Pid} = nkdomain:find(C),

    % Force check of disabled users
    Pid ! {nkchat_conversation_obj, check_time},
    timer:sleep(100),
    {ok, #obj_session{data=Data3}} = nkdomain_obj:get_session(C),
    Mon3 = element(2, Data3),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon3)),
    [] = nkdomain_monitor:get_disabled(Mon1),

    % We delete U1, must be disabled again
    ok = nkdomain:delete(root, U1),
    timer:sleep(100),
    {ok, #obj_session{data=Data4}} = nkdomain_obj:get_session(C),
    Mon4 = element(2, Data4),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon4)),
    [{U1, _Time}] = nkdomain_monitor:get_disabled(Mon2),

    % We unload conv1, when we reload it will remain disabled
    nkdomain_obj:unload(C, normal),
    timer:sleep(100),
    false = is_process_alive(CP),
    {ok, _, _, _, _} = nkdomain:load(C),
    {ok, #obj_session{data=Data5}} = nkdomain_obj:get_session(C),
    Mon5 = element(2, Data5),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon5)),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon4)),
    [{U1, _Time}] = nkdomain_monitor:get_disabled(Mon2),

    % Lets remove it from the conversation, or it will try to find it forever
    ok = nkchat_conversation_obj:remove_member(root, C, U1),
    ok3.















add1(C) ->
    nkchat_conversation_obj:add_member(root, C, admin).

rem1(C) ->
    nkchat_conversation_obj:remove_member(root, C, admin).




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


%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Class, Cmd, Data) ->
    Pid = get_client(),
    cmd(Pid, Class, Cmd, Data).

cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).

