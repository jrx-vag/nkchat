
-module(nkchat_test2).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-define(WS, "ws://127.0.0.1:9202/api/ws").
%%-define(WS, "wss://v1.netc.io/netcomp/chat/v00/nkapi/ws").
%%-define(HTTP, "http://127.0.0.1:10201/chat").

-include("nkchat.hrl").
-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").



login() ->
    login(admin, "1234").

login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass),
        meta => #{a=>nklib_util:to_binary(User)}
    },
    {ok, _SessId, _Pid, _Reply} = nkapi_client:start(root, ?WS, Login, Fun, #{}).



clear() ->
    nkdomain_store:delete_all_childs(root, "/chattest"),
    nkdomain_domain_obj:create(root, "/", "chattest", "Chat test").


create() ->
    nkdomain_domain_obj:create(root, "/", "chattest", "Chat test"),
    nkdomain_user_obj:create(root, "/chattest", u1, #{name=>u1, surname=>s1}),
    nkdomain_user_obj:create(root, "/chattest", u2, #{name=>u2, surname=>s2}),
    nkdomain_user_obj:create(root, "/chattest", u3, #{name=>u2, surname=>s3}),
    {ok, _, _, _} = nkchat_conversation_obj:create(root, "/chattest", "conv1", "Conv 1"),
    {ok, _, _, _} = nkchat_conversation_obj:create(root, "/chattest", "conv2", "Conv 2").


% f(C1), f(C2), [C1, C2] = nkchat_test2:get_cons().
get_cons() ->
    Search = #{filters => #{type => ?CHAT_CONVERSATION}, sort => [created_time]},
    {ok, 2, List} = nkdomain_domain_obj:find_childs(root, "/chattest", Search),
    [ObjId || {?CHAT_CONVERSATION, ObjId, _Path} <- List].

d() ->
    nkdomain_obj_lib:delete(root, "/chattest/chat.conversations/conv1", normal),
    lager:error("deleted"),
    nkchat_conversation_obj:create(root, "/chattest", "conv1", "Conv 1").

conv_t1() ->
    nkdomain_obj_lib:delete(root, "/chattest/users/u1", normal),
    nkdomain_obj_lib:delete(root, "/chattest/users/u2", normal),
    nkdomain_obj_lib:delete(root, "/chattest/chat.conversations/conv1", normal),
    timer:sleep(500),

    nkdomain_user_obj:create(root, "/chattest", u1, #{name=>u1, surname=>s1}),
    nkdomain_user_obj:create(root, "/chattest", u2, #{name=>u2, surname=>s2}),
    {ok, C, _, CP} = nkchat_conversation_obj:create(root, "/chattest", "conv1", "Conv 1"),
    timer:sleep(500),

    {ok, <<"admin">>} = nkchat_conversation_obj:add_member(root, C, admin),
    {error,member_already_present} = nkchat_conversation_obj:add_member(root, C, admin),
    ok = nkchat_conversation_obj:remove_member(root, C, admin),
    {error,member_not_found} = nkchat_conversation_obj:remove_member(root, C, admin),

    {ok, U1} = nkchat_conversation_obj:add_member(root, C, "/chattest/users/u1"),
    {ok, U2} = nkchat_conversation_obj:add_member(root, C, "/chattest/users/u2"),
    timer:sleep(100),
    {ok, #{<<"chat.conversation">>:=#{<<"member_ids">>:=Ids1}}} =
        nkdomain_store_es:object_store_read_raw(axft4mi, C),
    Ids1S = lists:sort([U1, U2]),
    Ids1S = lists:sort(Ids1),

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
    Pid ! {nkchat_conversation_obj, check_time},
    timer:sleep(100),
    {ok, #obj_session{data=Data3}} = nkdomain_obj:get_session(C),
    Mon3 = element(2, Data3),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon3)),
    [] = nkdomain_monitor:get_disabled(Mon1),

    ok = nkdomain_obj_lib:delete(root, U1, normal),
    timer:sleep(100),
    {ok, #obj_session{data=Data4}} = nkdomain_obj:get_session(C),
    Mon4 = element(2, Data4),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon4)),
    [{U1, _Time}] = nkdomain_monitor:get_disabled(Mon2),
    nkdomain_obj:unload(C, normal),
    timer:sleep(100),
    false = is_process_alive(CP),

    {ok, _, _, _} = nkdomain:load(C),
    {ok, #obj_session{data=Data5}} = nkdomain_obj:get_session(C),
    Mon5 = element(2, Data5),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon5)),
    Ids1A = lists:sort(nkdomain_monitor:get_objs(Mon4)),
    [{U1, _Time}] = nkdomain_monitor:get_disabled(Mon2),


    ok2.















add1(C) ->
    nkchat_conversation_obj:add_member(root, C, admin).

rem1(C) ->
    nkchat_conversation_obj:remove_member(root, C, admin).




%% ===================================================================
%% Client fun
%% ===================================================================


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

