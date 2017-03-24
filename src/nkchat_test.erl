
-module(nkchat_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-define(WS, "ws://127.0.0.1:9202/api/ws").
%%-define(WS, "wss://v1.netc.io/netcomp/chat/v00/nkapi/ws").
%%-define(HTTP, "http://127.0.0.1:10201/chat").

-include_lib("nkapi/include/nkapi.hrl").


conv_create(Name, Domain, Desc) ->
    cmd('chat.conversation', create, #{obj_name=>Name, description=>Desc, domain=>Domain}).

conv_get(Id) ->
    cmd('chat.conversation', get, #{id=>Id}).


conv_add_members(Id, Members) ->
    cmd('chat.conversation', add_members, #{id=>Id, member_ids=>Members}).

conv_remove_members(Id, Members) ->
    cmd('chat.conversation', remove_members, #{id=>Id, member_ids=>Members}).

conv_delete() -> ok.






%% ===================================================================
%% Client fun
%% ===================================================================



%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Class, Cmd, Data) ->
    Pid = nkdomain_test:get_client(),
    cmd(Pid, Class, Cmd, Data).

cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).



