
-module(nkchat_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-define(WS, "ws://127.0.0.1:9202/api/ws").
%%-define(WS, "wss://v1.netc.io/netcomp/chat/v00/nkapi/ws").
%%-define(HTTP, "http://127.0.0.1:10201/chat").

-include_lib("nkchat.hrl").
-include_lib("nkapi/include/nkapi.hrl").


login() ->
    nkdomain_test:login("/chattest/users/u2", "1234").

domain_find_convs() ->
    cmd(domain, find_all_childs, #{type=>?CHAT_CONVERSATION, sort=>[type, path]}).





conv_create(Domain, Name, Desc) ->
    cmd('chat.conversation', create, #{obj_name=>Name, description=>Desc, domain=>Domain}).

conv_get() ->
    cmd('chat.conversation', get, #{}).

conv_get(Id) ->
    cmd('chat.conversation', get, #{id=>Id}).

conv_add_member(Id, Member) ->
    cmd('chat.conversation', add_member, #{id=>Id, member_id=>Member}).

conv_remove_member(Id, Member) ->
    cmd('chat.conversation', remove_member, #{id=>Id, member_id=>Member}).

conv_delete(Id) ->
    cmd('chat.conversation', delete, #{id=>Id}).

conv_get_messages(Id, Spec) ->
    cmd('chat.conversation', get_messages, Spec#{id=>Id}).



message_create(ConvId, Msg) ->
    cmd('chat.message', create, #{conversation_id=>ConvId, 'chat.message'=>#{message=>Msg}}).

message_get(MsgId) ->
     cmd('chat.message', get, #{id=>MsgId}).

message_update(MsgId, Msg) ->
    cmd('chat.message', update, #{id=>MsgId, 'chat.message'=>#{message=>Msg}}).

message_delete(MsgId) ->
    cmd('chat.message', delete, #{id=>MsgId}).



session_find() ->
    cmd('chat.session', find, #{}).

session_find(UserId) ->
    cmd('chat.session', find, #{user_id=>UserId}).

session_create() ->
    cmd('chat.session', create, #{}).

session_create(UserId) ->
    cmd('chat.session', create, #{user_id=>UserId}).

session_start(SessId) ->
    cmd('chat.session', start, #{id=>SessId}).

session_info() ->
    cmd('chat.session', get_info, #{}).

session_info(SessId) ->
    cmd('chat.session', get_info, #{id=>SessId}).

session_stop() ->
    cmd('chat.session', stop, #{}).

session_delete(Id) ->
    cmd('chat.session', delete, #{id=>Id}).

session_set_active(ConvId) ->
    cmd('chat.session', set_active_conversation, #{conversation_id=>ConvId}).

session_add_conversation(ConvId) ->
    cmd('chat.session', add_conversation, #{conversation_id=>ConvId}).

session_remove_conversation(ConvId) ->
    cmd('chat.session', remove_conversation, #{conversation_id=>ConvId}).




%% ===================================================================
%% Client fun
%% ===================================================================



%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Class, Cmd, Data) ->
    Pid = nkdomain_test:get_client(),
    cmd(Pid, Class, Cmd, Data).

cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).



