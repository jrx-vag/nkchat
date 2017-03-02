
-module(nkchat_test).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).

-define(WS, "ws://127.0.0.1:10201/chat").
%%-define(WS, "wss://v1.netc.io/netcomp/chat/v00/nkapi/ws").
-define(HTTP, "http://127.0.0.1:10201/chat").


%% ===================================================================
%% Types
%% ===================================================================



start() ->
    Spec = #{
    	callback => ?MODULE,
        plugins => [nkelastic, nkchat],
        api_server => "ws:all:10201/chat, http://all:10201/chat",
        api_server_timeout => 180,
        elastic_url => "https://cluster.netc.io/es/",
        elastic_user => "user",
        elastic_pass => "es",
        debug => [
        ]
    },
    nkservice:start(chat, Spec).


stop() ->
    nkservice:stop(chat).
	


%% ===================================================================
%% Public functions
%% ===================================================================

create_indices() ->
    nkchat_es:create_indices(chat).


%% @doc
login() ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{user => <<"test">>, password => <<>>},
    {ok, _, C, _} = nkservice_api_client:start(chat, ?WS, Login, Fun, #{}),
    C.


user_create(Name, Surname, Login, Pass) ->
    Data = #{name=>Name, surname=>Surname, login=>Login, password=>Pass},
    case cmd(user, create, Data) of
        {ok, #{<<"user_id">>:=UserId}} ->
            {ok, UserId};
        {error, Error} ->
            {error, Error}
    end.


user_delete(UserId) ->
    cmd(user, delete, #{user_id=>UserId}).


user_search(Spec) ->
    cmd(user, search, Spec).


user_search2() ->
    Spec = #{
        fields => '_all',
        sort_by => [name],
        sort_order => desc
    },
    user_search(Spec).


conversation_create(Name, Desc, UserIds) ->
    Data = #{name=>Name, description=>Desc, user_ids=>UserIds},
    case cmd(conversation, create, Data) of
        {ok, #{<<"conversation_id">>:=ConvId}} ->
            {ok, ConvId};
        {error, Error} ->
            {error, Error}
    end.


conversation_delete(ConvId) ->
    cmd(conversation, delete, #{conversation_id=>ConvId}).


conversation_add(ConvId, UserIds) ->
    cmd(conversation, add_members, #{conversation_id=>ConvId, user_ids=>UserIds}).


conversation_del(ConvId, UserIds) ->
    cmd(conversation, remove_members, #{conversation_id=>ConvId, user_ids=>UserIds}).


conversation_get(ConvId) ->
    cmd(conversation, get_members, #{conversation_id=>ConvId}).


conversation_search(Spec) ->
    cmd(conversation, search, #{search_spec=>Spec}).


message_create(ConvId, UserId, Message) ->
    Data = #{conversation_id=>ConvId, user_id=>UserId, message=>Message},
    case cmd(message, create, Data) of
        {ok, #{<<"message_id">>:=MsgId}} ->
            {ok, MsgId};
        {error, Error} ->
            {error, Error}
    end.


message_update(MsgId, Message) ->
    cmd(message, update, #{message_id=>MsgId, message=>Message}).


message_delete(MsgId) ->
    cmd(message, delete, #{message_id=>MsgId}).


message_search(Spec) ->
    cmd(message, search, #{search_spec=>Spec}).


%% ===================================================================
%% Service callbacks
%% ===================================================================

api_server_allow(_Req, State) ->
%%    lager:error("Allow: ~p", [_Req]),
    {true, State}.


api_server_login(#{user:=User}, State) ->
    {true, User, #{}, State}.



%% ===================================================================
%% Internal
%% ===================================================================


api_client_fun(_Req, UserData) ->
    lager:error("TEST CLIENT req: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.


get_client() ->
    [{_, Pid}|_] = nkservice_api_client:get_all(),
    Pid.


cmd(Sub, Cmd, Data) ->
    cmd(get_client(), Sub, Cmd, Data).


cmd(Pid, Sub, Cmd, Data) ->
    nkservice_api_client:cmd(Pid, chat, Sub, Cmd, Data).


cmd_http(Sub, Cmd, Data) ->
    Body = #{
        class => admin,
        subclass => Sub,
        cmd => Cmd,
        data => Data
    },
    nkservice_util:http(post, [?HTTP, "/rpc"], #{body=>Body}).

