%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc User Object

-module(nkchat_message_obj_ui).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([table/1, table_data/2]).

-include("nkchat.hrl").

-define(ID, <<"domain_detail_chat_messages_table">>).
-define(ID_SUBDOMAINS, <<"domain_detail_chat_messages_table_subdomains">>).


%% @doc
table(Session) ->
    Spec = Session#{
        table_id => ?ID,
        subdomains_id => ?ID_SUBDOMAINS,
        filters => [?ID_SUBDOMAINS],
        columns => [
            #{
                id => pos,
                type => pos,
                name => domain_column_pos
            },
            #{
                id => conversation,
                type => text,
                name => domain_column_conversation,
                sort => true
            },
            #{
                id => text,
                type => text,
                name => domain_column_text,
                editor => text,
                fillspace => 2
            },
            #{
                id => created_by,
                type => text,
                name => domain_column_created_by,
                sort => true
            },
            #{
                id => created_time,
                type => date,
                name => domain_column_created_time,
                sort => true
            },
            #{
                id => enabled_icon,
                type => {icon, <<"enabled_icon">>}
            },
            #{
                id => delete,
                type => {fixed_icon, <<"fa-trash">>}
            }
        ],
        left_split => 1,
        right_split => 2,
        on_click => [
            #{
                id => <<"fa-times">>,
                type => enable
            },
            #{
                id => <<"fa-check">>,
                type => disable
            },
            #{
                id => <<"fa-trash">>,
                type => delete
            }            
        ]
    },
    Table = #{
        id => ?ID,
        class => webix_ui,
        value => nkadmin_webix_datatable:datatable(Spec)
    },
    KeyData = #{data_fun => fun ?MODULE:table_data/2},
    Session2 = nkadmin_util:set_key_data(?ID, KeyData, Session),
    {Table, Session2}.


%% @doc
table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, #{srv_id:=SrvId, domain_id:=DomainId}) ->
    SortSpec = case Sort of
        {<<"conversation">>, Order} ->
            <<Order/binary, ":path">>;
        {Field, Order} when Field==<<"created_by">>; Field==<<"created_time">> ->
            <<Order/binary, $:, Field/binary>>;
        _ ->
            <<"desc:path">>
    end,
    Filters = table_filter(maps:to_list(Filter), #{type=>message}),
    FindSpec = #{
        filters => Filters,
        fields => [<<"path">>, <<"created_by">>, <<"created_time">>,
                   <<"parent_id">>, <<"message.text">>],
        sort => SortSpec,
        from => Start,
        size => Size
    },
    Fun = case Filter of
        #{?ID_SUBDOMAINS := 0} -> find;
        _ -> find_all
    end,
    case nkdomain_domain_obj:Fun(SrvId, DomainId, FindSpec) of
        {ok, Total, List, _Meta} ->
            Data = table_iter(List, Start+1, []),
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.


%% @private
table_filter([], Acc) ->
    Acc;

table_filter([{_, <<>>}|Rest], Acc) ->
    table_filter(Rest, Acc);

table_filter([{<<"conversation">>, Data}|Rest], Acc) ->
    Acc2 = Acc#{<<"path">> => nkdomain_admin_detail:search_spec(<<"/conversations/",Data/binary>>)},
    table_filter(Rest, Acc2);

table_filter([{<<"text">>, Data}|Rest], Acc) ->
    Acc2 = Acc#{<<"message.text">> => Data},
    table_filter(Rest, Acc2);

table_filter([{<<"created_by">>, Data}|Rest], Acc) ->
    Acc2 = Acc#{<<"created_by">> => nkdomain_admin_detail:search_spec(Data)},
    table_filter(Rest, Acc2);

table_filter([_|Rest], Acc) ->
    table_filter(Rest, Acc).



%% @private
table_iter([], _Pos, Acc) ->
    lists:reverse(Acc);

table_iter([Entry|Rest], Pos, Acc) ->
    #{
        <<"obj_id">> := ObjId,
        <<"path">> := Path,
        <<"created_time">> := CreatedTime,
        <<"message">> := #{
            <<"text">> := Text
        } = _Message
    } = Entry,
    CreatedBy = maps:get(<<"created_by">>, Entry, <<>>),
    Enabled = case maps:get(<<"enabled">>, Entry, true) of
        true -> <<"fa-times">>;
        false -> <<"fa-check">>
    end,
    {ok, Path2, _MessageName} = nkdomain_util:get_parts(<<"message">>, Path),
    {ok, _Domain, ConversationName} = nkdomain_util:get_parts(<<"conversation">>, Path2),
    Data = #{
        pos => Pos,
        id => ObjId,
        conversation => ConversationName,
        text => Text,
        created_by => CreatedBy,
        created_time => CreatedTime,
        enabled_icon => Enabled
    },
    table_iter(Rest, Pos+1, [Data|Acc]).
