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
                id => file_id,
                type => text,
                name => domain_column_file_id
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
                type => disable
            },
            #{
                id => <<"fa-check">>,
                type => enable
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
    %% Get the timezone_offset from the filter list and pass it to table_filter
    ClientTimeOffset = maps:get(<<"timezone_offset">>, Filter, <<"0">>),
    case table_filter(maps:to_list(Filter), #{type=>message}, #{timezone_offset => ClientTimeOffset}) of
        {ok, Filters} -> 
            FindSpec = #{
                filters => Filters,
                fields => [<<"path">>, <<"created_by">>, <<"created_time">>,
                           <<"parent_id">>, <<"message.text">>, <<"message.file_id">>],
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
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
table_filter([], Acc, _) ->
    {ok, Acc};

table_filter([{_, <<>>}|Rest], Acc, Info) ->
    table_filter(Rest, Acc, Info);

table_filter([{<<"conversation">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"path">> => nkdomain_admin_detail:search_spec(<<"/conversations/",Data/binary>>)},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"text">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"message.text">> => Data},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"file_id">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"message.file_id">> => Data},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"created_by">>, Data}|Rest], Acc, Info) ->
    Acc2 = Acc#{<<"created_by">> => nkdomain_admin_detail:search_spec(Data)},
    table_filter(Rest, Acc2, Info);

table_filter([{<<"created_time">>, <<"custom">>}|_Rest], _Acc, _Info) ->
    {error, date_needs_more_data};

table_filter([{<<"created_time">>, Data}|Rest], Acc, #{timezone_offset:=Offset}=Info) ->
    SNow = nklib_util:timestamp(),
    {_,{H,M,S}} = nklib_util:timestamp_to_gmt(SNow),
    Now = SNow - H*3600 - M*60 - S,
    OffsetSecs = Offset * 60,
    % Now is the server time for today at 00:00 in seconds
    % OffsetSecs is difference between the client time and the server in seconds
    case Data of
        <<"today">> ->
            Now2 = (Now + OffsetSecs)*1000,
            Filter = list_to_binary([">", nklib_util:to_binary(Now2)]);
        <<"yesterday">> ->
            Now2 = (Now - 24*60*60 + OffsetSecs)*1000,
            Now3 = (Now + OffsetSecs)*1000,
            Filter = list_to_binary(["<", nklib_util:to_binary(Now2), "-", nklib_util:to_binary(Now3),">"]);
        <<"last_7">> ->
            Now2 = (Now - 7*24*60*60 + OffsetSecs)*1000,
            Filter = list_to_binary([">", nklib_util:to_binary(Now2)]);
        <<"last_30">> ->
            Now2 = (Now - 30*24*60*60 + OffsetSecs)*1000,
            Filter = list_to_binary([">", nklib_util:to_binary(Now2)]);
        <<"custom">> ->
            Filter = <<"">>;
        _ ->
            Filter = <<"">>
    end,
    Acc2 = Acc#{<<"created_time">> => Filter},
    table_filter(Rest, Acc2, Info);

table_filter([_|Rest], Acc, Info) ->
    table_filter(Rest, Acc, Info).



%% @private
table_iter([], _Pos, Acc) ->
    lists:reverse(Acc);

table_iter([Entry|Rest], Pos, Acc) ->
    #{
        <<"obj_id">> := ObjId,
        <<"path">> := Path,
        <<"created_time">> := CreatedTime
    } = Entry,
    Message = maps:get(<<"message">>, Entry, #{}),
    MessageText = maps:get(<<"text">>, Message, <<>>),
    MessageFileId = maps:get(<<"file_id">>, Message, <<>>),
    CreatedBy = maps:get(<<"created_by">>, Entry, <<>>),
    Enabled = case maps:get(<<"enabled">>, Entry, true) of
        true -> <<"fa-times">>;
        false -> <<"fa-check">>
    end,
    Css = case maps:get(<<"enabled">>, Entry, true) of
        true -> <<"">>;
        false -> <<"webix_cell_disabled">>
    end,
    {ok, Path2, _MessageName} = nkdomain_util:get_parts(<<"message">>, Path),
    {ok, _Domain, ConversationName} = nkdomain_util:get_parts(<<"conversation">>, Path2),
    Data = #{
        pos => Pos,
        id => ObjId,
        conversation => ConversationName,
        text => MessageText,
        file_id => MessageFileId,
        created_by => CreatedBy,
        created_time => CreatedTime,
        enabled_icon => Enabled,
        <<"$css">> => Css
    },
    table_iter(Rest, Pos+1, [Data|Acc]).
