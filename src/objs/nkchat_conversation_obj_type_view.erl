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

-module(nkchat_conversation_obj_type_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/2, fields/0, sort_field/1, filter_field/3, entry/2, element_updated/3]).
-export([subview/3]).

-include("nkchat.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_admin.hrl").

%% @doc
view(Path, Session) ->
    table(#{is_subtable=>false}, Path, Session).

%% @doc
subview(Opts, Path, Session) ->
    table(Opts#{is_subtable=>true, created_by_hidden=>true}, Path, Session).


%% @doc
table(Opts, Path, Session) ->
%%    Id = case Opts of
%%        #{table_id:=TableId} ->
%%            TableId;
%%        _ ->
%%            nkdomain_admin_util:make_type_view_id(?CHAT_CONVERSATION)
%%    end,
    Table1 = #{
        is_subtable => maps:get(is_subtable, Opts),
        columns => lists:flatten([
            #{
                id => checkbox,
                type => checkbox
            },
            #{
                id => domain,
                type => text,
                %fillspace => <<"1.5">>,
                name => domain_column_domain,
                is_html => true,
                sort => true,
                options => get_agg_name(<<"domain_id">>, Path, Session)
            },
            #{
                id => obj_name,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_id,
                sort => true,
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => name,
                type => text,
                name => domain_column_name,
                sort => true
            },
            #{
                id => conversation_type,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_type,
                sort => true,
                options => get_agg_term(<<"conversation.type">>, Path, Session)
            },
            #{
                id => created_time,
                type => date,
                fillspace => <<"0.5">>,
                name => domain_column_created_time,
                sort => true
            },
            #{
                id => members,
                type => text,
                name => domain_column_members,
                options => get_agg_name(<<"conversation.members.member_id">>, Path, Session),
                is_html => true
            }
        ]),
        left_split => 1,
        on_click => []
    },
    case Opts of
        #{header:=Header} ->
            Table1#{header => Header};
        _ ->
            Table1
    end.


%% @doc 
fields() ->
    [
        <<"domain_id">>,
        <<"srv_id">>,
        <<"obj_name">>,
        <<"name">>,
        <<"path">>,
        <<"created_by">>,
        <<"created_time">>,
        <<"conversation.type">>,
        <<"conversation.members.member_id">>
    ].


%% @doc
sort_field(<<"conversation_type">>) -> <<"conversation.type">>;
sort_field(_) -> <<>>.


%% @doc
filter_field(<<"conversation_type">>, Data, Acc) ->
    nkdomain_admin_util:add_search_filter(<<"conversation.type">>, Data, Acc);
filter_field(<<"members">>, Data, Acc) ->
    nkdomain_admin_util:add_filter(<<"conversation.members.member_id">>, Data, Acc);
filter_field(_Field, _Data, Acc) ->
    Acc.


%% @doc
entry(Entry, Base) ->
    #{
        <<"obj_id">> := ObjId,
        <<"obj_name">> := ObjName,
        <<"conversation">> := #{
              <<"type">> := Type
        } = ChatConv
    } = Entry,
    Name = case nkchat_conversation:get_pretty_name(ObjId) of
        {ok, #{name:=N}} ->
            N;
        _ ->
            maps:get(<<"name">>, Entry, <<>>)
    end,
    Members = maps:get(<<"members">>, ChatConv, []),
    ObjName2 = case ObjName of
        <<"mh-", _/binary>> -> <<"(dynamic)">>;
        _ -> ObjName
    end,
    MemberIds1 = [nkdomain_admin_util:obj_id_url(M)|| #{<<"member_id">>:=M} <- Members],
    Base#{
        obj_name => nkdomain_admin_util:obj_id_url(ObjId, ObjName2),
        name => Name,
        conversation_type => Type,
        members => nklib_util:bjoin(MemberIds1, <<", ">>)
    }.





%% @doc
%%table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, _Opts, Session) ->
%%%%    Filter2 = case Opts of
%%%%        #{orig_type:=?DOMAIN_USER, obj_id:=UserId} ->
%%%%            Filter#{<<"created_by">> => UserId};
%%%%        _ ->
%%%%            Filter
%%%%    end,
%%    Filter2 = Filter,
%%    #admin_session{domain_id=DomainId} = Session,
%%    SortSpec = case Sort of
%%        {<<"service">>, Order} ->
%%            <<Order/binary, ":srv_id">>;
%%        {<<"name">>, Order} ->
%%            <<Order/binary, ":name_sort">>;
%%        {<<"conversation">>, Order} ->
%%            <<Order/binary, ":path">>;
%%        {<<"type">>, Order} ->
%%            <<Order/binary, ":conversation.type">>;
%%        {Field, Order} when Field==<<"created_by">>; Field==<<"created_time">>; Field==<<"obj_name">> ->
%%            <<Order/binary, $:, Field/binary>>;
%%        _ ->
%%            <<"desc:path">>
%%    end,
%%    Offset = maps:get(<<"timezone_offset">>, Filter2, 0),
%%    case table_filter(maps:to_list(Filter2), #{timezone_offset=>Offset}, #{type=>?CHAT_CONVERSATION}) of
%%        {ok, Filters} ->
%%            % lager:warning("NKLOG Filters ~s", [nklib_json:encode_pretty(Filter)]),
%%            FindSpec = #{
%%                filters => Filters,
%%                fields => [
%%                        <<"domain_id">>,
%%                        <<"srv_id">>,
%%                        <<"obj_name">>,
%%                        <<"name">>,
%%                        <<"path">>,
%%                        <<"created_by">>,
%%                        <<"created_time">>,
%%                        <<"conversation.type">>,
%%                        <<"conversation.members.member_id">>
%%                ],
%%                sort => SortSpec,
%%                from => Start,
%%                size => Size
%%            },
%%            SubDomainsFilterId = nkdomain_admin_util:make_type_view_subfilter_id(?CHAT_CONVERSATION),
%%            Fun = case maps:get(SubDomainsFilterId, Filter2, 1) of
%%                0 -> search;
%%                1 -> search_all
%%            end,
%%            case nkdomain_domain:Fun(DomainId, FindSpec) of
%%                {ok, Total, List, _Meta} ->
%%                    Data = table_iter(List, Start+1, [], Session),
%%                    {ok, Total, Data};
%%                {error, Error} ->
%%                    {error, Error}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.
%%
%%
%%%% @private
%%table_filter([], _Info, Acc) ->
%%    {ok, Acc};
%%
%%table_filter([Term|Rest], Info, Acc) ->
%%    case nkdomain_admin_util:table_filter(Term, Info, Acc) of
%%        {ok, Acc2} ->
%%            table_filter(Rest, Info, Acc2);
%%        {error, Error} ->
%%            {error, Error};
%%        unknown ->
%%            case Term of
%%                {<<"type">>, Data} ->
%%                    Acc2 = Acc#{<<"conversation.type">> => Data},
%%                    table_filter(Rest, Info, Acc2);
%%                {<<"members">>, Data} ->
%%                    Acc2 = Acc#{<<"conversation.members.member_id">> => Data},
%%                    table_filter(Rest, Info, Acc2);
%%                _ ->
%%                    table_filter(Rest, Info, Acc)
%%            end
%%    end.
%%
%%
%%%% @private
%%table_iter([], _Pos, Acc, _Session) ->
%%    lists:reverse(Acc);
%%
%%table_iter([Entry|Rest], Pos, Acc, Session) ->
%%    Base = nkdomain_admin_util:table_entry(?CHAT_CONVERSATION, Entry, Pos),
%%    #{
%%        ?CHAT_CONVERSATION := #{
%%            <<"type">> := Type
%%        }
%%    } = Entry,
%%    Name = maps:get(<<"name">>, Entry, <<>>),
%%    ChatConv = maps:get(?CHAT_CONVERSATION, Entry, #{}),
%%    Members = maps:get(<<"members">>, ChatConv, []),
%%    #{<<"obj_id">>:=ObjId, <<"obj_name">> := ObjName} = Entry,
%%    ObjName2 = case ObjName of
%%        <<"mh-", _/binary>> -> <<"(dynamic)">>;
%%        _ -> ObjName
%%    end,
%%    MemberIds1 = [nkdomain_admin_util:obj_id_url(M)|| #{<<"member_id">>:=M} <- Members],
%%    Data = Base#{
%%        name => Name,
%%        obj_name := nkdomain_admin_util:obj_id_url(ObjId, ObjName2),
%%        type => Type,
%%        members => nklib_util:bjoin(MemberIds1, <<", ">>)
%%    },
%%    table_iter(Rest, Pos+1, [Data|Acc], Session).


%% @private
element_updated(_ObjId, Value, _Session) ->
    #{
        <<"name">> := Name
    } = Value,
    Update = #{name => Name},
    {ok, Update}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, ?CHAT_CONVERSATION, Path, Session).




%% @private
get_agg_term(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_term(Field, ?CHAT_CONVERSATION, Path, Session).
