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

%% @doc media.call Object

-module(nkchat_media_call_obj_type_view).
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
                id => conversation,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_conversation,
                sort => false,
                options => get_agg_name(<<"media.call.conversation_id">>, Path, Session),
                is_html => true
            },
            #{
                id => media_call_type,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_type,
                sort => true,
                options => get_agg_term(<<"media.call.type">>, Path, Session)
            },
            #{
                id => members,
                type => text,
                name => domain_column_members,
                options => get_agg_name(<<"conversation.members.member_id">>, Path, Session),
                is_html => true
            },
            #{
                id => created_by,
                type => text,
                fillspace => <<"0.5">>,
                name => domain_column_created_by,
                sort => false,
                options => get_agg_name(<<"created_by">>, Path, Session),
                is_html => true % Will allow us to return HTML inside the column data
            },
            #{
                id => created_time,
                type => date,
                fillspace => <<"0.5">>,
                name => domain_column_created_time,
                sort => true
            },
            #{
                id => duration,
                type => time_duration,
                fillspace => <<"0.5">>,
                name => domain_column_duration,
                sort => false
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
        <<"is_deleted">>,
        <<"deleted_time">>,
        <<"media.call.started_time">>,
        <<"media.call.conversation_id">>,
        <<"media.call.type">>,
        <<"media.call.members.member_id">>
    ].


%% @doc
sort_field(<<"media_call_type">>) -> <<"media.call.type">>;
sort_field(_) -> <<>>.


%% @doc
filter_field(<<"media_call_type">>, Data, Acc) ->
    nkdomain_admin_util:add_search_filter(<<"media.call.type">>, Data, Acc);
filter_field(<<"members">>, Data, Acc) ->
    nkdomain_admin_util:add_filter(<<"media.call.members.member_id">>, Data, Acc);
filter_field(_Field, _Data, Acc) ->
    Acc.


%% @doc
entry(Entry, Base) ->
    #{
        <<"obj_id">> := ObjId,
        <<"obj_name">> := ObjName,
        <<"media.call">> := MediaCall
    } = Entry,
    ConvId = maps:get(<<"conversation_id">>, MediaCall, <<>>),
    Type = maps:get(<<"type">>, MediaCall, <<>>),
    ConvName = case nkchat_conversation:get_pretty_name(ConvId) of
        {ok, #{name:=N}} ->
            nkdomain_admin_util:obj_id_url(ConvId, N);
        {error, object_not_found} ->
            nkdomain_admin_util:obj_id_url(ConvId, <<"(deleted)">>);
        _Other ->
            <<>>
    end,
    StartedTime = maps:get(<<"started_time">>, MediaCall, 0),
    DeletedTime = maps:get(<<"deleted_time">>, Entry, 0),
    Duration = case {Entry, nklib_util:to_integer(DeletedTime), nklib_util:to_integer(StartedTime)} of
        {#{<<"is_deleted">> := true}, D, S} when D > 0 andalso S > 0 andalso D >= S ->
            D - S;
        _ ->
            0
    end,
    Members = maps:get(<<"members">>, MediaCall, []),
    MemberIds1 = [nkdomain_admin_util:obj_id_url(M)|| #{<<"member_id">>:=M} <- Members],
    Base#{
        obj_name => nkdomain_admin_util:obj_id_url(ObjId, ObjName),
        conversation => ConvName,
        media_call_type => Type,
        members => nklib_util:bjoin(MemberIds1, <<", ">>),
        duration => Duration
    }.





%% @private
element_updated(_ObjId, _Value, _Session) ->
    {ok, #{}}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, ?MEDIA_CALL, Path, Session).




%% @private
get_agg_term(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_term(Field, ?MEDIA_CALL, Path, Session).
