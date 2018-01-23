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

-module(nkchat_message_obj_type_view).
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
%%%            <<?ID/binary, "__", TableId/binary>>;
%%            TableId;
%%        _ ->
%%            nkdomain_admin_util:make_type_view_id(?CHAT_MESSAGE)
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
                name => domain_column_domain,
                is_html => true,
                sort => true,
                options => get_agg_name(<<"domain_id">>, Path, Session)
            },
            #{
                id => conversation,
                type => text,
                name => domain_column_conversation,
                sort => false,
                options => get_agg_name(<<"parent_id">>, Path, Session),
                is_html => true
            },
            #{
                id => created_time,
                type => date,
                name => domain_column_created_time,
                sort => true
            },
            case Opts of
                #{created_by_hidden:=true} ->
                    [];
                _ ->
                    #{
                        id => created_by,
                        type => text,
                        name => domain_column_created_by,
                        sort => false,
                        options => get_agg_name(<<"created_by">>, Path, Session),
                        is_html => true % Will allow us to return HTML inside the column data
                    }
            end,
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
                name => domain_column_file_id,
                options => [
                    #{id=><<>>, value=><<>>},
                    #{id=>with_attach, value=><<"With attach">>},
                    #{id=>without_attach, value=><<"Without attach">>}
                ],
                is_html => true
            }
        ]),
        left_split => 1,
%        right_split => 2,
        on_click => [
%            #{
%                id => <<"fa-times">>,
%                type => disable
%            },
%            #{
%                id => <<"fa-check">>,
%                type => enable
%            },
%            #{
%                id => <<"fa-trash">>,
%                type => delete
%            }            
        ]
    },
    case Opts of
        #{header:=Header} ->
            Table1#{header => Header};
        _ ->
            Table1
    end.



fields() ->
    [
        <<"created_by">>,
        <<"srv_id">>,
        <<"path">>,
        <<"created_time">>,
        <<"parent_id">>,
        <<"domain_id">>,
        <<"message.text">>,
        <<"message.file_id">>
    ].


%% @doc
sort_field(_) -> <<>>.


%% @doc
filter_field(<<"conversation">>, Data, Acc) ->
    nkdomain_admin_util:add_filter(<<"parent_id">>, Data, Acc);
filter_field(<<"text">>, Data, Acc) ->
    nkdomain_admin_util:add_multiword_filter(<<"message.text">>, Data, Acc);
filter_field(<<"file_id">>, <<"with_attach">>, Acc) ->
    nkdomain_admin_util:add_exists_filter(<<"message.file_id">>, true, Acc);
filter_field(<<"file_id">>, <<"without_attach">>, Acc) ->
    nkdomain_admin_util:add_exists_filter(<<"message.file_id">>, false, Acc);
filter_field(<<"file_id">>, _, Acc) ->
    Acc;
filter_field(_Field, _Data, Acc) ->
    Acc.



%% @doc
entry(Entry, Base) ->
    #{
        <<"parent_id">> := ParentId,
        ?CHAT_MESSAGE := Message
    } = Entry,
    MessageText = maps:get(<<"text">>, Message, <<>>),
    MessageFileId = maps:get(<<"file_id">>, Message, <<>>),
    Conv = case nkdomain:get_name(ParentId) of
        {ok, #{name:=Name}} ->
            nkdomain_admin_util:obj_id_url(ParentId, Name);
        _ ->
            <<>>
    end,
    File = case MessageFileId of
        <<>> ->
            <<>>;
        _ ->
            case nkdomain:get_obj(MessageFileId) of
                {ok, #{name:=FileName, ?DOMAIN_FILE:=FileObj}} ->
                    #{content_type:=CT, size:=Size} = FileObj,
                    FileTxt = <<FileName/binary, $(, CT/binary, ", ",
                                (nklib_util:to_binary(Size div 1024))/binary, "KB)">>,
                    nkdomain_admin_util:obj_id_url(MessageFileId, FileTxt);
                _ ->
                    <<>>
            end
    end,
    Base#{
        conversation => Conv,
        text => MessageText,
        file_id => File
    }.



%%%% @doc
%%table_data(#{start:=Start, size:=Size, sort:=Sort, filter:=Filter}, Opts, Session) ->
%%    Filter2 = case Opts of
%%        #{orig_type:=?DOMAIN_USER, obj_id:=UserId} ->
%%            Filter#{<<"created_by">> => UserId};
%%        _ ->
%%            Filter
%%    end,
%%    #admin_session{domain_id=DomainId} = Session,
%%    SortSpec = case Sort of
%%        {<<"conversation">>, Order} ->
%%            <<Order/binary, ":path">>;
%%        {Field, Order} when Field==<<"created_by">>; Field==<<"created_time">> ->
%%            <<Order/binary, $:, Field/binary>>;
%%        _ ->
%%            <<"desc:path">>
%%    end,
%%    Offset = maps:get(<<"timezone_offset">>, Filter2, 0),
%%    case table_filter(maps:to_list(Filter2), #{timezone_offset=>Offset}, #{type=>message}) of
%%        {ok, Filters} ->
%%            % lager:warning("NKLOG Filters ~s", [nklib_json:encode_pretty(Filter)]),
%%            FindSpec = #{
%%                filters => Filters,
%%                fields => [
%%                        <<"created_by">>,
%%                        <<"srv_id">>,
%%                        <<"path">>,
%%                        <<"created_time">>,
%%                        <<"parent_id">>,
%%                        <<"domain_id">>,
%%                        <<"message.text">>,
%%                        <<"message.file_id">>
%%                ],
%%                sort => SortSpec,
%%                from => Start,
%%                size => Size
%%            },
%%            SubDomainsFilterId = nkdomain_admin_util:make_type_view_subfilter_id(?CHAT_MESSAGE),
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
%%                {<<"conversation">>, Data} ->
%%                    Acc2 = Acc#{<<"parent_id">> => Data},
%%                    table_filter(Rest, Info, Acc2);
%%                {<<"text">>, Data} ->
%%                    Acc2 = Acc#{<<"message.text">> => Data},
%%                    table_filter(Rest, Info, Acc2);
%%                {<<"file_id">>, <<"with_attach">>} ->
%%                    Acc2 = Acc#{<<"message.file_id">> => <<"prefix:file">>},
%%                    table_filter(Rest, Info, Acc2);
%%                {<<"file_id">>, <<"false">>} ->
%%                    Acc2 = Acc#{<<"message.file_id">> => <<>>},
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
%%    Base = nkdomain_admin_util:table_entry(?CHAT_MESSAGE, Entry, Pos),
%%    #{
%%        <<"parent_id">> := ParentId,
%%        ?CHAT_MESSAGE := Message
%%    } = Entry,
%%    MessageText = maps:get(<<"text">>, Message, <<>>),
%%    MessageFileId = maps:get(<<"file_id">>, Message, <<>>),
%%    Conv = case nkdomain:get_name(ParentId) of
%%        {ok, #{name:=Name}} ->
%%            nkdomain_admin_util:obj_id_url(ParentId, Name);
%%        _ ->
%%            <<>>
%%    end,
%%    File = case MessageFileId of
%%        <<>> ->
%%            <<>>;
%%        _ ->
%%            case nkdomain:get_obj(MessageFileId) of
%%                {ok, #{name:=FileName, ?DOMAIN_FILE:=FileObj}} ->
%%                    #{content_type:=CT, size:=Size} = FileObj,
%%                    FileTxt = <<FileName/binary, $(, CT/binary, ", ",
%%                                (nklib_util:to_binary(Size div 1024))/binary, "KB)">>,
%%                    nkdomain_admin_util:obj_id_url(MessageFileId, FileTxt);
%%                _ ->
%%                    <<>>
%%            end
%%    end,
%%    Data = Base#{
%%        conversation => Conv,
%%        text => MessageText,
%%        file_id => File
%%    },
%%    table_iter(Rest, Pos+1, [Data|Acc], Session).


%% @private
element_updated(_ObjId, Value, _Session) ->
    #{
        <<"text">> := Text
    } = Value,
    Update = #{
        ?CHAT_MESSAGE => #{
            text => Text
        }
    },
    {ok, Update}.


%% @private
get_agg_name(Field, Path, Session) ->
    nkdomain_admin_util:get_agg_name(Field, ?CHAT_MESSAGE, Path, Session).




