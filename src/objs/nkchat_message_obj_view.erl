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

%% @doc Image Job Object View

-module(nkchat_message_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("Nkchat Message " ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    ConvId = maps:get(parent_id, Obj, <<>>),
    ConvName = case nkchat_conversation:get_pretty_name(ConvId) of
        {ok, #{name:=CName}} ->
            CName;
        _ ->
            <<>>
    end,
    Enabled = maps:get(enabled, Obj, true),
    Msg = maps:get(?CHAT_MESSAGE, Obj, #{}),
%    VisibleBatch = <<>>,
    MsgType = maps:get(type, Msg, <<"text">>),
    MsgText = maps:get(text, Msg, <<>>),
    MsgFile = maps:get(file_id, Msg, <<>>),
    MsgFileValue = get_parsed_file(MsgFile),
    MsgBody = maps:get(body, Msg, #{}),
    MsgFields = get_message_body_fields(IsNew, MsgType, MsgBody),
    FormId = nkdomain_admin_util:make_obj_view_id(?CHAT_MESSAGE, ObjId),
    FileUrl = nkdomain_admin_util:get_file_url(MsgFile, Session),
    ContentType = case MsgFileValue of
        #{file_type := FT} ->
            FT;
        _ ->
            <<>>
    end,
    Base = case ContentType of
        <<"image/", _/binary>> ->
            IconImage = <<"<img style='width:100%; height:auto;' src='", FileUrl/binary, "'/>">>,
            #{with_image => IconImage};
        <<"video/", _/binary>> ->
            IconImage = <<"<video style='width:100%; height:auto;' ref='video-player' controls='true' controlslist='nodownload' src='", FileUrl/binary, "'/>">>,
            #{with_image => IconImage};
        <<"audio/", _/binary>> ->
            IconImage = <<"<audio style='width:100%; height:auto;' ref='audio-player' controls='true' controlslist='nodownload' src='", FileUrl/binary, "'/>">>,
            #{with_image => IconImage};
        <<>> ->
            #{};
        _ ->
            case IsNew of
                true -> 
                    #{};
                false ->
                    #{with_image => <<"<a href=\"", FileUrl/binary, "\">Content type not supported</a>">>}
            end
    end,
    Spec = Base#{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
%        visible_batch => VisibleBatch,
        groups => [
            #{
                header => ?CHAT_MESSAGE,
                values => [
                    case IsNew of
                        true ->
                            {ok, Domains} = nkdomain_admin_util:get_domains("/", Session),
                            Opts = [
                                #{id=>Id, value=>Value} ||
                                {Id, Value} <- [{<<"root">>, <<"/">>}|Domains]
                            ],
                            #{
                                id => <<"domain">>,
                                type => combo,
                                label => <<"Domain">>,
                                value => DomainId,
                                editable => true,
                                options => Opts,
                                hidden => true
                            };
                        false ->
                            DomainPath = case nkdomain_db:find(DomainId) of
                                #obj_id_ext{path=DP} -> DP;
                                _ -> <<>>
                            end,
                            #{
                                id => <<"domain">>,
                                type => text,
                                label => <<"Domain">>,
                                value => DomainPath,
                                editable => false
                            }
                    end,
                    #{
                        id => <<"obj_name">>,
                        type => text,
                        label => <<"Object name">>,
                        value => ObjName,
                        required => false,
                        editable => true
                    },
                    #{
                        id => <<"parent_id">>,
                        type => html,
                        label => <<"Conversation">>,
                        value => get_obj_id_url_or_empty(ConvId, ConvName),
                        required => true,
                        hidden => IsNew,
                        editable => false
                    },
                    #{
                        id => <<"new_parent_id">>,
                        type => combo,
                        label => <<"Conversation">>,
                        value => <<>>,
                        suggest_type => ?CHAT_CONVERSATION,
                        suggest_field => <<"name">>,
                        suggest_template => <<"#name#">>,
                        required => IsNew,
                        hidden => not IsNew,
                        editable => true
                    }
                ]
            },
            #{
                header => <<"CONFIGURATION">>,
                %batch => <<>>,
                values => [
                    #{
                        id => <<"type">>,
                        type => text,
                        label => <<"Type">>,
                        value => MsgType,
                        editable => false
                    },
                    #{
                        id => <<"text">>,
                        type => text,
                        label => <<"Text">>,
                        value => MsgText,
                        required => true,
                        editable => true
                    },
                    #{
                        id => <<"file_id">>,
                        type => combo,
                        label => <<"File">>,
                        value => MsgFileValue,
                        suggest_type => ?DOMAIN_FILE,
                        suggest_field => <<"name">>,
                        suggest_template => <<"#name# (#file_type#, #file_size#)">>,
                        required => MsgFileValue =/= <<>>,
                        editable => true
                    }
                    | MsgFields                    
                ]
            },
            nkadmin_webix_form:creation_fields(Obj, IsNew)
        ]
    },
    Data = #{
        id => FormId,
        class => webix_ui,
        value => nkadmin_webix_form:form(Spec, ?CHAT_MESSAGE, Session)
    },
    {ok, Data, Session}.




update(ObjId, Data, _Session) ->
    lager:error("NKLOG UPDATE ~p", [Data]),
    #{
        <<"obj_name">> := ObjName,
        <<"text">> := Text
    } = Data,
    FileId = maps:get(<<"file_id">>, Data, <<>>),
    QText = maps:get(<<"quote_text">>, Data, <<>>),
    case nkdomain:get_obj(ObjId) of
        {ok, Obj} ->
            Msg = maps:get(?CHAT_MESSAGE, Obj, #{}),
            Msg2 = Msg#{text => Text},
            Msg3 = case FileId of
                <<>> ->
                    maps:without([file_id], Msg2);
                _ ->
                    Msg2#{file_id => FileId}
            end,
            Body = maps:get(body, Msg, #{}),
            Body2 = case Body of
                #{<<"quote">> := Quote} ->
                    Body#{<<"quote">> => Quote#{<<"text">> => QText}};
                _ ->
                    Body
            end,
            Msg4 = case maps:size(Body2) of
                0 ->
                    Msg3;
                _ ->
                    Msg3#{body => Body2}
            end,
            lager:error("UPDATED MSG: ~p", [Msg4]),
            case nkdomain:update(ObjId, #{?CHAT_MESSAGE => Msg4}) of
                {ok, _} ->
                    case nkdomain:update_name(ObjId, ObjName) of
                        {ok, _} ->
                            ?LLOG(notice, "message ~s updated", [ObjId]),
                            ok;
                        {error, Error} ->
                            ?LLOG(notice, "could not update message ~s: ~p", [ObjId, Error]),
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



create(Data, #admin_session{user_id=UserId}=_Session) ->
    lager:error("NKLOG CREATE ~p", [Data]),
    #{
        <<"obj_name">> := ObjName,
        <<"new_parent_id">> := NewParentId,
        <<"text">> := Text
    } = Data,
    FileId = maps:get(<<"file_id">>, Data, <<>>),
    Msg = case FileId of
        <<>> ->
            #{<<"text">> => Text};
        _ ->
            #{<<"file_id">> => FileId, <<"text">> => Text}
    end,
    QuoteId = maps:get(<<"quote_message_id">>, Data, <<>>),
    Msg2 = case QuoteId of
        <<>> ->
            Msg;
        _ ->
            case nkdomain:get_obj(QuoteId) of
                {ok, #{obj_id := QId, ?CHAT_MESSAGE := QuotedMsg, created_by := QuotedUser}=QuotedObj} ->
                    Q = maps:with([file_id, text], QuotedMsg),
                    Q2 = Q#{user_id => QuotedUser, message_id => QId, type => <<"text">>},
                    Msg#{body => #{quote => Q2}};
                _ ->
                    Msg
            end
    end,
    case nkdomain:get_obj(NewParentId) of
        {ok, #{obj_id := ParentId, domain_id := DomainId}} ->
            Create = #{
                type => ?CHAT_MESSAGE,
                domain_id => DomainId,
                parent_id => ParentId,
                obj_name => ObjName,
                created_by => UserId,
                ?CHAT_MESSAGE => Msg2
            },
            lager:error("NKLOG CREATE MSG: ~p", [Create]),
            case nkdomain_obj_make:create(Create) of
                {ok, #obj_id_ext{obj_id=ObjId}, []} ->
                    {ok, ObjId};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
get_parsed_file(<<>>) ->
    <<>>;

get_parsed_file(FileId) ->
    case nkdomain:get_obj(FileId) of
        {ok, #{?DOMAIN_FILE := File}=Obj} ->
            #{
                path := Path,
                created_by := CreatedBy,
                created_time := CreatedTime
            } = Obj,
            {ok, Domain, ShortName} = nkdomain_util:get_parts(?DOMAIN_FILE, Path),
            #{
                content_type := Type,
                size := Size,
                store_id := StoreId
            } = File,
            Size2 = nkdomain_admin_util:get_size_bin(Size),
            #{
                checkbox => <<"0">>,
                id => FileId,
                obj_name => nkdomain_admin_util:obj_id_url(FileId, ShortName),
                name => maps:get(name, Obj, <<>>),
                domain => nkdomain_admin_util:obj_path_url(Domain, Domain),
                created_by => nkdomain_admin_util:obj_id_url(CreatedBy),
                created_time => CreatedTime,
                file_type => Type,
                file_size => Size2,
                store_id => nkdomain_admin_util:obj_id_url(StoreId)
            };
        _ ->
            <<>>
    end.


%% @private
get_message_body_fields(IsNew, MsgType, MsgBody) ->
    get_quote_fields(IsNew, MsgBody)
    ++
    get_message_body_fields_by_type(MsgType, maps:without([<<"quote">>], MsgBody)).


%% @private
get_message_body_fields_by_type(_MsgType, MsgBody) ->
    get_body_field(MsgBody).


%% @private
get_quote_fields(true, _) ->
    [
        #{
            id => <<"quote_message_id">>,
            type => combo,
            label => <<"Quoted message">>,
            suggest_type => ?CHAT_MESSAGE,
            suggest_field => <<"text">>,
            suggest_template => <<"#created_by#: '#!text#' in ##conversation#">>, %% #!column# enforces data escaping
            value => <<>>,
            editable => true
        }
    ];

get_quote_fields(IsNew, #{<<"quote">> := Quote}) ->
    QType = maps:get(<<"type">>, Quote, <<>>),
    QText = maps:get(<<"text">>, Quote, <<>>),
    QFileId = maps:get(<<"file_id">>, Quote, <<>>),
    QMsgId = maps:get(<<"message_id">>, Quote, <<>>),
    QUserId = maps:get(<<"user_id">>, Quote, <<>>),
    [
        #{
            id => <<"quote_type">>,
            type => text,
            label => <<"Quote type">>,
            value => QType,
            editable => false
        },
        #{
            id => <<"quote_user_id">>,
            type => html,
            label => <<"Quoted User">>,
            value => get_obj_id_url_or_empty(QUserId),
            editable => false
        },
        #{
            id => <<"quote_message_id">>,
            type => html,
            label => <<"Quoted message">>,
            value => get_obj_id_url_or_empty(QMsgId),
            editable => false
        },
        #{
            id => <<"quote_file_id">>,
            type => html,
            label => <<"Quoted file">>,
            value => get_obj_id_url_or_empty(QFileId),
            editable => false
        },
        #{
            id => <<"quote_text">>,
            type => text,
            label => <<"Quoted text">>,
            value => QText,
            editable => true
        }
    ];
    
get_quote_fields(_, _) ->
    [].
    

%% @private
get_body_field(Body) ->
    case maps:size(Body) of
        0 ->
            [];
        _ ->
            [#{
                id => <<"body">>,
                type => textarea,
                label => <<"Body">>,
                value => nklib_json:encode_pretty(Body),
                height => 300,
                editable => false
            }]
    end.


%% @private
get_obj_id_url_or_empty(<<>>) ->
    <<>>;

get_obj_id_url_or_empty(ObjId) ->
    nkdomain_admin_util:obj_id_url(ObjId).


%% @private
get_obj_id_url_or_empty(<<>>, _Name) ->
    <<>>;

get_obj_id_url_or_empty(ObjId, Name) ->
    nkdomain_admin_util:obj_id_url(ObjId, Name).



%% @private
%class_on_change(FormId) ->
%    <<"function() {
%        var value = this.getValue();
%        $$('", FormId/binary, "').showBatch(value);
%    }">>.
