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

%% @doc Conversation Object

-module(nkchat_conversation_obj_view).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([view/3, update/3, create/2]).

-include("nkchat.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_admin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(LLOG(Type, Txt, Args),
    lager:Type("NkChat Conversation obj_view" ++ Txt, Args)).



%% @private
view(Obj, IsNew, #admin_session{domain_id=Domain}=Session) ->
    ObjId = maps:get(obj_id, Obj, <<>>),
    DomainId = maps:get(domain_id, Obj, Domain),
    ObjName = maps:get(obj_name, Obj, <<>>),
    Name = maps:get(name, Obj, <<>>),
    Description = maps:get(description, Obj, <<>>),
    Enabled = maps:get(enabled, Obj, true),
    Conv = maps:get(?CHAT_CONVERSATION, Obj, #{}),
    Type = maps:get(type, Conv, <<"channel">>),
    AllowMembersEdit = Type =:= <<"channel">> orelse IsNew, % TODO: Remove when conversation type update is fixed
    Members = maps:get(members, Conv, []),
    MemberIds = [maps:get(member_id, Member) || Member <- Members],
    MembersValue = binary:list_to_bin(lists:join(<<",">>, MemberIds)),
    MemberOpts = get_members_opts(MemberIds),
    DefaultTypes = [<<"channel">>, <<"self">>, <<"one2one">>, <<"private">>],
    TypeOpts = nkdomain_admin_util:get_agg_term_with_defaults(<<?CHAT_CONVERSATION/binary, ".type">>, ?CHAT_CONVERSATION, <<"/">>, DefaultTypes, Session),
    FormId = nkdomain_admin_util:make_obj_view_id(?CHAT_CONVERSATION, ObjId),
    Spec = #{
        form_id => FormId,
        buttons => [
            #{type => case Enabled of true -> disable; _ -> enable end, disabled => IsNew},
            #{type => delete, disabled => IsNew},
            #{type => save}
        ],
        groups => [
            #{
                header => ?CHAT_CONVERSATION,
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
                                options => Opts
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
                        editable => true
                    },
                    #{
                        id => <<"name">>,
                        type => text,
                        label => <<"Name">>,
                        required => true,
                        value => Name,
                        editable => true
                    },
                    #{
                        id => <<"description">>,
                        type => text,
                        label => <<"Description">>,
                        required => false,
                        value => Description,
                        editable => true
                    },
                    #{
                        id => <<"type">>,
                        type => combo,
                        label => <<"Type">>,
                        value => Type,
                        required => IsNew,
                        editable => IsNew,
                        options => TypeOpts
                    },
                    #{
                        id => <<"members">>,
                        type => multicombo,
                        label => <<"Members">>,
                        value => MembersValue,
                        suggest_type => ?DOMAIN_USER,
                        suggest_field => <<"user_name">>,
                        suggest_template => <<"#user_name# #user_surname#">>,
                        required => false,
                        editable => AllowMembersEdit, %true
                        options => MemberOpts
                    }
                ]
            },
            nkadmin_webix_form:creation_fields(Obj, IsNew)
        ]
    },
    Data = #{
        id => FormId,
        class => webix_ui,
        value => nkadmin_webix_form:form(Spec, ?CHAT_CONVERSATION, Session)
    },
    {ok, Data, Session}.




%% TODO: Conversation type is not updated
update(ObjId, Data, _Session) ->
    #{
        <<"obj_name">> := ObjName
    } = Data,
    ObjName2 = nklib_parse:normalize(ObjName, #{}),
    Type = maps:get(<<"type">>, Data, <<"channel">>),
    Members = maps:get(<<"members">>, Data, <<>>),
    %?LLOG(notice, "Members: ~p", [Members]),
    MemberIds = binary:split(Members, <<",">>, [global]),
    MemberIds2 = lists:filter(
        fun(T) -> T =/= <<>> end,
        MemberIds),
    %?LLOG(notice, "MemberIds: ~p", [MemberIds2]),
    {ToAdd, ToRemove} = case nkdomain:get_obj(ObjId) of
        {ok, #{?CHAT_CONVERSATION := Conv}} ->
            OldMembers = maps:get(members, Conv, []),
            %?LLOG(notice, "OldMembers: ~p", [OldMembers]),
            OldMemberIds = [maps:get(member_id, Member) || Member <- OldMembers],
            %?LLOG(notice, "OldMemberIds: ~p", [OldMemberIds]),
            RemMembers = lists:filter(
                fun(T) -> (T =/= <<>>) and (not lists:member(T, MemberIds2)) end,
                OldMemberIds),
            AddMembers = lists:filter(
                fun(T) -> (T =/= <<>>) and (not lists:member(T, OldMemberIds)) end,
                MemberIds2),
            ?LLOG(notice, "Removing: ~p", [RemMembers]),
            ?LLOG(notice, "Adding: ~p", [AddMembers]),
            {AddMembers, RemMembers};
        {error, _Error} ->
            ?LLOG(warning, "[update] Error ~p while getting conversation object", [_Error]),
            {[],[]}
    end,
    Type2 = get_conv_type(Type, MemberIds2),
    Update = maps:with([<<"name">>, <<"description">>], Data),
    Update2 = Update#{?CHAT_CONVERSATION => #{<<"type">> => Type2}},
    Update3 = case Update2 of
        #{<<"name">> := Name} ->
            Update2#{<<"name">> => nklib_parse:normalize(Name, #{space=>$_, allowed=>[$-,$_], not_to_lowercase => true})};
        _ ->
            Update2
    end,
    case nkdomain:update(ObjId, Update3) of
        {ok, _} ->
            case nkdomain:update_name(ObjId, ObjName2) of
                {ok, _} ->
                    lists:map(
                        fun(M) ->
                            nkchat_conversation:add_member(ObjId, M, #{silent => true})
                        end,
                        ToAdd),
                    lists:map(
                        fun(M) ->
                            nkchat_conversation:remove_member(ObjId, M, #{silent => true})
                        end,
                        ToRemove),
                    ?LLOG(notice, "~s ~s updated", [?CHAT_CONVERSATION, ObjId]),
                    ok;
                {error, Error} ->
                    ?LLOG(notice, "could not update ~s ~s: ~p", [?CHAT_CONVERSATION, ObjId, Error]),
                    {error, Error}
            end;
        {error, Error} ->
            ?LLOG(notice, "could not update ~s ~s: ~p", [?CHAT_CONVERSATION, ObjId, Error]),
            {error, Error}
    end.


create(Data, #admin_session{srv_id=SrvId, user_id=UserId}=_Session) ->
    #{
        <<"domain">> := DomainId,
        <<"obj_name">> := ObjName,
        <<"name">> := Name,
        <<"description">> := Description,
        <<"type">> := Type,
        <<"members">> := Members
    } = Data,
    MemberIds = binary:split(Members, <<",">>, [global]),
    MemberIds2 = lists:filter(
        fun(T) -> T =/= <<>> end,
        MemberIds),
    Type2 = get_conv_type(Type, MemberIds2),
    ObjNameFollowsMembers = is_direct_conv(Type2),
    Opts = case ObjName of
        <<>> ->
            % Use normalized Name as ObjName
            #{obj_name => nklib_parse:normalize(Name, #{})};
        _ ->
            #{obj_name => nklib_parse:normalize(ObjName, #{})}
    end,
    Opts1 = Opts#{
        push_srv_id => SrvId, % TODO: Pass correct SrvId at login
        type => Type2,
        created_by => UserId,
        name => nklib_parse:normalize(Name, #{space=>$_, allowed=>[$-,$_], not_to_lowercase => true}),
        description => Description,
        initial_member_ids => MemberIds2,
        obj_name_follows_members => ObjNameFollowsMembers
    },
    Opts2 = case is_direct_conv(Type2) of
        true ->
            Opts1#{obj_name => <<>>};
        false ->
            Opts1
    end,
    ?LLOG(warning, "Create Opts: ~p", [Opts2]),
    case nkchat_conversation:create(DomainId, Opts2) of
        {ok, ObjId, _} ->
            {ok, ObjId};
        {error, {object_already_exists,_}} ->
            case ObjName of
                <<>> ->
                    % ObjName was empty, repeat again using a random obj_name
                    case nkchat_conversation:create(DomainId, Opts2#{obj_name => <<>>}) of
                        {ok, ObjId, _} ->
                            {ok, ObjId};
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    {error, object_already_exists}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @private
get_conv_type(DesiredType, Members) ->
    case DesiredType of
        <<"self">> ->
            case erlang:length(Members) of
                0  ->
                    ?LLOG(warning, "Type changed from ~p to <<\"channel\">>", [DesiredType]),
                    <<"channel">>;
                1 ->
                    <<"self">>;
                2 ->
                    ?LLOG(warning, "Type changed from ~p to <<\"one2one\">>", [DesiredType]),
                    <<"one2one">>;
                _ ->
                    ?LLOG(warning, "Type changed from ~p to <<\"private\">>", [DesiredType]),
                    <<"private">>
            end;
        <<"one2one">> ->
            case erlang:length(Members) of
                0  ->
                    ?LLOG(warning, "Type changed from ~p to <<\"channel\">>", [DesiredType]),
                    <<"channel">>;
                1 ->
                    ?LLOG(warning, "Type changed from ~p to <<\"self\">>", [DesiredType]),
                    <<"self">>;
                2 ->
                    <<"one2one">>;
                _ ->
                    ?LLOG(warning, "Type changed from ~p to <<\"private\">>", [DesiredType]),
                    <<"private">>
            end;
        <<"private">> ->
            case erlang:length(Members) of
                0  ->
                    ?LLOG(warning, "Type changed from ~p to <<\"channel\">>", [DesiredType]),
                    <<"channel">>;
                1 ->
                    ?LLOG(warning, "Type changed from ~p to <<\"self\">>", [DesiredType]),
                    <<"self">>;
                2 ->
                    ?LLOG(warning, "Type changed from ~p to <<\"one2one\">>", [DesiredType]),
                    <<"one2one">>;
                _ ->
                    <<"private">>
            end;
        <<>> ->
            ?LLOG(warning, "Type changed from ~p to <<\"channel\">>", [DesiredType]),
            <<"channel">>;
        _ ->
            DesiredType
    end.



%% @private
is_direct_conv(<<"self">>) ->
    true;

is_direct_conv(<<"one2one">>) ->
    true;

is_direct_conv(<<"private">>) ->
    true;

is_direct_conv(_Type) ->
    false.


get_members_opts(Ids) ->
    get_members_opts(Ids, []).

%% @private
get_members_opts([], Acc) ->
    lists:reverse(Acc);

get_members_opts([Id|Ids], Acc) ->
    case nkdomain:get_name(Id) of
        {ok, #{obj_id:=ObjId, name:=Name}} ->
            get_members_opts(Ids, [#{id => ObjId, user_name => Name, user_surname => <<>>} | Acc]);
        {error, _Error} ->
            ?LLOG(warning, "Unknown member ID found: ~p", [Id]),
            get_members_opts(Ids, [#{id => Id, user_name => Id, user_surname => <<>>} | Acc])
    end.
