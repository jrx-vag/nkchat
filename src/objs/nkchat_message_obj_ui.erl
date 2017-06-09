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

-export([table_data/5, table/2, make_table/1]).

-include("nkchat.hrl").





table_data(Start, Size, _Filter, _Sort, #{srv_id:=SrvId, domain_id:=DomainId}) ->
    Spec = #{
        filters => #{
            type => message
        },
        fields => [<<"path">>, <<"parent_id">>, <<"created_by">>, <<"created_time">>, <<"message">>, <<"enabled">>],
        sort => [<<"path">>],
        start => Start,
        size => Size,
        size => 100
    },
    case nkdomain_domain_obj:find_all(SrvId, DomainId, Spec) of
        {ok, Total, List, _Meta} ->
            Data = lists:map(
                fun(Entry) ->
                    #{
                        <<"obj_id">> := ObjId,
                        <<"path">> := Path,
                        <<"parent_id">> := ConvId,
                        <<"created_time">> := CreatedTime,
                        <<"message">> := Msg
                    } = Entry,
                    CreatedBy = maps:get(<<"created_by">>, Entry, <<>>),
                    Text = maps:get(<<"text">>, Msg, <<>>),
                    HasFile = maps:is_key(<<"file_id">>, Msg),
                    Enabled = maps:get(<<"enabled">>, Entry, true),
                    case Enabled of
                        true ->
                            EnabledIcon = <<"fa-times">>;
                        false ->
                            EnabledIcon = <<"fa-check">>
                    end,
                    #{
                        id => ObjId,
                        path => Path,
                        conversation => ConvId,
                        text => Text,
                        has_file => HasFile,
                        created_by => CreatedBy,
                        created_time => CreatedTime,
                        enabled_icon => EnabledIcon
                    }
                end,
                List),
            {ok, Total, Data};
        {error, Error} ->
            {error, Error}
    end.







table(Srv, Domain) ->
    Spec = #{
        filters => #{
            type => ?CHAT_MESSAGE
        },
        fields => [<<"path">>, <<"parent_id">>, <<"created_by">>, <<"created_time">>, <<"message">>, <<"enabled">>],
        sort => [<<"path">>],
        size => 100
    },
    case nkdomain_domain_obj:find_all(Srv, Domain, Spec) of
        {ok, _Total, List, _Meta} ->
            Data = lists:map(
                fun(Entry) ->
                    #{
                        <<"obj_id">> := ObjId,
                        <<"path">> := Path,
                        <<"parent_id">> := ConvId,
                        <<"created_time">> := CreatedTime,
                        <<"message">> := Msg
                    } = Entry,
                    CreatedBy = maps:get(<<"created_by">>, Entry, <<>>),
                    Text = maps:get(<<"text">>, Msg, <<>>),
                    HasFile = maps:is_key(<<"file_id">>, Msg),
                    Enabled = maps:get(<<"enabled">>, Entry, true),
                    case Enabled of
                        true ->
                            EnabledIcon = <<"fa-times">>;
                        false ->
                            EnabledIcon = <<"fa-check">>
                    end,
                    #{
                        id => ObjId,
                        path => Path,
                        conversation => ConvId,
                        text => Text,
                        has_file => HasFile,
                        created_by => CreatedBy,
                        created_time => CreatedTime,
                        enabled_icon => EnabledIcon
                    }
                end,
                List),
            make_table(Data);
        {error, Error} ->
            {error, Error}
    end.


make_table(Data) ->
    #{
        view => <<"scrollview">>,
        id => <<"body">>,
        borderless => true,
        type => <<"space">>,
        css => <<"flex-tmp">>,
        scroll => <<"xy">>,
        body => #{
            rows => [objects_table(Data)]
        }
    }.










objects_table(Data) ->
    #{
        type => <<"space">>,
        minHeight => 300,
        minWidth => 400,
        rows => [
            #{
                height => 40,
                cols => [
                    #{
                        view => <<"button">>,
                        type => <<"iconButton">>,
                        icon => <<"refresh">>,
                        autowidth => true,
                        label => <<"Refresh">>,
                        click => <<"function() {
                                    var grid = $$(\"domain_detail_chat_messages_table\");
                                    grid.showProgress();
                                    webix.delay(function() {
                                        grid.hideProgress();
                                    }, null, null, 300);
                                    }">>
                    },
                    #{
                        view => <<"button">>,
                        type => <<"iconButton">>,
                        icon => <<"pause">>,
                        autowidth => true,
                        label => <<"Pause">>,
                        click => <<"function() {
                                    var grid = $$(\"domain_detail_chat_messages_table\");
                                    grid.showProgress();
                                    webix.delay(function() {
                                        grid.hideProgress();
                                    }, null, null, 300);
                                    }">>
                    },
                    #{
                        view => <<"button">>,
                        type => <<"iconButton">>,
                        icon => <<"plus">>,
                        autowidth => true,
                        label => <<"New">>,
                        click => <<"function() {
                                    var grid = $$(\"domain_detail_chat_messages_table\");
                                    grid.showProgress();
                                    webix.delay(function() {
                                        grid.hideProgress();
                                    }, null, null, 300);
                                    }">>
                    },
                    #{},
                    #{
                        view => <<"layout">>,
                        cols => [
                            #{
                                view => <<"label">>,
                                autowidth => true, % This label is defined separately to be able to set its width to 'autowidth'
                                label => <<"Show subdomains: ">>,
                                align => <<"right">>
                            },
        					#{
                                id => <<"domain_detail_chat_messages_tableShowSubdomains">>,
                                view => <<"checkbox">>,
                                name => <<"show_subdomains_checkbox">>,
                                width => 20,
                                value => 1,
                                on => #{
                                    onChange => <<"function() {
                                                    var grid = $$(\"domain_detail_chat_messages_table\");
                                                    var pager = grid.getPager();
                                                    var page = pager.config.page;
                                                    var start = page * pager.config.size;
                                                    console.log('grid', grid);
                                                    //grid.loadNext(number count,number start,function callback,string url,boolean now);
                                                    grid.clearAll();
                                                    grid.loadNext(grid.config.datafetch, 0, null, grid.config.url, true);
                                                    }">>
                                }
                            }
                        ]
                    }
                ]
            },
            #{
                rows => [
                    % create default objects table data,
                    create_default_objects_table_data(Data),
                    #{
                        view => <<"toolbar">>,
                        css => <<"highlighted_header header6">>,
                        paddingX => 5,
                        paddingY => 5,
                        height => 40,
                        cols => [
                            #{
                                view => <<"pager">>,
                                id => <<"pagerA">>,
                                template => <<"{common.first()}{common.prev()}&nbsp; {common.pages()}&nbsp; {common.next()}{common.last()}&nbsp;Total:&nbsp;#count#">>,
                                autosize => true,
                                height => 35,
                                group => 5
                            }
                        ]
                    }
                ]
            }
        ]
    }.



create_default_objects_table_data(_Data) ->
    #{
        id => <<"domain_detail_chat_messages_table">>,
        view => <<"datatable">>,
        css => <<"nk_datatable">>,
        dragColumn => true,
        resizeColumn => true,
        select => true,
        editable => true,
        editaction => <<"dblclick">>,
        scrollX => true,
        rightSplit => 2,
        navigation => true,
        nkFilters => [<<"domain_detail_chat_messages_tableShowSubdomains">>],
        export => true,
        columns => [
    		#{
                id => <<"pos">>,
                header => <<"#">>,
                width => 50
            },
            #{
                id => <<"path">>,
                header => [<<"Path">>, #{ content => <<"serverFilter">> }],
                fillspace => <<"2">>,
                minWidth => <<"100">>,
                sort => <<"server">>
            },
            #{
                id => <<"conversation">>,
                header => [<<"Conversation">>, #{ content => <<"serverFilter">> }],
                fillspace => <<"1">>,
                minWidth => <<"100">>,
                sort => <<"server">>
            },
            #{
                id => <<"text">>,
                header => [<<"Text">>, #{ content => <<"serverFilter">> }],
                fillspace => <<"2">>,
                minWidth => <<"100">>,
                editor => <<"text">>,
                sort => <<"server">>
            },
            #{
                id => <<"has_file">>,
                header => [<<"Attachment">>, #{ content => <<"serverFilter">> }],
                fillspace => <<"1">>,
                minWidth => <<"100">>,
                sort => <<"server">>
            },
            #{
                id => <<"created_by">>,
                header => [<<"Created By">>, #{ content => <<"serverFilter">> }],
                fillspace => <<"1">>,
                minWidth => <<"100">>,
                sort => <<"server">>
            },
            #{
                id => <<"created_time">>,
                header => [<<"Created Time">>, #{ content => <<"serverFilter">> }],
                fillspace => <<"1">>,
                minWidth => <<"100">>,
                editor => <<"text">>,
                sort => <<"server">>,
                format => <<"function(value) {
                    //                                     'en-US', 'es-ES', etc.
                    return (new Date(value)).toLocaleString();
                }">>
            },
            #{
                id => <<"enabled_icon">>,
                header => <<"&nbsp;">>,
                width => 30,
                css => <<"nk_cell_checkbox">>,
                checkValue => true,
                uncheckValue => false,
                template => <<"{common.checkbox()}">>
            },
    		#{
                id => <<"delete">>,
                header => <<"&nbsp;">>,
                width => 30,
                template => <<"<span style='cursor:pointer;' class='webix_icon fa-trash'></span>">>
            }
            %%
            %%            #{
            %%                id => <<"parentUuid">>, header => [<<"Parent UUID">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 80, fillspace => 1
            %%            },
            %%            #{
            %%                id => <<"typeName">>, header => [<<"Type">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 120, fillspace => 2, editor => <<"select">>, template => <<"<div class='type#type#'>#typeName#</div>">>
            %%            },
            %%            #{
            %%                id => <<"shortName">>, header => [<<"Name">>, #{ content => <<"extendedFilter">> }], sort => <<"string">>, minWidth => 120, fillspace => 2, editor => <<"text">>
            %%            },
            %%            #{
            %%                id => <<"enabled">>, header => [<<"Enabled?">>, <<"">>], sort => <<"string">>, minWidth => 50, fillspace => 1, template => <<"<span  style='cursor:pointer;' class='webix_icon #enabledIcon#'></span>">>
            %%            },
            %%            #{
            %%                id => <<"view">>, header => <<"&nbsp;">>, width => 35, template => <<"<span style='cursor:pointer;' class='webix_icon fa-eye'></span>">>
            %%            }
        ],
        pager => <<"pagerA">>,
%        data => Data,
        url => <<"wsProxy->">>,
        save => <<"wsProxy->">>,
        onClick => #{
            <<"fa-trash">> => <<"function(e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be deleted. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if(res) {
                            webix.$$(\"domain_detail_chat_messages_table\").remove(id);
                        }
                    }
                });
            }">>,
            <<"fa-check">> => <<"function(e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be disabled. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if (res) {
                            var item = webix.$$(\"domain_detail_chat_messages_table\").getItem(id);
                            item.enabled = false;
                            item.enabled_icon = \"fa-times\";
                            webix.$$(\"domain_detail_chat_messages_table\").refresh(id);
                        }
                    }
                });
            }">>,
            <<"fa-times">> => <<"function(e, id, node) {
                webix.confirm({
                    text: \"This object will be enabled. <br/> Are you sure?\",
                    ok: \"Yes\",
                    cancel: \"Cancel\",
                    callback: function(res) {
                        if (res) {
                            var item = webix.$$(\"domain_detail_chat_messages_table\").getItem(id);
                            item.enabled = true;
                            item.enabled_icon = \"fa-check\";
                            webix.$$(\"domain_detail_chat_messages_table\").refresh(id);
                        }
                    }
                });
            }">>
        },
        ready => <<"function() {
            webix.extend(this, webix.ProgressBar);
        }">>,
        on => #{
            <<"onBeforeLoad">> => <<"function() {
                webix.ui.datafilter.customFilter2 = {
                    refresh: function(master, node, column) {
                        node.onchange = function() {};
                        node.onclick = function(e) {
                            // Prevent the column from changing the order when clicking the filter
                            e.stopPropagation();
                        };
                    },
                    render: function(a, b) {
                        return  \"<select style='width:100%; height:25px; font-family:Verdana'; id=\"+b.columnId+\">\" +
                                \"<option>Old</option>\" +
                                \"<option>New</option>\" +
                                \"</select>\";
                    }
                };
                webix.ui.datafilter.extendedFilter2 = webix.extend({
                    refresh:function(master, node, column){
                        //event handlers
                        node.onclick = function(e) {
                            // Prevent the column from changing the order when clicking the filter
                            e.stopPropagation();
                        };
                        node.onkeyup = function(){
                            let input = this.children[0].children[0];
                            if (input.prevValue !== input.value) {
                                console.log('Filter ' + column.columnId + ' changed: ' + input.value);
                                master.clearAll();
                                let newObj =
                                {
                                    id: 1,
                                    uuid: 123456789,
                                    parentUuid: 987654321,
                                    type: 0,
                                    typeName: \"User\",
                                    shortName: \"user\",
                                    enabled: true,
                                    enabledIcon: \"fa-check\"
                                };
                                if (column.columnId === 'id') {
                                    newObj.id = input.value;
                                } else if (column.columnId === 'uuid') {
                                    newObj.uuid = input.value;
                                } else if (column.columnId === 'parentUuid') {
                                    newObj.parentUuid = input.value;
                                } else if (column.columnId === 'typeName') {
                                    newObj.typeName = input.value;
                                } else if (column.columnId === 'shortName') {
                                    newObj.shortName = input.value;
                                }
                                master.add(newObj, 0);
                            };
                            input.prevValue = input.value;
                        }
                    }
                }, webix.ui.datafilter.textFilter);
            }">>
        }
    }.


%%create_objects_array(N, T) when (N =< 0) ->
%%    T;
%%create_objects_array(N, T) ->
%%    UUID = 999999999+N,
%%    ParentUUID = UUID - 1,
%%    Type = UUID rem 5,
%%    Enabled = ((UUID rem 5) rem 2) =:= 0,
%%    if
%%        Enabled ->
%%            Icon = <<"fa-check">>;
%%        true ->
%%            Icon = <<"fa-times">>
%%    end,
%%    case Type of
%%        0 -> TypeName = <<"User">>,
%%            ShortName = list_to_binary([<<"user">>,list_to_binary(integer_to_list(UUID))]);
%%        1 -> TypeName = <<"File">>,
%%            ShortName = list_to_binary([<<"file">>,list_to_binary(integer_to_list(UUID))]);
%%        2 -> TypeName = <<"Node">>,
%%            ShortName = list_to_binary([<<"node">>,list_to_binary(integer_to_list(UUID))]);
%%        3 -> TypeName = <<"User session">>,
%%            ShortName = list_to_binary([<<"user session">>,list_to_binary(integer_to_list(UUID))]);
%%        _ -> TypeName = <<"Service">>,
%%            ShortName = list_to_binary([<<"service">>,list_to_binary(integer_to_list(UUID))])
%%    end,
%%    %% io:format(ShortName),
%%    create_objects_array (N-1, [
%%        #{
%%            id => N,
%%            uuid => UUID,
%%            parentUuid => ParentUUID,
%%            type => Type,
%%            typeName => TypeName,
%%            shortName => ShortName,
%%            enabled => Enabled,
%%            enabledIcon => Icon
%%        }
%%        | T]).