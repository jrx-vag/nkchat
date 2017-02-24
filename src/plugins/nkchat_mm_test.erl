%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% -------------------------------------------------------------------


-module(nkchat_mm_test).

-compile([export_all]).



%%login() ->
%%    Data = #{
%%        name => <<"pruebas">>,
%%        login_id => <<"carlosj.gf@gmail.com">>,
%%        password => <<"carlos12">>
%%    },
%%    Url = "https://chat.netc.io:9443/api/v3/users/login",
%%    case dkv_util:http(post, Url, #{body=>Data}) of
%%        {ok, Hds, Resp, _} ->
%%            Token = nklib_util:get_value(<<"Token">>, Hds),
%%            User = nklib_json:decode(Resp),
%%            {ok, Token, User};
%%        {error, Error} ->
%%            {error, Error}
%%    end.


cmd(T, Cmd) ->
    get(T, nklib_util:to_list(Cmd)).



% curl -i -H 'Authorization: Bearer hyr5dmb1mbb49c44qmx4whniso' http://localhost:8065/api/v3/users/me




%%post1() ->
%%    Opts = #{
%%        body => #{
%%            % icon_url=> <<"http://www.jaraxa.com/img/jaraxa-logo.png">>,
%%            % username => <<"NetComposer">>,
%%            % channel => <<"off-topic">>,
%%            channel => <<"@carlos2">>,
%%            text => post_text1()
%%        }
%%    },
%%    Url = "https://dkv.netc.io:9443/hooks/8376mxck33y97qhgtnx1e3uq5y",



post_text1() -> <<"
---
Build Break - Project X - December 12, 2015 - 15:32 GMT +0  
For @carlos2

| Component  | Tests Run   | Tests Failed                                   |
|:-----------|:------------|:-----------------------------------------------|
| Server     | 948         | :white_check_mark: 0                           |
| Web Client | 123         | [:warning: 2 (see details)](http://www.jaraxa.com) |
| iOS Client | 78          | [:warning: 3 (see details)](http://linktologs) |
---
">>.


post2() ->
%%    Opts = #{
%%        body => #{
%%            icon_url=> <<"http://www.jaraxa.com/img/jaraxa-logo.png">>,
%%            username => <<"NetComposer">>,
%%            % channel => <<"off-topic">>,
%%            % channel => <<"@carlos2">>,
%%            attachments => [post_attach1()]
%%        }
%%    },
%%    Url = "https://dkv.netc.io:9443/hooks/gg7fmp4nofb6me634qe6tj7j1e",
%%    dkv_util:http(post, Url, Opts).




post_attach1() -> #{
    fallback => <<"test">>,         % For notifications
    color => <<"#FF8000">>,
    pretext => <<"This is optional pretext that shows above the attachment.">>,
    text => <<"This is the text of the attachment. It should appear just above an image of the Mattermost logo. The left border of the attachment should be colored orange, and below the image it should include additional fields that are formatted in columns. At the top of the attachment, there should be an author name followed by a bolded title. Both the author name and the title should be hyperlinks.">>,
    author_name => <<"Mattermost">>,
    author_icon => <<"http://www.mattermost.org/wp-content/uploads/2016/04/icon_WS.png">>,
    author_link => <<"http://www.mattermost.org/">>,
    title => <<"Example Attachment">>,
    title_link => <<"http://docs.mattermost.com/developer/message-attachments.html">>,
    fields => [
        #{
            short => false,
            title => <<"Long Field">>,
            value => <<"Testing with a very long piece of text that will take up the whole width of the table. And then some more text to make it extra long.">>
        },
        #{
            short =>true,
            title => <<"Column One">>,
            value => <<"Testing">>
        },
        #{
            short => true,
            title => <<"Column Two">>,
            value => <<"Testing">>
        },
        #{
            short => false,
            title => <<"Another Field">>,
            value => <<"Testing">>
        }
    ],
    image_url => <<"http://www.mattermost.org/wp-content/uploads/2016/03/logoHorizontal_WS.png">>
}.








