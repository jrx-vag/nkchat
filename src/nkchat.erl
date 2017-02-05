%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Carlos Gonzalez Florido.  All Rights Reserved.
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


-module(nkchat).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([start/0, stop/0]).

%% ===================================================================
%% Types
%% ===================================================================



start() ->
    Spec1 = #{
    	callback => ?MODULE,
        plugins => [nkchat_mm_proxy],  
        mm_proxy_listen => "http://all:8200",
        %web_server => "https:all:8081",
        %web_server_path => "./www",
        % api_server => "ws:all:10106/dkv, http://all:10107/dkv",
        % api_server_timeout => 180,
        % elastic_url => "https://cluster.netc.io/es/",
        % elastic_user => "user",
        % elastic_pass => "es",
        debug => [
        	nkchat_mm_proxy_server_http
        	% nkchat_mm_proxy_server_ws, 
        	% nkchat_mm_proxy_client_ws
        ]
        % api_gelf_server => "c2.netc.io"
    },
    % Spec2 = nkmedia_util:add_certs(Spec1),
    nkservice:start(mm, Spec1).


stop() ->
    nkservice:stop(mm).
	


%% ===================================================================
%% Public functions
%% ===================================================================
