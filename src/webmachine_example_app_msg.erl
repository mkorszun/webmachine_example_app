-module(webmachine_example_app_msg).
-author('mkorszun@gmail.com').

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2]).
-export([process_post/2, to_json/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

process_post(ReqData, State) ->
    Doc = {[{<<"type">>, <<"msg">>}, {<<"body">>, wrq:req_body(ReqData)}]},
    {ok, Doc1} = couchbeam:save_doc(couchdb_connection(), Doc),
    lager:info("Message created, id = ~p", [couchbeam_doc:get_id(Doc1)]),
    {true, ReqData, State}.

to_json(ReqData, State) ->
    DB = couchdb_connection(),
    %% Get docs meta-data
    {ok, Docs} = couchbeam_view:all(DB),
    lager:info("Reading all messages, size=~p...", [length(Docs)]),
    %% Read all docs
    DocsJson = lists:foldl(fun({Meta}, Acc) ->
                    Id = proplists:get_value(<<"id">>, Meta),
                    {ok, {Doc}} = couchbeam:open_doc(DB, Id),
                    [{struct, Doc} | Acc]
                end,
        [], Docs),
    {mochijson2:encode(DocsJson), ReqData, State}.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

couchdb_connection() ->
    %% Get Couchdb URI from env variable
    URI = os:getenv("CLOUDANT_URL"),
    lager:info("Creating db connection ~s", [URI]),
    %% Parse URI to get db attributes
    {ok, {_, UserPass, Host, _, _, _}} = http_uri:parse(URI),
    [User, Pass] = string:tokens(UserPass, ":"),

    %% Open db connection
    Conn = couchbeam:server_connection(Host, 5984),
    Opts = [{basic_auth, {User, Pass}}],
    {ok, DB} = couchbeam:open_or_create_db(Conn, "messages", Opts),
    DB.

%% ###############################################################
%% ###############################################################
%% ###############################################################