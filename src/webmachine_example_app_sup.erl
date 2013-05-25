-module(webmachine_example_app_sup).
-author('mkorszun@gmail.com').

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% ###############################################################
%% API
%% ###############################################################

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ###############################################################
%% SUPERVISOR CALLBACKS
%% ###############################################################

init([]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Dispatch} = file:consult(filename:join([priv_dir(App), "dispatch.conf"])),
    Port = case os:getenv("PORT") of false -> 8000; AnyPort -> AnyPort end,
    WebConfig = [{ip, "0.0.0.0"}, {port, Port}, {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    {ok, {{one_for_one, 10, 10}, [Web]}}.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################