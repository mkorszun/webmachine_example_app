-module(webmachine_example_app).
-author('mkorszun@gmail.com').

-behaviour(application).
-export([start/0, stop/0]).
-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION API
%% ###############################################################

start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(lager),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(sasl),
    ensure_started(ibrowse),
    ensure_started(couchbeam),
    ensure_started(webmachine),
    application:start(webmachine_example_app).

stop() ->
    application:stop(webmachine_example_app),
    application:stop(webmachine),
    application:stop(couchbeam),
    application:stop(ibrowse),
    application:stop(sasl),
    application:stop(public_key),
    application:stop(lager),
    application:stop(syntax_tools),
    application:stop(compiler),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets).

%% ###############################################################
%% APPLICATION CALLBACKS
%% ###############################################################

start(_Type, _StartArgs) ->
    webmachine_example_app_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################