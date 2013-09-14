-module(led_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_child/1
	]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name) ->
    supervisor:start_child(?SERVER, [Name]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, 
           [{led, {led, start_link, []}, permanent, 5000, worker, [led]}] } }.

