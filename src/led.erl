%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2012, Frank Hunleth
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2012 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module(led).

-behaviour(gen_server).

%% API
-export([list/0,
	 open/1,
	 close/1,
	 start_link/1,
	 set_brightness/2,
	 brightness/1,
	 max_brightness/1,
	 disable_triggers/1,
	 blink/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(LED_SYSFS_DIR, "/sys/class/leds/").

-record(state, {
	  %% LED name (of the form "beaglebone:green:usr0")
	  name,

	  %% LED class directory path
	  dir_name,
	  
	  %% Handles
	  brightness_file_handle
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Return the list of LEDs available on the system
list() ->
    case file:list_dir(?LED_SYSFS_DIR) of
	{ok, LedNames} ->
	    {ok, LedNames};
	{error, enoent} ->
	    % No LEDs exposed through sysfs
	    {ok, []}
    end.

%% @doc Open one of the LEDs returned by list/1. An LED must
%%      be opened before it can be used.
open(Name) ->
    led_sup:start_child(Name).

%% @doc Return all resources associated with 
close(Name) ->
    Pid = pg2:get_closest_pid(Name),
    gen_server:cast(Pid, close).

%% @doc Change the brightness of the LED. For many LEDs this just 
%%      controls whether they are on (1) or off (0)
set_brightness(Name, BrightnessLevel) ->
    Pid = pg2:get_closest_pid(Name),
    gen_server:call(Pid, {set_brightness, BrightnessLevel}).

%% @doc Return the current LED brightness
brightness(Name) ->
    Pid = pg2:get_closest_pid(Name),
    gen_server:call(Pid, brightness).

%% @doc Get the maximum brightness that may be passed to 
%%      set_brightness/2.
max_brightness(Name) ->
    Pid = pg2:get_closest_pid(Name),
    gen_server:call(Pid, max_brightness).

%% @doc Disable all triggers on the LED    
disable_triggers(Name) ->
    Pid = pg2:get_closest_pid(Name),
    gen_server:call(Pid, disable_triggers).

%% @doc Configure a trigger to blink the LED
blink(Name, OnTimeMillis, OffTimeMillis) ->
    Pid = pg2:get_closest_pid(Name),
    gen_server:call(Pid, {blink, OnTimeMillis, OffTimeMillis}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Name) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name]) ->
    LedDirName = ?LED_SYSFS_DIR ++ Name ++ "/",
    BrightnessFile = LedDirName ++ "brightness",
    {ok, BrightnessFileHandle} = file:open(BrightnessFile, [read, write]),
    ok = pg2:create(Name),
    ok = pg2:join(Name, self()),
    io:format("pg2:join(~p, ~p)~n", [Name, self()]),
    State = #state{name = Name,
		   dir_name = LedDirName,
		   brightness_file_handle = BrightnessFileHandle},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_brightness, BrightnessLevel}, _From, State) ->
    file:pwrite(State#state.brightness_file_handle, 
		0, 
		integer_to_list(BrightnessLevel)),
    Reply = ok,
    {reply, Reply, State};

handle_call(brightness, _From, State) ->
    {ok, ValueAsString} = file:pread(State#state.brightness_file_handle, 
				     0, 32),
    {Value, _} = string:to_integer(ValueAsString),
    Reply = {ok, Value},
    {reply, Reply, State};
    
handle_call(max_brightness, _From, State) ->
    Value = read_sysfs_integer(State#state.dir_name ++ "max_brightness"),
    Reply = {ok, Value},
    {reply, Reply, State};

handle_call(disable_triggers, _From, State) ->
    write_sysfs_string(State#state.dir_name ++ "trigger", "none"),
    {reply, ok, State};

handle_call({blink, OnTimeMillis, OffTimeMillis}, _From, State) ->
    write_sysfs_string(State#state.dir_name ++ "trigger", "timer"),
    write_sysfs_string(State#state.dir_name ++ "delay_on", integer_to_list(OnTimeMillis)),
    write_sysfs_string(State#state.dir_name ++ "delay_off", integer_to_list(OffTimeMillis)),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(close, State) ->
    io:format("close~n"),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    io:format("terminating~n"),
    file:close(State#state.brightness_file_handle),
    ok = pg2:leave(State#state.name, self()),
    pg2:delete(State#state.name),
    io:format("pg2:delete(~p)~n", [State#state.name]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_sysfs_integer(Filename) ->
    {ok, Handle} = file:open(Filename, [read]),
    {ok, ValueAsString} = file:pread(Handle, 0, 32),
    file:close(Handle),
    {Value, _} = string:to_integer(ValueAsString),
    Value.

write_sysfs_string(Filename, Value) ->
    {ok, Handle} = file:open(Filename, [write]),
    file:pwrite(Handle, 0, Value),
    file:close(Handle).
