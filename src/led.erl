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
	 start_link/1,
	 close/1,
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
	  %% LED class directory path
	  dir_name,

	  %% Handles
	  brightness_file_handle
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Return the list of LEDs available on the system
-spec list() -> {ok, [atom()]}.
list() ->
    case file:list_dir(?LED_SYSFS_DIR) of
	{ok, LedNames} ->
	    AtomizedNames = [list_to_atom(X) || X <- LedNames],
	    {ok, AtomizedNames};
	{error, enoent} ->
	    % No LEDs exposed through sysfs
	    {ok, []}
    end.

%% @doc Open one of the LEDs returned by list/1. An LED must
%%      be opened before it can be used.
-spec start_link(atom()) -> {ok, pid()} | {error, _}.
start_link(LedName) ->
    gen_server:start_link({local, LedName}, ?MODULE, [LedName], []).

%% @doc Return all resources associated with
-spec close(atom()) -> ok.
close(LedName) ->
    gen_server:cast(LedName, close).

%% @doc Change the brightness of the LED. For many LEDs this just
%%      controls whether they are on (1) or off (0)
-spec set_brightness(atom(), non_neg_integer()) -> ok.
set_brightness(LedName, BrightnessLevel) ->
    gen_server:call(LedName, {set_brightness, BrightnessLevel}).

%% @doc Return the current LED brightness
-spec brightness(atom()) -> {ok, non_neg_integer()}.
brightness(LedName) ->
    gen_server:call(LedName, brightness).

%% @doc Get the maximum brightness that may be passed to
%%      set_brightness/2.
-spec max_brightness(atom()) -> {ok, non_neg_integer()}.
max_brightness(LedName) ->
    gen_server:call(LedName, max_brightness).

%% @doc Disable all triggers on the LED
-spec disable_triggers(atom()) -> ok.
disable_triggers(LedName) ->
    gen_server:call(LedName, disable_triggers).

%% @doc Configure a trigger to blink the LED
-spec blink(atom(), non_neg_integer(), non_neg_integer) -> ok.
blink(LedName, OnTimeMillis, OffTimeMillis) ->
    gen_server:call(LedName, {blink, OnTimeMillis, OffTimeMillis}).

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
init([LedName]) ->
    StringName = atom_to_list(LedName),
    LedDirName = ?LED_SYSFS_DIR ++ StringName ++ "/",
    BrightnessFile = LedDirName ++ "brightness",
    {ok, BrightnessFileHandle} = file:open(BrightnessFile, [read, write]),
    State = #state{dir_name = LedDirName,
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
    file:close(State#state.brightness_file_handle),
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
-spec read_sysfs_integer(string()) -> integer().
read_sysfs_integer(Filename) ->
    {ok, Handle} = file:open(Filename, [read]),
    {ok, ValueAsString} = file:pread(Handle, 0, 32),
    file:close(Handle),
    {Value, _} = string:to_integer(ValueAsString),
    Value.

-spec write_sysfs_string(string(), string()) -> ok.
write_sysfs_string(Filename, Value) ->
    {ok, Handle} = file:open(Filename, [write]),
    file:pwrite(Handle, 0, Value),
    file:close(Handle).
