-module(hlr_server).

-behaviour(gen_server).

-export([
  start_link/0,
  register_phone_number/2,
  get_phone_controller_pid/1
]).

-export([
  init/1,
  handle_cast/2
]).

%%%%%%%
% Api %
%%%%%%%

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_phone_number(PhoneNumber, ControllerPid)->
  gen_server:cast(?MODULE, {new_number, {PhoneNumber, ControllerPid}}).

get_phone_controller_pid(PhoneNumber) ->
  gen_server:call(?MODULE, {get_controller_pid, PhoneNumber}).

%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

init(_Args) ->
    {ok, #{}}.

handle_call({get_controler_pid, PhoneNumber}, State) ->
  maps:get(PhoneNumber, State, not_found).

handle_cast({new_number, {PhoneNumber, ControllerPid}}, State) ->
  io:format("The PhoneNumber is: ~p, and the Controller is: ~p \n", 
  [PhoneNumber, ControllerPid]),
    {noreply, maps:put(PhoneNumber, ControllerPid, State)};
handle_cast(_Info, State) ->
  io:format("Unexpected msg: ~p", [_Info]),
  {noreply, State}.
