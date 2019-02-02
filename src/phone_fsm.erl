-module(phone_fsm).
-behaviour(gen_fsm).

-export([
  start_link/1, 
  connect/2, 
  disconnect/1,
  action/2
]).
-export([init/1]).
-export([
  idle/2, 
  calling/2,
  connecting/2,
  receiving/2
]). % Callback States
-export([
  busy/1,
  rejected/1,
  accept/1,
  hangup/1,
  inbound/2
]). % Api Controller - Controller


-import(hlr, []).

%%%%%%%
% API %
%%%%%%%

start_link(PhoneNumber) ->
  gen_fsm:start_link(?MODULE, PhoneNumber, []).

% the init callback is executed on genserver process, not on the calling one
init(PhoneNumber) ->
  hlr:attach(PhoneNumber),
  {ok, idle, []}.

stop(FsmPid) ->
  to_end.

%%%%%%%%%%%%%
% Phone-Controller Api %
%%%%%%%%%%%%%

connect(ControllerPid, PhonePid) ->
  io:format("~p, ~p\n", [ControllerPid, PhonePid]),
  gen_fsm:send_event(ControllerPid, {attach_phone, PhonePid}),
  ok.

disconnect(ControllerPid) ->
  io:format("Discoonect controller: ~p\n", [ControllerPid]),
  gen_fsm:send_event(ControllerPid, {detach_phone}),
  ok.

action(FsmPid, {outbound, PhoneNumber}) -> 
  gen_fsm:send_event(FsmPid, {outbound, PhoneNumber});
action(FsmPid, accept) -> 
  gen_fsm:send_event(FsmPid, accept);
action(FsmPid, reject) -> 
  gen_fsm:send_event(FsmPid, reject);
action(FsmPid, hangup) -> 
  gen_fsm:send_event(FsmPid, hangup).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Controller-Controller API %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

busy(FsmPid) ->
  gen_fsm:send_event(FsmPid, reject).

rejected(FsmPid) ->
  gen_fsm:send_event(FsmPid, reject).

accept(FsmPid) ->
  gen_fsm:send_event(FsmPid, accept).

hangup(FsmPid) ->
  gen_fsm:send_event(FsmPid, hangup).

inbound(CallingPid, CalledPid) ->
  gen_fsm:send_event(CalledPid, {inbound, CallingPid}).


%%%%%%%%%%%%%
% FSM Callbacks %
%%%%%%%%%%%%%

idle({outbound, PhoneNumber}, State) ->
  {ok, CalledPid} = hlr:lookup_id(PhoneNumber),
  phone_fsm:inbound(self(), CalledPid),
  {next_state, calling, State};
idle({inbound, CallingPid}, State) ->
  NewState = maps:put(calling_controller, CallingPid, State),
  PhonePid = maps:get(phone_pid, State),
  phone:reply(PhonePid, {inbound, c}),
  io:format("Hola alguien llama al controlador\n"),
  {next_state, connecting, NewState};
idle({attach_phone, PhonePid}, _State) ->
  {next_state, idle, #{phone_pid => PhonePid}};
idle({detach_phone}, State) ->
  {next_state, idle, maps:remove(phone_pid, State)}.

calling(connected, _State) ->
  {next_state, receiving};
calling(hang_up, _State) ->
  {next_state, idle}.


receiving({hang_up}, State) ->
  {next_state, receiving, State};
receiving({detach_phone}, State) ->
  io:format("Impossible to detach phone, ongoing call"),
  {next_state, receiving, State}.

connecting({connected}, State) ->
  {next_state, receiving, State};
connecting({rejected}, State) ->
  {next_state, idle, State}.