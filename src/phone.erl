-module(phone).

-behaviour(gen_server).

-export([
  start_link/1,
  init/1  
]). %

-export([ disconnect/1, action/2 ]). % API
-export([ handle_call/3]). % Callbacks

-import(phone_fsm,[]).
-import(hlr, []).

%%%%%%%%%%%%%
% Start Fsm %
%%%%%%%%%%%%%

start_link(PhoneNumber) ->
  gen_server:start_link(?MODULE , PhoneNumber, []).

init(PhoneNumber) ->
  {ok, ControllerPid} = hlr:lookup_id(PhoneNumber),
  phone_fsm:connect(ControllerPid, self()),
  io:format("The Controller Pid is: ~p \n", [ControllerPid]),  
  State = #{
    controller_pid => ControllerPid,
    phone_number => PhoneNumber
  },
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%
% User -> Phone Api %
%%%%%%%%%%%%%%%%%%%%%

stop(PhoneNumber) ->
  gen_server:cast(local_name(PhoneNumber), stop).  

disconnect(PhonePid) ->
  gen_server:call(PhonePid, detach).

action(PhonePid, {call, PhoneNumber}) ->
  gen_server:call(PhonePid, {action, {call, PhoneNumber}});
action(PhonePid, accept) ->
  gen_server:call(PhonePid, {action, accept});
action(PhonePid, reject) ->
  gen_server:call(PhonePid, {action, reject });
action(PhonePid, hangup) ->
  gen_server:call(PhonePid, {action, hangup}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API Controller -> Phone %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

reply(PhonePid, {inbound, CallingPhone}) -> 
  gen_server:call(PhonePid, {income_call, CallingPhone}),
  ok;
reply(PhonePid, accept) -> 
  ok;
reply(PhonePid, invalid) -> 
  ok;
reply(PhonePid, reject) -> 
  ok;
reply(PhonePid, hangup) -> 
  ok.



%%%%%%%%%%%%%
% Callbacks %
%%%%%%%%%%%%%

terminate(_Reason, _State) ->
  ok.

handle_call({action, {call, PhoneNumber}}, From, State) ->
  ControllerPid = maps:get(controller_pid, State),
  phone_fsm:action(ControllerPid, {outbound, PhoneNumber}),
  Reply = #{},
  {reply, Reply, State};
handle_call(detach, _From, State) ->
  phone_fsm:disconnect(maps:get(controller_pid, State)),
  {reply, [], maps:remove(controller_pid, State)};
handle_call({income_call, CallingPhone}, PhoneController, State) ->
  PhoneNumber = maps:get(phone_number, State),
  io:format("A called from ~p has arrived at ~p", [CallingPhone, PhoneNumber]),
  {reply, accept, State}.

handle_cast(stop, _State) ->
  {stop, normal, []};
handle_cast(Msg, _State) ->
  io:format("Unexpected msg: ~p.", Msg),
  {noreply, []}. 


%%%%%%%%%%%
% Private %
%%%%%%%%%%%

local_name(PhoneNumber) ->
  {local, {phone, PhoneNumber}}.

controller_name(PhoneNumber) ->
  {local, {controller, PhoneNumber}}.