-module(hlr).
-export([new/0, kill/0, attach/1, detach/0, lookup_id/1, lookup_ms/1]).

new() ->
  ets:new(msisdn2pid, [public, named_table]),
  ets:new(pid2msisdn, [public, named_table]),
  ok.

kill() ->
  ets:delete(msisdn2pid),
  ets:delete(pid2msisdn),
  ok.

%This function needs to be called from pid process,
%that is from the phone controller associated to Ms
attach(Ms) ->
  io:format("The controller pid is ~p and the phone is: ~p \n", [self(), Ms]),
  ets:insert(msisdn2pid, {Ms, self()}),
  ets:insert(pid2msisdn, {self(), Ms}),
  ok.


detach() ->
  case ets:lookup(pid2msisdn, self()) of
    [{Pid, Ms}] ->
      ets:delete(pid2msisdn, Pid),
      ets:delete(msisdn2pid, Ms);
    [] ->
      ok
  end .

lookup_id(Ms) ->
  case ets:lookup(msisdn2pid, Ms) of
    [{Ms, Pid}] -> {ok, Pid};
    [] -> {error, invalid}
  end.

lookup_ms(Pid) ->
    case ets:lookup(pid2msisdn, Pid) of
    [{Pid, Ms}] -> {ok, Ms};
    [] -> {error, invalid}
  end.