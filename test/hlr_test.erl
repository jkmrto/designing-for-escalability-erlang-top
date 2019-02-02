-module(hlr_test).
-include_lib("eunit/include/eunit.hrl").
-define(DBG(Str, Args), io:format(Str, Args)).

%all the tests within the module are run by calling mod:test()

setup(Ms) -> 
    hlr:new(), 
    hlr:attach(Ms).

clean() -> 
    hlr:kill().

lookup_ms_test_() ->
    Ms = 12345, 
    {setup,
        fun() -> setup(Ms) end, % setup
        fun(_) -> clean() end, % cleanup
        fun () -> 
            ?_assertMatch({ok, Ms}, hlr:lookup_ms(self())) 
        end
    }.

lookup_id_test() ->
    Ms = 12345, 
    {setup,
        fun() -> setup(Ms) end, % setup
        fun(_) -> clean() end, % cleanup
        fun () -> 
            Pid = self(),
            ?_assertMatch({ok, Pid}, hlr:lookup_id(Ms)) 
        end
    }.



