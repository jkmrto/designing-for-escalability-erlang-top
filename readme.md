# Designing for Scalability for Erlang and Otp

Own code done when reading the book **"Designing for scalability for Erlang and Otp"**


## Start Erl session loading ebin files 
``` Erlang
erl -pa ebin
```

## Test
```Erlang
bash run_test.sh
```


## Compile
```Erlang
bash compile.sh
```

## Create HLR Registry

The HLR should be able to map from a PhoneNumber coming through a caller phone, to the controller Pid of the called phone,
this could be done using the state of the genserver.

Start HLR:
```Erlang
c(hlr).
hlr:new().
c(phone_fsm).
phone_fsm:start_link("649").
c(phone).
{ok, P649} = phone:start_link("649").


phone_fsm:start_link("456").
phone_fsm:start_link("317").
{ok, P317} = phone:start_link("317").
{ok, P456} = phone:start_link("456").

%Call from 640 to 317
phone:action(P649, {call, "317"}).

phone:disconnect(P649).
phone:action(P649, {call, 317})
```

Next to do:

See how behaves when makeing a call from phone:
1. phone:action(PhonePid, {call, PhoneNumber})
We need to get an answer from the inbound call




