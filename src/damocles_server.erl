-module(damocles_server).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(interface, {name, ip}).

init(_) -> {ok, []}.

handle_call({add_interface, Ip}, _, State) ->
  case damocles_lib:ensure_local_interface_ip4(Ip) of
    {error, Reason} -> {reply, {error, Reason}, State};
    Interface -> {reply, Interface, [#interface{name=Interface, ip = Ip} | State]}
  end;
handle_call(_,_, State) -> {reply, ok, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

code_change(_, _, State) -> {ok, State}.

terminate(_, State) -> 
  %Attempts to tear down each interface we've created
  _ = rpc:pmap({damocles_lib, teardown_local_interface_ip4}, [], [Name || #interface{name = Name}<- State]), 
  {ok, []}.