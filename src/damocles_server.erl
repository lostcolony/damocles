-module(damocles_server).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {interfaces = ordsets:new(), currentHandle=10, ipsToHandles = dict:new()}).
-record(interface, {name, ip, transient=true}).


init(_) ->
  %If we can't create traffic control, die. Die hard. With a vengeance. 
  ok = damocles_lib:initialize_traffic_control(), 

  %At this point make sure if we're exiting due to a shutdown we trap it, so terminate is called, and we (attempt to) undo our system manipulations.
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({add_interface, Ip}, _, State) ->
  case damocles_lib:add_local_interface_ip4(Ip) of
    {error, Reason} -> {reply, {error, Reason}, State};
    Interface ->
      OldInterfaces = State#state.interfaces, 
      case add_handles_for_interface(Ip, State) of
        error -> 
          ok = damocles_lib:teardown_local_interface_ip4(Interface),
          {reply, error, State};
        {NewHandle, NewDict} ->
          {reply, Interface, #state{interfaces = ordsets:add_element(#interface{name = Interface, ip = Ip}, OldInterfaces), currentHandle = NewHandle, ipsToHandles = NewDict}}
      end
  end;
handle_call({ensure_interface, IpOrAdapter}, _, State) ->
  case damocles_lib:ensure_local_interface_ip4(IpOrAdapter) of
    false -> 
      {reply, false, State};
    {Ip, Interface} -> 
      OldInterfaces = State#state.interfaces, 
      case add_handles_for_interface(Ip, State) of
        error -> 
          ok = damocles_lib:teardown_local_interface_ip4(Interface),
          {reply, error, State};
        {NewHandle, NewDict} ->
          {reply, Interface, #state{interfaces = ordsets:add_element(#interface{name = Interface, ip = Ip}, OldInterfaces), currentHandle = NewHandle, ipsToHandles = NewDict}}
      end
  end;
handle_call(_,_, State) -> {reply, ok, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

code_change(_, _, State) -> {ok, State}.

terminate(_Reason, State) -> 
  %Attempts to tear down each interface we've created, in parallel
  _ = rpc:pmap({damocles_lib, teardown_local_interface_ip4}, [], [Name || #interface{name = Name, transient = true}<- ordsets:to_list(State#state.interfaces)]), 
  _ = damocles_lib:teardown_traffic_control(),
  {ok, []}.


-spec add_handles_for_interface(nonempty_string(), #state{}) -> {integer(), dict:dict({nonempty_string(), nonempty_string()}, integer())} | error.
add_handles_for_interface(Ip, #state{currentHandle = CurrentHandle, ipsToHandles = HandleDict, interfaces = Interfaces}) ->
  OtherIps = [ X#interface.ip || X <- Interfaces],
  case lists:member(Ip, OtherIps) of
    true -> {CurrentHandle, HandleDict};
    false -> 
      lists:foldl(
        fun
          (_, error) -> error;
          (OtherIp, {Handle, Dict}) ->
          damocles_lib:log("ALL: ~p, ~p, ~p", [OtherIp, Handle, Dict]),
          try
            ok = damocles_lib:add_class_filter_for_ips(Ip, OtherIp, Handle),
            ok = damocles_lib:add_class_filter_for_ips(OtherIp, Ip, Handle+1),
            NewDict = dict:store({Ip, OtherIp}, Handle, dict:store({OtherIp, Ip}, Handle+1, Dict)),
            {Handle+2, NewDict}
          catch _:Reason ->
            damocles_lib:log("Failed to create class filters between ~p and ~p because ~p", [Ip, OtherIp, Reason]),
            lists:foreach(
              fun(H) ->
                damocles_lib:delete_class_filter(H) %Just delete every item we created in this fold.
              end, lists:seq(CurrentHandle, Handle+1)),
            error
          end
        end, {CurrentHandle, HandleDict}, OtherIps)
  end.