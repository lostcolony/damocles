-module(damocles_server).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


-record(handle, {id :: integer(), rules = [] :: damocles_lib:tc_rules()}).
-record(state, {interfaces = ordsets:new(), currentHandle=10, ipsToHandles = dict:new() :: ip_to_handle_dict()}).
-record(interface, {name :: string(), ip :: string(), transient=true}).

-type ip_to_handle_dict() :: dict:dict({nonempty_string(), nonempty_string()}, #handle{}).

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
handle_call({get_rules_for_connection, IpOrAdapter1, IpOrAdapter2}, _, State = #state{interfaces = Interfaces, ipsToHandles = HandleDict}) ->
  Ip1 = (get_interface_for_ip_or_adapter(IpOrAdapter1, Interfaces))#interface.ip,
  Ip2 = (get_interface_for_ip_or_adapter(IpOrAdapter2, Interfaces))#interface.ip,
  case dict:find({Ip1, Ip2}, HandleDict) of
    {ok, #handle{rules = Rules}} -> {reply, Rules, State};
    _ -> {reply, undefined, State}
  end;
handle_call({isolate_interface, IpOrAdapter}, _, State) ->
  {[MatchingInterface], Others} = partition_interfaces(IpOrAdapter, State#state.interfaces),
  {SuccessOrFailure, NewDict} = apply_rule_to_all_node_connections(MatchingInterface, Others, [{drop, 100}], State#state.ipsToHandles),
  {reply, SuccessOrFailure, State#state{ipsToHandles = NewDict}};
handle_call({restore_interface, IpOrAdapter}, _, State) ->
  {[MatchingInterface], Others} = partition_interfaces(IpOrAdapter, State#state.interfaces),
  {SuccessOrFailure, NewDict} = remove_all_rules_for_node_connections(MatchingInterface, Others, State#state.ipsToHandles),
  {reply, SuccessOrFailure, State#state{ipsToHandles = NewDict}};  
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

get_interface_for_ip_or_adapter(IpOrAdapter, Interfaces) ->
  List = ordsets:to_list(Interfaces),
  case [X || X <- List, X#interface.ip == IpOrAdapter orelse X#interface.name == IpOrAdapter] of
    [] -> undefined;
    [A] -> A
  end.

partition_interfaces(IpOrAdapter, Interfaces) ->
  List = ordsets:to_list(Interfaces),
  lists:partition(
    fun(Interface) ->
      Interface#interface.ip == IpOrAdapter orelse Interface#interface.name == IpOrAdapter
    end, List).

-spec add_handles_for_interface(nonempty_string(), #state{}) -> {integer(), dict:dict({nonempty_string(), nonempty_string()}, #handle{})} | error.
add_handles_for_interface(Ip, #state{currentHandle = CurrentHandle, ipsToHandles = HandleDict, interfaces = Interfaces}) ->
  OtherIps = [ X#interface.ip || X <- Interfaces],
  case lists:member(Ip, OtherIps) of
    true -> {CurrentHandle, HandleDict};
    false -> 
      lists:foldl(
        fun
          (_, error) -> error;
          (OtherIp, {Handle, Dict}) ->
          try
            ok = damocles_lib:add_class_filter_for_ips(Ip, OtherIp, Handle),
            ok = damocles_lib:add_class_filter_for_ips(OtherIp, Ip, Handle+1),
            NewDict = dict:store({Ip, OtherIp}, #handle{id=Handle}, dict:store({OtherIp, Ip}, #handle{id=Handle+1}, Dict)),
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

-spec remove_all_rules_for_node_connections(#interface{}, [#interface{}], ip_to_handle_dict()) -> {ok, ip_to_handle_dict()} | {error, [{nonempty_string(), nonempty_string()}], ip_to_handle_dict()}. 
remove_all_rules_for_node_connections(#interface{ip = IpOfNode}, OtherInterfaces, HandleDict) ->
  IpHandleSets =  lists:flatten([ [{IpOfNode, Interface#interface.ip, dict:fetch({IpOfNode, Interface#interface.ip}, HandleDict)}, {Interface#interface.ip, IpOfNode, dict:fetch({Interface#interface.ip, IpOfNode}, HandleDict)}] || Interface <- OtherInterfaces]),
  Result = 
    lists:foldl(
      fun
        ({Ip1, Ip2, Handle}, {Acc, AccDict}) ->
          case damocles_lib:delete_packet_rules(Handle#handle.id) of
            ok -> {Acc, dict:store({Ip1, Ip2}, Handle#handle{rules = []}, AccDict)};
            {error, Reason} -> damocles_lib:log("Failed to remove rules for ~p -> ~p: ~p", [Ip1, Ip2, Reason]), {[{Ip1, Ip2} | Acc], AccDict}
          end
      end, {[], HandleDict}, IpHandleSets),
  case Result of
    {[], NewDict} -> {ok, NewDict};
    {Failed, NewDict} -> {error, Failed, NewDict}
  end.

%Calling this should guarantee one of three things about the connections to/from the specified IP.
% 1. In the event of success, all connections have successfully had the rule applied.
% 2. In the event of failure, all connections have had their rules removed; no packet drops/delays are being applied.
% 3. In the event of failing to guarantee one of the prior two, we are in an indeterminate state where some interfaces 
%    may have had the rule applied, and others have whatever they had prior to this function being called. This is bad. 
%    An exception will be thrown in such a case, causing the process to restart, and in doing so attempt to tear down and 
%    replace all interface configuration.
-spec apply_rule_to_all_node_connections(#interface{}, [#interface{}], damocles_lib:tc_rules(), ip_to_handle_dict()) -> {ok | error, ip_to_handle_dict()}.
apply_rule_to_all_node_connections(#interface{ip = IpOfNode} = InterfaceOfNode, OtherInterfaces, Rules, HandleDict) ->
  IpHandleSets =  lists:flatten([ [{IpOfNode, Interface#interface.ip, dict:fetch({IpOfNode, Interface#interface.ip}, HandleDict)}, {Interface#interface.ip, IpOfNode, dict:fetch({Interface#interface.ip, IpOfNode}, HandleDict)}] || Interface <- OtherInterfaces]),
  Result = 
    lists:foldl(
      fun
        (_, error) -> error;
        ({Ip1, Ip2, Handle}, ok) ->
          case damocles_lib:set_packet_rules(Handle#handle.id, Rules) of
            ok -> ok;
            {error, Reason} ->  
              damocles_lib:log("Failed to apply rule for ~p -> ~p: ~p", [Ip1, Ip2, Reason]), 
              error
          end
      end, ok, IpHandleSets),
  case Result of
    error ->
      {ok, NewDict} = remove_all_rules_for_node_connections(InterfaceOfNode, OtherInterfaces, HandleDict),
      {error, NewDict}; %We return error to indicate we failed, and the new dictionary shows there are no rules being applied to the connections.
    ok -> %All worked; just need to update the dictionary for each entry to the new rule
      Return = 
        lists:foldl(
          fun({Ip1, Ip2, Handle}, Dict) ->
            dict:store({Ip1, Ip2}, Handle#handle{rules = Rules}, Dict)
          end, HandleDict, IpHandleSets),
      {ok, Return}
  end.



