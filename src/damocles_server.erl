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

-spec handle_call(_, _, #state{}) -> {reply, _, #state{}}.
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
handle_call({register_interface, IpOrAdapter}, _, State) ->
  case damocles_lib:register_local_interface_ip4(IpOrAdapter) of
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
handle_call({isolate_between_interfaces, SetA, SetB}, _, State) ->
  server_apply_rule_between_nodesets(SetA, SetB, [{drop, 100}], State);
handle_call({isolate_interface, IpOrAdapter}, _, State) ->
  server_apply_rule_to_all_connections_to_interface(IpOrAdapter, [{drop, 100}], State);
handle_call({isolate_one_way, Src, Dst}, _, State) ->
  server_apply_rule_one_way(Src, Dst, [{drop, 100}], State);
handle_call({packet_loss_one_way, Src, Dst, DropRate}, _, State) ->
  case check_drop_rate(DropRate) of
    ok -> server_apply_rule_one_way(Src, Dst, [{drop, DropRate}], State);
    A -> {reply, A, State}
  end;
handle_call({packet_loss_interface, IpOrAdapter, DropRate}, _, State) ->
  case check_drop_rate(DropRate) of
    ok -> server_apply_rule_to_all_connections_to_interface(IpOrAdapter, [{drop, DropRate}], State);
    A -> {reply, A, State}
  end;
handle_call({packet_loss_between_interfaces, SetA, SetB, DropRate}, _, State) ->
  case check_drop_rate(DropRate) of
    ok -> server_apply_rule_between_nodesets(SetA, SetB, [{drop, DropRate}], State);
    A -> {reply, A, State}
  end;
handle_call({packet_loss_global, DropRate}, _, State) ->
  case check_drop_rate(DropRate) of
    ok -> server_apply_rule_globally([{drop, DropRate}], State);
    A -> {reply, A, State}
  end;
handle_call({delay_one_way, Src, Dst, Amount}, _, State) ->
  server_apply_rule_one_way(Src, Dst, [{delay, Amount}], State);
handle_call({delay_interface, IpOrAdapter, Amount}, _, State) ->
  server_apply_rule_to_all_connections_to_interface(IpOrAdapter, [{delay, Amount}], State);
handle_call({delay_between_interfaces, SetA, SetB, Amount}, _, State) ->
  server_apply_rule_between_nodesets(SetA, SetB, [{delay, Amount}], State);
handle_call({delay_global, Amount}, _, State) ->
  server_apply_rule_globally([{delay, Amount}], State);
handle_call({restore_one_way, Src, Dst}, _, State = #state{interfaces = Interfaces, ipsToHandles = Dict}) ->
  SrcIp = (get_interface_for_ip_or_adapter(Src, Interfaces))#interface.ip,
  DstIp =  (get_interface_for_ip_or_adapter(Dst, Interfaces))#interface.ip,
  Handle = dict:fetch({SrcIp, DstIp},Dict),
  case damocles_lib:delete_packet_rules(Handle#handle.id) of
    ok -> 
      NewState = State#state{ipsToHandles = dict:store({SrcIp, DstIp}, Handle#handle{rules = []}, Dict)},
      {reply, ok, NewState};
    {error, Reason} -> 
      damocles_lib:log("Failed to remove rules for ~p -> ~p: ~p", [SrcIp, DstIp, Reason]),
      {reply, error, State}
  end;
handle_call({restore_interface, IpOrAdapter}, _, State) ->
  {[MatchingInterface], Others} = partition_interfaces(IpOrAdapter, State#state.interfaces),
  Ip = MatchingInterface#interface.ip,
  OtherIps = [X#interface.ip || X <- Others],
  case remove_all_rules_between_node_and_nodesets(Ip, OtherIps, State#state.ipsToHandles) of
    {ok, NewDict} ->
      {reply, ok, State#state{ipsToHandles = NewDict}}; 
    {error, Failed, NewDict} ->
      {reply, {error, Failed}, State#state{ipsToHandles = NewDict}}
  end;
handle_call(restore_all_interfaces, _, State) ->
  InterfaceSets =  interface_sets([X#interface.ip || X <- ordsets:to_list(State#state.interfaces)]), 
  {Failed, NewIpHandleDict} = 
    lists:foldl(
      fun({Ip, OtherIps}, {FailedAcc, Dict}) ->
        case remove_all_rules_between_node_and_nodesets(Ip, OtherIps, Dict) of
          {ok, NewDict} -> {FailedAcc, NewDict};
          {error, NewFails, NewDict} -> {NewFails ++ FailedAcc, NewDict}
        end
      end, {[], State#state.ipsToHandles}, InterfaceSets),
  case Failed of
    [] -> {reply, ok, State#state{ipsToHandles = NewIpHandleDict}};
    _ -> {reply, {error, Failed}, State#state{ipsToHandles = NewIpHandleDict}}
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

check_drop_rate(DropRate) when is_integer(DropRate) andalso (DropRate < 0 orelse DropRate > 100) -> {error, invalid_drop_rate};
check_drop_rate(DropRate) when is_float(DropRate) andalso (DropRate < 0.0 orelse DropRate > 1.0) -> {error, invalid_drop_rate};
check_drop_rate(_) -> ok.

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

%Will either apply the stated rules to all connections between known interfaces, clear all rules on 
%connections between known interfaces, or throw due to being in an inconsistent state.
server_apply_rule_globally(Rules, State) ->
  InterfaceSets =  interface_sets([X#interface.ip || X <- ordsets:to_list(State#state.interfaces)]), 
  {Failed, NewIpHandleDict} = 
    lists:foldl(
      fun
        (_, {error, NewDict}) -> {error, NewDict};
        ({Ip, OtherIps}, {ok, Dict}) ->
          case apply_rule_between_node_and_nodesets(Ip, OtherIps, Rules, Dict) of
            {ok, NewDict} -> {ok, NewDict};
            {error, NewDict} -> {error, NewDict}
          end
      end, {ok, State#state.ipsToHandles}, InterfaceSets),
  case Failed of
    ok -> {reply, ok, State#state{ipsToHandles = NewIpHandleDict}};
    error -> 
      {_, ok, NewState} = handle_call(restore_all_interfaces, undefined, State),
      {reply, error, NewState}
  end.

server_apply_rule_one_way(Src, Dst, Rules, State = #state{interfaces = Interfaces, ipsToHandles = HandleDict}) ->
  SrcIp = (get_interface_for_ip_or_adapter(Src, Interfaces))#interface.ip,
  DstIp =  (get_interface_for_ip_or_adapter(Dst, Interfaces))#interface.ip,
  {ok, Handle} = dict:find({SrcIp, DstIp}, HandleDict),
  case add_rules_to_handle(SrcIp, DstIp, Handle, Rules, HandleDict) of
    error -> {reply, error, State};
    {ok, NewDict} -> {reply, ok, State#state{ipsToHandles = NewDict}}
  end.

server_apply_rule_to_all_connections_to_interface(IpOrAdapter, Rules, State) ->
  {[MatchingInterface], Others} = partition_interfaces(IpOrAdapter, State#state.interfaces),
  Ip = MatchingInterface#interface.ip,
  OtherIps = [X#interface.ip || X <- Others],
  {SuccessOrFailure, NewDict} = apply_rule_between_node_and_nodesets(Ip, OtherIps, Rules, State#state.ipsToHandles),
  {reply, SuccessOrFailure, State#state{ipsToHandles = NewDict}}.

% Handles both nodesets, and individual interfaces. 
server_apply_rule_between_nodesets(SetA = [H | _], SetB, Rules, State) when is_integer(H) -> 
  server_apply_rule_between_nodesets([SetA], SetB, Rules, State);
server_apply_rule_between_nodesets(SetA, SetB = [H | _], Rules, State) when is_integer(H) -> 
  server_apply_rule_between_nodesets(SetA, [SetB], Rules, State);
server_apply_rule_between_nodesets(SetARaw, SetBRaw, Rules, State) ->
  SetA = [(get_interface_for_ip_or_adapter(X, State#state.interfaces))#interface.ip || X <- SetARaw],
  SetB = [(get_interface_for_ip_or_adapter(X, State#state.interfaces))#interface.ip || X <- SetBRaw],
  {SuccessOrFailure, NewDict} = apply_rule_between_nodeset_and_nodeset(SetA, SetB, Rules, State#state.ipsToHandles),
  {reply, SuccessOrFailure, State#state{ipsToHandles = NewDict}}.

% Either all connections between the two nodesets have the rule applied, or all connections
% are restored to normal, or, it throws, process dies.
apply_rule_between_nodeset_and_nodeset(NodesA, NodesB, Rules, HandleDict) ->
  Result = 
    lists:foldl(
      fun
        (_, error) -> error;
        (Node, {ok, Dict}) ->
          case apply_rule_between_node_and_nodesets(Node, NodesB, Rules, Dict) of
            {ok, NewDict} -> {ok, NewDict};
            {error, _} -> error
          end
      end, {ok, HandleDict}, NodesA),
  case Result of
    {ok, NewDict} -> {ok, NewDict};
    error ->
      NewDict = 
        lists:foldl(
          fun(Node, Dict) ->
            {ok, NewDict} = remove_all_rules_between_node_and_nodesets(Node, NodesA, Dict),
            NewDict
          end, HandleDict, NodesA),
      {error, NewDict}
  end.

%Calling this should guarantee one of three things about the connections to/from the specified IP.
% 1. In the event of success, all connections have successfully had the rule applied.
% 2. In the event of failure, all connections have had their rules removed; no packet drops/delays are being applied.
% 3. In the event of failing to guarantee one of the prior two, we are in an indeterminate state where some interfaces 
%    may have had the rule applied, and others have whatever they had prior to this function being called. This is bad. 
%    An exception will be thrown in such a case, causing the process to restart, and in doing so attempt to tear down and 
%    replace all interface configuration.
apply_rule_between_node_and_nodesets(NodeIp, NodeIpSet, Rules, HandleDict) ->
  IpHandleSets = lists:flatten([ [{NodeIp, X, dict:fetch({NodeIp, X}, HandleDict) }, {X, NodeIp,  dict:fetch({X, NodeIp}, HandleDict)}] || X <-NodeIpSet, X /= NodeIp ]),
  Result = 
    lists:foldl(
      fun
        (_, error) -> error;
        ({Ip1, Ip2, Handle}, {ok, Dict}) -> add_rules_to_handle(Ip1, Ip2, Handle, Rules, Dict)
      end, {ok, HandleDict}, IpHandleSets),
  case Result of
    error ->
      {ok, NewDict} = remove_all_rules_between_node_and_nodesets(NodeIp, NodeIpSet, HandleDict),
      {error, NewDict}; %We return error to indicate we failed, and the new dictionary shows there are no rules being applied to the connections.
    {ok, NewDict} -> 
      {ok, NewDict}
  end.

add_rules_to_handle(Ip1, Ip2, Handle, Rules, Dict) ->
  NewRuleTypes = [element(1, Rule) || Rule <- Rules],
  CurrentRules = 
    lists:foldl(
      fun(Type, List) ->
        lists:keydelete(Type, 1, List)
      end, Handle#handle.rules, NewRuleTypes), 
  case damocles_lib:set_packet_rules(Handle#handle.id, Rules ++ CurrentRules) of
    ok -> {ok, dict:store({Ip1, Ip2}, Handle#handle{rules = (Rules ++ CurrentRules)}, Dict)};
    {error, Reason} ->  
      damocles_lib:log("Failed to apply rule for ~p -> ~p: ~p", [Ip1, Ip2, Reason]), 
      error
  end.

remove_all_rules_between_node_and_nodesets(NodeIp, NodeIpSet, HandleDict) ->
  IpHandleSets = lists:flatten([ [{NodeIp, X, dict:fetch({NodeIp, X}, HandleDict) }, {X, NodeIp,  dict:fetch({X, NodeIp}, HandleDict)}] || X <-NodeIpSet ]),
  Result = 
    lists:foldl(
      fun
        ({Ip1, Ip2, Handle}, {Acc, AccDict}) ->
          case Handle#handle.rules of
            [] -> {Acc, AccDict};
            _ ->
              case damocles_lib:delete_packet_rules(Handle#handle.id) of
                ok -> {Acc, dict:store({Ip1, Ip2}, Handle#handle{rules = []}, AccDict)};
                {error, Reason} -> damocles_lib:log("Failed to remove rules for ~p -> ~p: ~p", [Ip1, Ip2, Reason]), {[{Ip1, Ip2} | Acc], AccDict}
              end
            end
      end, {[], HandleDict}, IpHandleSets),
  case Result of
    {[], NewDict} -> {ok, NewDict};
    {Failed, NewDict} -> {error, Failed, NewDict}
  end.



interface_sets([_]) -> [];
interface_sets([H | T]) when T /= []->
  [{H, T}] ++ interface_sets(T).