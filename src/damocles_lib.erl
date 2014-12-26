-module(damocles_lib).

-export([
  initialize_traffic_control/0,
  add_local_interface_ip4/1, 
  ensure_local_interface_ip4/1, 
  teardown_local_interface_ip4/1,
  teardown_traffic_control/0, 
  ping/2]).


-spec add_local_interface_ip4([byte(), ...]) -> nonempty_string() | {error, _}.
add_local_interface_ip4(Ip) ->
  case ip4_is_in_use(Ip) of
    {true, _} -> {error, ip_already_in_use};
    false -> 
      Interface = get_unused_local_adapter(),
      case os:cmd("sudo ifconfig " ++ Interface ++ " " ++ Ip ++ " netmask 255.255.255.255") of
        [] -> Interface;
        Error -> {error, Error}
      end
  end.

-spec ensure_local_interface_ip4(nonempty_string()) -> {nonempty_string(), nonempty_string()} | false.
ensure_local_interface_ip4(IpOrAdapter) ->
  case catch(ip4_is_in_use(IpOrAdapter)) of
    {true, Adapter} -> 
      Ips = proplists:get_value(Adapter, get_adapters_and_ips()),
      {Ips, Adapter};
    _ -> 
      case interface_exists(IpOrAdapter) of
        true ->
          Ips = proplists:get_value(IpOrAdapter, get_adapters_and_ips()),
          {Ips, IpOrAdapter};
        false -> false
      end
  end. 

%There is probably a better queue to use, but htb seems the most straightforward that gives me the options I want.
-spec initialize_traffic_control() -> ok | {error, _}.
initialize_traffic_control() ->
  try
    [] = os:cmd("sudo tc qdisc add dev lo handle 1: root htb"),
    [] = os:cmd("sudo tc class add dev lo parent 1: classid 1:1 htb rate 1000Mbps"),
    ok
  catch _:Reason ->
    log(<<"Unable to create root qdisc and add class. Ensure running with sudo privs, and that no root qdisc exists on lo (run damocles_lib:teardown_traffic_control().)">>),
    {error, Reason}
  end.

-spec teardown_local_interface_ip4(nonempty_string()) -> ok | {error, string()}.
teardown_local_interface_ip4(Interface) -> 
  Resp = os:cmd("sudo ifconfig " ++ Interface ++ " down"),
  %The response doesn't matter in the success case; so long as it's gone all is well.
  case interface_exists(Interface) of 
    true -> {error, Resp};
    false -> ok
  end.

-spec teardown_traffic_control() -> ok | {error, _}.
teardown_traffic_control() ->
  try
    [] = os:cmd("sudo tc qdisc del dev lo root"),
    ok
  catch _:Reason ->
    log("Failed to tear down root qdisc on interface lo; exiting, but user intervention may be required for future startup"),
    {error, Reason}
  end.

-spec ping(nonempty_string(), nonempty_string()) -> string().
ping(From, To) ->
  os:cmd("ping -w 1 -I " ++ From ++ " " ++ To).

-spec get_unused_local_adapter() -> nonempty_string().
get_unused_local_adapter() ->
  Used = [list_to_integer(Rest) || {"lo:" ++ Rest, _} <- get_adapters_and_ips(), Rest /= ""],
  Number = 
    case length(Used) of
      0 -> 0;
      _ -> lists:max(Used) + 1
    end,
  "lo:" ++ integer_to_list(Number).

-spec ip4_is_in_use(nonempty_string()) -> false | {true, nonempty_string()}.
ip4_is_in_use(Ip) ->
  Adapter = proplists:get_value(Ip, lists:flatten([ [{X, Adapter} || X <- Ips] || {Adapter, Ips} <- get_adapters_and_ips()])),
  case Adapter of
    undefined -> false;
    _ -> {true, Adapter}
  end.

-spec interface_exists(nonempty_string()) -> boolean().
interface_exists(Interface) ->
  lists:member(Interface, [Name || {Name, _}<- get_adapters_and_ips()]).

-spec get_adapters_and_ips() -> [{nonempty_string(), [nonempty_string()]}].
get_adapters_and_ips() ->
  {ok, Items} = inet:getifaddrs(),
  [{Name, get_ip4s_from_props(Props)} || {Name, Props} <- Items].

-spec get_ip4s_from_props([{_, _}]) -> [nonempty_string()].
get_ip4s_from_props(Props) -> [ip4_tuple_as_list(Ip) || {addr, Ip} <- Props, size(Ip) == 4].

ip4_tuple_as_list({A, B, C, D}) ->
  integer_to_list(A) ++ "." ++
  integer_to_list(B) ++ "." ++
  integer_to_list(C) ++ "." ++
  integer_to_list(D).


log(Data) ->
  log("~p~n", [Data]).

log(F, Data) ->
  io:fwrite(F, Data).