-module(damocles_lib).

-export([
  initialize_traffic_control/0,
  add_local_interface_ip4/1, 
  ensure_local_interface_ip4/1, 
  teardown_local_interface_ip4/1,
  teardown_traffic_control/0, 
  add_class_filter_for_ips/3, 
  delete_class_filter/1,
  build_packet_rules/1,
  set_packet_rules/2,
  delete_packet_rules/1,
  ping/2,  
  log/1,
  log/2]).

-type tc_rules() :: [{drop, integer() | float()} | {delay, integer()}].
-export_type([tc_rules/0]).


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
    [] = os:cmd("sudo tc filter add dev lo parent 1: protocol ip pref 1 u32"),
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

-spec add_class_filter_for_ips(nonempty_string(), nonempty_string(), integer()) -> ok | error.
add_class_filter_for_ips(Src, Dst, Handle) -> 
  try 
    Ex1 = "sudo tc class add dev lo parent 1:1 classid 1:" ++ integer_to_list(Handle) ++ " htb rate 10Mbps ",
    [] = os:cmd(Ex1),
    Ex2 = "sudo tc filter add dev lo parent 1: handle ::" ++ integer_to_list(Handle) ++ " protocol ip prior 1 u32 match ip src " ++ Src ++ 
      " match ip dst " ++ Dst ++ " flowid 1:" ++ integer_to_list(Handle),
    [] = os:cmd(Ex2),
    ok
  catch _:_ ->
    log("Failed to create class and filter for ~p to ~p on handle ~p", [Src, Dst, Handle]),
    error
  end.

-spec delete_class_filter(integer()) -> ok | {error, nonempty_string()}.
delete_class_filter(Handle) -> 
  RespOdd = os:cmd("sudo tc filter del dev lo parent 1: handle 800::" ++ integer_to_list(Handle) ++ " prior 1 protocol ip u32"),
  Resp = os:cmd("sudo tc class del dev lo parent 1:1 classid 1:" ++ integer_to_list(Handle) ++ " htb rate 100Mbps "),
  case Resp of
    [] -> ok;
    Resp -> log(RespOdd), log(Resp), {error, Resp}
  end.

-spec build_packet_rules(tc_rules()) -> string().
build_packet_rules(List) ->
  "netem " ++
  lists:flatten(lists:map(
    fun
      ({drop, Percentage}) when is_integer(Percentage) ->
        " drop " ++ integer_to_list(Percentage) ++ "% ";
      ({drop, Percentage}) when is_float(Percentage) ->
        io_lib:format(" drop ~.2f% ", [Percentage*100]);
      ({delay, MS}) -> 
        " delay " ++ integer_to_list(MS) ++ "ms "
    end, List)).

-spec set_packet_rules(integer(), string() | tc_rules()) -> ok | {error, any()}.
set_packet_rules(Handle, [H | _] = Rules) when is_tuple(H) -> set_packet_rules(Handle, build_packet_rules(Rules));
set_packet_rules(Handle, Rules) ->
  BaseCommand = "sudo tc qdisc ~s dev lo parent 1:" ++ integer_to_list(Handle) ++ " handle " ++ integer_to_list(Handle) ++ ": " ++ Rules,
  try
    case os:cmd(io_lib:format(BaseCommand, ["add"])) of
      [] -> ok;
      Error -> 
        case os:cmd(io_lib:format(BaseCommand, ["change"])) of
          [] -> ok;
          "RTNETLINK answers: No such file or directory" -> {error, Error};
          Error2 -> {error, Error2} %
        end
    end
  catch _:Reason ->
    log("Failed to add packet rules for ~p~n", [Handle]),
    {error, Reason}
  end. 

delete_packet_rules(Handle) ->
  case catch(os:cmd("sudo tc qdisc del dev lo parent 1:" ++ integer_to_list(Handle) ++ " handle " ++ integer_to_list(Handle))) of
    [] -> ok;
    "RTNETLINK answers: Invalid argument" -> ok; %Already gone/never existed.
    Error -> {error, Error}
  end.


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