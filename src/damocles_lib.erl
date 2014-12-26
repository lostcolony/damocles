-module(damocles_lib).

-export([add_local_interface_ip4/1, teardown_local_interface_ip4/1]).
-compile(export_all).

-spec add_local_interface_ip4([byte(), ...]) -> nonempty_string() | {error, timeout | integer()}.
add_local_interface_ip4(Ip) ->
  case ip4_is_in_use(Ip) of
    {true, _} -> {error, ip_already_in_use};
    false -> 
      Interface = get_unused_local_adapter(),
      Port = open_port({spawn, "sudo ifconfig " ++ Interface ++ " " ++ Ip ++ " netmask 255.255.255.0"}, []),
      case read_port(Port) of
        ok -> Interface;
        {error, timeout} ->
          case interface_exists(Interface) of
            true -> Interface;
            false -> {error, timeout}
          end;
        {error, Code} -> {error, Code}
      end
  end.

-spec ensure_local_interface_ip4(nonempty_string()) -> {nonempty_string(), nonempty_string()} | false.
ensure_local_interface_ip4(IpOrAdapter) ->
  case catch(ip4_is_in_use(IpOrAdapter)) of
    {true, Adapter} -> 
      Ips = proplists:get_value(Adapter, get_adapters_and_ips()),
      log(Ips),
      {Ips, Adapter};
    _ -> 
      case interface_exists(IpOrAdapter) of
        true ->
          Ips = proplists:get_value(IpOrAdapter, get_adapters_and_ips()), 
          log(Ips),
          {Ips, IpOrAdapter};
        false -> false
      end
  end. 


-spec teardown_local_interface_ip4(string()) -> ok | {error, timeout | integer()}.
teardown_local_interface_ip4(Interface) -> 
  Port = open_port({spawn, "sudo ifconfig " ++ Interface ++ " down"}, []),
  case read_port(Port) of
    ok -> ok;
    {error, timeout} ->
      case interface_exists(Interface) of
        true -> {error, timeout};
        false -> ok
      end;
    {error, Code} -> {error, Code}
  end.

-spec read_port(Port::port()) -> ok | {error, timeout | integer()}.
read_port(Port) ->
  receive
    {Port, {exit_status, 0}} -> ok;
    {Port, {exit_status, Status}} -> {error, Status};
    {Port, {data, Data}} -> log(Data), read_port(Port);
    _ -> read_port(Port)
  after 1000 -> {error, timeout}
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