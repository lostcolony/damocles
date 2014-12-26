-module(damocles).

-export([start/0, start_link/0, add_interface/1, ensure_interface/1, stop/0]).



start() -> gen_server:start({local, damocles_server}, damocles_server, [], []).

start_link() -> gen_server:start_link({local, damocles_server}, damocles_server, [], []).

add_interface(Ip) -> gen_server:call(damocles_server, {add_interface, Ip}).

ensure_interface(IpOrAdapter) -> gen_server:call(damocles_server, {ensure_interface, IpOrAdapter}).

stop() -> gen_server:cast(damocles_server, stop).