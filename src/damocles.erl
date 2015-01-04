-module(damocles).

-export([
  start/0, 
  start_link/0, 
  add_interface/1, 
  register_interface/1,
  isolate_one_way/2, 
  isolate_interface/1,
  isolate_between_interfaces/2,
  packet_loss_one_way/3,
  packet_loss_interface/2,
  packet_loss_between_interfaces/3,
  packet_loss_global/1,
  delay_one_way/3,
  delay_interface/2, 
  delay_between_interfaces/3, 
  delay_global/1,
  restore_one_way/2,
  restore_interface/1,
  restore_all_interfaces/0,
  get_rules_for_connection/2,
  stop/0]).



start() -> gen_server:start({local, damocles_server}, damocles_server, [], []).

start_link() -> gen_server:start_link({local, damocles_server}, damocles_server, [], []).

%Creates an interface that will be torn down when Damocles is stopped.
-spec add_interface(nonempty_string()) -> nonempty_string() | error.
add_interface(Ip) -> gen_server:call(damocles_server, {add_interface, Ip}, infinity).

%Adds knowledge of an interface so that its traffic may be controlled, but will not be torn down when Damocles is stopped.
-spec register_interface(nonempty_string()) -> nonempty_string() | error.
register_interface(IpOrAdapter) -> gen_server:call(damocles_server, {register_interface, IpOrAdapter}).

%Returns a proplist of all rules that have been applied to packets between Src -> Dst.
-spec get_rules_for_connection(nonempty_string(), nonempty_string()) -> damocles_lib:tc_rules().
get_rules_for_connection(IpOrAdapterSrc, IpOrAdapterDst) -> gen_server:call(damocles_server, {get_rules_for_connection, IpOrAdapterSrc, IpOrAdapterDst}).

%Isolates src from being able to communicate with dst, but not vice versa (dst can still send packets to src).
-spec isolate_one_way(nonempty_string(), nonempty_string()) -> ok | error.
isolate_one_way(Src, Dst) -> gen_server:call(damocles_server, {isolate_one_way, Src, Dst}).

%Prevents all traffic from other interfaces Damocles knows about to and from the interface specified.
-spec isolate_interface(nonempty_string()) -> ok | error.
isolate_interface(IpOrAdapter) ->  gen_server:call(damocles_server, {isolate_interface, IpOrAdapter}).

%All nodes from setA will be unable to talk to nodes in setB (and vice versa).
-spec isolate_between_interfaces([nonempty_string()] | nonempty_string(), [nonempty_string()] | nonempty_string()) -> ok | error.
isolate_between_interfaces(IpSetA, IpSetB) -> gen_server:call(damocles_server, {isolate_between_interfaces, IpSetA, IpSetB}).

%Induces one way packet loss between src -> dst (so not dst -> src) at the rate specified. Rate is either an integer percentage
% (i.e., 50 = 50%), or a float between 0.0 and 1.0 (so 0.5 = 50%).
-spec packet_loss_one_way(nonempty_string(), nonempty_string(), number()) -> ok | error.
packet_loss_one_way(Src, Dst, Rate) -> gen_server:call(damocles_server, {packet_loss_one_way, Src, Dst, Rate}).

%Induces packet loss at the specified rate for all connections between specified interface, and all others known about.
%NOTE THAT THIS IS APPLIED TO BOTH SENDING, AND RECEIVING.
%i.e, if you apply a packet loss of 10% you have a 10% chance of losing any sent packet, and any received packet, thus a round
%trip send and ack can be expected to succeed only 81% (90% success send * 90% success ack) of the time.
-spec packet_loss_interface(nonempty_string(), number()) -> ok | error.
packet_loss_interface(IpOrAdapter, Rate) -> gen_server:call(damocles_server, {packet_loss_interface, IpOrAdapter, Rate}).

%Induces packet loss at the specified rate between all connections running between interfaces in set A, to interfaces in set B
-spec packet_loss_between_interfaces([nonempty_string()] | nonempty_string(), [nonempty_string()] | nonempty_string(), number()) -> ok | error.
packet_loss_between_interfaces(SetA, SetB, Rate) -> gen_server:call(damocles_server, {packet_loss_between_interfaces, SetA, SetB, Rate}).

%Induces packet loss across the entire network. 
%NOTE THAT THIS IS APPLIED TO BOTH SENDING, AND RECEIVING.
%i.e, if you apply a packet loss of 10% you have a 10% chance of losing any sent packet, and any received packet, thus a round
%trip send and ack can be expected to succeed only 81% (90% success send * 90% success ack) of the time.
-spec packet_loss_global(pos_integer()) -> ok | error.
packet_loss_global(Rate) -> gen_server:call(damocles_server, {packet_loss_global, Rate}).

%Applies the specified integer amount of delay, in millis, to packets between Src -> Dst
-spec delay_one_way(nonempty_string(), nonempty_string(), pos_integer()) -> ok | error.
delay_one_way(Src, Dst, Amount) -> gen_server:call(damocles_server, {delay_one_way, Src, Dst, Amount}).

%Applies the specified integer amount of delay, in millis, to all packets to and from the specified interface.
-spec delay_interface(nonempty_string(), pos_integer()) -> ok | error.
delay_interface(IpOrAdapter, Amount) -> gen_server:call(damocles_server, {delay_interface, IpOrAdapter, Amount}).

%Applies the specified integer amount of delay, in millis, to all packets sent from an interface in SetA, to an interface in SetB
-spec delay_between_interfaces([nonempty_string()] | nonempty_string(), [nonempty_string()] | nonempty_string(), pos_integer()) -> ok | error.
delay_between_interfaces(SetA, SetB, Amount) -> gen_server:call(damocles_server, {delay_between_interfaces, SetA, SetB, Amount}).

%Applies the specified integer amount of delay, in millis, to all packets sent between two interfaces Damocles knows about.
-spec delay_global(pos_integer()) -> ok | error.
delay_global(Amount) -> gen_server:call(damocles_server, {delay_global, Amount}).

%Restores the specified connection from src -> dst (but not vice versa).
-spec restore_one_way(nonempty_string(), nonempty_string()) -> ok | error.
restore_one_way(Src, Dst) -> gen_server:call(damocles_server, {restore_one_way, Src, Dst}).

%Restores all connections for the given interface
-spec restore_interface(nonempty_string()) -> ok | error.
restore_interface(IpOrAdapter) -> gen_server:call(damocles_server, {restore_interface, IpOrAdapter}).

%Restores all connections across all known interfaces.
-spec restore_all_interfaces() -> ok | error.
restore_all_interfaces() -> gen_server:call(damocles_server, restore_all_interfaces).

stop() -> gen_server:cast(damocles_server, stop).