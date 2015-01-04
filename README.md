# Damocles

Damocles is an Erlang library intended to make writing and running distributed application tests easier. In this first release, it does this by creating local interfaces on a single machine and controlling the flow of packets between those packets, allowing it to run on any Linux (currently) machine without affecting other apps/traffic/etc. It also allows for distributed tests to be run easily in a continuous integration environment, without the need to spin up or allocate separate VMs for each application instance.

## Requirements
Damocles requires:
  - Running on a Linux (developed on Mint 17) that:
    - uses 'ifconfig' to add/remove interfaces, and 'lo' is the local interface
    - has tc and netem.
    - has make
    - has sudo permissions for running the above for whatever user you run Damocles as.
    
  - Erlang installed and on your path (tested on R17; but no use of R17 features means it can likely run on earlier versions). 
  
## Installation
If using Damocles from an Erlang application, you can just add it to your test dependencies.

For those wishing to treat Damocles as a command line utility,  you'll need to build the code and run it as a release.

Get the code
```sh
git clone https://github.com/lostcolony/damocles [location]
```

Build the code
```sh
cd [location]
make
```

## Usage

##### Starting Damocles
From Erlang, you can start Damocles as an application, or call damocles:start() or damocles:start_link() as appropriate.

For those wishing to use Damocles as a command line app, execute 
```
[location]/batch/start.sh
```
Note that this executes ifconfig and tc commands with sudo; you may need to run it with sudo if it fails.

##### Using Damocles 
From Erlang, all commands exist in damocles.erl. A listing is below with examples

From the command line, you can execute any function using batch/damocles_external, where the first argument is the function from damocles.erl you want to execute, and successive parameters are the arguments you wish to pass in. Lists can be expressed as comma separated strings (see examples).
```
[location]/batch/damocles_external add_interface "10.10.10.10"
```

###### MISC: Things to keep in mind
- Rules may only be applied between IPs that have been added/registered with Damocles.
- Also, both drop and delay rules may be applied separately and will persist until you have restored the node connection.
- However, setting a new drop value to a connection will overwrite an existing drop value; same with delay overwriting an existing value.
- All functions do one of three things. Return ok, return 'error' (which depending on the function called may mean nothing occurred, or, if it was a function that affected multiple connections, it means all the connections you referenced in the call have been reset), or throw. If an exception is thrown from within Damocles (as opposed to the RPC interface), and the process has restarted (if started as a command line application the supervisor is used), all interfaces and such we knew about have been torn down so that we're in a 'known' state; you will need to recreate/reregister them.
- Things can go wrong! 
  - First, since this requires sudo, you may have to get permissions set up properly.
  - Running make while Damocles is running can lead to weirdness in the release. Easiest fix is to rm -rf the _rel folder, and then kill the app process (ps aux | grep beam). Then make should work.
  - Since there is implicit OS state, and I'm not trying to wipe out anything at startup, only a clean shutdown, it may be you have portions of code you need to clean up. damocles_lib:teardown* functions are callable for the Erlang users; the command line users can run sudo erl -pa ebin from the Damocles folder to start up the Erlang shell, and from there run the teardown commands. It should not be necessary however; removal of prior traffic control constructs occurs on application startup (but not otherwise; restarting through the supervisor will not trigger it, as it should be cleaned up as part of the terminate).

##### Stopping
From Erlang, stop the application if it was started that way, or call damocles:stop().

From the command line, execute the stop script
```
[location]/batch/stop.sh
```

## Examples
##### Creating new local adapters
Use an IP from a reserved range for internal network IPs. These adapters will be torn down when Damocles is stopped.

From Erlang:
```
damocles:add_interface("10.10.10.10").
damocles:add_interface("10.10.10.11").
damocles:add_interface("10.10.10.12").
damocles:add_interface("10.10.10.13").
damocles:add_interface("10.10.10.14").
```

From the command line:
```
[location]/batch/damocles_external add_interface "10.10.10.10"
[location]/batch/damocles_external add_interface "10.10.10.11"
[location]/batch/damocles_external add_interface "10.10.10.12"
[location]/batch/damocles_external add_interface "10.10.10.13"
[location]/batch/damocles_external add_interface "10.10.10.14"
```

##### Registering an existing local adapter with IP 10.10.10.10
Use an IP from an existing local adapter. These adapters will not be torn down when Damocles is stopped, but will have any rules you have applied to them torn down.

From Erlang:
```
damocles:add_interface("10.10.10.15").
```

From the command line:
```
[location]/batch/damocles_external add_interface "10.10.10.15"
```


##### Prevent traffic from a source IP (10.10.10.10) to a destination IP (10.10.10.11)
Prevent all traffic flowing out from source to destination, but no traffic flowing the other direction.

From Erlang:
```
damocles:isolate_one_way("10.10.10.10", "10.10.10.11").
```

From the command line:
```
[location]/batch/damocles_external isolate_one_way 10.10.10.10 10.10.10.11
```

##### Prevent all traffic to and from an interface 
Will prevent all traffic to and from the specified interface from those other interfaces Damocles knows about (and no others; i.e., it will still be reachable from 127.0.0.1)

From Erlang:
```
damocles:isolate_interface("10.10.10.10").
```

From the command line:
```
[location]/batch/damocles_external isolate_interface 10.10.10.10
```

##### Create node partitions
Used to isolate two sets of nodes from each other. Note that any nodes not included in either set retain any pre-existing rules (or lack thereof). That is, if you have nodes running on (prefix).10, .11, .12, .13, and .14, and call this with [.10, .11], and [.13, .14], as per the example below, .10 and .11 can still talk, but neither can reach .13 or .14. Similarly, .13 and .14 can talk, but neither can reach .10 or .11. And .12 can still talk to everyone.

From Erlang:
```
damocles:isolate_between_interfaces(["10.10.10.10", "10.10.10.11"], ["10.10.10.13", "10.10.10.14"])
```

From the command line:
```
[location]/batch/damocles_external isolate_between_interfaces "10.10.10.10,10.10.10.11" "10.10.10.13,10.10.10.14"
```

##### Induce packet loss between a src IP and dst IP
Similar to preventing traffic between the two (and overwrites it), this causes only a percentage of packets to be dropped between the src IP and dst IP, but not from the dst IP to the src IP. The third argument is
the percent chance of a packet being dropped; this can either be a whole integer percentage (20 = 20%), or a
float value between 0.0 and 1.0 (0.2 = 20%).

From Erlang:
```
damocles:packet_loss_one_way("10.10.10.10", "10.10.10.11", 0.05).
```

From the command line:
```
[location]/batch/damocles_external packet_loss_one_way 10.10.10.10 10.10.10.11 .05
```

##### Induce packet loss for all traffic flowing into or out of an interface
Causes a percentage of packets to be dropped for all traffic flowing in or out of this interface. Note that this applies both in and out, so a 10% chance to drop means that a send and acknowledgement will have a 10% chance to fail on the send, -and- a 10% chance to fail on the acknowledgement.

From Erlang:
```
damocles:packet_loss_interface("10.10.10.10", 0.05).
```

From the command line:
```
[location]/batch/damocles_external packet_loss_interface 10.10.10.10 .05
```

##### Induce packet loss for all traffic between sets of interfaces
Similar to creating node partitions, this causes a percentage of packets to be dropped for all traffic flowing between a node in the first set, to a node in the second set. Note that this applies both in and out, so a 10% chance to drop means that a send and acknowledgement will have a 10% chance to fail on the send, -and- a 10% chance to fail on the acknowledgement.

From Erlang:
```
damocles:packet_loss_between_interfaces(["10.10.10.10", "10.10.10.11], ["10.10.10.13", "10.10.10.14"], 0.05).
```

From the command line:
```
[location]/batch/damocles_external packet_loss_between_interfaces "10.10.10.10,10.10.10.11" "10.10.10.13,10.10.10.14" .05
```

##### Induce packet loss for all communication between known nodes
This causes a percentage of packets to be dropped for all traffic flowing between two nodes that Damocles knows about. Note that this applies both in and out, so a 10% chance to drop means that a send and acknowledgement will have a 10% chance to fail on the send, -and- a 10% chance to fail on the acknowledgement.

From Erlang:
```
damocles:packet_loss_global(0.05).
```

From the command line:
```
[location]/batch/damocles_external packet_loss_global .05
```

##### Induce packet delay between a src IP and dst IP
Similar to preventing traffic between the two (and overwrites it), this causes a fixed delay to be imposed on packets between the src IP and the dst IP, and not the reverse. The delay is an integer in milliseconds.

From Erlang:
```
damocles:delay_one_way("10.10.10.10", "10.10.10.11", 100).
```

From the command line:
```
[location]/batch/damocles_external delay_one_way 10.10.10.10 10.10.10.11 100
```

##### Induce packet delay for all traffic flowing into or out of an interface
Causes all packets to and from the specified IP to be delayed by the specified amount. Note that this applies both in and out, so a 100ms delay will affect both a sent packet, and an acknowledgement, so that things like pings will take 200ms.

From Erlang:
```
damocles:delay_interface("10.10.10.10", 100).
```

From the command line:
```
[location]/batch/damocles_external delay_interface 10.10.10.10 100
```

##### Induce packet loss for all traffic between sets of interfaces
Similar to creating node partitions, this causes a delay for all traffic flowing between a node in the first set, to a node in the second set. Note that this applies both in and out, so a 100ms delay means that a send and acknowledgement will have a 100ms delay on the send, -and- a 100ms delay on the acknowledgement, for a total ping time of 200ms.

From Erlang:
```
damocles:delay_between_interfaces(["10.10.10.10", "10.10.10.11], ["10.10.10.13", "10.10.10.14"], 100).
```

From the command line:
```
[location]/batch/damocles_external delay_between_interfaces "10.10.10.10,10.10.10.11" "10.10.10.13,10.10.10.14" 100
```

##### Induce packet delays for all packets sent between known interfaces
This causes a delay on all packets flowing between two nodes that Damocles knows about. Note that this applies both in and out, so a 100 ms delay means that a send and acknowledgement will have a 100ms dekay on the send, -and- a 100ms delay on the acknowledgement.

From Erlang:
```
damocles:delay_global(100).
```

From the command line:
```
[location]/batch/damocles_external delay_global 100
```

##### Restore a connection between a src IP and dst IP
Will undo any delay/drop you've imposed between the two IPs (one way).

From Erlang:
```
damocles:restore_one_way("10.10.10.10", "10.10.10.11").
```

From the command line:
```
[location]/batch/damocles_external restore_one_way 10.10.10.10 10.10.10.11
```


##### Restore all connections to and from an interface
Will undo any delay/drop you've imposed on the traffic flowing into or out of an interface.

From Erlang:
```
damocles:restore_interface("10.10.10.10").
```

From the command line:
```
[location]/batch/damocles_external restore_interface 10.10.10.10
```


##### Restore the entire network
Will undo any delay/drop you've imposed on the traffic flowing between interfaces that Damocles knows about.

From Erlang:
```
damocles:restore_all_interfaces().
```

From the command line:
```
[location]/batch/damocles_external restore_all_interfaces
```

##### See what IPs Damocles currently is aware of
Returns a list of all IPs Damocles is aware of and can configure.

From Erlang:
```
damocles:get_known_ips().
```

From the command line:
```
[location]/batch/damocles_external get_known_ips
```


##### See what rules Damocles is applying to a given connection
Returns a proplist of the rules Damocles is applying between a src and dst IP. Note that it only
tells you what packets going from src -> dst have applied; you need to query separately to get
dst -> src (by calling it with the arguments in the reverse order).

From Erlang:
```
damocles:get_rules_for_connection("10.10.10.10", "10.10.10.11").
```

From the command line:
```
[location]/batch/damocles_external get_rules_for_connection 10.10.10.10 10.10.10.11
```

## License
----

MIT
