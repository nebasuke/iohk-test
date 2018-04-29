# Cloud Haskell - Node broadcasting
This package implements the exercise of [IOHK](https://iohk.io/careers/tests/haskell-developer-test).

## Assumptions and design choices
- The chosen protocol is TCP. According to the assignment specification every message should reach every node. I therefore assume that dropping packages (unless nodes die) is not allowed.
    + I decided to not try to reconnect, since per [Cloud Haskell semantics](https://haskell-distributed.github.io/static/semantics.pdf) this could mean that some of the messages that were sent never get received (despite later messages getting received successfully). This would violate the assignmen constraint of every message reaching every node. 
    + Although possible, the program does not try to recover a node temporarily disconnecting (for example by storing old messages and on reconnect resending previous messages). 
- Ordering of received messages is kept by using time stamps as part of the message. The timestamp is created at the time of sending. 
- I use the first node of the node list to be a dedicated node for sending stop messages, to ensure the stop messages arrive. The first node should therefore be the most stable node in the network.

## Command-line

```
IOHK Cloud Haskell test task - Bas van Gijzel (2018)

Usage: iohk-test-exe.EXE --send-for INT --wait-for INT [--with-seed INT]
                         [--node-list PATH]
  Launch Cloud Haskell with given nodes + exec times, and start messaging

Available options:
  -h,--help                Show this help text
  --send-for INT           Sending period in seconds
  --wait-for INT           Grace period in seconds
  --with-seed INT          Initial seed for RNG in messages
  --node-list PATH         File path for node list (default: "nodelist.txt")
```

## Defining nodes
The file *nodelist.txt* contains an example of valid hostnames/ip addresses with ports and will be loaded at the start of the program, unless a different file is specified using the --node-list PATH option. 
Extra whitespace and line endings are allowed within the node list file. 