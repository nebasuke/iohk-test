# iohk-test

## Assumptions
- The chosen protocol is TCP. According to the specification every message should reach every node, so it is assumed that dropping packages (unless nodes die) is not allowed.
  + We do not reconnect, per Cloud Haskell semantics, since this would mean that we could miss some packages in the middle violating the constraint of every message reaching every node.
- Ordering of received messages is kept by using time stamps as part of the message. The timestamp is created at the time of sending. 
 