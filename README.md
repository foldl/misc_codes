# misc codes

A collection of miscellaneous codes.

## millionaires.erl

An implementation of Yao's solution to the [Millionaires' Problem](http://en.wikipedia.org/wiki/Yao's_Millionaires'_Problem).

Usage:

1. Start two nodes.
1. On the node that has more computing power (this is named alice): millionaires:connect(node of bob).
1. Input wealth on each node by millionaires:input(). (io:get\_password() is used here)

Note:

Supported weatlth range is (1, MAX\_VALUE). Set MAX\_VALUE up to 50000, it could be solved within half an hour.

Acknowledge: Prime generation code (c) Joe Armstrong with some modifications.
