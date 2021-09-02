# pqueue
High Performance Priority Queue for Erlang/Elixir based on ETS.

Note: messages with different priorities are interleaved.
Probability of the next message is determined by its priority.
