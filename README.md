# pqueue
High Performance Priority Queue for Erlang/Elixir based on ETS.

Note: messages with different priorities are interleaved.
Probability of the next message is determined by its priority.

## Performance:

Push: avg. per element: 2.59 μs (Nelems = 1M)
Pop: avg. per element: 1.58 μs (Nelems = 1M)

Measured on OTP23 w/o HiPE, CPU: Intel(R) Core(TM) i7-10510U
