# ipqueue
Interleaved Priority Queue for Erlang/Elixir based on ordered set ETS table.

As opposed to the traditional priority queue, messages with different priorities are interleaved.
Probability of the next message is determined by its priority.

## Performance:

Push: avg. per element: 1.79 μs (Nelems = 1M)

Pop: avg. per element: 1.1 μs (Nelems = 1M)

Measured on OTP23 w/o HiPE, CPU: Intel(R) Core(TM) i7-10510U
