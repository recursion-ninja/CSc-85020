Professor Li raised a question during my presentation regarding computing the steady state distribution of a node in a senor network using only local knowledge.

I reviewed the paper, *Data Persistence in Large-Scale Sensor Networks with Decentralized Fountain Codes*, which states:

> In this paper, we choose a variant of the Metropolis algorithm [16], which is a generalization of the natural random walks for the Markov chain with a non-uniform steady-state distribution.
> ...
> Clearly, the Metropolis algorithm is distributed, since each node only needs the steady-state probabilities of its neighbors to calculate the transition matrix.

Further reviewing the Metropolitan algorithm from the cited book, a local node would compute it's steady state distribution by asking it's neighbor's for their steady state distribution.
With careful message passing throughout the network, this recursively defined steady state distribution can be computed by each node "pulling in" and memoizing all the requisite information from the network, without each node needing to know the complete network topology, instead needing only the local aggregation of the topology metadata (state distributions).
In essence, the Metropolis algorithm is implemented as a collaborative, distributed computation which globally yields the steady state distribution of the each node being known by itself and it's neighbors.

[16]: https://utexas.instructure.com/files/44880730/download?download_frd=1
