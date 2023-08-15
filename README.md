# Cache Coherence Protocol Using Murphi

In this project, I designed two different cache coherence protocols. The first one is the base line MSI protocol, and the other one is the optimization MESI protocol.

For the base line MSI protocol, it has three states, modified (M), shared (S) and invalid (I). The modified state indicates that a processor has modified the data in its cache and that the data is not consistent with the data in the main memory. The shared state indicates that a cache line is present in multiple caches and is read-only or unmodified. Multiple processors can have read-only copies of the data in their caches, and they can all access it without performing any updates. The invalid state indicates that a cache line is not valid or is empty. The MSI protocol maintains cache coherence by allowing only one processor at a time to have the Modified state, while allowing multiple processors to have the Shared state.

For the optimization, I choose to add the exclusive (E) state to implement the MESI protocol. The exclusive state indicates that a cache line is present only in one cache, and the data is unmodified. It's similar to the Shared state in MSI.
The MESI protocol provides some optimizations over the MSI protocol by introducing the Exclusive state. When a processor wants to read a cache line that is in the Exclusive state, it can read the data directly from its cache without performing any snoop operations. This can reduce the overhead of cache-to-cache transfers, resulting in improved performance compared to MSI.
