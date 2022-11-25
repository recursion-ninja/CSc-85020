## Synthesis of Population Counting algorithms


Defines a routine for synthesizing an algorithm parameterized by $d$, which computes the number of set bits in a bit-vector of fixed length $d$.
This opperation is often refer to as "population count" or "PopCount."
Resulting synthesized algorithm(s) utilize "bit-twiddling" techniques.


## Desiderata

Does there exist an algorithm $\mathcal{A}: (d \in \mathbb{N}) \to \left(\lambda: \left\{\,1,\;0\,\right\}^{d} \to \mathbb{N}\right)$ which given the bit-vector dimension $d$, generates the bit-twiddling function (including masks) $\mathcal{A}(d)$ such that the following hold?

$$ \forall \vec{v} \in \left\{\,1,\;0\,\right\}^{\ast}\quad \left\lvert \vec{v} \right\rvert \le 2^{d} \implies \mathcal{A}(d)(\vec{v}) = \text{number of set bits} $$


### Installation

```bash
$ cabal build
```
