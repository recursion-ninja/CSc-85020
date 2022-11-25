---
title:  |
        Coding Theory and Program Control Flow Elision

author: Alex Washburn

date:   Oct 26^th^ 2022

bibliography: ./assets/references.bib
biblatexoptions:
  - backend=biber
  - style=authoryear-ibid
cls: ./assets/chicago-fullnote-bibliography-short-title-subsequent.csl
header-includes: |
    \usepackage[labelformat=empty]{caption}
    \newcommand{\CodeSchema}[4]{\ensuremath{\boldsymbol{\bigl(\bigr.}\,#1,~#2,~#3\,\boldsymbol{\bigl.\bigr)}_{#4}}}
---


# Introduction {.s}

Coding theory is the study of the properties of codes and their respective fitness for specific applications.
Within the CSc 85020 course, coding theory has mainly been explored within the sub-domain of erasure and error detection and correction.
However, coding theory has broad applications in the study of data compression.
Furthermore, coding theory is integral to both theoretical and practical constructions within cryptography.
While these three sub-domains capture much of coding theory, there are additional applications as well.
My project proposal involes exploring one such niche application; "Bit-Twiddling."


# Motivation from Analogy

In the case of a linear code with a coding scheme $\CodeSchema{n}{k}{d}{q}$, the generator matrix $G$ "projects" the $k-\text{dimensional}$ message-space into the *larger* $n-\text{dimensional}$ code-space and the parity check matrix $H$ "collapses" the $n-\text{dimensional}$ space back into the $k-\text{dimensional}$.
These transformations preserve messages space inputs, i.e. $\forall \vec{m} \in \mathcal{M}\; \vec{m}*H*G = \vec{m}$.
We know that, by the Theorem of Lagrange [@TheoremOfLagrange], for each $\vec{m} \in \mathcal{M}$ there are $\lvert\mathcal{C}\rvert - \lvert\mathcal{M}\rvert = n - k$ elements in the codespace $\mathcal{C}$.
Linear codes cleverly use the $(n - k)$ increase in algebreic space to "absorb" errors.
Each error which occurs on the codeword "perterbs" the corresponding message.
So long as few enough errors occur, the perterbations will not "shift" the original message $\vec{m} \in \mathcal{M}$ to an "adjacent" message $\vec{m}^{'} \in \mathcal{M}$.
This technique of using a coding scheme to project a smaller message space $\mathcal{M}$ into a larger code-space $\mathcal{C}$, permit some operation on codewords in  $\mathcal{C}$, then collapse $\mathcal{C}$ back to the smaller space $\mathcal{M}$ can be used for more than just erasure/error detection/correction.
In the example of the error correcting linear codes, the operations permitted on code words were a limited number of errors.
However, we can conceive of other operations which would be of interest.


# Coding for Bit Counting

There is an important operation in many low level software systems which is counting the number of bits set in a bit-vector of a known length.
This operation is so important that, all modern non-RISC architectures support a microcode primitive to compute the set bits in a machine word.
This operation is often called "Pop-Count," an abbreviation of set bit population count.
Unfortunately, a programmer in a higher level language cannot always rely on the existence of such an operation and may require their own Pop-Count implementation.
An obvious high-level language implementation might look similar to the following example:

```C
inline Pop_Count_0 ( bit_vector )
{
    unsigned set_bits = 0;
    for ( n : 0 .. length ( bit_vector ) - 1 )
    {
        if ( bit_vector[n] == 1 )
            set_bits = set_bits + 1
    }
    return set_bits
}
```

However, if the programmer *a prior* knows the maximum length of the bit-vector, a "bit-twiddling" technique can be used instead.
For example, $\forall \vec{v} \in \left\{\,0,\;1\,\right\}^{\ast} \text{when} \left\lvert \vec{v} \right\rvert \le 8$ the following operations compute the Pop-Count:

```C
inline Pop_Count_1 ( bit_vector )
{
    out = bit_vector
    out = (out     ) - ((out >> 1) & 85)
    out = (out & 51) + ((out >> 2) & 51)
    out = (out &  7) + ( out >> 4      )
    return out
}
```

Here is a further example for longer bit-vectors.
Consider that $\forall \vec{v} \in \left\{\,0,\;1\,\right\}^{\ast} \text{when} \left\lvert \vec{v} \right\rvert \le 16$ the following operations compute the Pop-Count:

```C
inline Pop_Count_2 ( bit_vector )
{
    out = bit_vector
    out = ( out         ) - ( ( out >> 1 ) & 21845 )
    out = ( out & 13107 ) + ( ( out >> 2 ) & 13107 )
    out = ( out &  3855 ) + ( ( out >> 4 ) &  3855 )
    out = ( out &    15 ) + ( ( out >> 8 )         )
    return out
}
```

These two examples are acomponied by two more examples for $\left\lvert \vec{v} \right\rvert \le 32$ and $\left\lvert \vec{v} \right\rvert \le 64$ on the Stanford Computer Science Graphics page maintained by Stanford Professor Sean Eron Anderson and Professor Randal Bryant, the Dean of Computer Science at Carnegie Mellon University [@Stanford-Bit-Twiddling-Hacks].

Each example first takes the input bit-vector $\vec{v}$ from a message space with dimension $2^{d}$ and projects it to a code-space with the same dimension $2^{d}$.
Then it performs $i \in \left\{ d, d - 1, \hdots, d - log_{2}^{d} \right\}$ collapasing transformations, splitting the code-space of dimension $2^{i}$ into two dimensions of $2^{i-1}$ with all bits from the corresponding half of the $2^{i}$ space retained in the $2^{i-1}$ space.
The two $2^{i-1}$ spaces are then collaped together into a single $2^{i-1}$ space and added together.
This process occurs a logrithmic number of times with respect to the bit-vector length.
When the process halts, the final $d - log_{2}^{d}$ will hold the number of set bits in the input bit-vector $\vec{v}$.
Noatably, all implementations applying coding theoery have no branching, rather a fixed number of arithmetic and bitwise operations.
We will return to this observation later.

Let us conclude the exploration of this examplewith some motivating questions for future work:

- Does this technique generalize for all $d$?

- Given $d$ how to compute mask values?


# Coding for Field Multiplication

A second example of applying coding theory for bit-twiddling was tacitly presented in the analysis of *Nuclei* for Galios Field derived network coding [@shojania2009nuclei].
Unfortunately for the curious reader, the paper focused entirely on the application of the network code and not on the bit-twiddling.
The bit-twiddling coding occurred in the loop `loop_gf_multiply_byte`, where the authors presented the original code with branching inside the `while` loop and an second version removing said branching.
The illustrating code snippet figures from [@shojania2009nuclei] are included below.

![Original Nuclei Loop from [@shojania2009nuclei].](./assets/Nuclei-Example-Original.png){ height=750px }

![Modified Nuclei Loop from [@shojania2009nuclei].](./assets/Nuclei-Example-Coded.png){ height=750px }

Here, in the second example, the message-space $\mathcal{M}$ has dimension $24$.
The code-space $\mathcal{C}$ has dimension $32$.
The message $\vec{m}$ is projected to the corresponding codeword.
A 24-bit multiplication operation is performed on the codeword.
Subsequently, a series of bit-wise operations are performed on the codeword.
The codeword is collapsed back to the 24-bit message $\vec{m}^{'}$.
The output 8-bit word $b_{1}^{'} = \left\{ \vec{m}^{'}_{1}, \vec{m}^{'}_{2}, \hdots, \vec{m}^{'}_{8} \right\}$ contains the result of on the input 8-bit word $b_{1} =  \left\{ \vec{m}_{1}, \vec{m}_{2}, \hdots, \vec{m}_{8} \right\}$ multiplied by the specified factor within the Galois Field.
Similarly output 8-bit words $b_{2}^{'} = \left\{ \vec{m}^{'}_{9}, \vec{m}^{'}_{10}, \hdots, \vec{m}^{'}_{16} \right\}$ and 8-bit word $b_{3}^{'} = \left\{ \vec{m}^{'}_{17}, \vec{m}^{'}_{18}, \hdots, \vec{m}^{'}_{24} \right\}$ contain the result of on the input 8-bit words $b_{2} = \left\{ \vec{m}_{9}, \vec{m}_{10}, \hdots, \vec{m}_{16} \right\}$ and $b_{3} = \left\{ \vec{m}_{17}, \vec{m}_{18}, \hdots, \vec{m}_{24} \right\}$ multiplied by the specified factor within the Galois Field.
Note that the first version of `loop_gf_multiply_byte` multiplies an 8-bit factor and a single 8-bit word.
However, this coding theory application of bit-twiddling allows the second version of `loop_gf_multiply_byte` to multiply an 8-bit factor and *three* 8-bit word packed together within a 32-bit word.
Both versions of `loop_gf_multiply_byte` will have the `while` perform the same number of iterations, as the break condition is dependent only on the specified factor, and the factor is decremented identically in both versions.
This means that coding theory permits, theoretically, a three-fold increase in throughput, assuming the overhead of the 24-bit multiplication in the second version is comparable to the corresponding operations within the first version.
Furthermore, the bit-twiddling within the second version has removed the branching within the `while` loop body.

# Eliding Branching

Interestingly, we see the removal of branching in both examples of bit-twiddling.
While my selection of bit-twiddling examples is intentional, I assure you this is not selection bias masquerading as a general technique.
Rather, the ability to remove branching in performance critical code is a reoccurring desire.
This desire can be pursued by programmers at the level of a programming language or by a compiler transforming an semantic specification into assembler, micro-code, or an intermediate representation output.
The absence of branching, from conditional statements or looping constructs is imporant in many practical terms.
Not only does this very often result in a faster runtime than explicit looping in terms of primitive operation count, but it also provides other unique opportunities for optimization.
One such form of optimization is constructing either a Single Instruction Multiple Data (SIMD) or a Multiple Instruction Single Data (MISD) pipeline on supporting hardware.
The former is general supported on both RISC and non-RISC CPUs, where the latter is supported on GPUs.
Another such optimization is removing branch prediction, and branch prediction misses within CPUs.
Another still is exposing potential operational or data parallelism that can be executed simultaneously on both CPUs and GPUs.
This this litany of benefits, there remains little question why all potential avenues of branch elision are pursued.
But-twiddling is one such avenue.


# Project Goals

The goals I would like to pursue for my CSc 85020 course project fall into two categories, *immediate* and *ambitious*.
The former are, in my option, more readily obtainable while the latter build upon their successful completion and require more mathematical and engineering development.

## Immediate goals

Within the time-frame of this semester, I propose completing the following *immediate goals:*

  1.  Given the Pop-Count problem for bit-vectors of length $2^d$, does there always exist an $\mathcal{O}(\,d\,)$ bit-twiddling function?
      I suspect yes.
      This intuition stems from having example implementations for $d \in \{\; 3,\, 4,\, 5,\, 6 \;\}$; $d = 3$ and $d = 4$ provided above, $d = 5$ and $d = 6$ provided in [@Stanford-Bit-Twiddling-Hacks].
      Each implementation has a "similar" structure, with the appearance of an implementation for $d+1$ being an extension of $d$.

  2.  Assuming the truth of (1), does there exist an algorithm  
      $\mathcal{A}: (d \in \mathbb{N}) \to \left(\lambda: \left\{\,1,\;0\,\right\}^{d} \to \mathbb{N}\right)$ which generates the bit-twiddling function (including masks) `Pop_Count` such that the following holds?

      $$ \forall \vec{v} \in \left\{\,1,\;0\,\right\}^{\ast}\quad \left\lvert \vec{v} \right\rvert \le 2^{d} \implies \mathcal{A}(d)(\vec{v}) = \text{number of set bits} $$

      Within the scope of this semester, I propose providing a realized implementation of $\mathcal{A}$ if it exists.

## Ambitious Goals

The following *ambitious goals* may be out of scope for the project's time table, but I will make an attempt to complete them if possible.
If not, these serve as lines of inquiry for future work:

  3.  What are the runtimes of $\mathcal{A}$ and any generated $\mathcal{A}(d)$?

  4.  Are the coding theory techniques uncovered in this inquiry generalizable past Pop-Count? Usable to construct other codes?


# Prior Work

Review of contemporary and historical literature, while not exceptionally thorough, has not yielded yet any specific publications on coding theory and either the domain of bit-twiddling or the application of control-.
In fact study of bit-twiddling does not appear as the topic of any publication, at least not by that keyword, save the aforementioned [@Stanford-Bit-Twiddling-Hacks] and [@AutomaticAbstractionForCongruences].
Some work has been published specifying precicely the definition and semantics of bit-vector operations upon which bit-twiddling techniques are build, notably [@DecisionBitVectorsFixedSizeTheory], [@DecisionBitVectorArithmetic], and [@EquivalenceBitVectorsAlgebra].
However, these works tend to focus on verification of properties for hardware specifications rather than software algorithms with elidible control flow constructs.
I myself have utilized documented bit-twiddling techniques superficially to remove loops in model specifications which reduced the state-space for model verification with exceptional efficacy.
The combination of this sparse yet seminal prior work will be pivitol in directing my project applying coding theory to the application of control-flow elision via bit-twiddling techniques.


---


# References