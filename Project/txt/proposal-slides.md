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
---

# Introduction {.s}

## What is coding theory?

The study of the properties of codes and their respective fitness for specific applications.

## Applications

- Erasure/Error Detection/Correction
- Data Compression
- Cryptographic coding
- ***Other...***


## Linear Codes (reminder)

![Slide from Lecture 2](./assets/Lecture-2-Example.png){ height=750px }


## Linear Codes (other)

Apply same algebreic coding techniques in another domain?


## "Bit Twiddling" (Example 1)

Consider the problem of counting set bits within a bit-vector.

```C
// Count set bits
inline Count_Bits ( bit_vector )
{
    unsigned set_bits = 0;
    for ( n : 0 .. len ( bit_vector ) - 1 )
    {
        if ( bit_vector[n] == 1 )
            set_bits = set_bits + 1
    }
    return set_bits
}
```

## "Bit Twiddling" (Example 1)

This works[^1] when $\left\lvert \mathtt{bit\_vector} \right\rvert \le 8$.

```C
// Count set bits
inline Count_Bits ( bit_vector )
{
    out = bit_vector
    out = (out     ) - ((out >> 1) & 85)
    out = (out & 51) + ((out >> 2) & 51)
    out = (out &  7) + ( out >> 4      )
    return out
}
```

## "Bit Twiddling" (Example 1)

This works[^1] when $\left\lvert \mathtt{bit\_vector} \right\rvert \le 16$.

```C
// Count set bits
inline Count_Bits ( bit_vector )
{
    out = bit_vector
    out = ( out         ) - ( ( out >> 1 ) & 21845 )
    out = ( out & 13107 ) + ( ( out >> 2 ) & 13107 )
    out = ( out &  3855 ) + ( ( out >> 4 ) &  3855 )
    out = ( out &    15 ) + ( ( out >> 8 )         )
    return out
}
```

[^1]: [@Stanford-Bit-Twiddling-Hacks]


## "Bit Twiddling" (Example 1)

Considering $\left\lvert \mathtt{bit\_vector} \right\rvert \le 2^n$.

- Does this technique generalize for all $n$?

- How to compute mask values?


## "Bit Twiddling" (Example 2)

![Original Nuclei Loop from [@shojania2009nuclei].](./assets/Nuclei-Example-Original.png){ height=750px }


## "Bit Twiddling" (Example 2)

![Modified Nuclei Loop from [@shojania2009nuclei].](./assets/Nuclei-Example-Coded.png){ height=750px }


## Control Flow Elision

Applying coding theory to:

  1. `Count_Bits`
  2. `loop_gf_multiply_byte`

Produces code with *no branching!*

- Better compiler assembler/IR/micro-code generation
- Better CPU branch prediction (none)
- Better SIMD pipeline processing
- More parallelizable within GPU pipelines

## Next Steps

*How to use coding theory to generalize and generate Bit Twiddling transformations?*

## Conclusion

:::::::::::::: {.columns align=top}
::: {.column width="50%"}
\
**Thank you!**
\
\
\
\
\
\
::::

::: {.column width="50%"}
\
**Questions?**
::::
::::::::::::::

## References {.allowframebreaks}