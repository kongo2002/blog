---
tags: c, cpu, profiling, programming
title: Read time stamp counter in C
author: Gregor Uhlenheuer
date: 2010-05-31
summary: Ever wanted to get hold of the RDTSC? This is how I got it done in C.
---
While searching for a method to benchmark/profile some functions in C I
stumbled upon the RDTSC [^1]. The TSC is a specific
register available at least on x86 processors since the Pentium which counts
the processor ticks since reset. The TSC is returned on the RDTSC call (opcode
`0F 31`) in EDX:EAX.

The following C function worked for me on x86_64 linux:

~~~ {.C}
static inline unsigned long rdtsc()
{
    unsigned int lo, hi;
    __asm__ __volatile__("rdtsc" : "=a" (lo), "=d" (hi));
    return (unsigned long) hi << 32 | lo;
}
~~~

[^1]: Read Time Stamp Counter
