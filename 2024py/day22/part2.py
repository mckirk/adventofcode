#!/usr/bin/env python3

from lib import *


MOD = 16777216


def step(x):
    x = (x^(x << 6)) % MOD
    x = (x^(x >> 5)) % MOD
    x = (x^(x << 11)) % MOD
    return x


def stepn(x, n):
    deltas = []
    prices_by_seq = dict()
    for _ in range(n):
        p = x % 10
        x = step(x)
        np = x % 10
        deltas.append(np-p)
        if len(deltas) >= 4:
            seq = tuple(deltas[-4:])
            if seq not in prices_by_seq:
                prices_by_seq[seq] = np
    return prices_by_seq


def run(inp: Input):
    prices_by_seqs = Counter()
    for x in inp.parsed:
        for seq, p in stepn(x, 2000).items():
            prices_by_seqs[seq] += p

    l = [(p, seq) for seq, p in prices_by_seqs.items()]
    l.sort(reverse=True)

    return l[0][0]


def main():
    run_on_inputs(run, {2: 23})


if __name__ == "__main__":
    main()
