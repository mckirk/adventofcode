#!/usr/bin/env python3

from functools import cache
from itertools import count
from lib import *
import sys

sys.setrecursionlimit(10**6)


def run(inp: Input):
    con = defaultdict(set)
    for f,t in inp.parsed:
        assert f != t
        con[f].add(t)
        con[t].add(f)

    ordered = sorted(con.keys(), key=lambda x: len(con[x]), reverse=True)

    @cache
    def find_clique(start, size):
        if size == 1:
            return [{start}]
        
        res = []
        for x in con[start]:
            if ordered.index(x) < ordered.index(start):
                continue
            for smaller in find_clique(x, size-1):
                if all(start in con[s] for s in smaller):
                    res.append(smaller | {start})
        
        return res

    max_clique = None
    for i in count(1):
        current_clique = None
        for node in con.keys():
            cliques = find_clique(node, i)
            if cliques:
                current_clique = next(iter(cliques))
                break
        else:
            break
        max_clique = current_clique

    return ",".join(sorted(max_clique))


def main():
    run_on_inputs(run, {1: "co,de,ka,ta"})


if __name__ == "__main__":
    main()
