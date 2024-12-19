#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    rules, yours, others = inp.parsed
    N = len(rules)

    poss = {i: {n for n in range(N)} for i in range(N)}
    
    res = 0
    for t in others + [yours]:
        invalid = False
        for n in t:
            for r in rules:
                if r[1] <= n <= r[2] or r[3] <= n <= r[4]:
                    break
            else:
                invalid = True
                break
        if invalid:
            continue

        for i, n in enumerate(t):
            invalid_rules = set()
            for ri in poss[i]:
                r = rules[ri]
                if r[1] <= n <= r[2] or r[3] <= n <= r[4]:
                    continue
                invalid_rules.add(ri)
            poss[i] -= invalid_rules

    for p in sorted(poss.values(), key=len):
        for p2 in poss.values():
            if len(p) == 1 and p2 != p and (x := list(p)[0]) in p2:
                p2.remove(x)

    poss = {i: list(p)[0] for i, p in poss.items()}

    res = 1
    for i, n in enumerate(yours):
        r = rules[poss[i]]
        if r[0][0] != "departure":
            continue
        res *= n

    return res


def main():
    run_on_inputs(run, {2: None})


if __name__ == "__main__":
    main()
