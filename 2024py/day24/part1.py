#!/usr/bin/env python3

from itertools import count
import operator
from lib import *


def run(inp: Input):
    state, instrs = inp.parsed

    unknown = set()
    dependencies = dict()
    for out, instr in instrs.items():
        d = ({instr[0], instr[2]}, instr[1])
        dependencies[out] = d
        unknown |= (d[0] | {out}) - state.keys()

    while unknown:
        for u in unknown:
            depends, instr = dependencies[u]
            vals = [state.get(w) for w in depends]
            if any(v is None for v in vals):
                continue
            if instr.and_:
                state[u] = operator.and_(*vals)
            elif instr.or_:
                state[u] = operator.or_(*vals)
            elif instr.xor_:
                state[u] = operator.xor(*vals)
            else:
                assert False
            break
        else:
            assert False
        unknown.remove(u)

    bits = ""
    for i in count():
        k = f"z{i:02}"
        if k in state:
            bits = str(state[k]) + bits
        else:
            break

    return int(bits, 2)


def main():
    run_on_inputs(run, {1: 4, 2: 2024})


if __name__ == "__main__":
    main()
