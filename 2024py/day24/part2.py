#!/usr/bin/env python3

from itertools import count
import itertools
import operator
from lib import *
from z3 import BitVec, Extract, Solver, sat, ForAll, And, Exists, Concat, BitVecVal


BITS = 46


def state_transl(bit_name, state):
    if bit_name[0] in "xyz":
        var = bit_name[0]
        idx = int(bit_name[1:])
        return Extract(idx, idx, state[var])
    else:
        if bit_name in state:
            return state[bit_name]
        else:
            dv = BitVec(bit_name, 1)
            state[bit_name] = dv
            return dv


def eval(dependencies: dict, state, constrs: list):
    for u, (depends, instr) in dependencies.items():
        out = state_transl(u, state)
        vals = [state_transl(d, state) for d in depends]

        if instr["and"] is not None:
            constrs.append(out == operator.and_(*vals))
        elif instr["or"] is not None:
            constrs.append(out == operator.or_(*vals))
        elif instr["xor"] is not None:
            constrs.append(out == operator.xor(*vals))
        else:
            assert False
    return constrs


def print_deps(deps):
    for n, (inps, instr) in deps.items():
        if instr["and"] is not None:
            print(f"{n} = {' AND '.join(inps)}")
        if instr["or"] is not None:
            print(f"{n} = {' OR '.join(inps)}")
        if instr["xor"] is not None:
            print(f"{n} = {' XOR '.join(inps)}")


def limit_to(instrs, input_bits):
    needed = set()
    dependencies = dict()
    for out, instr in instrs.items():
        if out[0] == "z" and int(out[1:]) <= input_bits:
            needed.add(out)
        d = ({instr[0], instr[2]}, instr[1])
        dependencies[out] = d

    while True:
        new_needed = set(needed)
        for n in needed:
            new_needed |= dependencies.get(n, (set(),))[0]
        if new_needed == needed:
            break
        needed = new_needed

    possible = {f"x{i:02}" for i in range(input_bits + 1)} | {
        f"y{i:02}" for i in range(input_bits + 1)
    }
    while True:
        new_possible = set(possible)
        for out, (depends, _) in dependencies.items():
            if depends.issubset(possible):
                new_possible.add(out)
        if new_possible == possible:
            break
        possible = new_possible

    if not needed.issubset(possible):
        return None, None

    return (
        {out: d for out, d in dependencies.items() if out in needed},
        {out: d for out, d in dependencies.items() if out in possible},
    )


def switch(k1, k2, d):
    def s(x):
        if x == k1:
            return k2
        if x == k2:
            return k1
        return x

    return {s(k): v for k, v in d.items()}


def try_with(deps, input_bits):
    s = Solver()

    state = dict()
    x_inner, y_inner = BitVec("x", input_bits), BitVec("y", input_bits)
    state["x"] = Concat(BitVecVal(0, 1), x_inner)
    state["y"] = Concat(BitVecVal(0, 1), y_inner)
    state["z"] = BitVec("z", input_bits + 1)

    res_constrs = [state["z"] == state["x"] + state["y"]]
    eval(deps, state, res_constrs)

    vars = [v for n, v in state.items() if n not in "xy"]

    s.add(
        ForAll(
            [x_inner, y_inner],
            Exists(vars, And(*res_constrs)),
        )
    )

    return s.check() == sat


def find_switches(instrs, input_bits, max_bits, switch_count):
    if input_bits == max_bits:
        yield []
        return

    print(input_bits)

    cur_deps, cur_poss = limit_to(instrs, input_bits)

    if cur_deps is None:
        return

    if try_with(cur_deps, input_bits):
        for r in find_switches(
            instrs, input_bits + 1, max_bits, switch_count
        ):
            yield r

    if switch_count < 4:
        for n, o in itertools.product(cur_deps, cur_poss.keys() - cur_deps.keys()):
            if n == o:
                continue
            switched = switch(n, o, instrs)
            switched_deps, _ = limit_to(switched, input_bits)

            if switched_deps is None:
                continue

            if try_with(switched_deps, input_bits):
                for r in find_switches(
                    switched,
                    input_bits + 1,
                    max_bits,
                    switch_count + 1,
                ):
                    yield r + [n, o]


def run(inp: Input):
    _, instrs = inp.parsed

    for i in [BITS - 1]:
        print(i)
        for s in find_switches(instrs, 1, i, 0):
            print(",".join(sorted(s)))
            return


def main():
    run_on_inputs(run, {})


if __name__ == "__main__":
    main()
