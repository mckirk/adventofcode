#!/usr/bin/env python3

from lib import *
import z3

BITS = 64


def v(x):
    return z3.BitVecVal(x, BITS)


combo_table = {
    0: lambda regs: v(0),
    1: lambda regs: v(1),
    2: lambda regs: v(2),
    3: lambda regs: v(3),
    4: lambda regs: regs["A"],
    5: lambda regs: regs["B"],
    6: lambda regs: regs["C"],
    7: lambda regs: 42,
}


def shift(op, regs):
    return regs["A"] >> combo_table[op](regs)


def adv(op, regs):
    regs["A"] = shift(op, regs)


def bxl(op, regs):
    regs["B"] = regs["B"] ^ op


def bst(op, regs):
    regs["B"] = combo_table[op](regs) & v(7)


def jnz(op, regs):
    if regs["T"]:
        regs["const"].append(regs["A"] != 0)
        regs["P"] = op - 2
    else:
        regs["const"].append(regs["A"] == 0)


def bxc(op, regs):
    regs["B"] = regs["B"] ^ regs["C"]


def out(op, regs):
    t = regs["T"]
    regs["const"].append((combo_table[op](regs) & v(7)) == t[0])
    regs["T"] = t[1:]


def bdv(op, regs):
    regs["B"] = shift(op, regs)


def cdv(op, regs):
    regs["C"] = shift(op, regs)


op_table = {0: adv, 1: bxl, 2: bst, 3: jnz, 4: bxc, 5: out, 6: bdv, 7: cdv}


def run(inp: Input):
    regs, prog = inp.parsed
    for i in range(0, len(prog), 2):
        inst, op = prog[i:][:2]
        print(f"{i:02}: {op_table[inst].__name__}({op})")

    A = z3.BitVec("A", BITS)

    regs["A"] = A
    regs["B"] = v(0)
    regs["C"] = v(0)
    regs["P"] = 0
    regs["T"] = prog
    regs["const"] = []

    while regs["P"] < len(prog):
        inst, op = prog[regs["P"]:][:2]
        op_table[inst](op, regs)
        regs["P"] += 2
    
    o = z3.Optimize()
    o.add(*regs["const"])
    h = o.minimize(A)
    o.check()
    o.lower(h)
    return o.model()[A].as_long()


def main():
    run_on_inputs(run, {3: 117440})


if __name__ == "__main__":
    main()
