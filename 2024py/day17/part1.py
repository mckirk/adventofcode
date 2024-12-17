#!/usr/bin/env python3

import math
from lib import *


@dataclass
class State:
    regs: dict
    ops: list


combo_table = {
    0: lambda regs: 0,
    1: lambda regs: 1,
    2: lambda regs: 2,
    3: lambda regs: 3,
    4: lambda regs: regs["A"],
    5: lambda regs: regs["B"],
    6: lambda regs: regs["C"],
    7: lambda regs: 42,
}


def adv(op, regs):
    regs["A"] = int(math.trunc(regs["A"] / 2 ** combo_table[op](regs)))


def bxl(op, regs):
    regs["B"] = regs["B"] ^ op


def bst(op, regs):
    regs["B"] = combo_table[op](regs) & 7


def jnz(op, regs):
    if regs["A"] == 0:
        return
    regs["P"] = op - 2


def bxc(op, regs):
    regs["B"] = regs["B"] ^ regs["C"]


def out(op, regs):
    regs["O"].append(combo_table[op](regs) & 7)


def bdv(op, regs):
    regs["B"] = int(math.trunc(regs["A"] / 2 ** combo_table[op](regs)))


def cdv(op, regs):
    regs["C"] = int(math.trunc(regs["A"] / 2 ** combo_table[op](regs)))


op_table = {0: adv, 1: bxl, 2: bst, 3: jnz, 4: bxc, 5: out, 6: bdv, 7: cdv}


def run(inp: Input):
    regs, prog = inp.parsed
    regs["P"] = 0
    regs["O"] = []
    while regs["P"] < len(prog):
        inst, op = prog[regs["P"] :][:2]
        op_table[inst](op, regs)
        regs["P"] += 2
    return ",".join(str(n) for n in regs["O"])


def main():
    run_on_inputs(run, {1: "4,6,3,5,6,3,5,2,1,0", 2: "4,2,5,6,7,7,7,7,3,1,0"})


if __name__ == "__main__":
    main()
