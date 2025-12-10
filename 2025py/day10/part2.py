#!/usr/bin/env python3

from ortools.sat.python import cp_model

from lib import *


def solve(buttons, joltage):
    model = cp_model.CpModel()
    max_joltage = max(joltage)

    button_vars = [model.new_int_var(0, max_joltage, f"button{i}") for i, _ in enumerate(buttons)]

    for i, t in enumerate(joltage):
        relevant_buttons = [
            bv
            for b, bv in zip(buttons, button_vars)
            if i in b
        ]
        model.add(sum(relevant_buttons) == t)

    model.minimize(sum(button_vars))

    solver = cp_model.CpSolver()
    status = solver.solve(model)

    return sum(solver.values(button_vars))

def run(inp: Input):
    res = 0
    count = len(inp.parsed)
    done = 0
    for _, buttons, joltage in inp.parsed:
        res += solve(buttons, joltage)
        done += 1
        print(f"{done}/{count} done")
    return res


def main():
    run_on_inputs(run, {1: 33})


if __name__ == "__main__":
    main()
