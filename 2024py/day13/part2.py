#!/usr/bin/env python3
from sympy import symbols, Eq, solve

from lib import *


cost = [3, 1]


def run(inp: Input):
    res = 0
    for eq in inp.parsed:
        but_a_x, but_a_y, but_b_x, but_b_y, r_x, r_y = eq

        r_x += 10000000000000
        r_y += 10000000000000

        a, b = symbols("a b")

        eq1 = Eq(but_a_x * a + but_b_x * b, r_x)
        eq2 = Eq(but_a_y * a + but_b_y * b, r_y)

        solution = solve((eq1, eq2), (a, b))

        sa, sb = solution[a], solution[b]
        if sa.is_integer and sb.is_integer:
            res += cost[0] * sa + cost[1] * sb
    return res


def main():
    run_on_inputs(run)


if __name__ == "__main__":
    main()
