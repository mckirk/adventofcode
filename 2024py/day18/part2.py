#!/usr/bin/env python3

from itertools import count
from lib import *


def run(inp: Input, dims, limit):
    vs = [V(*c) for c in inp.parsed]
    for i in count():
        trunc = set(vs[: -i - 1])
        mp = SimpleMap(V(dims[1], dims[1]), (dims[1] + 1, dims[1] + 1), trunc)

        start_state = SimpleState(V(0, 0), 0, mp)

        final = a_star([start_state])

        if final is not None:
            f = vs[-i - 1]
            return f"{f.x},{f.y}"


def main():
    run_on_inputs(
        run, {1: "6,1"}, dims={1: (0, 6), None: (0, 70)}, limit={1: 12, None: 1024}
    )


if __name__ == "__main__":
    main()
