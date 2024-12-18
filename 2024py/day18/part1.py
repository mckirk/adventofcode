#!/usr/bin/env python3

from lib import *


def run(inp: Input, dims, limit):
    mp = SimpleMap(
        V(dims[1], dims[1]),
        (dims[1] + 1, dims[1] + 1),
        set(V(*c) for c in inp.parsed[:limit]),
    )

    start_state = SimpleState(V(0, 0), 0, mp)

    final = a_star([start_state])

    return final.steps


def main():
    run_on_inputs(
        run, {1: 22}, dims={1: (0, 6), None: (0, 70)}, limit={1: 12, None: 1024}
    )


if __name__ == "__main__":
    main()
