#!/usr/bin/env python3

from dataclasses import field
from lib import *
from path import *

dirs = [d.value for d in [Dir.E, Dir.W, Dir.N, Dir.S]]


@dataclass
class Map:
    target: V
    limits: tuple[int, int]
    fallen: set[V]


@dataclass(unsafe_hash=True, frozen=True)
class State(AStarState):
    pos: V
    steps: int = field(hash=False, compare=False)

    mp: Map = field(hash=False, compare=False)

    def incurred_cost(self):
        return self.steps

    def estimated_cost(self):
        return self.mp.target.dist(self.pos)

    def is_goal(self) -> bool:
        return self.pos == self.mp.target

    def next_states(self) -> Iterable["AStarState"]:
        blocked = self.mp.fallen
        for d in dirs:
            np = self.pos + d
            if np in blocked or not np.within(self.mp.limits):
                continue
            yield State(np, self.steps + 1, self.mp)


def run(inp: Input, dims, limit):
    mp = Map(
        V(dims[1], dims[1]),
        (dims[1] + 1, dims[1] + 1),
        set(V(*c) for c in inp.parsed[:limit]),
    )

    start_state = State(V(0, 0), 0, mp)

    final = a_star([start_state])

    return final.steps


def main():
    run_on_inputs(
        run, {1: 22}, dims={1: (0, 6), None: (0, 70)}, limit={1: 12, None: 1024}
    )


if __name__ == "__main__":
    main()
