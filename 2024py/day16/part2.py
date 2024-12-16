#!/usr/bin/env python3

from dataclasses import Field, field
from lib import *
from path import AStarState, a_star


@dataclass(unsafe_hash=True, frozen=True)
class State(AStarState):
    mp: dict = field(hash=False, compare=False)
    target: V = field(hash=False, compare=False)

    pos: V
    dir: V

    turns: int = field(hash=False, compare=False)
    steps: int = field(hash=False, compare=False)

    been: set = field(default_factory=set, hash=False, compare=False)

    def incurred_cost(self):
        return self.steps + 1000 * self.turns

    def estimated_cost(self):
        dir_to_target = self.target - self.pos
        dist_to_target = self.pos.dist(self.target)

        dot = dir_to_target.dot(self.dir)

        if dot == dist_to_target:
            turns = 0
        elif dot > 0:
            turns = 1
        else:
            turns = 2

        return dist_to_target + 1000 * turns

    def cost(self):
        return self.incurred_cost() + self.estimated_cost()

    def is_goal(self) -> bool:
        return self.pos == self.target

    def next_states(self) -> Iterable["AStarState"]:
        new_been = self.been | {self.pos}
        np = self.pos + self.dir
        if self.mp.get(np) in ".E" and np not in self.been:
            yield State(
                self.mp,
                self.target,
                np,
                self.dir,
                self.turns,
                self.steps + 1,
                new_been,
            )

        if (
            self.mp.get(self.pos + self.dir.turn(1)) == "."
            or self.mp.get(self.pos - self.dir) == "."
        ):
            yield State(
                self.mp,
                self.target,
                self.pos,
                self.dir.turn(1),
                self.turns + 1,
                self.steps,
                new_been,
            )

        if (
            self.mp.get(self.pos + self.dir.turn(-1)) == "."
            or self.mp.get(self.pos - self.dir) == "."
        ):
            yield State(
                self.mp,
                self.target,
                self.pos,
                self.dir.turn(3),
                self.turns + 1,
                self.steps,
                new_been,
            )

    def __lt__(self, other: "AStarState"):
        return self.cost() < other.cost()


def run(inp: Input):
    mp = {V(x, y): c for (x, y), c in inp.as_pos}
    ps = {c: V(x, y) for (x, y), c in inp.as_pos}

    start = ps["S"]
    target = ps["E"]

    start_states = [State(mp, target, start, Dir.E.value, 0, 0)]

    path_tiles = set([start, target])
    for path in a_star(start_states, True):
        path_tiles |= path.been
    return len(path_tiles)


def main():
    run_on_inputs(run, {1: 45})


if __name__ == "__main__":
    main()
