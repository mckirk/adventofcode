#!/usr/bin/env python3

from collections import deque
from pprint import pprint
from lib import *


@dataclass
class Map:
    target: V
    limits: tuple[int, int]
    blocked: set[V]

    def valid(self, p: V, in_cheat: bool):
        return p.within(self.limits) and (in_cheat or p not in self.blocked)


def bfs(mp: Map, start: V) -> dict[V, int]:
    distance = dict()
    queue: deque[V] = deque()
    distance[start] = 0
    queue.append(start)
    while queue:
        p = queue.popleft()
        for np, _ in p.adj(limits=mp.limits, diagonal=False):
            if not mp.valid(np, False):
                continue
            if np not in distance:
                distance[np] = distance[p] + 1
                queue.append(np)
    return distance


def grid_around(mp: Map, start: V, dist):
    for xd in range(-dist, dist + 1):
        for yd in range(-dist, dist + 1):
            np = start + V(xd, yd)
            d = np.dist(start)
            if d <= dist and mp.valid(np, False):
                yield np, d


def run(inp: Input):
    S = [p for p, c in inp.as_pos if c == "S"][0]
    E = [p for p, c in inp.as_pos if c == "E"][0]
    blocked = set(p for p, c in inp.as_pos if c == "#")
    mp = Map(E, inp.limits, blocked)

    dist_s = bfs(mp, S)
    dist_e = bfs(mp, E)
    legit = dist_s[E]

    potential_cheats = set()
    for p, c in inp.as_pos:
        if c == "#":
            continue

        for np, d in grid_around(mp, p, 20):
            potential_cheats.add((p, np, d))

    counter = Counter()
    res = 0
    for cheat_start, cheat_end, cheat_dist in potential_cheats:
        if cheat_start not in dist_s or cheat_end not in dist_e:
            continue
        dist = dist_s[cheat_start] + cheat_dist + dist_e[cheat_end]
        saved = legit - dist

        if saved <= 0:
            continue

        counter[saved] += 1

        if saved >= 100:
            res += 1

    pprint(sorted(counter.items()))

    return res


def main():
    run_on_inputs(run, {1: None})


if __name__ == "__main__":
    main()
