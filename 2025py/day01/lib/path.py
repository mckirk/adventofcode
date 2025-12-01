from abc import ABC, abstractmethod
from collections import defaultdict
from dataclasses import dataclass
from typing import Iterable, Protocol, Self

from lib.cmp import Min
from lib.prioq import PriorityQueue
from lib.pos import V

class ValidCost(Protocol):
    def __add__(self, x: Self, /) -> Self: ...
    def __lt__(self, other: Self, /) -> bool: ...


class ValidKey(Protocol):
    def __eq__(self, x: Self, /) -> bool: ...
    def __hash__(self, /) -> int: ...


class AStarState(ABC):
    @abstractmethod
    def incurred_cost(self) -> ValidCost: ...

    @abstractmethod
    def estimated_cost(self) -> ValidCost: ...

    @abstractmethod
    def is_goal(self) -> bool: ...

    @abstractmethod
    def next_states(self) -> Iterable["AStarState"]: ...

    @abstractmethod
    def key(self) -> ValidKey: ...

    def cost(self):
        return self.incurred_cost() + self.estimated_cost()

    def __lt__(self, other: "AStarState"):
        return self.cost() < other.cost()


def a_star(start_states):
    to_do: PriorityQueue[AStarState] = PriorityQueue()
    for start_state in start_states:
        to_do.put(start_state)

    best_cost = defaultdict(Min)

    while not to_do.empty():
        cur = to_do.get()

        if cur.is_goal():
            return cur

        for next_state in cur.next_states():
            if best_cost[next_state.key()].update(next_state.incurred_cost()):
                to_do.put(next_state)


@dataclass
class SimpleMap:
    target: V
    limits: tuple[int, int]
    blocked: set[V]

    def valid(self, p: V):
        return p.within(self.limits) and p not in self.blocked


@dataclass
class SimpleState(AStarState):
    pos: V
    steps: int

    mp: SimpleMap

    def incurred_cost(self):
        return self.steps

    def estimated_cost(self):
        return self.mp.target.dist(self.pos)

    def is_goal(self) -> bool:
        return self.pos == self.mp.target

    def next_states(self):
        for np, _ in self.pos.adj(limits=self.mp.limits, diagonal=False):
            if not self.mp.valid(np):
                continue
            yield SimpleState(np, self.steps + 1, self.mp)

    def key(self):
        return self.pos
