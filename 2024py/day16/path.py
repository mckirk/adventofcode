from abc import ABC, abstractmethod
from collections import defaultdict
from typing import Iterable

from cmp import Min
from prioq import PriorityQueue


class AStarState(ABC):
    @abstractmethod
    def incurred_cost(self): ...

    @abstractmethod
    def estimated_cost(self): ...

    def cost(self):
        return self.incurred_cost() + self.estimated_cost()

    @abstractmethod
    def is_goal(self) -> bool: ...

    @abstractmethod
    def next_states(self) -> Iterable["AStarState"]: ...

    def __lt__(self, other: "AStarState"):
        return self.cost() < other.cost()


def a_star(start_states, find_all_best=False):
    to_do: PriorityQueue[AStarState] = PriorityQueue()
    for start_state in start_states:
        to_do.put(start_state)

    best_found = None
    best_cost = defaultdict(Min)

    while not to_do.empty():
        cur = to_do.get()

        if cur.is_goal():
            if not find_all_best:
                yield cur
                return
            else:
                if best_found is None:
                    best_found = cur.incurred_cost()
                yield cur

        for next_state in cur.next_states():
            if best_found is not None and next_state.cost() > best_found:
                continue
            if best_cost[next_state].update(next_state.incurred_cost(), find_all_best):
                to_do.put(next_state)
