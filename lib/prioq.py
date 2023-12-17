import heapq
from typing import Generic, TypeVar

T = TypeVar("T")


class PriorityQueue(Generic[T]):
    def __init__(self):
        self.elements = []

    def empty(self):
        return len(self.elements) == 0

    def put(self, item: T):
        heapq.heappush(self.elements, item)

    def get(self) -> T:
        return heapq.heappop(self.elements)
