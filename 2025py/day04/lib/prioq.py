import heapq
from typing import Generic, Protocol, Self, TypeVar


class SupportsRichComparison(Protocol):
    def __lt__(self, other: Self, /) -> bool: ...
    def __gt__(self, other: Self, /) -> bool: ...


T = TypeVar("T", bound=SupportsRichComparison)


class PriorityQueue(Generic[T]):
    def __init__(self):
        self.elements = []

    def empty(self):
        return len(self.elements) == 0

    def put(self, item: T):
        heapq.heappush(self.elements, item)

    def get(self) -> T:
        return heapq.heappop(self.elements)
