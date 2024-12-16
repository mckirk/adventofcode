import operator
from typing import Any, Generic, Optional, TypeVar, Callable

T = TypeVar("T")


class Base(Generic[T]):
    def __init__(
        self, elem: Optional[T] = None, key: Optional[Callable[[T], Any]] = None
    ):
        self.elem = elem
        self.key = key if key is not None else lambda x: x

    def empty(self):
        return self.elem is None

    def get(self) -> T:
        assert self.elem is not None
        return self.elem


class Min(Generic[T], Base[T]):
    def update(self, elem: T, leq = False):
        op = operator.le if leq else operator.lt
        if self.elem is None or op(self.key(elem), self.key(self.elem)):
            self.elem = elem
            return True
        return False


class Max(Generic[T], Base[T]):
    def update(self, elem: T):
        if self.elem is None or self.key(elem) > self.key(self.elem):
            self.elem = elem
            return True
        return False
