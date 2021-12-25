#!/usr/bin/env python3

# import numpy as np
# from collections import defaultdict
import re

from math import floor, ceil
from typing import Iterable, Iterator, Tuple

class Elem:
    def __init__(self):
        pass

    @staticmethod
    def make(elem):
        if type(elem) is int:
            return Num(elem)
        else:
            return Pair([Elem.make(sub_elem) for sub_elem in elem])

    def is_pair(self): ...
    def iter_elems(self) -> Iterator[Tuple["Elem", "Pair", int]]: ...
    def magnitude(self): ...

    def is_num(self):
        return not self.is_pair()


class Num(Elem):
    def __init__(self, n):
        super().__init__()

        self.n = n

    def is_pair(self): return False

    def iter_elems(self, parent, depth):
        yield (self, parent, depth)

    def magnitude(self):
        return self.n

    def __str__(self):
        return str(self.n)

    def __repr__(self):
        return str(self)

class Pair(Elem):
    def __init__(self, lr: Iterable[Elem]):
        super().__init__()

        self.left, self.right = lr

        # self.left.parent = self
        # self.right.parent = self

        # self.left = Elem.make(list[0], self.depth+1, self)
        # self.right = Elem.make(list[1], self.depth+1, self)

    def is_pair(self): return True

    def is_num_pair(self):
        return self.left.is_num() and self.right.is_num()

    def iter_elems(self, parent, depth):
        yield (self, parent, depth)
        for x in self.left.iter_elems(self, depth+1): yield x
        for y in self.right.iter_elems(self, depth+1): yield y

    def magnitude(self):
        return 3*self.left.magnitude() + 2*self.right.magnitude()

    def __str__(self):
        return f"[{self.left}, {self.right}]"

    def __repr__(self):
        return str(self)

    def replace_child(self, to_replace, new):
        if self.left == to_replace:
            self.left = new
        elif self.right == to_replace:
            self.right = new

def split(n):
    half = n / 2
    return [floor(half), ceil(half)]

def add(ns, n):
    return ns.append(n)

def try_explode(pairs: Elem):
    last_num = None
    add_to_next = None
    skip_nums = 0

    done_explode = False

    for e, parent, depth in pairs.iter_elems(None, 0):
        if e.is_num():
            if skip_nums > 0:
                skip_nums -= 1
                continue

            if add_to_next is not None:
                e.n += add_to_next
                break
            
            last_num = e

        # print(f"{e} at depth {e.depth}")

        if e.is_pair() and e.is_num_pair() and depth >= 4 and not done_explode:
            parent.replace_child(e, Elem.make(0))
            done_explode = True

            if last_num is not None:
                last_num.n += e.left.n

            add_to_next = e.right.n
            skip_nums = 2

    return done_explode

def try_split(pairs: Elem):
    for e, parent, depth in pairs.iter_elems(None, 0):
        if e.is_num() and e.n >= 10:
            half = e.n / 2
            parent.replace_child(e, Elem.make([floor(half), ceil(half)]))

            return True

    return False
            
def reduce(pairs):
    while True:
        if try_explode(pairs): continue
        if not try_split(pairs): break

def add_pairs(left, right):
    pairs = Pair([left, right])
    reduce(pairs)

    return pairs


toy_input = """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"""

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    inp = [eval(l) for l in inp]

    # pairs = [Elem.make(eval(line)) for line in inp]

    # while len(pairs) > 1:
    #     summed = add_pairs(pairs[0], pairs[1])
    #     pairs.pop(0)
    #     pairs[0] = summed

    # print(f"Puzzle1: {pairs[0].magnitude()}")

    max_magnitude = 0

    for i in range(len(inp)):
        for j in range(len(inp)):
            if i == j: continue

            left = Elem.make(inp[i])
            right = Elem.make(inp[j])
            summed = add_pairs(left, right)

            mag = summed.magnitude()

            if mag > max_magnitude:
                max_magnitude = mag

    print(f"Puzzle2: {max_magnitude}")

    pass

if __name__ == "__main__": main()