#!/usr/bin/env python3

from functools import lru_cache
from os import path
from z3 import *


monkeys = dict()
with open(path.join(path.dirname(__file__), "input.txt")) as f:
    for l in f.readlines():
        name = l[:4]
        rest = l[6:]
        ws = rest.strip().split(" ")
        if len(ws) == 1:
            monkeys[name] = int(rest)
        else:
            monkeys[name] = ws


def do():
    @lru_cache(maxsize=None)
    def eval(name):
        m = monkeys[name]
        if type(m) is not list:
            return m
        else:
            s1, s2 = eval(m[0]), eval(m[2])
            o = m[1]
            if o == "+":
                return s1 + s2
            if o == "*":
                return s1 * s2
            if o == "-":
                return s1 - s2
            if o == "/":
                return s1 / s2
            if o == "=":
                return s1 == s2


    return eval("root")

print(do())

monkeys["humn"] = Int('me')
monkeys["root"][1] = "="

solve(do() == True)