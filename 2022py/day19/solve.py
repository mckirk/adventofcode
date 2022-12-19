#!/usr/bin/env python3

from collections import defaultdict
from functools import cache
from math import ceil
from os import path
import re
import sys

sys.setrecursionlimit(1000000000)


def ints(string):
    return list(map(int, re.findall(r"-?[0-9]+", string)))


blueprints = []
with open(path.join(path.dirname(__file__), "input.txt")) as f:
    for l in f.readlines():
        blueprints.append(ints(l))


class Cmpable:
    def __init__(self, v, info=None):
        self.v = v
        self.info = info

    def __str__(self):
        v, info = self.v, self.info
        return f"{v=}, {info=}"


class Max:
    def __init__(self):
        self.best = None

    def update(self, new):
        if self.best is None or new.v > self.best.v:
            self.best = new

    @property
    def value(self):
        return self.best


def tryBlueprint(bp, limit):
    i, oreBotOre, clayBotOre, obsidBotOre, obsidBotClay, geodeBotOre, geodeBotObsid = bp

    oreBotPrice = [oreBotOre, 0, 0, 0]
    clayBotPrice = [clayBotOre, 0, 0, 0]
    obsidBotPrice = [obsidBotOre, obsidBotClay, 0, 0]
    geodeBotPrice = [geodeBotOre, 0, geodeBotObsid, 0]

    prices = [oreBotPrice, clayBotPrice, obsidBotPrice, geodeBotPrice]

    bestBotsAt = defaultdict(Max)

    @cache
    def bestAt(time, res, bots):
        assert time <= limit

        rem = time - limit
        best = Max()
        best.update(Cmpable(res[3] + bots[3]*rem, bots))

        bestBotsAt[time].update(Cmpable(bots[3]))

        if time == limit or bestBotsAt[time].value.v > bots[3]:
            return best.value

        for i, price in enumerate(prices):
            delta = [p-r for r, p in zip(res, price)]
            possible, stepsNeeded = True, 0
            for d, b in zip(delta, bots):
                if not d:
                    continue
                elif not b:
                    possible = False
                    break

                stepsNeeded = max(stepsNeeded, ceil(d / b))

            stepsNeeded += 1

            if possible and (limit - time >= stepsNeeded):
                newRes = tuple(r+(b*stepsNeeded)-p for r, b,
                               p in zip(res, bots, price))
                newBots = list(bots)
                newBots[i] += 1

                best.update(bestAt(time+stepsNeeded, newRes, tuple(newBots)))

        return best.value

    res = bestAt(0, tuple([0]*4), tuple([1] + [0]*3))

    return res


s = 0
for i, bp in enumerate(blueprints, start=1):
    s += i*tryBlueprint(bp, 24).v

print(s)

p = 1
for bp in blueprints[:3]:
    p *= tryBlueprint(bp, 32).v

print(p)
