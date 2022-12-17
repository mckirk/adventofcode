#!/usr/bin/env python3

import functools
import operator
from os import path


shapes = [
    ["####"],

    [".#.",
     "###",
     ".#."],

    ["..#",
     "..#",
     "###"],

    ["#",
     "#",
     "#",
     "#"],

    ["##",
     "##"]
]


def parseShape(lines: list[str]):
    out = []
    for l in lines:
        n = 0
        for i, c in enumerate(l):
            if c == '.':
                continue

            n += 1 << i
        out.append(n)
    return out


parsed = [parseShape(s) for s in shapes]


with open(path.join(path.dirname(__file__), "input.txt")) as f:
    dirs = f.readlines()[0].strip()

newLine  = parseShape(["#.......#"])[0]
fullLine = parseShape(["#########"])[0]


def simStone(state, shapeIdx, dirIdx):
    shape = parsed[shapeIdx]
    shapeIdx = (shapeIdx+1) % 5

    prevHeight = len(state)

    state = [newLine for _ in range(3+len(shape))] + list(state)

    def canMove(xPos, yPos):
        for yAdd, n in enumerate(shape):
            shifted = n << xPos
            if state[yPos+yAdd] & shifted:
                return False
        return True

    def setStone(xPos, yPos):
        for yAdd, n in enumerate(shape):
            shifted = n << xPos
            assert not state[yPos+yAdd] & shifted
            state[yPos+yAdd] |= shifted

    xPos, yPos = (3, 0)

    done = False
    while not done:
        dir = 1 if dirs[dirIdx] == ">" else -1
        dirIdx = (dirIdx+1) % len(dirs)

        if canMove(xPos+dir, yPos):
            xPos += dir

        if canMove(xPos, yPos+1):
            yPos += 1
        else:
            done = True

    setStone(xPos, yPos)

    for i, l in enumerate(state):
        if l != newLine:
            break

    state = state[i:]

    gainedHeight = len(state) - prevHeight

    for i in range(len(state)):
        if functools.reduce(operator.__or__, state[i:i+4], 0) == fullLine:
            break

    state = tuple(state[:i+4])

    return gainedHeight, (state, shapeIdx, dirIdx)


def printState(state):
    print("-" * 9)
    for l in state:
        print("".join("#" if l & (1 << i) else "." for i in range(9)))
    print("-" * 9)


def simFor(n):
    state = (fullLine,)
    height = 0
    shapeIdx = 0
    dirIdx = 0

    memory = dict()

    for i in range(n):
        inp = (state, shapeIdx, dirIdx)

        if inp in memory:
            prevI, prevHeight,  = memory[inp]
            segLength = i - prevI
            segGained = height - prevHeight
            break

        memory[inp] = (i, height)

        gained, (state, shapeIdx, dirIdx) = simStone(state, shapeIdx, dirIdx)
        height += gained
    else:
        return height

    times = (n-i) // segLength
    i += segLength*times
    height += segGained*times

    for _ in range(i, n):
        gained, (state, shapeIdx, dirIdx) = simStone(state, shapeIdx, dirIdx)
        height += gained

    return height


def main():
    print(simFor(2022))
    print(simFor(1000000000000))


main()
