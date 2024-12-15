#!/usr/bin/env python3

from lib import *


dir_map = {
    ">": V(1, 0),
    "<": V(-1, 0),
    "^": V(0, -1),
    "v": V(0, 1),
}


def run(inp: Input):
    mp, mvs = inp.blocks

    border = set()
    boxes = set()
    r = None
    for y, l in enumerate(mp.splitlines()):
        for x, c in enumerate(l):
            p = V(x, y)
            if c == "O":
                boxes.add(p)
            if c == "#":
                border.add(p)
            if c == "@":
                r = p

    def move(p: V, dir: V):
        np = p + dir
        if np in border:
            return False

        if np in boxes:
            if not move(np, dir):
                return False

        if p in boxes:
            boxes.remove(p)
            boxes.add(np)

        return True

    def print_map():
        max_x, max_y = max(b.x for b in border), max(b.y for b in border)

        for y in range(max_y + 1):
            for x in range(max_x + 1):
                p = V(x, y)
                if p in border:
                    print("#", end="")
                elif p == r:
                    print("@", end="")
                elif p in boxes:
                    print("O", end="")
                else:
                    print(".", end="")
            print()

    for mv in mvs.replace("\n", ""):
        dir = dir_map[mv]
        if move(r, dir):
            r += dir
        # print(f"Move {mv}:")
        # print_map()
        # print()

    res = 0
    for box in boxes:
        res += box.x + 100 * box.y

    return res


def main():
    run_on_inputs(
        run,
        {
            2: 2028,
            1: 10092,
        },
    )


if __name__ == "__main__":
    main()
