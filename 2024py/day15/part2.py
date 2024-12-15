#!/usr/bin/env python3

from lib import *


dir_map = {
    ">": V(1, 0),
    "<": V(-1, 0),
    "^": V(0, -1),
    "v": V(0, 1),
}

delta = V(1, 0)


def run(inp: Input):
    mp, mvs = inp.blocks

    border = set()
    boxes = set()
    r = None
    for y, l in enumerate(mp.splitlines()):
        for x, c in enumerate(l):
            p = V(2 * x, y)
            p2 = p + delta
            if c == "O":
                boxes.add(p)
            if c == "#":
                border.add(p)
                border.add(p2)
            if c == "@":
                r = p

    def get_box(p: V):
        if p in boxes:
            return (p, p + delta)
        if p - delta in boxes:
            return (p - delta, p)
        return None

    def move(r: V, dir: V):
        nonlocal boxes
        boxes_to_move = set()
        prev_pos = {r}

        while prev_pos:
            new_pos = set()

            for p in prev_pos:
                np = p + dir
                if np in border:
                    return False

                box = get_box(np)
                if box is not None:
                    if dir == V(-1, 0):
                        new_pos.add(box[0])
                    elif dir == V(1, 0):
                        new_pos.add(box[1])
                    else:
                        new_pos.add(box[0])
                        new_pos.add(box[1])
                    boxes_to_move.add(box[0])

            prev_pos = new_pos

        new_box_pos = {b + dir for b in boxes_to_move}
        boxes -= boxes_to_move
        boxes |= new_box_pos

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
                    print("[", end="")
                elif p - delta in boxes:
                    print("]", end="")
                else:
                    print(".", end="")
            print()

    # print_map()

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
            # 3: 42,
            1: 9021,
        },
    )


if __name__ == "__main__":
    main()
