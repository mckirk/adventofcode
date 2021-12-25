#!/usr/bin/env python3

import re
from collections import defaultdict
from typing import Tuple

from BitVector import BitVector

toy_input = """v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"""

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    dim_x, dim_y = len(inp[0]), len(inp)

    moving_right = BitVector(size = dim_x*dim_y)
    moving_down  = BitVector(size = dim_x*dim_y)

    for y in range(dim_y):
        for x in range(dim_x):
            c = inp[y][x]

            if c == '>':
                moving_right[x + y*dim_x] = 1
            elif c == 'v':
                moving_down[x + y*dim_x] = 1

    mask_left   = BitVector(size = dim_x*dim_y)
    mask_right  = BitVector(size = dim_x*dim_y)
    mask_top    = BitVector(size = dim_x*dim_y)
    mask_bottom = BitVector(size = dim_x*dim_y)
    for y in range(dim_y):
        mask_left[0 + y*dim_x] = 1
        mask_right[(dim_x - 1) + y*dim_x] = 1
    for x in range(dim_x):
        mask_top[x  + 0*dim_x] = 1
        mask_bottom[x + (dim_y-1)*dim_x] = 1

    def shift_right(b: BitVector):
        rotated = b.deep_copy()
        rotated >> 1
        
        crossed = rotated & mask_left
        rotated &= ~mask_left

        crossed << dim_x
        rotated |= crossed
        return rotated

    def shift_left(b: BitVector):
        rotated = b.deep_copy()
        rotated << 1
        
        crossed = rotated & mask_right
        rotated &= ~mask_right

        crossed >> dim_x
        rotated |= crossed
        return rotated

    def shift_down(b: BitVector):
        rotated = b.deep_copy()
        rotated >> dim_x

        return rotated

    def shift_up(b: BitVector):
        rotated = b.deep_copy()
        rotated << dim_x

        return rotated

    def bits_zero(b: BitVector):
        return all(v == 0 for v in b.vector)

    def print_board(pairs):
        for y in range(dim_y):
            for x in range(dim_x):
                for (b, c) in pairs:
                    if b[x + y*dim_x]:
                        print(c, end = "")
                        break
                else:
                    print(".", end = "")
            print()

    def print_boards():
        print_board([(moving_right, ">"), (moving_down, "v")])
    
    def step():
        nonlocal moving_right, moving_down
        can_move_right = moving_right & ~shift_left(moving_down | moving_right)
        can_move_right ^= shift_right(can_move_right)
        moving_right ^= can_move_right

        can_move_down = moving_down & ~shift_up(moving_down | moving_right)
        can_move_down ^= shift_down(can_move_down)
        moving_down ^= can_move_down

        if bits_zero(can_move_right) and bits_zero(can_move_down):
            return False

        return True

    i = 0

    # print_board([(mask_left, "|"), (mask_right, "|"), (mask_top, "-"), (mask_bottom, "-")])

    # print_boards()
    while step():
        i += 1
        # print()
        # print_boards()

    print(i+1)

    pass

if __name__ == "__main__": main()
