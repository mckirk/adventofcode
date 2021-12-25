#!/usr/bin/env python3

dirs = {"forward": (1,0), "down": (0,1), "up": (0,-1)}

def main():
    with open("./input.txt", "r") as f:
        inp = f.read()

    x,y = 0,0
    for l in inp.strip().splitlines():
        d,n = l.split()
        x_mod,y_mod = dirs[d]

        n = int(n)
        x += x_mod * n
        y += y_mod * n

    print(f"Puzzle 1: {x*y}")

    x,y,aim = 0,0,0
    for l in inp.strip().splitlines():
        d,n = l.split()
        x_mod,aim_mod = dirs[d]

        n = int(n)
        x += x_mod * n
        aim += aim_mod * n
        y += x_mod * aim * n

    print(f"Puzzle 2: {x*y}")

if __name__ == "__main__": main()