#!/usr/bin/env python3

# import numpy as np
# from collections import defaultdict
import re

toy_input = "target area: x=20..30, y=-10..-5"

def calc_xvel(x_delta):
    # (x_vel+1)*(x_vel) / 2 = x_delta
    # x_vel² + x_vel - 2*x_delta = 0
    # x_vel = (-1 +- sqrt(1 + 8*x_delta)) / 2

    from math import sqrt
    x_vel = round((-1 + sqrt(1 + 8*abs(x_delta))) / 2)

    if x_delta < 0: x_vel = -x_vel

    return x_vel

def calc_yvel(y_delta, x_vel):
    pass

def sim_trajectory(traj, x_target, y_target):
    x, y = 0, 0
    xd, yd = traj

    positions = []

    while True:
        positions.append((x,y))

        if x >= x_target[0] and x <= x_target[1] and y >= y_target[0] and y <= y_target[1]: return (True, positions)
        if x >  x_target[1] or  y <  y_target[0]: return (False, positions)

        x += xd
        y += yd

        if xd != 0:
            xd -= round((xd / abs(xd)))

        yd -= 1

def draw_pos(positions, x_target, y_target):
    pass

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    x_from, x_to, y_from, y_to = [int(n) for n in re.match(r"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)", inp[0]).groups()]

    x_vel_from, x_vel_to = calc_xvel(x_from), calc_xvel(x_to)

    x_target, y_target = (x_from, x_to), (y_from, y_to)

    possible_trajs = dict()
    max_traj, max_y = None, None

    for x_vel in range(x_vel_from, x_to+1):
        for y_vel in range(-1000, 1000):
            traj = (x_vel, y_vel)
            hit, positions = sim_trajectory(traj, x_target, y_target)
            if hit:
                max_y_pos = max(y for x,y in positions)
                possible_trajs[traj] = max_y_pos

                if max_y is None or max_y_pos > max_y:
                    max_traj = traj
                    max_y = max_y_pos

    print(f"Puzzle1: {max_y}")
    print(f"Puzzle2: {len(possible_trajs)}")

    pass

if __name__ == "__main__": main()