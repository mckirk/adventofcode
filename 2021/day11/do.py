#!/usr/bin/env python3

import numpy as np
from scipy.signal import convolve2d

flash_kernel = np.array([
    [1, 1, 1],
    [1, 0, 1],
    [1, 1, 1]])

def sim_step(octopi: np.ndarray):
    octopi += 1

    flashed = np.zeros_like(octopi, dtype=bool)

    while True:
        unflashed = np.logical_not(flashed)

        flashers = (octopi > 9) & unflashed

        if np.sum(flashers) == 0:
            break

        neighbors: np.ndarray = convolve2d(flashers, flash_kernel, mode='same')

        octopi += neighbors.astype('uint8')

        flashed |= flashers

    octopi[flashed] = 0
    octopi[octopi > 9] = 9

    return np.sum(flashed)

def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    ns = [[int(n) for n in l] for l in inp]

    octopi: np.ndarray = np.array(ns, dtype='uint8')
    
    num_octopi = len(octopi.flatten())

    step_num, total_flashes = 1, 0
    while True:
        flashes = sim_step(octopi)
        total_flashes += flashes

        if step_num == 100:
            print(f"Puzzle1: {total_flashes}")

        if flashes == num_octopi:
            print(f"Puzzle2: {step_num}")
            break

        step_num += 1

    pass

if __name__ == "__main__": main()