#!/usr/bin/env python3
from collections import defaultdict, Counter
from pathlib import Path
from pprint import pprint
import numpy as np
from shapely import Polygon

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

dir_transl = dict(U=Dir.N, R=Dir.E, L=Dir.W, D=Dir.S)


def main():
    ins = []
    for l in lines:
        ins.append(l.split())

    points = []
    pos = V(0, 0)
    num_pts = 0

    for _, _, enc in ins:
        enc = enc[2:-1]
        d = dir_transl["RDLU"[int(enc[-1])]]
        l = int(enc[:-1], 16)

        print(d, l)

        pos += d.value * l
        num_pts += l

        points.append(pos.tuple)

    print(Polygon(points).area + num_pts // 2 + 1)


if __name__ == "__main__":
    main()
