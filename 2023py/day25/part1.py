#!/usr/bin/env python3
from collections import defaultdict, Counter
from functools import reduce
from pathlib import Path
from pprint import pprint
from aocparser import parse
import networkx as nx
import matplotlib.pyplot as plt

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = "[{key:w}: {wl}|\n]"
parsed = parse(spec, input)


def visualize(connected):
    G = nx.Graph()
    for n, os in connected.items():
        for o in os:
            G.add_edge(n, o)
    nx.draw(G, with_labels=True)
    plt.show()


def all_reachable(connected, start):
    visited = set()

    def dfs(u):
        visited.add(u)
        for v in connected[u]:
            if v not in visited:
                dfs(v)

    dfs(start)
    return visited


def main():
    connected = defaultdict(set)
    for n, os in parsed.items():
        for o in os:
            connected[n].add(o)
            connected[o].add(n)

    # visualize(connected)

    pairs = [("rpd", "ttj"), ("vps", "htp"), ("dgc", "fqn")]

    for a, b in pairs:
        connected[a].remove(b)
        connected[b].remove(a)

    # visualize(connected)

    print(len(all_reachable(connected, a)) * len(all_reachable(connected, b)))


if __name__ == "__main__":
    main()
