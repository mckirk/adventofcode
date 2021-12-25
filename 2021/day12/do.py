#!/usr/bin/env python3

from os import path
from typing import overload
import numpy as np

from collections import defaultdict

import array

toy_input = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""

toy_output = """start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end"""

num_nodes = 0

class Node:
    def __init__(self, name: str):
        self.name = name

        self.is_start = self.name == "start"
        self.is_end = self.name == "end"

        self.is_small = not self.is_start and not self.is_end and (name == name.lower())

        self.connections = set()

        global num_nodes
        self.id = num_nodes
        num_nodes += 1

    def add_conn(self, other):
        self.connections.add(other)

    def __eq__(self, other):
        return self.name == other.name

    def __ne__(self, other):
        return self.name != other.name

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f"Node('{self.name}')"

class NodeCount:
    def __init__(self, init = None):
        if init is None:
            init = [0]*num_nodes

        self.array = array.array('B', init)

    def count(self, node):
        return self.array[node.id]

    def incr(self, node):
        self.array[node.id] += 1

class NodePath:
    def __init__(self, start_node, path_so_far = None, counts = None, visited_small_cave_twice = False):
        self.cur_node = start_node

        if path_so_far is None:
            self.node_path = [start_node]
        else:
            self.node_path = path_so_far + [start_node]

        if counts is None:
            self.counts = NodeCount()
        else:
            self.counts = NodeCount(counts.array)
        
        self.counts.incr(start_node)

        self.visited_small_cave_twice = visited_small_cave_twice

    def step(self, next_node):
        return NodePath(next_node, self.node_path, self.counts, self.visited_small_cave_twice)

    def __str__(self):
        return ",".join(str(n) for n in self.node_path)

    def __repr__(self):
        return str(self)

def find_paths(start, end, single_small_cave_twice = False):
    final_paths = []
    current_paths = [NodePath(start)]
    while current_paths:
        new_paths = []

        for path in current_paths:
            for other in path.cur_node.connections:
                new_path = path.step(other)

                if other.is_small:
                    if path.counts.count(other) > 0:
                        if not single_small_cave_twice or path.visited_small_cave_twice:
                            continue
                        else:
                            new_path.visited_small_cave_twice = True

                if other == end:
                    final_paths.append(new_path)
                
                if not other.is_start and not other.is_end:
                    new_paths.append(new_path)

        current_paths = new_paths

    return final_paths

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    nodes = dict()

    def get_node(name):
        if name not in nodes.keys():
            nodes[name] = Node(name)
        
        return nodes[name]

    for l in inp:
        start, end = l.split("-")

        start_node, end_node = get_node(start), get_node(end)

        start_node.add_conn(end_node)
        end_node.add_conn(start_node)

    global_start, global_end = nodes["start"], nodes["end"]

    paths1 = find_paths(global_start, global_end)
    print(f"Puzzle1: {len(paths1)}")

    # paths_to = defaultdict(list)
    # paths_from = defaultdict(list)

    # for inter_node in nodes.values():
    #     if inter_node.is_start or inter_node.is_end:
    #         continue

    #     paths_to[inter_node] += find_paths(1, global_start, inter_node)
    #     paths_from[inter_node] += find_paths(1, inter_node, global_end)

    # num_paths2 = 0

    # for inter_node in nodes.values():
    #     if inter_node.is_start or inter_node.is_end: #or not inter_node.is_small:
    #         continue

    #     print(f"Paths through {inter_node}:")
    #     for (_, pt, _) in paths_to[inter_node]:
    #         for (_, pf, _) in paths_from[inter_node]:
    #             assert pt[-1] == pf[0] == inter_node

    #             full_path = ",".join(str(n) for n in pt + pf[1:])

    #             if full_path not in toy_output.splitlines():
    #                 print(full_path)

    #     num_paths2 += len(paths_to[inter_node]) * len(paths_from[inter_node])

    # pure_big = find_paths(0, global_start, global_end)
    # num_paths2 += len(pure_big)

    # for _,path in paths:
    #     print("-".join(str(n) for n in path))


    paths2 = find_paths(global_start, global_end, True)
    print(f"Puzzle2: {len(paths2)}")

    pass

if __name__ == "__main__": main()