#!/usr/bin/env python3
from collections import defaultdict, Counter
from dataclasses import field
from pathlib import Path
from pprint import pprint
from aocparser import parse
import networkx as nx
import matplotlib.pyplot as plt


from lib import *
from lib import AStarState

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

board = {V(x, y): c for y, line in enumerate(lines) for x, c in enumerate(line)}
limits = (len(lines[0]), len(lines))
start = [V(x, 0) for x, c in enumerate(lines[0]) if c == "."][0]
end = [V(x, len(lines) - 1) for x, c in enumerate(lines[-1]) if c == "."][0]

valid = {v for v, c in board.items() if c != "#"}

DIRS = [V(1, 0), V(-1, 0), V(0, -1), V(0, 1)]


class Nodes:
    def __init__(self):
        self.nodes = dict()
        self.to_do = []

    def __getitem__(self, pos: V) -> "Node":
        node = self.nodes.get(pos)
        if not node:
            # print(f"Creating node {pos}")
            node = self.nodes[pos] = Node(pos, self)
            self.to_do.append(node)
        return node
    
    def find_all(self):
        while self.to_do:
            # print(len(self.to_do))
            node = self.to_do.pop()
            node.find_next()
    
    def add_edges(self, graph: nx.Graph):
        for node in self.nodes.values():
            for length, next_node in node.next:
                graph.add_edge(str(node), str(next_node), weight=length)


class Node:
    def __init__(self, pos: V, nodes: Nodes):
        self.pos = pos
        self.next = None
        self.nodes = nodes

    def __hash__(self):
        return hash(self.pos)
    
    def __eq__(self, other):
        return self.pos == other.pos

    def __str__(self):
        return f"({self.pos.x}, {self.pos.y})"

    def find_next(self):
        starts = [self.pos + d for d in DIRS if self.pos + d in valid]
        assert len(starts) > 0

        self.next = []

        for s in starts:
            seen = {self.pos, s}
            cur = s
            length = 1

            while True:
                ns = [cur + d for d in DIRS if cur + d in valid and cur + d not in seen]

                if not ns:
                    # dead end, only add if it's the target or the start
                    if cur == end or cur == start:
                        self.next.append((length, self.nodes[cur]))
                    break
                elif len(ns) == 1:
                    cur = ns[0]
                    seen.add(cur)
                    length += 1
                else:
                    # intersection
                    self.next.append((length, self.nodes[cur]))
                    break


@dataclass(frozen=True)
class State(AStarState):
    cur_node: Node
    seen: set[Node] = field(default_factory=set)
    path_length: int = 0

    def __hash__(self):
        return hash((self.cur_node, self.path_length, frozenset(self.seen)))
        
    def next_states(self):
        return [State(next_node, self.seen | {next_node}, self.path_length + length)
                for length, next_node in self.cur_node.next
                if next_node not in self.seen]
    
    def is_goal(self) -> bool:
        return self.cur_node.pos == end
    
    def incurred_cost(self):
        return -self.path_length
    
    def estimated_cost(self):
        return 0


def visualize(nodes: Nodes):
    # visualize graph
    G = nx.Graph()

    nodes.add_edges(G)

    # Draw the graph
    pos = nx.spring_layout(G)  # positions for all nodes

    # nodes
    nx.draw_networkx_nodes(G, pos, node_size=700)

    # edges
    nx.draw_networkx_edges(G, pos, width=6)

    # labels
    nx.draw_networkx_labels(G, pos, font_size=20, font_family='sans-serif')

    # edge labels
    edge_labels = dict([((u, v,), d['weight'])
                        for u, v, d in G.edges(data=True)])
    nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels)

    plt.axis('off')
    plt.show()


def main():
    nodes = Nodes()
    start_node = nodes[start]
    nodes.find_all()

    # visualize(nodes)

    state = State(start_node)
    a_star([state])

    
if __name__ == "__main__":
    main()
