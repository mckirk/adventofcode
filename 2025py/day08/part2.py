#!/usr/bin/env python3

from lib import *


def run(inp: Input):
    vs = []
    for coords in inp.parsed:
        vs.append(VNP.from_val(coords))

    dists = []
    for i, a in enumerate(vs):
        for j, b in enumerate(vs[i+1:], start=i+1):
            dists.append((np.linalg.norm(a-b), i, j))

    dists.sort()

    next_c = 0
    circuit = dict()
    by_circuit = defaultdict(set)
    for (d, i, j) in dists:
        if i in circuit and j not in circuit:
            ci = circuit[i]
            circuit[j] = ci
            by_circuit[ci].add(j)

            if len(by_circuit[ci]) == len(vs):
                return vs[i].x * vs[j].x
        elif i not in circuit and j in circuit:
            cj = circuit[j]
            circuit[i] = cj
            by_circuit[cj].add(i)

            if len(by_circuit[cj]) == len(vs):
                return vs[i].x * vs[j].x
        elif i in circuit and j in circuit:
            if circuit[i] != circuit[j]:
                ci, cj = circuit[i], circuit[j]
                for k in by_circuit[cj]:
                    circuit[k] = ci
                by_circuit[ci] |= by_circuit[cj]
                by_circuit[cj] = set()

                if len(by_circuit[ci]) == len(vs):
                    return vs[i].x * vs[j].x
        else:
            circuit[i] = circuit[j] = next_c
            by_circuit[next_c] = {i, j}
            next_c += 1

    return None


def main():
    run_on_inputs(run, {1: 25272})


if __name__ == "__main__":
    main()
