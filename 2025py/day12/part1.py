#!/usr/bin/env python3

from itertools import product
import numpy as np
from lib import *

from ortools.sat.python import cp_model

def print_variant(v):
    for l in v:
        for b in l:
            print("#" if b else ".", end="")
        print()
    print()


def run(inp: Input):
    shape_variants: list[list[tuple[int, np.ndarray]]] = []
    variant_count = 0
    for s in inp.parsed[0].values():
        variants: list[tuple[int, np.ndarray]] = []
        def add_variant(v: np.ndarray):
            nonlocal variant_count
            # print_variant(v)
            variants.append((variant_count, v))
            variant_count += 1

        shape = np.array([[1 if c == "#" else 0 for c in l] for l in s])
        add_variant(shape)

        for _ in range(3):
            shape = np.rot90(shape)
            add_variant(shape)
        
        shape = np.rot90(shape)
        shape = np.flip(shape, axis=0)
        add_variant(shape)

        for _ in range(3):
            shape = np.rot90(shape)
            add_variant(shape)
        
        shape_variants.append(variants)

    res = 0
    done = 0

    for w, l, presents in inp.parsed[1]:
        model = cp_model.CpModel()

        cells_coverage = defaultdict(list)

        for i, (count, variants) in enumerate(zip(presents, shape_variants)):
            vars_for_present = []
            for (vi, variant), x, y in product(variants, range(-2, w), range(-2, l)):
                occupied = []
                valid = True
                for dx, dy in product(range(3), repeat=2):
                    if variant[dy, dx]:
                        px, py = x + dx, y + dy
                        if 0 <= px < w and 0 <= py < l:
                            occupied.append((px, py))
                        else:
                            valid = False
                            break
                
                if valid and occupied:
                    start_at_var = model.new_bool_var(f"p{i}v{vi}_at_{x}_{y}")
                    vars_for_present.append(start_at_var)
                    for cell in occupied:
                        cells_coverage[cell].append(start_at_var)
            
            model.add(sum(vars_for_present) == count)
        
        for vars_in_cell in cells_coverage.values():
            model.add(sum(vars_in_cell) <= 1)

        solver = cp_model.CpSolver()
        status = solver.solve(model)

        if status in [cp_model.FEASIBLE, cp_model.OPTIMAL]:
            res += 1

        done += 1
        print(f"Done: {done}/{len(inp.parsed[1])}")

    return res


def main():
    run_on_inputs(run, {1: 2})


if __name__ == "__main__":
    main()
