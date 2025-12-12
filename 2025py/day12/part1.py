#!/usr/bin/env python3

from itertools import product
import numpy as np
from lib import *

from lib.pos_np import FrozenArray

from ortools.sat.python import cp_model


def print_variant(v):
    for l in v:
        for b in l:
            print("#" if b else ".", end="")
        print()
    print()


def run(inp: Input):
    shape_variants: list[list[tuple[int, FrozenArray]]] = []
    shape_area: list[int] = []

    variant_count = 0
    for s in inp.parsed[0].values():
        seen: set[FrozenArray] = set()
        variants: list[tuple[int, FrozenArray]] = []

        def add_variant(v: FrozenArray):
            nonlocal variant_count

            if v in seen:
                return

            # print_variant(v)
            variants.append((variant_count, v))
            seen.add(v)
            variant_count += 1

        shape = FrozenArray([[1 if c == "#" else 0 for c in l] for l in s])
        shape_area.append(np.sum(shape))

        add_variant(shape)

        for _ in range(3):
            shape = FrozenArray(np.rot90(shape))
            add_variant(shape)

        shape = FrozenArray(np.rot90(shape))
        shape = FrozenArray(np.flip(shape, axis=0))
        add_variant(shape)

        for _ in range(3):
            shape = FrozenArray(np.rot90(shape))
            add_variant(shape)

        shape_variants.append(variants)

    problems = inp.parsed[1]

    by_size: dict[tuple[int, int], list[list[int]]] = defaultdict(list)
    for w, l, presents in problems:
        by_size[(min(w, l), max(w, l))].append(presents)

    for presentss in by_size.values():
        presentss.sort(key=sum)

    res = 0
    done = 0
    infeasible: list[tuple[tuple[int, int], list[int]]] = []

    sorted_by_size = sorted(by_size.items(), key=lambda x: sum(x[0]), reverse=True)

    for (w, l), presentss in sorted_by_size:
        for presents in presentss:
            ub_area = sum(9 * count for count in presents)
            if (w - (w % 3)) * (l - (l % 3)) >= ub_area:
                done += 1
                res += 1
                print(f"Done: {done}/{len(inp.parsed[1])} (feasible by area)")
                continue

            if any(
                w <= wi
                and l <= li
                and all(p >= infs for p, infs in zip(presents, infss))
                for (wi, li), infss in infeasible
            ):
                done += 1
                print(f"Done: {done}/{len(inp.parsed[1])} (known infeasible)")
                continue

            lb_area = sum(sa * count for sa, count in zip(shape_area, presents))
            if lb_area > w * l:
                done += 1
                infeasible.append(((w, l), presents))
                print(f"Done: {done}/{len(inp.parsed[1])} (infeasible by area)")
                continue

            model = cp_model.CpModel()

            cells_coverage = defaultdict(list)

            for i, (count, variants) in enumerate(zip(presents, shape_variants)):
                vars_for_present = []
                for (vi, variant), x, y in product(
                    variants, range(-2, w), range(-2, l)
                ):
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

                model.add(sum(vars_for_present) >= count)

            for vars_in_cell in cells_coverage.values():
                model.add(sum(vars_in_cell) <= 1)

            solver = cp_model.CpSolver()
            status = solver.solve(model)

            if status in [cp_model.FEASIBLE, cp_model.OPTIMAL]:
                res += 1
                status_readable = "feasible"
            else:
                infeasible.append(((w, l), presents))
                status_readable = "infeasible"

            done += 1
            print(f"Done: {done}/{len(inp.parsed[1])} ({status_readable})")

    return res


def main():
    run_on_inputs(run, {1: 2})


if __name__ == "__main__":
    main()
