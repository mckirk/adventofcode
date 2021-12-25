#!/usr/bin/env python3

from typing import Tuple
import numpy as np
from collections import defaultdict
import re

from scipy import signal

toy_input = """on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"""

toy_input = """on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"""

def intersect_line(line1, line2):
    start1, end1 = line1
    start2, end2 = line2

    only_line1 = [(start1, min(end1, start2-1)), (max(end2+1, start1), end1)]
    only_line2 = [(start2, min(end2, start1-1)), (max(end1+1, start2), end2)]
    intersect = [(max(start1, start2), min(end1, end2))]

    def filter_lines(ls):
        return [l for l in ls if l[1] >= l[0]]

    return (filter_lines(only_line1), filter_lines(only_line2), filter_lines(intersect))

class Region:
    def __init__(self, type, x_dim: Tuple[int, int], y_dim: Tuple[int, int], z_dim: Tuple[int, int]):
        self.type = type
        self.x_dim, self.y_dim, self.z_dim = x_dim, y_dim, z_dim
        
        for dim in self.dims():
            assert dim[1] >= dim[0]

        self.children = []

    def dims(self):
        yield self.x_dim
        yield self.y_dim
        yield self.z_dim

    def size(self):
        size = 1
        for dim in self.dims():
            size *= dim[1] - dim[0] + 1

        return size

    def overlay(self, other):
        intersections = [intersect_line(my_dim, other_dim) for (my_dim, other_dim) in zip(self.dims(), other.dims())]

        regions = [[], [], []]

        for i in range(2**6):
            x_part = (i >> 4) & 3
            y_part = (i >> 2) & 3
            z_part = (i >> 0) & 3
            dim_and = x_part & y_part & z_part

            if dim_and == 0: # have "only self" parts and "only other" parts => empty region
                continue

            # now either dim_and == 3 (only 'intersect' parts), or dim_and either 1 or 2 ('self' or 'other')
            resulting_region_idx = dim_and-1

            x_dims = intersections[0][x_part-1]
            y_dims = intersections[1][y_part-1]
            z_dims = intersections[2][z_part-1]

            if not x_dims or not y_dims or not z_dims:
                continue

            if len(x_dims) > 1:
                if len(y_dims) > 1:
                    pass

            # assert len(x_dim) == 1
            # assert len(y_dim) == 1
            # assert len(z_dim) == 1

            types = [self.type, other.type, other.type] # intersection also gets 'other' type

            for x_dim in x_dims:
                for y_dim in y_dims:
                    for z_dim in z_dims:
                        regions[resulting_region_idx].append(Region(types[resulting_region_idx], x_dim, y_dim, z_dim))

        return regions

    def not_touching(self, other):
        dim_inter = [intersect_line(my_dim, other_dim) for (my_dim, other_dim) in zip(self.dims(), other.dims())]

        if any(inter == [] for (_,_,inter) in dim_inter):
            return [self]

        regs = []

        for only_x in dim_inter[0][0]:
            regs.append(Region(self.type, only_x, self.y_dim, self.z_dim))

        for inter_x in dim_inter[0][2]:
            for only_y in dim_inter[1][0]:
                regs.append(Region(self.type, inter_x, only_y, self.z_dim))

            for inter_y in dim_inter[1][2]:
                for only_z in dim_inter[2][0]:
                    regs.append(Region(self.type, inter_x, inter_y, only_z))

        return regs

    def within_bounds(self, bound):
        for (lower, higher) in self.dims():
            if lower < -bound or higher > bound: return False
        return True

    def __str__(self):
        return f"{self.type} x={self.x_dim},y={self.y_dim},z={self.z_dim}"

    def __repr__(self):
        return str(self)

def ensure_no_intersections(regions):
    done = set()
    for r1 in regions:
        done.add(id(r1))
        for r2 in regions:
            if id(r2) in regions: continue

            _, _, intersection = r1.overlay(r2)
            assert intersection == []

def overlay_all(regions):
    particles = []
    remaining = regions

    while True:
        top_rem = remaining.pop(0)
        if top_rem.type == 'on':
            particles.append(top_rem)
        if not remaining: break

        region_filter = remaining[0]

        next_particles = []
        for p in particles:
            not_touching = p.not_touching(region_filter)
            # left_intact, _, _ = p.overlay(region_filter)
            next_particles += not_touching
        
        particles = next_particles

    return particles

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    regions: list[Region] = []
    regions_50 = []
    for l in inp:
        m = re.match("(on|off) x=([-\d]+)..([-\d]+),y=([-\d]+)..([-\d]+),z=([-\d]+)..([-\d]+)", l)
        g = m.groups()

        ns = [int(n) for n in g[1:]]

        new_reg = Region(g[0], (ns[0], ns[1]), (ns[2], ns[3]), (ns[4], ns[5]))
        regions.append(new_reg)

        if new_reg.within_bounds(50):
            regions_50.append(new_reg)


    sum_50 = sum(p.size() for p in overlay_all(regions_50) if p.type == 'on')
    print(f"Puzzle1: {sum_50}")

    sum_total = sum(p.size() for p in overlay_all(regions) if p.type == 'on')
    print(f"Puzzle2: {sum_total}")

    # min_x = min(instr[1] for instr in instructions)
    # min_y = min(instr[3] for instr in instructions)
    # min_z = min(instr[5] for instr in instructions)
    # max_x = max(instr[2] for instr in instructions)
    # max_y = max(instr[4] for instr in instructions)
    # max_z = max(instr[6] for instr in instructions)

    # only_reg0, only_reg1, intersect01 = regions[0].overlay(regions[1])

    # regions_after_step1 = only_reg0 + only_reg1 + intersect01

    # regions_after_step2 = []
    # possible_dups = []

    # particles = [regions[2]]
    # for reg in regions_after_step1:
    #     next_particles = []
    #     for part in particles:
    #         only_reg, only_particle, intersect_reg_particle = reg.overlay(part)
    #         regions_after_step2 += only_reg + intersect_reg_particle
    #         next_particles += only_particle
    #     particles = next_particles

    # pass


    # active_regions = regions[:1]
    # for overlay_region in regions[1:]:
    #     overlay_particles = [overlay_region]

    #     new_regions = []
    #     for active_region in active_regions:
            
    #         while overlay_particles:
    #             top_particle = overlay_particles.pop()
    #                 new_active, new_overlay, intersect = active_region.overlay(overlay_region)



        


    pass

if __name__ == "__main__": main()