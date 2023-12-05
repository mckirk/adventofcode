#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def intersect_ranges(range1, range2):
    range1_s, range1_l = range1
    range2_s, range2_l = range2
    
    intersecting_range = None
    non_intersecting_range1 = None
    non_intersecting_range2 = None

    if range1_s <= range2_s <= range1_s + range1_l:
        intersect_end = min(range1_s + range1_l, range2_s + range2_l)
        intersecting_range = (range2_s, intersect_end - range2_s)
        non_intersecting_range1 = (range1_s, range2_s - range1_s)
        non_intersecting_range2 = (intersect_end, range2_s + range2_l - intersect_end)
    elif range2_s <= range1_s <= range2_s + range2_l:
        intersect_end = min(range1_s + range1_l, range2_s + range2_l)
        intersecting_range = (range1_s, intersect_end - range1_s)
        non_intersecting_range1 = (intersect_end, range1_s + range1_l - intersect_end)
        non_intersecting_range2 = (range2_s, range1_s - range2_s)
    else:
        return None, range1, range2
    
    if intersecting_range[1] <= 0:
        intersecting_range = None
    if non_intersecting_range1[1] <= 0:
        non_intersecting_range1 = None
    if non_intersecting_range2[1] <= 0:
        non_intersecting_range2 = None

    return intersecting_range, non_intersecting_range1, non_intersecting_range2

def main():
    seed_ranges = []
    p = lines[0].split(": ")[1].split()
    for i in range(0, len(p), 2):
        seed_ranges.append((int(p[i]), int(p[i+1])))

    maps = dict()
    cur_name = None
    cur_nums = []
    for line in lines[2:] + [""]:
        if not line:
            maps[cur_name[0]] = (cur_name[1], cur_nums)
            cur_name = None
            cur_nums = []
            continue
        
        if "map" in line:
            parts = line.split()[0].split("-")
            cur_name = (parts[0], parts[2])
            continue

        cur_nums.append([int(x) for x in line.split()])

    locs = []
    for seed_s, seed_l in seed_ranges:
        cur = "seed"
        cur_ranges = [(seed_s, seed_l)]
        while cur:
            m = maps.get(cur)
            if not m: break
            cur, ns = m
            new_ranges = []
            while cur_ranges:
                r = cur_ranges.pop()
                for dest, start, l in ns:
                    ir, ni1, ni2 = intersect_ranges(r, (start, l))
                    print(f"{cur}: intersecting {r} with {start, l} to get {ir}, {ni1}, {ni2}")
                    if ir:
                        ir_s, ir_l = ir
                        new_ranges.append((ir_s - start + dest, ir_l))
                    if ni1:
                        r = ni1
                    else:
                        break
                else:
                    new_ranges.append(r)
            cur_ranges = new_ranges
        locs += (cur_ranges)
    
    print(locs)
    print(min(p[0] for p in locs))

    
if __name__ == "__main__":
    main()
