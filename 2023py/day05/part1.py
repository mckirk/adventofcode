#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    seeds = [int(x) for x in lines[0].split(": ")[1].split()]

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
    for seed in seeds:
        cur = "seed"
        cur_n = seed
        while cur:
            m = maps.get(cur)
            print(f"{cur}, {cur_n} -> {m}")
            if not m: break
            cur, ns = m
            for dest, start, l in ns:
                if start <= cur_n < start + l:
                    cur_n = cur_n - start + dest
                    break
        locs.append(cur_n)
    
    print(locs)
    print(min(locs))

    
if __name__ == "__main__":
    main()
