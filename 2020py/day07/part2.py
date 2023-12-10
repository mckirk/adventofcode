#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    can_contain = dict()
    for line in lines:
        part1, part2 = line.split(" contain ")
        if part2.strip() == "no other bags.":
            continue

        contains = part2.split(", ")
        contains_parsed = []
        for c in contains:
            p = c.split()
            n = int(c[0])
            typ = " ".join(p[1:3])
            contains_parsed.append((n, typ))

        can_contain[" ".join(part1.split()[:2])] = contains_parsed

    def count_bags(typ):
        count = 1
        for n, b in can_contain.get(typ, []):
            count += n * count_bags(b)
        return count
    
    print(count_bags("shiny gold")-1)

    
if __name__ == "__main__":
    main()
