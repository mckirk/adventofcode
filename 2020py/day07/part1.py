#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
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

    can_contain_shiny_gold = set(["shiny gold"])
    last_len = 1
    while True:
        for bag, contained in can_contain.items():
            for _, bag2 in contained:
                if bag2 in can_contain_shiny_gold:
                    can_contain_shiny_gold.add(bag)
        
        if last_len == len(can_contain_shiny_gold):
            break
        last_len = len(can_contain_shiny_gold)

    print(len(can_contain_shiny_gold)-1)


    
if __name__ == "__main__":
    main()
