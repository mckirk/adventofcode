#!/usr/bin/env python3
from collections import defaultdict
from pathlib import Path
from pprint import pprint

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    ns = [int(n) for n in lines]
    
    N = 25
    cur_candidates = defaultdict(list)
    for i in range(N):
        for j in range(N):
            if j == i:
                continue
            cur_candidates[i].append(ns[i]+ns[j])

    for i in range(N, len(ns)):
        # pprint((i, cur_candidates))
        for j in range(N):
            if ns[i] in cur_candidates[i-j]:
                break
        else:
            print(ns[i])

        for j in range(N):
            cur_candidates[i].append(ns[i]+ns[i-j-1])

    

    
if __name__ == "__main__":
    main()
