#!/usr/bin/env python3
from collections import defaultdict
from pathlib import Path
from pprint import pprint

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
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
            p1 = ns[i]
            break

        for j in range(N):
            cur_candidates[i].append(ns[i]+ns[i-j-1])

    print(p1)

    sums = {(0, 0): 0}
    seen = set()
    while True:
        new_sums = dict()
        for (start, end), sum in sums.items():
            seen.add((start, end))
            if sum < p1:
                if end < len(ns) - 1 and not (start, end+1) in seen:
                    new_sums[(start, end+1)] = sum+ns[end]
                if start > 0 and not (start-1, end) in seen:
                    new_sums[(start-1, end)] = sum+ns[start-1]
            elif sum > p1:
                if end > 0 and not (start, end-1) in seen:
                    new_sums[(start, end-1)] = sum-ns[end]
                if start < len(ns) - 1 and not (start+1, end) in seen:
                    new_sums[(start+1, end)] = sum-ns[start]
            else:
                r = ns[start:end]
                print(min(r) + max(r))
                break
        
        sums = new_sums
    
if __name__ == "__main__":
    main()
