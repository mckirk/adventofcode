#!/usr/bin/env python3
from pathlib import Path
import numpy as np

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    nss = [[int(n) for n in line.split()] for line in lines]

    res = 0
    for ns in nss:
        nss_new = [ns]
        while not all(n == 0 for n in nss_new[-1]):
            nss_new.append(list(np.diff(nss_new[-1])))
        
        nss_new[-1] = [0] + nss_new[-1]

        for i in range(len(nss_new)-1):
            nss_new[-i-2] = [(nss_new[-i-2][0] - nss_new[-i-1][0])] + nss_new[-i-2]

        res += nss_new[0][0]

    print(res)
    
    
if __name__ == "__main__":
    main()
