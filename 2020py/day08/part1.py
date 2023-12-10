#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    ins = []
    for l in lines:
        ins.append(l.split(" "))

    acc_by_pos = dict()
    cur_acc = 0
    cur_pos = 0
    while True:
        # print(cur_pos, cur_acc)
        if cur_pos in acc_by_pos:
            break
        acc_by_pos[cur_pos] = cur_acc

        i, arg = ins[cur_pos]

        if i == "nop":
            cur_pos += 1
            continue

        if i == "acc":
            cur_pos += 1
            cur_acc += int(arg)
            continue

        if i == "jmp":
            cur_pos += int(arg)
            continue

        assert False
    
    print(cur_acc)

    
if __name__ == "__main__":
    main()
