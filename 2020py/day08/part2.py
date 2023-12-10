#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def run(ins, start_pos, start_acc):
    acc_by_pos = dict()
    cur_acc = start_acc
    cur_pos = start_pos
    while True:
        if cur_pos in acc_by_pos:
            return None
        acc_by_pos[cur_pos] = cur_acc

        if cur_pos >= len(ins):
            return cur_acc

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

def fix_bug(ins, good_pos):
    cur_acc = 0
    cur_pos = 0
    while True:
        if cur_pos >= len(ins):
            assert False

        i, arg = ins[cur_pos]

        if i == "nop":
            if cur_pos+int(arg) in good_pos:
                return run(ins, cur_pos+int(arg), cur_acc)
            cur_pos += 1
            
            continue

        if i == "acc":
            cur_pos += 1
            cur_acc += int(arg)
            continue

        if i == "jmp":
            if cur_pos + 1 in good_pos:
                return run(ins, cur_pos+1, cur_acc)
            cur_pos += int(arg)
            continue

        assert False


def main():
    ins = []
    for l in lines:
        ins.append(l.split(" "))

    terminates = set([len(lines)])
    for i in range(len(ins)):
        res = run(ins, i, 0)
        if res is not None:
            terminates.add(i)

    print(terminates)
    print(fix_bug(ins, terminates))

    
if __name__ == "__main__":
    main()
