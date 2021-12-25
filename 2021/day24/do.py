#!/usr/bin/env python3

import z3
from z3.z3 import simplify

def funcify():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    blocks = []
    block = None
    for l in inp:
        if l.startswith("inp"):
            if block is not None:
                blocks.append(block)
            block = []

        block.append(l.split(" "))

    blocks.append(block)

    def collapse(lst):
        if all(e == lst[0] for e in lst):
            return lst[0]
        else:
            return lst

    combined = []
    for i in range(len(blocks[0])):
        instrs = [blocks[j][i] for j in range(len(blocks))]
        combined.append([collapse(parts) for parts in zip(*instrs)])

    print(f"for i in range({len(blocks)}):")

    tbl = {"mul": "*", "add": "+", "mod": "%", "div": "//", "eql": "=="}
    for instr in combined:
        if instr[0] == "inp":
            print(f"    {instr[1]} = input[i]")
        else:
            if type(instr[2]) is tuple:
                print(f"    {instr[1]} = {instr[1]} {tbl[instr[0]]} {[int(n) for n in instr[2]]}[i]")
            else:
                print(f"    {instr[1]} = {instr[1]} {tbl[instr[0]]} {instr[2]}")

    pass

def func_z3(input):
    constraints = []

    w,x,y,z = 0,0,0,0
    for i in range(14):
        w = input[i]
        x = z % 26
        z = z / [1, 1, 1, 26, 1, 1, 1, 26, 26, 1, 26, 26, 26, 26][i]
        x_add = [13, 11, 14, -5, 14, 10, 12, -14, -8, 13, 0, -5, -9, -1][i]
        x = x + x_add
        y = 26*z + w + [0, 3, 8, 5, 13, 9, 6, 1, 1, 2, 7, 5, 8, 15][i]
        if x_add <= 0:
            # print(f"Step {i}: x = {z3.simplify(x)}")
            # z = z3.If(x != w, y, z)
            constraints.append(x == w)
        else:
            z = y

        z = z3.simplify(z)

    return (z, constraints)

def func(input):
    w, x, y, z = 0, 0, 0, 0
    for i in range(14):
        w = input[i]
        x = z
        x = x % 26
        z = z // [1, 1, 1, 26, 1, 1, 1, 26, 26, 1, 26, 26, 26, 26][i]
        x = x + [13, 11, 14, -5, 14, 10, 12, -14, -8, 13, 0, -5, -9, -1][i]
        x = x != w
        y = 25 * x
        y = y + 1
        z = z * y
        y = w
        y = y + [0, 3, 8, 5, 13, 9, 6, 1, 1, 2, 7, 5, 8, 15][i]
        y = y * x
        z = z + y

    return z
        
def func2(input):
    w, x, y, z = 0, 0, 0, 0
    for i in range(14):
        w = input[i]
        x = (z % 26) + [13, 11, 14, -5, 14, 10, 12, -14, -8, 13, 0, -5, -9, -1][i]
        z = z // [1, 1, 1, 26, 1, 1, 1, 26, 26, 1, 26, 26, 26, 26][i]

        if w is None:
            if x < 1 or x > 9:
                return None
            w = input[i] = x

        if x != w:
            z *= 26
            z += w + [0, 3, 8, 5, 13, 9, 6, 1, 1, 2, 7, 5, 8, 15][i]

    return z

def main():
    determ_indices = [3, 7, 8, 10, 11, 12, 13]
    remaining = [i for i in range(14) if i not in determ_indices]

    from itertools import product
    def brute(nums):
        for x in product(*[nums]*len(remaining)):
            input = [None]*14
            for n, ri in zip(x, remaining):
                input[ri] = n

            if func2(input) == 0:
                print("".join(str(n) for n in input))
                break

    brute(list(range(9, 0, -1)))
    brute(list(range(1, 10, 1)))

    # input = [int(n) for n in "27141191213911"]
    # func2(input)
    
    # input = [9,9,9] + [z3.Int(f"i{i}") for i in range(11)]
    input = [z3.Int(f"i{i}") for i in range(14)] #[z3.BitVec(f"i{i}", 50) for i in range(14)]
    constrs = [i >= 1 for i in input] + [i <= 9 for i in input]

    (z, add_constrs) = func_z3(input)
    measure = sum(inp*(10**f) for (inp, f) in zip(input, range(14, -1, -1)))

    opt = z3.Optimize()
    opt.add(z == 0, *constrs, *add_constrs)
    opt.maximize(measure)
    r = opt.check()
    if r == z3.sat:
        m = opt.model()
        print("".join(f"{m[inp]}" for inp in input))

    opt = z3.Optimize()
    opt.add(z == 0, *constrs, *add_constrs)
    opt.minimize(measure)
    r = opt.check()
    if r == z3.sat:
        m = opt.model()
        print("".join(f"{m[inp]}" for inp in input))


if __name__ == "__main__": main()
