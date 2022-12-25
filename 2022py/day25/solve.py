#!/usr/bin/env python3

from os import path

fuel = []

m = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}
inv = {0: "0", 1: "1", 2: "2", 3: "=", 4: "-"}


def to_snafu(n):
    s = ""
    while n:
        mod = n % 5
        c = inv[mod]
        i = m[c]
        s += c
        n -= i
        n //= 5

    return "".join(reversed(s))


with open(path.join(path.dirname(__file__), "input.txt")) as f:
    ls = f.readlines()
    for l in ls:
        n = 0
        mul = 1
        for c in reversed(l.strip()):
            n += m[c]*mul
            mul *= 5
        fuel += [n]


print(sum(fuel))
print(to_snafu(sum(fuel)))
