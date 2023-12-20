from math import gcd
from functools import reduce


def lcm(a, b):
    return a * b // gcd(a, b)


def lcmm(*args):
    return reduce(lcm, args)