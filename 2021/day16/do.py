#!/usr/bin/env python3

# import numpy as np
# from collections import defaultdict
# import re

from bitarray import bitarray
import bitarray.util as bitutil

TYPE_SUM = 0
TYPE_PROD = 1
TYPE_MIN = 2
TYPE_MAX = 3
TYPE_LIT = 4
TYPE_GT = 5
TYPE_LT = 6
TYPE_EQ = 7

def op_gt(iter):
    x,y = next(iter), next(iter)
    return (x > y) * 1

def op_lt(iter):
    x,y = next(iter), next(iter)
    return (x < y) * 1

def op_eq(iter):
    x,y = next(iter), next(iter)
    return (x == y) * 1

import math
ops = {
    TYPE_SUM: sum,
    TYPE_PROD: math.prod,
    TYPE_MIN: min,
    TYPE_MAX: max,
    TYPE_GT: op_gt,
    TYPE_LT: op_lt,
    TYPE_EQ: op_eq}

class Packet:
    def __init__(self, version, type, bit_length, data):
        self.version = version
        self.type = type
        self.bit_len = bit_length
        self.data = data

        self.is_literal = type == TYPE_LIT

    def get_recursive(self):
        yield self

        if not self.is_literal:
            for p in self.data:
                for p2 in p.get_recursive():
                    yield p2

    def eval(self):
        if self.is_literal:
            return self.data
        else:
            return ops[self.type](p.eval() for p in self.data)
    

def decode_packet(bits):
    ver = bitutil.ba2int(bits[0:3])
    type = bitutil.ba2int(bits[3:6])

    if type == TYPE_LIT:
        literal = bitarray()
        pos = 6

        keep_going = True
        while keep_going:
            keep_going = bits[pos]
            literal += bits[pos+1:pos+5]
            pos += 5

        return Packet(ver, type, pos, bitutil.ba2int(literal))
    else:
        len_typ = bits[6]

        if len_typ:
            num_packets = bitutil.ba2int(bits[7:7+11])

            sub_packets = []
            pos = 7+11
            for _ in range(num_packets):
                next_packet = decode_packet(bits[pos:])
                sub_packets.append(next_packet)

                pos += next_packet.bit_len
        else:
            total_bits = bitutil.ba2int(bits[7:7+15])

            sub_packets, sub_packet_len = [], 0
            pos = 7+15
            while True:
                next_packet = decode_packet(bits[pos:])
                sub_packets.append(next_packet)

                pos += next_packet.bit_len
                sub_packet_len += next_packet.bit_len

                if sub_packet_len == total_bits:
                    break

                assert sub_packet_len < total_bits
        
        return Packet(ver, type, pos, sub_packets)


def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    bits = bitutil.hex2ba(inp[0])

    root = decode_packet(bits)

    sum_versions = sum(p.version for p in root.get_recursive())

    print(f"Puzzle1: {sum_versions}")

    print(f"Puzzle2: {root.eval()}")

    pass

if __name__ == "__main__": main()