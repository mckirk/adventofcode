#!/usr/bin/env python3

def find_mostleast(ns):
    bit_count = [0]*len(ns[0])

    for l in ns:
        for i,b in enumerate(l):
            if b == '1': bit_count[i] += 1
            elif b == '0': bit_count[i] -= 1

    most_bits = "".join("01"[bc >= 0] for bc in bit_count)
    least_bits = "".join("01"[bc < 0] for bc in bit_count)

    return most_bits, least_bits


def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())

    most_bits, least_bits = find_mostleast(inp)
    gamma, epsilon = int(most_bits, 2), int(least_bits, 2)

    print(f"Puzzle 1: {gamma*epsilon}")

    print(f"Gamma: {gamma:012b}, epsilon: {epsilon:012b}")

    oxy_cand = list(inp)
    co2_cand = list(inp)

    pos = 0
    while len(oxy_cand) > 1:
        most_bits, least_bits = find_mostleast(oxy_cand)
        oxy_cand = [oc for oc in oxy_cand if oc[pos] == most_bits[pos]]
        pos += 1

    pos = 0
    while len(co2_cand) > 1:
        most_bits, least_bits = find_mostleast(co2_cand)
        co2_cand = [oc for oc in co2_cand if oc[pos] == least_bits[pos]]
        pos += 1

    oxy, co2 = int(oxy_cand[0], 2), int(co2_cand[0], 2)
    print(f"Puzzle 2: {oxy*co2}")


if __name__ == "__main__": main()