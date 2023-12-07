#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

strengths = "AKQJT98765432"

def get_type(hand):
    counts = [hand.count(c) for c in hand]
    counts.sort(reverse=True)
    if len(set(hand)) == 1:
        return (7, "five of a kind")
    elif len(set(hand)) == 2:
        return (6, "four of a kind") if counts[0] == 4 else (5, "full house")
    elif len(set(hand)) == 3:
        return (4, "three of a kind") if counts[0] == 3 else (3, "two pair")
    elif len(set(hand)) == 4:
        return (2, "one pair")
    else:
        return (1, "high card")

def main():
    hands = []
    for line in lines:
        hand, bid = line.split()
        type, label = get_type(hand)
        print(f"{hand} {label} {type}")
        hands.append((type, [-strengths.index(c) for c in hand], bid, hand))

    hands.sort()

    res = 0
    for rank, (_, _, bid, _) in enumerate(hands, start=1):
        res += int(bid)*rank

    print(res)
    
if __name__ == "__main__":
    main()
