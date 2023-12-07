#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

strengths = "AKQT98765432J"

def get_type(hand):
    hand = "".join(c for c in hand if c != "J")

    counts = [hand.count(c) for c in hand]
    counts.sort(reverse=True)
    if len(set(hand)) == 1 or len(set(hand)) == 0:
        return (7, "five of a kind")
    elif len(set(hand)) == 2:
        return (6, "four of a kind") if counts[0] == (len(hand)-1) else (5, "full house")
    elif len(set(hand)) == 3:
        return (4, "three of a kind") if counts[0] == (len(hand)-2) else (3, "two pair")
    elif len(set(hand)) == 4:
        return (2, "one pair")
    else:
        return (1, "high card")
    
def test_get_type():
    assert get_type("AAAAA") == (7, "five of a kind")
    assert get_type("AAAAJ") == (7, "five of a kind")
    assert get_type("AAAJJ") == (7, "five of a kind")
    assert get_type("AAJJJ") == (7, "five of a kind")
    assert get_type("AJJJJ") == (7, "five of a kind")
    assert get_type("JJJJJ") == (7, "five of a kind")

def main():
    test_get_type()

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
