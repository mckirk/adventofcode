#!/usr/bin/env python3

import numpy as np

def setify(chars):
    return frozenset(c for c in chars)

digit_def = {
    0: setify("abcefg"),
    1: setify("cf"),
    2: setify("acdeg"),
    3: setify("acdfg"),
    4: setify("bcdf"),
    5: setify("abdfg"),
    6: setify("abdefg"),
    7: setify("acf"),
    8: setify("abcdefg"),
    9: setify("abcdfg")}

def solve_entry(l):
    examples, output = [[frozenset(s) for s in part.split()] for part in l.split(" | ")]

    examples.sort(key = len)

    found_translations: dict[frozenset, set] = dict()

    def add_transl(new_garbled: frozenset, new_real: set):
        if new_garbled in found_translations.keys():
            return

        new_translations: dict[frozenset, set] = dict(found_translations)

        for found_garbled, found_real in found_translations.items():
            resolved_garbled = found_garbled.symmetric_difference(new_garbled)
            resolved_real = found_real.symmetric_difference(new_real)

            new_translations[resolved_garbled] = resolved_real
        
        new_translations[new_garbled] = new_real

        return new_translations

    def after_transl(garbled, possible_real):
        g, r = set(garbled), set(possible_real)
        for tg, tr in found_translations.items():
            if tg.issubset(g):
                g -= tg

                if not tr.issubset(r):
                    return None, None
                else:
                    r -= tr

        return g, r

    def apply_transl(garbled):
        real = set()
        garbled_remaining = set(garbled)

        for tg, tr in found_translations.items():
            if tg.issubset(garbled):
                real |= tr
                garbled_remaining -= tg

        assert len(garbled_remaining) == 0

        return real

    for example_garbled in examples:
        if example_garbled in found_translations.keys(): continue

        # print(f"Trying to get information from {example_garbled}...")

        possibles_new_transl = []

        for digit, real_for_digit in digit_def.items():
            if len(example_garbled) == len(real_for_digit):
                # print(f"Trying digit {real_for_digit}...")
                remaining_garbled, remaining_real = after_transl(example_garbled, real_for_digit)

                if remaining_garbled is None: continue

                # print(f"Possible new translation: {remaining_garbled} = {remaining_real}")

                possibles_new_transl.append((remaining_garbled, remaining_real))


        if len(possibles_new_transl) == 1:
            new_garbled, new_real = possibles_new_transl[0]
            found_translations = add_transl(frozenset(new_garbled), new_real)

    digits = []
    for digit_garbled in output:
        segments_real = apply_transl(digit_garbled)

        for digit, segments in digit_def.items():
            if segments == segments_real:
                digits.append(digit)
                break

    assert len(digits) == len(output)
    
    return digits

def main():
    with open("./input.txt", "r") as f:
        inp = list(f.read().strip().splitlines())

    all_digits = []
    sum = 0

    import functools

    for l in inp:
        digits = solve_entry(l)
        all_digits += digits

        num = functools.reduce(lambda s, x: s*10 + x, digits, 0)
        sum += num

    print(f"Puzzle 1: {all_digits.count(1) + all_digits.count(4) + all_digits.count(7) + all_digits.count(8)}")
    print(f"Puzzle 2: {sum}")

    pass

if __name__ == "__main__": main()