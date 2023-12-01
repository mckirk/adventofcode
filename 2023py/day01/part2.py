#!/usr/bin/env python3
from pathlib import Path
import re

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample.txt"
input = input_file.read_text().strip()

def main():
    lines = input.split("\n")
    
    s = 0
    for line in lines:
        # find overlapping as well
        nums = re.findall(r"(?=(\d|one|two|three|four|five|six|seven|eight|nine))", line, )
        nums2 = []
        for n in nums:
            if n == "one":
                nums2.append("1")
            elif n == "two":
                nums2.append("2")
            elif n == "three":
                nums2.append("3")
            elif n == "four":
                nums2.append("4")
            elif n == "five":
                nums2.append("5")
            elif n == "six":
                nums2.append("6")
            elif n == "seven":
                nums2.append("7")
            elif n == "eight":
                nums2.append("8")
            elif n == "nine":
                nums2.append("9")
            else:
                nums2.append(n)

        cal = nums2[0] + nums2[-1]
        s += int(cal)

        # print(f"{line}: {nums} -> {nums2} -> {cal}")
    print(s)
    
if __name__ == "__main__":
    main()
