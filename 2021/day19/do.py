#!/usr/bin/env python3

import numpy as np
# from collections import defaultdict
from collections import defaultdict
import re

toy_input = """--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14"""

def canonical_delta(relpos1: np.ndarray, relpos2: np.ndarray):
    delta = np.abs(relpos1 - relpos2)

    return frozenset(delta)

def invert_permutation(p):
    '''The argument p is assumed to be some permutation of 0, 1, ..., len(p)-1. 
    Returns an array s, where s[i] gives the index of i in p.
    '''
    s = np.empty_like(p)
    s[p] = np.arange(p.size)
    return s

class RelOrient:
    def __init__(self, delta, ax_assignments, flipped):
        self.delta = tuple(delta)
        self.ax_assignments = tuple(ax_assignments)
        self.flipped = tuple(flipped)

        self.factors = tuple(((flip * -2) + 1) for flip in flipped)

    @staticmethod
    def find(scanner1_rps, scanner2_rps):
        delta1 = scanner1_rps[1] - scanner1_rps[0]
        delta2 = scanner2_rps[1] - scanner2_rps[0]

        ax_assignment = []
        flipped = []

        for d in delta1:
            if d == 0: return None

            abs_d = abs(d)
            candidates = [i for i in range(3) if abs(delta2[i]) == abs_d]

            if len(candidates) != 1: return None

            ax_scanner2 = candidates[0]
            d_scanner2 = delta2[ax_scanner2]

            ax_assignment.append(ax_scanner2)
            flipped.append((d * d_scanner2) < 1)

        scanner2_rp = scanner2_rps[0]
        factors = (np.ones_like(scanner2_rp) * -2 * flipped) + 1

        scanner2_rp_projected = scanner2_rp[ax_assignment] * factors
        delta = scanner2_rp_projected - scanner1_rps[0]

        return RelOrient(-delta, ax_assignment, flipped)

    # def flip(self):
    #     return RelOrient(self.ax_assignments, [not f for f in self.flipped])

    # def inverse(self):
    #     inv_assign = invert_permutation(self.ax_assignments)

    #     return RelOrient(inv_assign, self.flipped)

    def chain(self, second: "RelOrient"):
        coords_proj = self.apply(second.delta)

        ax_assignments = [second.ax_assignments[ass] for ass in self.ax_assignments]
        flipped =  [second.flipped[ass] ^ flip for (ass, flip) in zip(self.ax_assignments, self.flipped)]

        return RelOrient(coords_proj, ax_assignments, flipped)

    def apply(self, coordinates):
        new_coord = tuple(d + (coordinates[ass] * factor) for (d, ass, factor) in zip(self.delta, self.ax_assignments, self.factors))

        return np.array(new_coord, dtype=int)

    def __hash__(self):
        return hash((self.delta, self.ax_assignments, self.flipped))

    def __eq__(self, other):
        return self.delta == other.delta and self.ax_assignments == other.ax_assignments and self.flipped == other.flipped

    def __str__(self):
        return "delta ({}), axes ({})".format(",".join(str(n) for n in self.delta), ",".join(str(ass * factor) for (ass, factor) in zip(self.ax_assignments, self.factors)))

    def __repr__(self):
        return str(self)

def set_unique(dict, key, val):
    prev = dict.get(key)
    if prev is not None:
        assert np.array_equal(prev, val)
    else:
        dict[key] = val


class Scanner:
    def __init__(self, id):
        self.id = id
        self.rel_poses_beacon = []

        self.known_beacons = set()

        self.delta_db = dict()

        self.possible_rel_pos_of_scanner = defaultdict(lambda: defaultdict(lambda: 0))
        self.rel_pos_of_scanner = dict()

    def add_rel_pos_beacon(self, rel_pos):
        self.rel_poses_beacon.append(rel_pos)
        self.known_beacons.add(tuple(rel_pos))

    def import_beacons(self, other: "Scanner"):
        projection = self.rel_pos_of_scanner.get(other)
        assert projection is not None

        for other_rp in other.rel_poses_beacon:
            projected = projection.apply(other_rp)

            self.add_rel_pos_beacon(projected)

    def build_delta_db(self):
        done = set()
        ambiguous = set()

        for rp1 in self.rel_poses_beacon:
            done.add(id(rp1))

            for rp2 in self.rel_poses_beacon:
                if id(rp2) in done: continue

                can_delta = canonical_delta(rp1, rp2)

                if can_delta in self.delta_db.keys():
                    ambiguous.add(can_delta)
                    continue

                self.delta_db[canonical_delta(rp1, rp2)] = (rp1, rp2)

        # remove deltas that could belong to multiple beacon pairs
        for k in ambiguous:
            self.delta_db.pop(k)

    def find_pos_relative_to_scanner(self, other: "Scanner"):
        overlap = self.delta_db.keys() & other.delta_db.keys()
        if len(overlap) == 0:
            return None

        rp_assignments = dict()
        
        for k in overlap:
            my_rps = self.delta_db[k]
            other_rps = other.delta_db[k]

            self.collect_possible_rel_pos(other, my_rps, other_rps)
            other.collect_possible_rel_pos(self, other_rps, my_rps)

        self.set_rel_pos(other)
        other.set_rel_pos(self)

        pass

    def collect_possible_rel_pos(self, other, my_rps, other_rps):
        other_rps_flipped = list(reversed(other_rps))
        rel_orient = RelOrient.find(my_rps, other_rps)
        rel_orient_flipped = RelOrient.find(my_rps, other_rps_flipped)

        if rel_orient is None:
            return

        # if np.array_equal(*rp_delta):
        #     rel_pos = rp_delta[0]
        # else:
        #     assert np.array_equal(*rp_delta_flipped)
        #     rel_pos = rp_delta_flipped[0]
        #     rel_orient = rel_orient.flip()

        self.possible_rel_pos_of_scanner[other][rel_orient] += 1
        self.possible_rel_pos_of_scanner[other][rel_orient_flipped] += 1

        # set_unique(self.rel_pos_of_scanner, other, rel_pos)
        # set_unique(self.rel_orient_to_scanner, other, rel_orient)

    def set_rel_pos(self, other):
        possible_rel_pos = list(self.possible_rel_pos_of_scanner[other].items())
        possible_rel_pos.sort(key=lambda x: x[1])

        self.rel_pos_of_scanner[other] = possible_rel_pos[-1][0]

    def known_scanners(self):
        return self.rel_pos_of_scanner.keys()

    def has_rel_pos(self, other):
        return other in self.known_scanners()

    def new_scanners(self, known):
        return self.known_scanners() - known


def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    scanners: "list[Scanner]" = []
    cur_scanner = None
    for l in inp:
        m = re.match("--- scanner (\d+) ---", l)
        if m:
            if cur_scanner:
                scanners.append(cur_scanner)
            
            cur_scanner = Scanner(m.group(1))
        elif l == "":
            continue
        else:
            rel_pos = np.array([int(n) for n in l.split(",")], dtype=int)
            cur_scanner.add_rel_pos_beacon(rel_pos)

    scanners.append(cur_scanner)

    for s in scanners:
        s.build_delta_db()

    done = set()
    for s1 in scanners:
        done.add(id(s1))
        for s2 in scanners:
            if id(s2) in done: continue

            s1.find_pos_relative_to_scanner(s2)

    base_scanner = scanners[0]

    while len(base_scanner.rel_pos_of_scanner) < len(scanners):
        for s in scanners:
            if s is base_scanner:
                continue

            if not base_scanner.has_rel_pos(s):
                continue

            new_scanners = s.new_scanners(base_scanner.known_scanners())

            if len(new_scanners) == 0:
                continue

            for ns in new_scanners:
                rel_pos_from_s = s.rel_pos_of_scanner[ns]
                proj_to_base = base_scanner.rel_pos_of_scanner[s].chain(rel_pos_from_s)

                base_scanner.rel_pos_of_scanner[ns] = proj_to_base

    for s in scanners:
        if s is base_scanner: continue

        base_scanner.import_beacons(s)

    print(f"Puzzle1: {len(base_scanner.known_beacons)}")

    distances = dict()
    for s1 in scanners:
        for s2 in scanners:
            key = frozenset([s1, s2])
            if key in distances.keys():
                continue

            s1_pos = base_scanner.rel_pos_of_scanner[s1].delta
            s2_pos = base_scanner.rel_pos_of_scanner[s2].delta

            dist = sum(abs(a-b) for (a,b) in zip(s1_pos, s2_pos))

            distances[key] = dist

    print(f"Puzzle2: {max(distances.values())}")

    pass

if __name__ == "__main__": main()