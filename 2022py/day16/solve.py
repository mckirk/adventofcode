#!/usr/bin/env python3

from collections import defaultdict
from os import path
import re


def ints(string):
    return list(map(int, re.findall(r"-?[0-9]+", string)))


with open(path.join(path.dirname(__file__), "input.txt")) as f:
    valves = dict()
    for l in f.readlines():
        ws = l[:-1].split(" ")
        valves[ws[1]] = (ints(l)[0], "".join(ws[9:]).split(","))


def get_reachable(valve, seen, cur_time, reachable):
    if valve not in reachable or reachable[valve] > cur_time:
        reachable[valve] = cur_time

    _, next_valves = valves[valve]
    for v in next_valves:
        if v not in seen:
            get_reachable(v, seen | set([valve]), cur_time+1, reachable)

    return reachable


reachable = {v: get_reachable(v, set(), 0, dict()) for v in valves.keys()}
openable = set(v for (v, (flow_rate, _)) in valves.items() if flow_rate > 0)

assert len(set(len(r) for r in reachable.values())) == 1


def get_flown(flow_rates, time):
    flown = 0
    t = 0
    cur_rate = 0
    for start, rate in sorted(flow_rates):
        elapsed = start - t
        flown += cur_rate * elapsed
        cur_rate += rate
        t = start
    elapsed = time - t
    flown += cur_rate * elapsed
    return flown


def solve(limit, count):
    states = [([("AA", 0)]*count, [], frozenset())]

    max_flown = 0

    best = defaultdict(lambda: -1)

    while states:
        valve_times, flow_rates, opened = states.pop()
        times = tuple(time for _, time in valve_times)
        flown = get_flown(flow_rates, limit)

        best_for = best[(times, opened)]
        if best_for > flown:
            continue
        best[(times, opened)] = flown

        if all(t >= limit for t in times):
            if flown > max_flown:
                max_flown = flown
            continue

        have_next = False

        to_move = min((t, i) for i, t in enumerate(times))[1]

        valve, time = valve_times[to_move]

        rem = limit - time

        for v in openable:
            if v in opened:
                continue

            cost = reachable[valve][v] + 1

            if cost > rem:
                continue

            have_next = True

            v_rate, _ = valves[v]

            new_valve_times = list(valve_times)
            new_valve_times[to_move] = (v, time+cost)

            new_flow_rates = flow_rates + [(time+cost, v_rate)]

            states.append((new_valve_times, new_flow_rates,
                          opened | frozenset([v])))

        if not have_next:
            new_valve_times = list(valve_times)
            new_valve_times[to_move] = (valve, limit)

            states.append((new_valve_times, flow_rates, opened))

    return max_flown


print(solve(30, 1))
print(solve(26, 2))
