#!/usr/bin/env python3
from collections import defaultdict, Counter
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = """\
[<broad:{origin:w}|flip:%{origin:w}|conj:&{origin:w}> -`> {dst:[{w}|,]}|\n]"""


LOW = False
HIGH = True


@dataclass
class Module:
    name: str
    children: dict[str, "Module"]

    def receive(self, pulse, origin):
        ...


@dataclass
class ToDo:
    origin: str
    dst: list[str]
    pulse: bool

    def __str__(self):
        return f"{self.origin.name} -{'high' if self.pulse else 'low'}-> {self.dst}"


class Broadcaster(Module):
    def receive(self, pulse, origin):
        return ToDo(self, self.children.keys(), pulse)


@dataclass
class FlipFlop(Module):
    state: bool = False

    def receive(self, pulse, origin):
        if pulse == HIGH:
            return None

        self.state = not self.state
        return ToDo(self, self.children.keys(), self.state)


@dataclass
class Conj(Module):
    states: dict[str, bool]

    def receive(self, pulse, origin):
        self.states[origin.name] = pulse

        return ToDo(
            self, self.children.keys(), LOW if all(self.states.values()) else HIGH
        )


def main():
    module_def = parse(spec, input)

    modules: dict[str, Module] = dict()

    for m in module_def:
        kind = m[0]

        if kind.flip:
            mod = FlipFlop(kind.origin, dict())
        if kind.conj:
            mod = Conj(kind.origin, dict(), dict())
        if kind.broad:
            mod = Broadcaster(kind.origin, dict())

        modules[kind.origin] = mod

    for m in module_def:
        kind = m[0]

        for d in m.dst:
            mod = modules.get(d)
            if mod is None:
                modules[d] = mod = Module(d, dict())
            if isinstance(mod, Conj):
                mod.states[kind.origin] = False

        modules[kind.origin].children = {d: modules[d] for d in m.dst}

    lows, highs = 0, 0

    for i in range(1000):
        todo = [ToDo(Module("button", dict()), ["broadcaster"], LOW)]
        while todo:
            t = todo.pop(0)
            for d in t.dst:
                # print(f"{t.origin.name} -{'high' if t.pulse else 'low'}-> {d}")
                new_todo = modules[d].receive(t.pulse, t.origin)
                if new_todo:
                    todo.append(new_todo)
                if t.pulse == LOW:
                    lows += 1
                if t.pulse == HIGH:
                    highs += 1

    print(lows, highs, lows * highs)


if __name__ == "__main__":
    main()
