#!/usr/bin/env python3
from collections import defaultdict, Counter
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from pprint import pprint
from aocparser import parse

from lib import *

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample2.txt"
input = input_file.read_text().strip()
lines = input.splitlines()
blocks = input.split("\n\n")

spec = """\
[<broad:{origin:w}|flip:%{origin:w}|conj:&{origin:w}> -\\> {dst:[{w}|, ]}|\n]"""


LOW = False
HIGH = True


@dataclass
class ToDo:
    origin: str
    dst: list[str]
    pulse: bool

    def __str__(self):
        return f"{self.origin.name} -{'high' if self.pulse else 'low'}-> {self.dst}"


@dataclass
class Module:
    name: str
    children: dict[str, "Module"] = field(default_factory=dict)
    inputs: list["Module"] = field(default_factory=list)
    sim_step = 1

    def add_input(self, module):
        self.inputs.append(module)

    def receive(self, pulse, origin):
        ...


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
    states: dict[str, bool] = field(default_factory=dict)

    def add_input(self, module):
        super().add_input(module)
        self.states[module.name] = False

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
            mod = FlipFlop(kind.origin)
        if kind.conj:
            mod = Conj(kind.origin)
        if kind.broad:
            mod = Broadcaster(kind.origin)

        modules[kind.origin] = mod

    for m in module_def:
        kind = m[0]

        for d in m.dst:
            mod = modules.get(d)
            if mod is None:
                modules[d] = mod = Module(d)
            mod.add_input(modules[kind.origin])

        modules[kind.origin].children = {d: modules[d] for d in m.dst}

    num_presses = 0
    cycles = dict()
    rx_inputs = [m.name for m in modules["rx"].inputs[0].inputs]
    print(rx_inputs)
    while True:
        todo = [ToDo(Module("button", dict()), ["broadcaster"], LOW)]
        num_presses += 1
        while todo:
            t = todo.pop(0)
            for d in t.dst:
                # print(f"{t.origin.name} -{'high' if t.pulse else 'low'}-> {d}")
                new_todo = modules[d].receive(t.pulse, t.origin)
                if new_todo:
                    todo.append(new_todo)

                if d in rx_inputs and t.pulse == LOW:
                    print(
                        f"{t.origin.name} -{'high' if t.pulse else 'low'}-> {d} at {num_presses}"
                    )
                    cycles[t.origin.name] = num_presses

        if len(cycles) == len(rx_inputs):
            break

    print(cycles, lcmm(*cycles.values()))


if __name__ == "__main__":
    main()
