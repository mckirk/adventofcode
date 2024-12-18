from pathlib import Path
from typing import Any, Optional
from collections import defaultdict, Counter

import aocparser

script_dir = Path(__file__).parent.parent
default_spec_name = "spec.aocp"


class Input:
    def __init__(self, content: Optional[str], spec_name: str = None):
        self.content = content

        if not content:
            return

        self.lines = self.content.splitlines()
        self.blocks = self.content.split("\n\n")

        if (
            spec_name
            and (spec_file := script_dir / spec_name).is_file()
            and (spec := spec_file.read_text())
        ):
            self.parsed = aocparser.parse(spec, self.content)
        else:
            self.parsed = None

    @classmethod
    def from_file(cls, path: Path, spec_name: str = None):
        if not path.is_file():
            return cls(None)
        else:
            return cls(path.read_text().strip(), spec_name)

    @property
    def exists(self):
        return bool(self.content)

    @property
    def limits(self):
        return (len(self.lines[0]), len(self.lines))

    @property
    def as_pos(self):
        for y, l in enumerate(self.lines):
            for x, c in enumerate(l):
                yield (x, y), c


problem_input = Input.from_file(script_dir / "input.txt", default_spec_name)


def get_sample_input(idx: int):
    return Input.from_file(script_dir / f"sample{idx}.txt", default_spec_name)


def run_on_inputs(run, expected_sample_results: dict[int, Any] = None, **kwargs):
    expected_sample_results = expected_sample_results or dict()

    def get_run_args(key):
        return {k: d.get(key) for k, d in kwargs.items()}

    for i, exp in expected_sample_results.items():
        inp = get_sample_input(i)
        if inp.exists:
            res = run(inp, **get_run_args(i))
            print(f"Sample {i}: {res}")
            if exp is not None and res != exp:
                print(
                    f"Did not match expected result for sample {i} ({exp}), aborting."
                )
                return

    res = run(problem_input, **get_run_args(None))
    print(f"Result: {res}")
