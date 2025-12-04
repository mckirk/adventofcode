from pathlib import Path
from typing import Any, Optional
import time

from prefixed import Float

from .pos import V
from .submit import submit_answer
import aocparser

script_dir = Path(__file__).parent.parent
default_spec_name = "spec.aocp"


class Input:
    def __init__(self, content: Optional[str], spec_name: str | None = None):
        self.content = content

        if not content:
            return

        self.lines = content.splitlines()
        self.blocks = content.split("\n\n")

        if (
            spec_name
            and (spec_file := script_dir / spec_name).is_file()
            and (spec := spec_file.read_text())
        ):
            self.parsed = aocparser.parse(spec, self.content)
        else:
            self.parsed = None

    @property
    def parsed_definite(self):
        assert self.parsed is not None
        return self.parsed

    @classmethod
    def from_file(cls, path: Path, spec_name: str | None = None):
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
                yield V(x, y), c


problem_input = Input.from_file(script_dir / "input.txt", default_spec_name)


def get_sample_input(idx: int):
    return Input.from_file(script_dir / f"sample{idx}.txt", default_spec_name)


def run_on_inputs(run, expected_sample_results: dict[int, Any] | None = None, *, auto_submit: bool | None = None, **kwargs):
    expected_sample_results = expected_sample_results or dict()

    # By default, auto-submit if we have an expected result for a sample input
    if auto_submit is None:
        auto_submit = any(v is not None for v in expected_sample_results.values())

    def get_run_args(key):
        return {k: d.get(key) for k, d in kwargs.items()}

    def run_with_timing(label: str, inp: Input, **run_args):
        start = time.perf_counter()
        res = run(inp, **run_args)
        end = time.perf_counter()
        duration = Float(end - start)
        print(f"{label}: {res} ({duration:.2h}s)")
        return res

    for i, exp in expected_sample_results.items():
        inp = get_sample_input(i)
        if inp.exists:
            res = run_with_timing(f"Sample {i}", inp, **get_run_args(i))
            if exp is not None and res != exp:
                print(
                    f"Did not match expected result for sample {i} ({exp}), aborting."
                )
                return

    res = run_with_timing("Result", problem_input, **get_run_args(None))

    if auto_submit:
        submit_answer(31337, str(res))

    return res
