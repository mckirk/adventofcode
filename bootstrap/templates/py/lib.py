from pathlib import Path

import aocparser

script_dir = Path(__file__).parent
spec_file = script_dir / "spec.aocp"


class Input:
    def __init__(self, path: Path):
        self.path = path
        if not path.is_file():
            self.content = None
            return

        self.content = path.read_text().strip()
        self.lines = self.content.splitlines()
        self.blocks = self.content.split(r"\n\n")

        if spec_file.is_file() and (spec := spec_file.read_text()):
            self.parsed = aocparser.parse(spec, self.content)
        else:
            self.parsed = None

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


problem_input = Input(script_dir / "input.txt")
sample1_input = Input(script_dir / "sample1.txt")
sample2_input = Input(script_dir / "sample2.txt")


def run_on_inputs(sample1_expected, sample2_expected, run):
    for i, (inp, exp) in enumerate(
        [(sample1_input, sample1_expected), (sample2_input, sample2_expected)], start=1
    ):
        if inp.exists:
            res = run(inp)
            print(f"Sample {i}: {res}")
            if exp is not None and res != exp:
                print(
                    f"Did not match expected result for sample {i} ({exp}), aborting."
                )
                return

    res = run(problem_input)
    print(f"Result: {res}")
