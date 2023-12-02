#!/usr/bin/env python3

import argparse
from datetime import datetime, timedelta
from enum import Enum
from functools import cache
from pathlib import Path
import re
import time
import html2text
from pydantic import BaseModel
import pytz
import requests
from tenacity import retry, stop_after_attempt, wait_random
import subprocess

TEMPLATE = """#!/usr/bin/env python3
from pathlib import Path

input_file = Path(__file__).parent / "input.txt"
# input_file = Path(__file__).parent / "sample1.txt"
input = input_file.read_text().strip()
lines = input.splitlines()

def main():
    pass
    
if __name__ == "__main__":
    main()
"""

DESC_REGEX = r"<main>(.*)</main>"
TIME_LEFT_REGEX = re.compile(r"You have ((?P<min>\d+)m )?((?P<sec>\d+)s )?left")

BAD_INPUT = "Please don't repeatedly request this endpoint before it unlocks!"


class Config(BaseModel):
    session: str


class SubmissionAnswer(Enum):
    CORRECT = 1
    TOO_HIGH = 2
    TOO_LOW = 3
    INCORRECT = 4
    TOO_RECENTLY = 5

script_dir = Path(__file__).parent
with open(script_dir/ "config.json", "r") as f:
    config = Config.model_validate_json(f.read())
tz = pytz.timezone("CET")


def create_argparser():
    parser = argparse.ArgumentParser(description="Advent of Code")
    parser.add_argument("--day", type=int, default=None)
    parser.add_argument("--year", type=int, default=None)
    return parser


def convert_html_to_markdown(html):
    h = html2text.HTML2Text()
    # Ignore converting links from HTML into Markdown
    h.ignore_links = True
    markdown = h.handle(html)
    return markdown


class AdventDay:
    def __init__(self, day: int, year: int):
        self.day = day
        self.year = year

        self.day_dir = script_dir.parent / f"{year}py" / f"day{day:02d}"

        self.part1_path = Path(self.day_dir) / "part1.py"
        self.part2_path = Path(self.day_dir) / "part2.py"
        self.input_path = Path(self.day_dir) / "input.txt"
        self.sample1_path = Path(self.day_dir) / "sample1.txt"
        self.sample2_path = Path(self.day_dir) / "sample2.txt"
        self.description_path = Path(self.day_dir) / "description.md"

        self.session = requests.Session()
        self.session.cookies.set("session", config.session)
        self.session.headers.update(
            {
                "User-Agent": "advent_bootstrap/0.0.1 (+https://github.com/mckirk/adventofcode)",
            }
        )

    def create_structure_part1(self):
        print(f"Creating directory {self.day_dir}...")
        self.day_dir.mkdir(exist_ok=True, parents=True)

        if not self.part1_path.exists():
            print(f"Creating part1.py...")
            self.part1_path.touch(exist_ok=False)
            self.part1_path.write_text(TEMPLATE)

        if not self.sample1_path.exists():
            print(f"Creating sample.txt...")
            self.sample1_path.touch(exist_ok=False)

    def create_structure_part2(self):
        # update description for part 2
        self.fetch_description()

        # copy part 1 to part 2
        if not self.part2_path.exists():
            print(f"Creating part2.py...")
            self.part2_path.touch(exist_ok=False)
            self.part2_path.write_text(self.part1_path.read_text().replace("sample1.txt", "sample2.txt"))

        if not self.sample2_path.exists():
            print(f"Creating sample2.txt...")
            self.sample2_path.touch(exist_ok=False)
            self.sample2_path.write_text(self.sample1_path.read_text())

    def wait_until_6am(self):
        wait_until = datetime(self.year, 12, self.day, 6, 0, 0, 0, tzinfo=tz)

        while (wait_delta := wait_until - datetime.now(tz)).total_seconds() > 0:
            print(f"Waiting {wait_delta.total_seconds()} seconds until 6am CET...")
            time.sleep(wait_delta.total_seconds())

    def fetch_input(self):
        if self.input_path.exists():
            print(f"Input already fetched!")
            return

        print(f"Fetching input...")

        @retry(stop=stop_after_attempt(3), wait=wait_random(min=1, max=2))
        def get_input():
            text = self.session.get(
                f"https://adventofcode.com/{self.year}/day/{self.day}/input"
            ).text

            if text.startswith(BAD_INPUT):
                raise Exception("Bad input!")

            return text

        self.input_path.write_text(get_input())

    def fetch_description(self):
        print(f"Fetching description...")

        @retry(stop=stop_after_attempt(3), wait=wait_random(min=1, max=2))
        def get_description():
            text = self.session.get(
                f"https://adventofcode.com/{self.year}/day/{self.day}"
            ).text
            return convert_html_to_markdown(re.findall(DESC_REGEX, text, re.DOTALL)[0])

        self.description_path.write_text(get_description())

    def start_editor(self):
        print(f"Starting editor...")
        subprocess.run(
            [
                "code",
                "-n",
                str(self.day_dir),
                str(self.part1_path),
                str(self.input_path),
                str(self.description_path),
            ]
        )

    @cache
    def submit_answer(self, part: int, answer: str):
        print(f"Submitting answer for part {part}...")
        resp = self.session.post(
            f"https://adventofcode.com/{self.year}/day/{self.day}/answer",
            data={"level": part, "answer": answer.strip()},
        )

        if resp.status_code != 200:
            raise Exception(f"Unexpected status code {resp.status_code}!")

        content = resp.text

        if "That's the right answer!" in content:
            return SubmissionAnswer.CORRECT, None
        elif "too high" in content:
            return SubmissionAnswer.TOO_HIGH, None
        elif "too low" in content:
            return SubmissionAnswer.TOO_LOW, None
        elif "That's not the right answer" in content:
            return SubmissionAnswer.INCORRECT, None
        elif "You don't seem to be solving the right level." in content:
            raise Exception("You don't seem to be solving the right level.")
        else:
            time_left_match = TIME_LEFT_REGEX.search(content)
            if time_left_match:
                min = int(time_left_match.group("min") or 0)
                sec = int(time_left_match.group("sec") or 0)
                return SubmissionAnswer.TOO_RECENTLY, datetime.now(tz) + timedelta(
                    minutes=min, seconds=sec
                )
            else:
                raise Exception("Could not interpret the submission result")

    def submit_answers_until_correct(self):
        for part in [1, 2]:
            if part == 2:
                self.create_structure_part2()

            while True:
                print(f"Enter answer for part {part}:")
                part_answer = input()

                answer, next_submit_time = self.submit_answer(part, part_answer)

                if answer == SubmissionAnswer.CORRECT:
                    print(f"Correct!")
                    break
                elif answer == SubmissionAnswer.TOO_HIGH:
                    print(f"Too high!")
                elif answer == SubmissionAnswer.TOO_LOW:
                    print(f"Too low!")
                elif answer == SubmissionAnswer.INCORRECT:
                    print(f"Incorrect!")
                elif answer == SubmissionAnswer.TOO_RECENTLY:
                    print(
                        f"Too recently submitted, waiting until {next_submit_time}..."
                    )
                    delta = (next_submit_time - datetime.now(tz)).total_seconds()
                    if delta > 0:
                        time.sleep(delta)
                else:
                    raise Exception("Unexpected submission answer!")

        print("gg")


def main():
    parser = create_argparser()
    args = parser.parse_args()

    now = datetime.now(tz)
    day = args.day or now.day
    year = args.year or now.year

    advent_day = AdventDay(day, year)

    advent_day.create_structure_part1()
    advent_day.wait_until_6am()
    advent_day.fetch_description()
    advent_day.fetch_input()
    advent_day.start_editor()
    advent_day.submit_answers_until_correct()


if __name__ == "__main__":
    main()
