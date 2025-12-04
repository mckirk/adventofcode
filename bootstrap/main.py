#!/usr/bin/env python3

import argparse
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
import re
import shutil
import time
import html2text
from pydantic import BaseModel
import pytz
import requests
from tenacity import retry, stop_after_attempt, wait_random
import subprocess
import socket
import select
import sys

DESC_REGEX = r"<main>(.*)</main>"
TIME_LEFT_REGEX = re.compile(r"You have ((?P<min>\d+)m )?((?P<sec>\d+)s )?left")

BAD_INPUT = "Please don't repeatedly request this endpoint before it unlocks!"

PART1_DONE = "The first half of this puzzle is complete! It provides one gold star: *"
PART2_DONE = "Both parts of this puzzle are complete! They provide two gold stars: **"


class Config(BaseModel):
    session: str


class DayStats(BaseModel):
    started: datetime | None
    part1: timedelta | None = None
    part2: timedelta | None = None


class SubmissionResponse(Enum):
    CORRECT = 1
    TOO_HIGH = 2
    TOO_LOW = 3
    INCORRECT = 4
    TOO_RECENTLY = 5
    ALREADY_SOLVED = 6


script_dir = Path(__file__).parent
with open(script_dir / "config.json", "r") as f:
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


def copy_directory_without_overwrite(src_dir: Path, dest_dir: Path):
    """
    Copies contents from src_dir to dest_dir without overwriting existing files.

    :param src_dir: Path to the source directory.
    :param dest_dir: Path to the destination directory.
    """
    if not src_dir.is_dir():
        raise FileNotFoundError(f"Source directory '{src_dir}' does not exist.")

    # Create destination directory if it doesn't exist
    dest_dir.mkdir(parents=True, exist_ok=True)

    for src_item in src_dir.rglob("*"):
        # Compute the relative path from the source directory
        relative_path = src_item.relative_to(src_dir)
        target_path = dest_dir / relative_path

        if src_item.is_dir():
            # Create subdirectories in destination
            target_path.mkdir(parents=True, exist_ok=True)
        elif src_item.is_file():
            if not target_path.exists():
                # Ensure the parent directory exists
                target_path.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(src_item, target_path)
                print(f"Copied: {src_item} -> {target_path}")
            else:
                print(f"Skipped (already exists): {target_path}")


class InputHandler:
    def __init__(self, port=31337):
        self.port = port
        self.inputs = None
        self.server_socket = None

    def __enter__(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(("127.0.0.1", self.port))
        server_socket.listen(1)
        server_socket.setblocking(False)
        inputs = [sys.stdin, server_socket]
        print(f"Listening for answers on stdin and localhost:{self.port}...")

        self.inputs = inputs
        self.server_socket = server_socket

        return self

    def __exit__(self, exc_type, exc_value, traceback):
        assert self.server_socket is not None
        self.server_socket.close()

    def wait_for_answer(self):
        assert self.server_socket is not None
        assert self.inputs is not None

        part_answer = None
        while not part_answer:
            readable, _, _ = select.select(self.inputs, [], [])
            for s in readable:
                if s is sys.stdin:
                    line = sys.stdin.readline()
                    if not line:
                        print("Input empty; retrying")
                        continue
                    part_answer = line.strip()
                elif s is self.server_socket:
                    conn, _ = self.server_socket.accept()
                    conn.setblocking(False)
                    self.inputs.append(conn)
                else:
                    data = bytes()
                    try:
                        while True:
                            b = s.recv(1)
                            if not b or b == b"\n":
                                break
                            data += b
                        if data:
                            part_answer = data.decode("utf-8").strip()
                    except Exception as e:
                        print(f"Error receiving data: {e}")
                    s.close()
                    self.inputs.remove(s)
        return part_answer


class AdventDay:
    def __init__(self, day: int, year: int):
        self.day = day
        self.year = year

        self.day_dir = script_dir.parent / f"{year}py" / f"day{day:02d}"

        self.template_dir = script_dir / "templates" / "py"

        self.part1_path = Path(self.day_dir) / "part1.py"
        self.part2_path = Path(self.day_dir) / "part2.py"
        self.input_path = Path(self.day_dir) / "input.txt"
        self.description_path = Path(self.day_dir) / "description.md"
        self.stats_path = Path(self.day_dir) / "stats.json"

        self.solution_cache: dict[tuple[int, str], SubmissionResponse] = dict()

        self.session = requests.Session()
        self.session.cookies.set("session", config.session)
        self.session.headers.update(
            {
                "User-Agent": "advent_bootstrap/0.0.1 (+https://github.com/mckirk/adventofcode)",
            }
        )

        self.parts_left = [1, 2]

    def create_structure_part1(self):
        print(f"Creating directory {self.day_dir}...")
        self.day_dir.mkdir(exist_ok=True, parents=True)

        if self.template_dir.is_dir():
            copy_directory_without_overwrite(self.template_dir, self.day_dir)

        if not self.part1_path.exists():
            print(f"Creating part1.py...")
            self.part1_path.touch(exist_ok=False)

    def create_structure_part2(self):
        # update description for part 2
        self.fetch_description()

        # copy part 1 to part 2
        if not self.part2_path.exists():
            print(f"Creating part2.py...")
            self.part2_path.touch(exist_ok=False)
            self.part2_path.write_text(self.part1_path.read_text())

    def wait_until_6am(self):
        wait_until = datetime(self.year, 12, self.day, 5, 0, 0, 0, tzinfo=pytz.utc)

        while (wait_delta := wait_until - datetime.now(pytz.utc)).total_seconds() > 0:
            print(f"Waiting {wait_delta.total_seconds()} seconds until 6am CET...")
            time.sleep(wait_delta.total_seconds())

    def update_stats(self, step: int):
        if step == 0:
            if not self.stats_path.exists():
                print(f"Creating stats.json...")
                self.stats_path.touch(exist_ok=False)
                self.stats_path.write_text(
                    DayStats(started=datetime.now(tz)).model_dump_json(indent=2)
                )
        else:
            stats = DayStats.model_validate_json(self.stats_path.read_text())

            changed = False
            if stats.started is not None:
                if step == 1 and stats.part1 is None:
                    stats.part1 = datetime.now(tz) - stats.started
                    changed = True
                elif step == 2 and stats.part2 is None:
                    stats.part2 = datetime.now(tz) - stats.started
                    changed = True

            if changed:
                self.stats_path.write_text(stats.model_dump_json(indent=2))

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

        desc = get_description()

        if PART1_DONE in desc:
            self.parts_left = [2]
        if PART2_DONE in desc:
            self.parts_left = [0]

        self.description_path.write_text(get_description())

    def start_editor(self, part):
        print(f"Starting editor...")
        subprocess.run(
            [
                "code",
                "-n",
                str(self.day_dir),
                str(self.part1_path if part == 1 else self.part2_path),
            ]
        )

    def submit_answer(self, part: int, answer: str):
        cached_solution = self.solution_cache.get((part, answer))
        if cached_solution is not None:
            print(f"Answer in cache")
            return cached_solution, None

        print(f"Submitting answer for part {part}...")
        resp = self.session.post(
            f"https://adventofcode.com/{self.year}/day/{self.day}/answer",
            data={"level": part, "answer": answer.strip()},
        )

        if resp.status_code != 200:
            raise Exception(f"Unexpected status code {resp.status_code}!")

        content = resp.text

        response = None
        if "That's the right answer!" in content:
            response = SubmissionResponse.CORRECT
        elif "too high" in content:
            response = SubmissionResponse.TOO_HIGH
        elif "too low" in content:
            response = SubmissionResponse.TOO_LOW
        elif "That's not the right answer" in content:
            response = SubmissionResponse.INCORRECT
        elif "You don't seem to be solving the right level." in content:
            response = SubmissionResponse.ALREADY_SOLVED

        if response is not None:
            self.solution_cache[(part, answer)] = response
            return response, None

        time_left_match = TIME_LEFT_REGEX.search(content)
        if time_left_match:
            min = int(time_left_match.group("min") or 0)
            sec = int(time_left_match.group("sec") or 0)
            return SubmissionResponse.TOO_RECENTLY, datetime.now(tz) + timedelta(
                minutes=min, seconds=sec
            )
        else:
            raise Exception("Could not interpret the submission result")

    def submit_answers_until_correct(self):
        for part in self.parts_left:
            if part == 2:
                self.create_structure_part2()
                self.update_stats(1)
                self.start_editor(2)

            while True:
                print(f"Enter answer for part {part}:")

                with InputHandler() as input_handler:
                    part_answer = input_handler.wait_for_answer()

                answer, next_submit_time = self.submit_answer(part, part_answer)

                if answer == SubmissionResponse.CORRECT:
                    print(f"Correct!")
                    break
                elif answer == SubmissionResponse.TOO_HIGH:
                    print(f"Too high!")
                elif answer == SubmissionResponse.TOO_LOW:
                    print(f"Too low!")
                elif answer == SubmissionResponse.INCORRECT:
                    print(f"Incorrect!")
                elif (
                    answer == SubmissionResponse.TOO_RECENTLY
                    and next_submit_time is not None
                ):
                    print(
                        f"Too recently submitted, waiting until {next_submit_time}..."
                    )
                    delta = (next_submit_time - datetime.now(tz)).total_seconds()
                    if delta > 0:
                        time.sleep(delta)
                elif answer == SubmissionResponse.ALREADY_SOLVED:
                    print(f"Seems to be already solved, continuing...")
                    break
                else:
                    raise Exception("Unexpected submission answer!")

        print("gg")
        self.update_stats(2)

        # update description to include solution
        self.fetch_description()


def main():
    parser = create_argparser()
    args = parser.parse_args()

    now = datetime.now(tz)
    day = args.day or now.day
    year = args.year or now.year

    advent_day = AdventDay(day, year)

    advent_day.create_structure_part1()
    advent_day.wait_until_6am()
    advent_day.update_stats(0)
    advent_day.fetch_description()

    if not advent_day.parts_left:
        print(f"Already solved!")
        return

    advent_day.fetch_input()
    advent_day.start_editor(1)
    advent_day.submit_answers_until_correct()


if __name__ == "__main__":
    main()
