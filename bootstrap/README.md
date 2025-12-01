# Advent of Code Bootstrap Script

This script automates the setup and submission process for Advent of Code puzzles. It handles creating the directory structure, fetching inputs and descriptions, and submitting answers directly from the command line.

## Features

- **Automatic Setup**: Creates the directory structure for the specified day (e.g., `2025py/day01`) and copies template files.
- **Input & Description Fetching**: Downloads the puzzle input and description (converted to Markdown) automatically.
- **Wait for Unlock**: If run before the puzzle unlocks (6 AM CET), it waits until the unlock time.
- **Stats Tracking**: Tracks the time taken to solve each part in `stats.json`.
- **VS Code Integration**: Automatically opens the day's directory and solution file in VS Code.
- **Answer Submission**: Allows submitting answers via the command line and handles responses (Correct, Too High/Low, Rate Limited, etc.).

## Prerequisites

- Python 3.12+
- A valid Advent of Code session cookie.

## Installation

1.  Install dependencies using `uv`:

    ```bash
    uv sync
    ```

2.  Configure your session cookie:
    - Create a `config.json` file in the `bootstrap` directory (or use the existing one).
    - Add your session cookie:
      ```json
      {
          "session": "YOUR_SESSION_COOKIE_HERE"
      }
      ```

## Usage

Run the script from the `bootstrap` directory:

```bash
uv run main.py
```

### Arguments

- `--day`: Specify the day (default: current day).
- `--year`: Specify the year (default: current year).

### Example

```bash
# Run for the current day
uv run main.py

# Run for a specific day
uv run main.py --day 1 --year 2024
```

## Workflow

1.  The script creates the directory for the day.
2.  It waits for the puzzle to unlock (if necessary).
3.  It fetches the input and description.
4.  It opens VS Code.
5.  You solve Part 1 and enter the answer in the terminal when prompted.
6.  If correct, it sets up Part 2 (fetches updated description, copies Part 1 code).
7.  You solve Part 2 and enter the answer.
8.  Stats are saved to `stats.json`.
