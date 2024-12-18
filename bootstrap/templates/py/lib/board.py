from abc import ABC, abstractmethod
from typing import Iterable
import colored


def get_limits(lines):
    return (len(lines[0]), len(lines))


def get_pos(lines, look_for):
    pos = set()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            if c == look_for:
                pos.add((x, y))

    return pos


class Board(ABC):
    def pprint(self, colors: dict[tuple, str]):
        ls = []
        for y, l in enumerate(self.by_lines()):
            cur_l = ""
            for x, c in enumerate(l):
                color = colors.get((x, y))
                if color:
                    cur_l += f"{colored.fore(color)}{c}{colored.Style.reset}"
                else:
                    cur_l += c
            ls.append(cur_l)

        print("\n".join(ls))

    @abstractmethod
    def by_lines(self) -> Iterable: ...


def num_diff(s1, s2):
    return sum(1 for x, y in zip(s1, s2) if x != y)


class ListBoard(Board):
    def __init__(self, board: list[str]):
        self.board = board
        self.limits = (len(board[0]), len(board))

    def by_lines(self):
        return self.board

    def to_dict(self):
        tiles = dict()
        for y, l in enumerate(self.board):
            for x, c in enumerate(l):
                tiles[(x, y)] = c

        return tiles

    def find_reflection_y(self, num_defects=0):
        for i, _ in enumerate(self.board):
            parts = self.board[:i][::-1], self.board[i:]
            min_len = min(len(p) for p in parts)
            if not min_len:
                continue
            parts = [p[:min_len] for p in parts]
            if num_defects:
                total_num_diff = sum(num_diff(*p) for p in zip(*parts))
                if total_num_diff == num_defects:
                    return i
            else:
                if parts[0] == parts[1]:
                    return i
        return None


class DictBoard(Board):
    def __init__(self, board: dict[tuple, str], fill: str):
        self.board = board
        self.fill = fill
        self.limits = (max(x + 1 for x, y in board), max(y + 1 for x, y in board))

    def by_lines(self):
        xl, yl = self.limits
        for y in range(yl):
            l = ""
            for x in range(xl):
                l += self.board.get((x, y), self.fill)
            yield l

    def to_list(self):
        return list(self.by_lines())

    def find(self, e):
        for p, c in self.board.items():
            if c == e:
                return p

        return None

    def find_all(self, e):
        for p, c in self.board.items():
            if c == e:
                yield p


def transpose(lines):
    return ["".join(l) for l in zip(*lines)]
