#!/usr/bin/env python3

import re
from collections import defaultdict
from typing import Tuple

import numpy as np
import sympy
import z3
from scipy import signal

W, X, Y, Z = range(4)
def var_idx(var): return "wxyz".index(var)

class State:
    def __init__(self, input):
        self.orig_inp = list(input)
        self.input = list(input)
        self.vars = [0]*4

    def int_cast(self, i):
        return i * 1

    def idiv(self, a, b):
        return a // b

    def set_var(self, idx, val):
        self.vars[idx] = val

class NumRange:
    def __init__(self, bounds):
        self.bounds = bounds
        l, u = self.bounds
        assert (l <= u)

    @staticmethod
    def build(bounds):
        l, u = bounds
        if l == u:
            return l
        else:
            return NumRange(bounds)

    def __add__(self, other):
        l, u = self.bounds
        if type(other) is int:
            return NumRange.build((l+other, u+other))
        
        ol, ou = other.bounds

        return NumRange.build((l+ol, u+ou))
    __radd__ = __add__

    def __mul__(self, other):
        l, u = self.bounds
        if type(other) is int:
            return NumRange.build((l*other, u*other))
        
        ol, ou = other.bounds

        return NumRange.build((l*ol, u*ou))
    __rmul__ = __mul__

    def __floordiv__(self, other):
        l, u = self.bounds
        if type(other) is int:
            return NumRange.build((l//other, u//other))
        
        ol, ou = other.bounds

        return NumRange.build((l//ou, u//ol))

    def __eq__(self, other):
        l, u = self.bounds
        if type(other) is int:
            if other >= l and other <= u:
                return NumRange.build((0, 1))
            else:
                return 0
        
        ol, ou = other.bounds

        if max(l, ol) >= min(u, ou):
            return NumRange.build((0, 1))
        else:
            return 0

    def __mod__(self, other):
        l, u = self.bounds

        if type(other) is int:
            if u < other:
                return NumRange.build(self.bounds)
            else:
                return NumRange.build((0, other-1))

        assert False


class RangeState:
    def __init__(self, num_inp):
        self.orig_inp = [NumRange((1, 9)) for _ in range(num_inp)]
        self.input = list(self.orig_inp)
        self.vars = [0]*4

    def int_cast(self, i):
        return i * 1

    def idiv(self, a, b):
        return a // b

    def set_var(self, idx, val):
        self.vars[idx] = val

    def try_determ_eq(self, a, b):
        if (a is b): return 1
        return None

    def try_lower(self, a, b):
        return False


class Z3State:
    def __init__(self, inp_count):
        self.orig_inp = [z3.Int(f"i{i}") for i in range(inp_count)] #[z3.BitVec(f"i{i}", 4) for i in range(inp_count)]
        self.input = list(self.orig_inp)
        self.vars = [0]*4

        self.input_constraints = self.inp_constrs()

        self.constrs = []

    def inp_constrs(self):
        return [(i >= 1) for i in self.orig_inp] + [(i < 10) for i in self.orig_inp]

    def int_cast(self, i):
        return z3.IntSort().cast(i)

    def idiv(self, a, b):
        return a / b

    def try_determ_eq(self, a, b):
        if a is b:
            return 1

        solv = z3.Solver()
        solv.add(a == b, *self.input_constraints)

        solv2 = z3.Solver()
        solv2.add(a != b, *self.input_constraints)

        r = solv.check()
        r2 = solv2.check()

        if r == z3.unsat:
            return 0
        elif r2 == z3.unsat:
            return 1

        return None

    def try_lower(self, a, b):
        solv = z3.Solver()
        solv.add(a >= b, *self.input_constraints)

        r = solv.check()

        if r == z3.unsat:
            return 1

        return None


    def set_var(self, idx, val):
        self.vars[idx] = val

        # if type(val) is not int and idx == Z:
        #     print(z3.simplify(val))

# class SymState:
#     def __init__(self, inp_count):
#         self.orig_inp = [sympy.symbols(f"i{i}", integer=True) for i in range(inp_count)]
#         self.input = list(self.orig_inp)
#         self.vars = [0]*4

#         self.constrs = []

#         self.eq_symbols = []
#         self.eq_constr = []

#     def inp_constrs(self):
#         return [(i >= 1) for i in self.orig_inp] + [(i < 10) for i in self.orig_inp]

#     def int_cast(self, i):
#         return i

#     def idiv(self, a, b):
#         return a // b

#     def set_var(self, idx, val):
#         if idx == 3:
#             self.constrs.append(val == 0)

#         self.vars[idx] = val

#     def eq(self, a, b):
#         eq_sym = sympy.symbols(f"eq{len(self.eq_symbols)}", integer=True)

class Arg:
    def __init__(self, arg):
        if arg in "wxyz":
            self.is_var = True
            self.var = var_idx(arg)
        else:
            self.is_var = False
            self.num = int(arg)

    def eval(self, state):
        if self.is_var:
            return state.vars[self.var]
        else:
            return self.num

    def set_state(self, state, val):
        assert self.is_var

        state.set_var(self.var, val)

class Instr:
    def __init__(self, args):
        self.args = list(args)

    def arg_vals(self, state):
        return [arg.eval(state) for arg in self.args]

    def set_out(self, state, val):
        self.args[0].set_state(state, val)
    
    def eval(self, state): ...

class InpInstr(Instr):
    def eval(self, state):
        self.set_out(state, state.input.pop(0))

class AddInstr(Instr):
    def eval(self, state):
        a, b = self.arg_vals(state)

        if a is 0:
            self.set_out(state, b)
            return
        elif b is 0:
            self.set_out(state, a)
            return

        self.set_out(state, a + b)

class MulInstr(Instr):
    def eval(self, state):
        a, b = self.arg_vals(state)

        if a is 0 or b is 0:
            self.set_out(state, 0)
            return

        if a is 1:
            self.set_out(state, b)
            return
        elif b is 1:
            self.set_out(state, a)
            return

        self.set_out(state, a * b)

class DivInstr(Instr):
    def eval(self, state):
        a, b = self.arg_vals(state)

        if b is 1:
            self.set_out(state, a)
            return

        self.set_out(state, state.idiv(a, state.int_cast(b)))

class ModInstr(Instr):
    def eval(self, state):
        a, b = self.arg_vals(state)

        if state.try_lower(a, b):
            self.set_out(state, a)
            return

        self.set_out(state, a % b)

class EqlInstr(Instr):
    def eval(self, state):
        a, b = self.arg_vals(state)
        res = state.try_determ_eq(a, b)
        if res is None:
            res = state.int_cast(a == b)

        self.set_out(state, res)

instr_map = {
    'inp': InpInstr,
    'add': AddInstr,
    'mul': MulInstr,
    'div': DivInstr,
    'mod': ModInstr,
    'eql': EqlInstr}

def main():
    with open("./input.txt", "r") as f:
       inp = list(f.read().strip().splitlines())
    # inp = toy_input.splitlines()

    instrs: list[Instr] = [instr_map[parts[0]](Arg(arg) for arg in parts[1:]) for parts in (l.split(" ") for l in inp)]

    s = RangeState(14)

    for i in instrs:
        i.eval(s)

    # s = State([1]*14)

    # for i in instrs:
    #     i.eval(s)

    # pass

    # for i in range(99999999999999, 0, -1):
    #     num = i
    #     inputs = []
    #     for _ in range(14):
    #         inputs.append(num % 10)
    #         num = num // 10

    #     if 0 in inputs: continue

    #     inputs.reverse()

    #     s = State(inputs)
    #     for instr in instrs:
    #         instr.eval(s)

    #     # print(f"{i}: {s.vars[3]}")
    #     if s.vars[3] == 0:
    #         print(i)


    # s = Z3State(14)

    # for i in instrs:
    #     i.eval(s)

    # simped = z3.simplify(s.vars[Z])

    # print(z3.solve(simped == 0, *s.input_constraints))

    # for i in range(14):
    #     constrs = s.inp_constrs()[0:1+i] + s.inp_constrs()[14:15+i]
    #     print(constrs)
    #     print(z3.solve(s.vars[3] == 0, *constrs))

    # print(sympy.simplify(s.vars[3]))

    # from sympy import Poly

    # polys = []
    # for i in s.orig_inp:
    #     polys.append(Poly()) 

    # print(sympy.solve(s.vars[3], *s.orig_inp))

    # for i in range(len(s.constrs)):
    #     print(z3.solve(*s.constrs[:i], *s.inp_constrs()))

    pass

if __name__ == "__main__": main()
