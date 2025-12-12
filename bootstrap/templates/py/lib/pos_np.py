import numpy as np


class VNP(np.ndarray):
    @classmethod
    def from_val(cls, input_array):
        obj = np.asarray(input_array).view(cls)
        return obj

    @property
    def x(self):
        return self[0]

    @property
    def y(self):
        return self[1]

    @property
    def z(self):
        return self[2]


class LineNP:
    def __init__(self, p, v):
        self.p = p
        self.v = v

    @classmethod
    def from_pv(cls, p, v):
        return cls(np.array(p), np.array(v))

    def __repr__(self):
        return f"{self.p} + t{self.v}"

    def intersect(self, other):
        try:
            t, s = np.linalg.solve(np.array([self.v, other.v]).T, other.p - self.p)
        except np.linalg.LinAlgError:
            return None, None

        return VNP.from_val(self.p + t * self.v), (t, -s)


class FrozenArray:
    __slots__ = ("_arr", "_hash")
    __array_priority__ = 1000  # ensure we win mixed ops

    def __init__(self, data):
        a = np.asanyarray(data)
        a = np.ascontiguousarray(a)
        a.setflags(write=False)
        self._arr = a
        self._hash = hash((a.dtype.str, a.shape, a.tobytes()))

    def __array__(self, dtype=None):
        if dtype is None:
            return self._arr
        return self._arr.astype(dtype, copy=False)

    def __array_ufunc__(self, ufunc, method, *inputs, **kwargs):
        # unwrap FrozenArray inputs
        unwrapped = [
            x._arr if isinstance(x, FrozenArray) else x
            for x in inputs
        ]
        result = getattr(ufunc, method)(*unwrapped, **kwargs)

        if isinstance(result, tuple):
            return tuple(
                FrozenArray(x) if isinstance(x, np.ndarray) else x
                for x in result
            )
        if isinstance(result, np.ndarray):
            return FrozenArray(result)
        return result  # scalar

    def __getitem__(self, idx):
        return self._arr[idx]

    def __len__(self):
        return len(self._arr)

    @property
    def shape(self):
        return self._arr.shape

    @property
    def dtype(self):
        return self._arr.dtype

    def __hash__(self):
        return self._hash

    def __eq__(self, other):
        if isinstance(other, FrozenArray):
            other = other._arr
        other = np.asanyarray(other)
        return (
            self._arr.shape == other.shape
            and self._arr.dtype == other.dtype
            and np.array_equal(self._arr, other)
        )

    def __repr__(self):
        return f"FrozenArray({self._arr!r})"

    def __add__(self, other): return np.add(self, other)
    def __radd__(self, other): return np.add(other, self)
    def __sub__(self, other): return np.subtract(self, other)
    def __rsub__(self, other): return np.subtract(other, self)
    def __mul__(self, other): return np.multiply(self, other)
    def __rmul__(self, other): return np.multiply(other, self)
    def __truediv__(self, other): return np.true_divide(self, other)
    def __rtruediv__(self, other): return np.true_divide(other, self)
    def __matmul__(self, other): return np.matmul(self, other)
    def __rmatmul__(self, other): return np.matmul(other, self)
    def __neg__(self): return np.negative(self)
