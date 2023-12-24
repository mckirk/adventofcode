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
