from dataclasses import dataclass


@dataclass
class Range:
    """Range class (end exclusive)"""
    start: int
    end: int

    @classmethod
    def from_len(cls, start: int, len: int):
        assert len >= 0
        return cls(start, start + len)
    
    def __contains__(self, item):
        return self.start <= item < self.end
    
    def __len__(self):
        return self.end - self.start
    
    def intersect(self, other):
        if self.start <= other.start < self.end:
            intersect_end = min(self.end, other.end)
            return Range(other.start, intersect_end)
        elif other.start <= self.start < other.end:
            intersect_end = min(self.end, other.end)
            return Range(self.start, intersect_end)
        else:
            return None
        
    def intersect_complete(self, other):
        """Returns (intersect, non_intersect1, non_intersect2)"""
        intersect = self.intersect(other)
        if not intersect:
            return None, self, other
        
        non_intersect1 = Range(self.start, intersect.start)
        non_intersect2 = Range(intersect.end, self.end)
        
        return intersect, non_intersect1, non_intersect2
