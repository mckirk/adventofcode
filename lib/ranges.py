def intersect_ranges(range1, range2):
    range1_s, range1_l = range1
    range2_s, range2_l = range2
    
    intersecting_range = None
    non_intersecting_range1 = None
    non_intersecting_range2 = None

    if range1_s <= range2_s <= range1_s + range1_l:
        intersect_end = min(range1_s + range1_l, range2_s + range2_l)
        intersecting_range = (range2_s, intersect_end - range2_s)
        non_intersecting_range1 = (range1_s, range2_s - range1_s)
        non_intersecting_range2 = (intersect_end, range2_s + range2_l - intersect_end)
    elif range2_s <= range1_s <= range2_s + range2_l:
        intersect_end = min(range1_s + range1_l, range2_s + range2_l)
        intersecting_range = (range1_s, intersect_end - range1_s)
        non_intersecting_range1 = (intersect_end, range1_s + range1_l - intersect_end)
        non_intersecting_range2 = (range2_s, range1_s - range2_s)
    else:
        return None, range1, range2
    
    if intersecting_range[1] <= 0:
        intersecting_range = None
    if non_intersecting_range1[1] <= 0:
        non_intersecting_range1 = None
    if non_intersecting_range2[1] <= 0:
        non_intersecting_range2 = None

    return intersecting_range, non_intersecting_range1, non_intersecting_range2