#include "lib/all.h"

using namespace std;

using R = uint64_t;

struct LineSegment {
    V32 start;
    V32 end;
};

struct Rect {
    V32 top_left;
    V32 bottom_right;
};

// does not work yet!

bool intersects(const LineSegment& ls, const Rect& r) {
    V32 ls_min{
        min(ls.start.x, ls.end.x),
        min(ls.start.y, ls.end.y)
    };
    V32 ls_max{
        max(ls.start.x, ls.end.x),
        max(ls.start.y, ls.end.y)
    };

    if (ls_max.x <= r.top_left.x || ls_min.x >= r.bottom_right.x) {
        return false;
    }
    if (ls_max.y <= r.top_left.y || ls_min.y >= r.bottom_right.y) {
        return false;
    }

    return true;
}

R run(Input &input) {
    vector<V32> tiles = input.parse_to<V32, vector<V32>>();
    vector<LineSegment> segments;
    for (int i = 0; i < tiles.size(); ++i) {
        LineSegment ls{tiles[i], tiles[(i + 1) % tiles.size()]};
        segments.push_back(ls);
    }

    R largest = 0;
    for (int i = 0; i < tiles.size(); ++i) {
        for (int j = i+1; j < tiles.size(); ++j) {
            Rect r{
                V32{min(tiles[i].x, tiles[j].x), min(tiles[i].y, tiles[j].y)},
                V32{max(tiles[i].x, tiles[j].x), max(tiles[i].y, tiles[j].y)}
            };

            for (const auto& seg : segments) {
                if (intersects(seg, r)) {
                    goto next_pair;
                }
            }

            V32 delta = r.bottom_right - r.top_left + V32{1,1};
            R dx = (R)delta.x;
            R dy = (R)delta.y;

            largest = max(largest, dx*dy);
        }
    next_pair:;
    }

    return largest;
}

int main(int argc, char** argv) {
    map<int, optional<R>> expected_results = {{1, 24}};
    run_on_inputs(run, expected_results, false, false);
}
