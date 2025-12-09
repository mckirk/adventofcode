#include "lib/all.h"

using namespace std;

using R = uint64_t;

R run(Input &input) {
    vector<V32> tiles = input.parse_to<V32, vector<V32>>();

    R largest = 0;
    for (int i = 0; i < tiles.size(); ++i) {
        for (int j = i+1; j < tiles.size(); ++j) {
            V32 delta = (tiles[i] - tiles[j]).abs() + V32{1,1};
            R dx = (R)delta.x;
            R dy = (R)delta.y;

            largest = max(largest, dx*dy);
        }
    }

    return largest;
}

int main(int argc, char** argv) {
    map<int, optional<R>> expected_results = {{1, 50}};
    run_on_inputs(run, expected_results);
}
