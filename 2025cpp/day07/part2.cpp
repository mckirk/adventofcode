#include "lib/all.h"

using namespace std;

using beams_t = BigInt<256>;

beams_t run(Input &input) {
    auto limits = input.limits();

    auto cur = vector<beams_t>(limits.second, 0);
    auto next = vector<beams_t>(limits.second, 0);
    for (const auto& line : input.lines) {
        for (auto [i, ch] : enumerate(line)) {
            if (ch == 'S') {
                next[i] = true;
                continue;
            }

            beams_t beams = cur[i];

            if (!beams) continue;

            if (ch == '^') {
                next[i-1] += beams;
                next[i+1] += beams;
            } else {
                next[i] += beams;
            }
        }

        swap(cur, next);
        fill(next.begin(), next.end(), 0);
    }

    return accumulate(cur.begin(), cur.end(), beams_t());
}

int main(int argc, char** argv) {
    map<int, optional<beams_t>> expected_results = {{1, beams_t(40)}};
    run_on_inputs(run, expected_results);
}
