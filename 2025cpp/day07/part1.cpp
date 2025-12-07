#include "lib/all.h"

#include <ranges>

using namespace std;

int run(Input &input) {
    auto limits = input.limits();

    int res = 0;
    auto cur = vector<bool>(limits.second, false);
    auto next = vector<bool>(limits.second, false);
    for (const auto& line : input.lines) {
        for (auto [i, ch] : enumerate(line)) {
            if (ch == 'S') {
                next[i] = true;
                continue;
            }

            if (!cur[i]) continue;

            if (ch == '^') {
                res += 1;
                next[i-1] = true;
                next[i+1] = true;
            } else {
                next[i] = true;
            }
        }

        swap(cur, next);
        fill(next.begin(), next.end(), false);
    }

    return res;
}

int main(int argc, char** argv) {
    map<int, optional<int>> expected_results = {{1, 21}};
    run_on_inputs(run, expected_results);
}
