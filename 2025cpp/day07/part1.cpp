#include "lib/all.h"

#include <ranges>

using namespace std;

int run(Input &input) {
    auto as_pos = input.as_pos();

    optional<V> start = nullopt;
    auto splitters = unordered_set<V>{};

    for (const auto& [pos, ch] : as_pos) {
        if (ch == 'S') {
            start = pos;
        } else if (ch == '^') {
            splitters.insert(pos);
        }
    }

    int res = 0;
    unordered_set<V> cur = {start.value()};
    unordered_set<V> next = {};

    auto add_next = [&next](const V &np) {
        next.insert(np);
    };

    for(int i=0; i<input.limits().first; ++i) {
        for (auto &p : cur) {
            auto np = p + V{1, 0};
            if (splitters.contains(p)) {
                res += 1;
                add_next(np + V{0,  1});
                add_next(np + V{0, -1});
            } else {
                add_next(np);
            }
        }

        swap(cur, next);
        next.clear();
    }

    return res;
}

int main(int argc, char** argv) {
    map<int, optional<int>> expected_results = {{1, 21}};
    run_on_inputs(run, expected_results);
}
