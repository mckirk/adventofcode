#include "lib/all.h"

using namespace std;

using R = uint64_t;
using Node = uint16_t;

struct State {
    R path_count[4];

    void add(State subres, bool is_dac, bool is_fft) {
        for (int i = 0; i < 4; ++i) {
            path_count[i | is_dac | (is_fft<<1)] += subres.path_count[i];
        }
    }
};

class Solution {
public:
    map<string, Node> known{};
    vector<vector<Node>> edges{};
    vector<optional<State>> cache{};

    Node dac_node, fft_node;

    Solution(const vector<string>& lines) {
        auto num_nodes = lines.size()+2;

        edges = vector(num_nodes, vector<Node>());
        cache.resize(num_nodes, {});

        // prealloc IDs 0 and 1
        get_node("svr");
        get_node("out");

        dac_node = get_node("dac");
        fft_node = get_node("fft");

        for (const auto& l : lines) {
            auto l_stream = istringstream(l);

            string from{};
            vector<Node> to{};
            l_stream >> from;

            from.pop_back();

            string neighbor{};
            while (l_stream >> neighbor) {
                to.push_back(get_node(neighbor));
            }

            edges[get_node(from)] = to;
        }
    }

    Node get_node(const string& name) {
        auto found = known.find(name);

        if (found == known.end()) {
            Node id = known.size();
            known[name] = id;
            return id;
        } else {
            return found->second;
        }
    }

    State number_of_paths(Node from) {
        if (from == 1) {
            return State{{1, 0, 0, 0}};
        }

        auto cached = cache[from];
        if (cached.has_value()) {
            return cached.value();
        }

        bool is_dac = from == dac_node;
        bool is_fft = from == fft_node;

        State res = State{{0, 0, 0, 0}};
        for (const auto& n : edges[from]) {
            res.add(number_of_paths(n), is_dac, is_fft);
        }

        cache[from] = res;
        return res;
    }
};

R run(Input &input) {
    auto sol = Solution(input.lines);

    return sol.number_of_paths(0).path_count[3];
}

int main(int argc, char** argv) {
    map<int, optional<R>> expected_results = {{2, 2}};
    run_on_inputs(run, expected_results);
}
