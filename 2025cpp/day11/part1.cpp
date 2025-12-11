#include "lib/all.h"

using namespace std;

using R = int;
using Node = uint16_t;

class Solution {
public:
    map<string, Node> known{};
    vector<vector<Node>> edges{};
    vector<R> cache{};

    Solution(const vector<string>& lines) {
        auto num_nodes = lines.size()+2;

        edges = vector(num_nodes, vector<Node>());
        cache.resize(num_nodes, -1);

        // prealloc IDs 0 and 1
        get_node("you");
        get_node("out");

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

    R number_of_paths(Node from) {
        if (from == 1) {
            return 1;
        }

        if (cache[from] != -1) {
            return cache[from];
        }

        R res = 0;
        for (const auto& n : edges[from]) {
            res += number_of_paths(n);
        }

        cache[from] = res;
        return res;
    }
};

R run(Input &input) {
    auto sol = Solution(input.lines);

    return sol.number_of_paths(0);
}

int main(int argc, char** argv) {
    map<int, optional<R>> expected_results = {{1, 5}};
    run_on_inputs(run, expected_results);
}
