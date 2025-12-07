#include "lib/all.h"

using namespace std;

int run(Input &input) {
    return 0;
}

int main(int argc, char** argv) {
    map<int, optional<int>> expected_results = {{1, {}}};
    run_on_inputs(run, expected_results);
}
