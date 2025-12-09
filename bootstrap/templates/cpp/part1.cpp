#include "lib/all.h"

using namespace std;

using R = int;

R run(Input &input) {
    return 0;
}

int main(int argc, char** argv) {
    map<int, optional<R>> expected_results = {{1, {}}};
    run_on_inputs(run, expected_results);
}
