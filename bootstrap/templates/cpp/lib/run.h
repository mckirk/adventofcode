#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <chrono>
#include <map>
#include <optional>
#include <functional>
#include <filesystem>
#include <type_traits>
#include <format>

#include "str.h"
#include "pos.h"
#include "submit.h"

using namespace std;
namespace fs = filesystem;

struct Input {
    string content;
    vector<string> lines;
    vector<string> blocks;

    Input(string c) : content(move(c)) {
        if (content.empty()) return;

        // Remove \r characters
        content.erase(remove(content.begin(), content.end(), '\r'), content.end());

        // Split lines
        stringstream ss(content);
        string line;
        while (getline(ss, line)) {
            lines.push_back(line);
        }

        // Split blocks
        size_t start = 0;
        size_t end = content.find("\n\n");
        while (end != string::npos) {
            blocks.push_back(content.substr(start, end - start));
            start = end + 2;
            end = content.find("\n\n", start);
        }
        blocks.push_back(content.substr(start));
    }

    static Input from_file(const fs::path& path, bool raw = false) {
        if (!fs::exists(path)) return Input("");
        ifstream f(path);
        stringstream buffer;
        buffer << f.rdbuf();
        string content = buffer.str();
        
        if (!raw) {
            strip(content);
        }
        
        return Input(content);
    }

    bool exists() const { return !content.empty(); }

    pair<int, int> limits() const {
        if (lines.empty()) return {0, 0};
        return {static_cast<int>(lines[0].length()), static_cast<int>(lines.size())};
    }

    auto as_pos() const {
        return PosIterable{lines};
    }
};

inline Input get_problem_input(bool raw = false) {
    return Input::from_file("input.txt", raw);
}

inline Input get_sample_input(int idx, bool raw = false) {
    return Input::from_file(format("sample{}.txt", idx), raw);
}

template <typename Func, typename ResultType = invoke_result_t<Func, Input&>>
ResultType run_on_inputs(
    Func run,
    map<int, optional<ResultType>> expected_sample_results = {},
    bool raw_input = false,
    optional<bool> auto_submit = nullopt
) {
    if (!auto_submit.has_value()) {
        auto_submit = false;
        for (const auto& [k, v] : expected_sample_results) {
            if (v.has_value()) {
                auto_submit = true;
                break;
            }
        }
    }

    auto run_with_timing = [&](string label, Input& inp) -> ResultType {
        auto start = chrono::high_resolution_clock::now();
        ResultType res = run(inp);
        auto end = chrono::high_resolution_clock::now();
        chrono::duration<double> duration = end - start;
        cout << format("{}: {} ({:.6f}s)\n", label, res, duration.count());
        return res;
    };

    for (const auto& [i, exp] : expected_sample_results) {
        Input inp = get_sample_input(i, raw_input);
        if (inp.exists()) {
            ResultType res = run_with_timing("Sample " + to_string(i), inp);
            if (exp.has_value() && res != exp.value()) {
                cout << format("Did not match expected result for sample {} ({}), aborting.\n", i, exp.value());
                exit(1);
            }
        }
    }

    Input problem_input = get_problem_input(raw_input);
    ResultType res = run_with_timing("Result", problem_input);

    if (auto_submit.value_or(false)) {
        stringstream ss;
        ss << res;
        submit_answer(31337, ss.str());
    }

    return res;
}
