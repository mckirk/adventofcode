#pragma once

void strip(std::string &s) {
    const char* ws = " \t\n\r\f\v";
    size_t last = s.find_last_not_of(ws);
    if (last != std::string::npos) s.erase(last + 1);
    size_t first = s.find_first_not_of(ws);
    if (first != std::string::npos) s.erase(0, first);
    else s.clear();
}
