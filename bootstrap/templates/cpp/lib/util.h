#include <ranges>

using namespace std;

template <typename T>
auto enumerate(const T& c) {
    size_t index = 0;
    return c | views::transform([&index](const auto& v) mutable {
        return pair{index++, v};
    });
}