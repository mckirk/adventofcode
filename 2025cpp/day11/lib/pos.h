#pragma once

#include <compare>
#include <string>
#include <format>
#include <vector>
#include <istream>
#include <iterator>

using namespace std;

template<typename D>
struct V {
    D x;
    D y;

    auto operator<=>(const V& other) const
    {
        if (auto cmp = x <=> other.x; cmp != 0) return cmp;
        return y <=> other.y;
    }

    bool operator==(const V& other) const {
        return x == other.x && y == other.y;
    }
    bool operator!=(const V& other) const {
        return !(*this == other);
    }

    // arithmetic
    V operator+(const V& o) const {
        return V{x + o.x, y + o.y};
    }

    V operator-(const V& o) const {
        return V{x - o.x, y - o.y};
    }

    V& operator+=(const V& o) {
        x += o.x;
        y += o.y;
        return *this;
    }

    V& operator-=(const V& o) {
        x -= o.x;
        y -= o.y;
        return *this;
    }

    V abs() const {
        return V{std::abs(x), std::abs(y)};
    }

    template <class Out>
    requires output_iterator<Out, const char&>
    Out format_to(Out out) const {
        return std::format_to(out, "V{{{}, {}}}", x, y);
    }

    string to_string() const {
        string s;
        format_to(back_inserter(s));
        return s;
    }
};

using V32 = V<int32_t>;
using V64 = V<int64_t>;

namespace std {
    template<typename D>
    struct hash<V<D>> {
        size_t operator()(const V<D>& p) const noexcept {
            size_t h1 = hash<int>{}(p.x);
            size_t h2 = hash<int>{}(p.y);
            return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
        }
    };

    template<typename D>
    struct formatter<V<D>> {
        constexpr auto parse(format_parse_context& ctx) {
            // No custom format specs for now.
            return ctx.begin();
        }

        auto format(const V<D>& v, format_context& ctx) const {
            return v.format_to(ctx.out());
        }
    };
}


template<typename D>
struct PosIterator {
    using iterator_concept  = forward_iterator_tag; // or input if you prefer
    using iterator_category = forward_iterator_tag; // for legacy algorithms
    using difference_type   = ptrdiff_t;
    using value_type        = pair<V<D>, char>;
    using reference         = value_type; // returning by value/proxy is OK
    using pointer           = void;

    const vector<string>* lines = nullptr;
    size_t row = 0;
    size_t col = 0;
    size_t row_dim = 0;
    size_t col_dim = 0;

    PosIterator() = default;

    PosIterator(const vector<string>& l, size_t x, size_t y)
        : lines(&l), row(y), col(x),
          row_dim(l.size()),
          col_dim(l.empty() ? 0 : l[0].size()) {}

    reference operator*() const {
        return { V{static_cast<int>(col), static_cast<int>(row)},
                 (*lines)[row][col] };
    }

    PosIterator& operator++() {
        ++col;
        if (col >= col_dim) {
            col = 0;
            ++row;
        }
        return *this;
    }

    PosIterator operator++(int) {
        auto tmp = *this;
        ++(*this);
        return tmp;
    }

    friend bool operator==(const PosIterator& a, const PosIterator& b) {
        return a.lines == b.lines && a.row == b.row && a.col == b.col;
    }
};

static_assert(forward_iterator<PosIterator<uint64_t>>);

template<typename D>
struct PosIterable {
    const vector<string>& lines;

    PosIterator<D> begin() const { return PosIterator{lines, D{}, D{}}; }
    PosIterator<D> end() const { return PosIterator{lines, D{}, lines.size()}; }
};

// support parsing from "x,y" strings

template<typename D>
istream& operator>>(istream& is, V<D>& p) {
    char comma{};
    if (is >> p.x >> comma >> p.y && comma == ',') {
        return is;
    } else {
        is.setstate(ios::failbit);
        return is;
    }
}