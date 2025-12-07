#pragma once

#include <compare>
#include <string>
#include <format>
#include <vector>

using namespace std;

struct V {
    int r{};
    int c{};

    // row then col lexicographical order, for adding to containers
    auto operator<=>(const V&) const = default;

    // arithmetic
    auto operator+(const V& o) const -> V {
        return V{r + o.r, c + o.c};
    }

    auto operator-(const V& o) const -> V {
        return V{r - o.r, c - o.c};
    }

    auto operator+=(const V& o) -> V& {
        r += o.r;
        c += o.c;
        return *this;
    }

    auto operator-=(const V& o) -> V& {
        r -= o.r;
        c -= o.c;
        return *this;
    }

    template <class Out>
    requires output_iterator<Out, const char&>
    Out format_to(Out out) const {
        return std::format_to(out, "V{{r={}, c={}}}", r, c);
    }

    string to_string() const {
        string s;
        format_to(back_inserter(s));
        return s;
    }
};

namespace std {
    template<>
    struct hash<V> {
        size_t operator()(const V& p) const noexcept {
            size_t h1 = hash<int>{}(p.r);
            size_t h2 = hash<int>{}(p.c);
            return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
        }
    };

    template <>
    struct formatter<V> {
        constexpr auto parse(format_parse_context& ctx) {
            // No custom format specs for now.
            return ctx.begin();
        }

        auto format(const V& v, format_context& ctx) const {
            return format_to(ctx.out(), "({}, {})", v.r, v.c);
        }
    };
}


struct PosIterator {
    using iterator_concept  = forward_iterator_tag; // or input if you prefer
    using iterator_category = forward_iterator_tag; // for legacy algorithms
    using difference_type   = ptrdiff_t;
    using value_type        = pair<V, char>;
    using reference         = value_type; // returning by value/proxy is OK
    using pointer           = void;

    const vector<string>* lines = nullptr;
    size_t row = 0;
    size_t col = 0;
    size_t row_dim = 0;
    size_t col_dim = 0;

    PosIterator() = default;

    PosIterator(const vector<string>& l, size_t r, size_t c)
        : lines(&l), row(r), col(c),
          row_dim(l.size()),
          col_dim(l.empty() ? 0 : l[0].size()) {}

    reference operator*() const {
        return { V{static_cast<int>(row), static_cast<int>(col)},
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

static_assert(forward_iterator<PosIterator>);

struct PosIterable {
    const vector<string>& lines;

    PosIterator begin() const { return PosIterator{lines, 0, 0}; }
    PosIterator end() const { return PosIterator{lines, lines.size(), 0}; }
};