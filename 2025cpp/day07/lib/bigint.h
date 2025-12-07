#pragma once
#include <cstdint>
#include <string>
#include <algorithm>
#include <format>

template<unsigned BITS>
class BigInt {
private:
    static constexpr size_t LIMBS = (BITS + 63) / 64;
    uint64_t limbs[LIMBS]{};

public:
    BigInt() = default;
    BigInt(uint64_t value) {
        limbs[0] = value;
    }

    BigInt operator+(const BigInt& other) const {
        BigInt result;
        uint64_t carry = 0;
        for (size_t i = 0; i < LIMBS; ++i) {
            __uint128_t sum = (__uint128_t)limbs[i] + other.limbs[i] + carry;
            result.limbs[i] = (uint64_t)sum;
            carry = (uint64_t)(sum >> 64);
        }
        return result;
    }

    BigInt& operator+=(const BigInt& other) {
        *this = *this + other;
        return *this;
    }

    explicit operator bool() const {
        for (size_t i = 0; i < LIMBS; ++i) {
            if (limbs[i] != 0) return true;
        }
        return false;
    }

    bool operator==(const BigInt& other) const {
        for (size_t i = 0; i < LIMBS; ++i) {
            if (limbs[i] != other.limbs[i]) return false;
        }
        return true;
    }

    bool operator!=(const BigInt& other) const {
        return !(*this == other);
    }

    std::string to_string() const {
        if (!*this) return "0";
        BigInt temp = *this;
        std::string s;
        while (temp) {
            uint64_t remainder = 0;
            for (size_t i = LIMBS; i-- > 0; ) {
                __uint128_t val = ((__uint128_t)remainder << 64) | temp.limbs[i];
                temp.limbs[i] = (uint64_t)(val / 10);
                remainder = (uint64_t)(val % 10);
            }
            s.push_back(char('0' + remainder));
        }
        std::reverse(s.begin(), s.end());
        return s;
    }

    friend ostream& operator<<(ostream& os, const BigInt& n) {
        return os << n.to_string();
    }
};

namespace std {
    template<unsigned BITS>
    struct formatter<BigInt<BITS>> {
        constexpr auto parse(format_parse_context& ctx) {
            return ctx.begin();
        }

        auto format(const BigInt<BITS>& n, format_context& ctx) const {
            return format_to(ctx.out(), "{}", n.to_string());
        }
    };
}