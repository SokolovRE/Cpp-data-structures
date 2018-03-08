
template <typename Type>
class Polynomial {
 private:
    std::map<size_t, Type> coefficients_;

 public:
    Polynomial(const std::vector<Type>& coefficients) {
        for (size_t i = 0; i < coefficients.size(); ++i) {
            if (coefficients[i] != 0) {
                coefficients_[i] = coefficients[i];
            }
        }
    }

    Polynomial(Type coefficient = Type()) {

        if (coefficient != 0) {
            coefficients_[0] = coefficient;
        }
    }

    template<class iterator>
    Polynomial(iterator begin, iterator end) {
        size_t power = 0;
        for (; begin != end; ++begin) {
            if (*begin != 0) {
                coefficients_[power] = *begin;
            }
            ++power;
        }
    }

    const Type operator[](size_t power) const {
        if (coefficients_.count(power)) {
            return coefficients_.at(power);
        } else {
            return 0;
        }
    }

    int Degree() const {
        if (coefficients_.size() == 0) {
           return -1;
        }
        auto last = coefficients_.end();
        --last;
        return last->first;
    }

    typename std::map<size_t, Type>::const_iterator begin() const {
        return coefficients_.begin();
    }

    typename std::map<size_t, Type>::const_iterator end() const {
        return coefficients_.end();
    }

    friend std::ostream& operator<<(std::ostream& stream
                                  , const Polynomial<Type>& Polynom) {
        if (Polynom.Degree() == -1) {
            stream << 0;
            return stream;
        }
        auto element = Polynom.coefficients_.rbegin();
        auto last = Polynom.coefficients_.rend();
        while (element != last) {
            if (element->second == -1) {
                stream << '-';
                if (element->first == 0) {
                    stream << 1;
                }
            } else if (element->second != 1) {
                stream << element->second;
            } else if (element->first == 0) {
                stream << 1;
            }
            if (element->first > 0) {
                if (element->second != 1 && element->second != -1) {
                    stream << '*';
                }
                stream << 'x';
                if (element->first > 1) {
                    stream << '^' << element->first;
                }
            }
            ++element;
            if (element != last && element->second > 0) {
                stream << '+';
            }
        }
        return stream;
    }

    friend bool operator==(const Polynomial<Type>& left
                         , const Polynomial<Type>& right) {
        return left.coefficients_ == right.coefficients_;
    }

    friend bool operator!=(const Polynomial<Type>& left
                         , const Polynomial<Type>& right) {
        return left.coefficients_ != right.coefficients_;
    }

    friend Polynomial<Type> operator+(const Polynomial<Type>& left
                                    , const Polynomial<Type>& right) {
        Polynomial<Type> sum = left;
        for (auto iterator = right.begin(); iterator != right.end(); ++iterator) {
            sum.coefficients_[iterator->first] += iterator->second;
            if (sum.coefficients_[iterator->first] == 0) {
                sum.coefficients_.erase(iterator->first);
            }
        }
        return sum;
    }

    friend Polynomial<Type> operator-(const Polynomial<Type>& left
                                    , const Polynomial<Type>& right) {
        Polynomial<Type> deduction = left;
        for (auto iterator = right.begin(); iterator != right.end(); ++iterator) {
            deduction.coefficients_[iterator->first] -= iterator->second;
            if (deduction.coefficients_[iterator->first] == 0) {
                deduction.coefficients_.erase(iterator->first);
            }
        }
        return deduction;
    }

    friend Polynomial<Type> operator*(const Polynomial<Type>& left
                                    , const Polynomial<Type>& right) {
        Polynomial<Type> multiplication;
        for (auto l_iterator = left.begin(); l_iterator != left.end(); ++l_iterator) {
            for (auto r_iterator = right.begin(); r_iterator != right.end(); ++r_iterator) {
                multiplication.coefficients_[l_iterator->first + r_iterator->first]
                                                += l_iterator->second * r_iterator->second;
                if (multiplication.coefficients_[l_iterator->first + r_iterator->first] == 0) {
                    multiplication.coefficients_.erase(l_iterator->first + r_iterator->first);
                }
            }
        }
        return multiplication;
    }

    friend Polynomial<Type>& operator+=(Polynomial<Type>& left
                                      , const Polynomial<Type>& right) {
        for (auto iterator = right.begin(); iterator != right.end(); ++iterator) {
            left.coefficients_[iterator->first] += iterator->second;
            if (left.coefficients_[iterator->first] == 0) {
                left.coefficients_.erase(iterator->first);
            }
        }
        return left;
    }

    friend Polynomial<Type>& operator-=(Polynomial<Type>& left
                                      , const Polynomial<Type>& right) {
        for (auto iterator = right.begin(); iterator != right.end(); ++iterator) {
            left.coefficients_[iterator->first] -= iterator->second;
            if (left.coefficients_[iterator->first] == 0) {
                left.coefficients_.erase(iterator->first);
            }
        }
        return left;
    }

    friend Polynomial<Type>& operator*=(Polynomial<Type>& left
                                      , const Polynomial<Type>& right) {
        Polynomial<Type> multiplication;
        for (auto l_iterator = left.begin(); l_iterator != left.end(); ++l_iterator) {
            for (auto r_iterator = right.begin(); r_iterator != right.end(); ++r_iterator) {
                multiplication.coefficients_[l_iterator->first + r_iterator->first]
                                                += l_iterator->second * r_iterator->second;
                if (multiplication.coefficients_[l_iterator->first + r_iterator->first] == 0) {
                    multiplication.coefficients_.erase(l_iterator->first + r_iterator->first);
                }
            }
        }
        left = multiplication;
        return left;
    }

    Polynomial<Type> operator/(const Polynomial<Type>& other) const {
        Polynomial<Type> quotient, polynom = *this;
        while (polynom.Degree() - other.Degree() >= 0) {
            quotient.coefficients_[polynom.Degree() - other.Degree()]
                                        = polynom[polynom.Degree()] / other[other.Degree()];
            Polynomial<Type> var;
            var.coefficients_[polynom.Degree() - other.Degree()]
                                        = polynom[polynom.Degree()] / other[other.Degree()];
            polynom -= var * other;
        }
        return quotient;
    }

    Polynomial<Type> operator%(const Polynomial<Type>& other) const {
        Polynomial<Type> polynom = *this, var;
        while (polynom.Degree() - other.Degree() >= 0) {
            Polynomial<Type> var;
            var.coefficients_[polynom.Degree() - other.Degree()]
                                    = polynom[polynom.Degree()] / other[other.Degree()];
            polynom -= var * other;
        }
        return polynom;
    }

    Polynomial<Type> operator,(const Polynomial<Type>& other) const {
        Polynomial<Type> first_polynom = *this, second_polynom = other;
        while (first_polynom.Degree() != -1 && second_polynom.Degree() != -1) {
            if (first_polynom.Degree() > second_polynom.Degree()) {
                first_polynom = first_polynom % second_polynom;
            } else if (first_polynom.Degree() == second_polynom.Degree()
                         && first_polynom[first_polynom.Degree()]
                         > second_polynom[second_polynom.Degree()]) {
                first_polynom = first_polynom % second_polynom;
            } else {
                second_polynom = second_polynom % first_polynom;
            }
        }
        first_polynom += second_polynom;
        return first_polynom / first_polynom[first_polynom.Degree()];
    }

    Type operator()(const Type& var) const {
        Type temp_var = 1, value = 0;
        size_t power = 0;
        for (auto iterator = coefficients_.begin(); iterator != coefficients_.end(); ++iterator) {
            while (power < iterator->first) {
                temp_var *= var;
                ++power;
            }
            value += temp_var * iterator->second;
        }
        return value;
    }
};
