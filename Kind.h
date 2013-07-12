#pragma once
//Kind:20130628
#include <iostream>

namespace tiger_cat
{
class Kind{
public:
	Kind() : kind_str_(){}

	static auto FUNCTION_CALL() -> const Kind {
		return Kind("FUNCTION_CALL");
	}

	static auto FUNCTION_DECLARATION() -> const Kind {
		return Kind("FUNCTION_DECLARATION");
	}

	static auto VARIABLE_REFERENCE() -> const Kind {
		return Kind("VARIABLE_REFERENCE");
	}

	static auto VARIABLE_DECLARATION() -> const Kind {
		return Kind("VARIABLE_DECLARATION");
	}

	auto ToString()const -> const std::string {
		return kind_str_;
	}

	auto IsUninitialized()const -> const bool {
		return kind_str_.empty();	
	}
private:
    Kind(const std::string& kind_str) : kind_str_(kind_str){}
	std::string kind_str_;

};
auto operator==(const Kind& left, const Kind& right) -> bool {
	return left.ToString() == right.ToString();
}
auto operator!=(const Kind& left, const Kind& right) -> bool {
	return !(left == right);
}
auto operator<<(std::ostream& os, const Kind& word) -> std::ostream& {
	os << "Kind: " << word.ToString();
	return os;	
}
}

