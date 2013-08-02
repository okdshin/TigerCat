#pragma once
//Code:20130718
#include <iostream>
#include <string>

namespace codia
{
class Code{
public:
    Code(const std::string& code_str) : code_str_(code_str){}
	
	auto ToString()const -> const std::string {
		return code_str_;	
	}

	friend auto operator<<(std::ostream& os, const Code& code) -> std::ostream& {
		os << code.code_str_;
		return os;	
	}

private:
	std::string code_str_;

};
}

