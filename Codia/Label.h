#pragma once
//Label:20130723
#include <iostream>
#include <string>

namespace codia
{
class Label{
public:
    Label(const std::string& label_str) : label_str_(label_str){}

	auto ToString()const -> const std::string {
		return label_str_;	
	}

private:
	const std::string label_str_;

};
}

