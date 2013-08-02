#pragma once
//LabelStack:20130723
#include <iostream>
#include <vector>
#include <boost/lexical_cast.hpp>
#include "Label.h"

namespace codia
{
class LabelStack{
public:
    LabelStack(const Label& base_label) : base_label_(base_label), count_(0) {}
    ~LabelStack(){}

	auto Push() -> void {
		label_stack_.push_back(Label(
			base_label_.ToString()+boost::lexical_cast<std::string>(count_)));
		++count_;
	} 

	auto Top()const -> const Label {
		assert(!label_stack_.empty());
		return label_stack_.back();	
	}

	auto Pop() -> void {
		assert(!label_stack_.empty());
		label_stack_.pop_back();	
	}

private:
	const Label base_label_;
	int count_;
	std::vector<Label> label_stack_;

};
}

