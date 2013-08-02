#pragma once
//BasicStack:20130726
#include <iostream>
#include <vector>
#include <cassert>

namespace codia
{
template<class Type>
class BasicStack{
public:
    BasicStack() : vect_(){}

	auto Push(const Type& val) -> void {
		vect_.push_back(val);	
	}

	auto Top()const -> const Type {
		assert(!vect_.empty());
		return vect_.back();	
	}

	auto Pop() -> void {
		assert(!vect_.empty());
		vect_.pop_back();	
	}

private:
	std::vector<Type> vect_;
};
}

