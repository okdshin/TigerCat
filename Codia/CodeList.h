#pragma once
//CodeList:20130718
#include <iostream>
#include <vector>
#include <memory>
#include <boost/lexical_cast.hpp>
#include "Code.h"

namespace codia
{
class CodeList{
public:
	using Ptr = std::shared_ptr<CodeList>;
	static auto Create() -> Ptr {
		return Ptr(new CodeList());	
	}

	auto Pushback(const Code& code) -> void {
		code_vect_.push_back(code);	
	}

	auto GetCurrentLineNumberStr()const -> const std::string {
		return boost::lexical_cast<std::string>(code_vect_.size());
	}

	auto PushCurrentCode() -> void {
		mark_list_.push_back(code_vect_.size()-1);	
	}

	auto ChangeAndPopTopCode(const Code& code) -> void {
		code_vect_[mark_list_.back()] = code;
		mark_list_.pop_back();
	}
	
	auto Output(std::ostream& os)const -> void {
		for(const auto& code : code_vect_){
			os << code << "\n";	
		}
	}

	auto RemoveCommandsAfterJump() -> void {
		for(unsigned int i = 0; i < code_vect_.size(); ++i){
			if(code_vect_[i].ToString()[0] == '\t' 
					&& code_vect_[i].ToString()[1] == 'j'
					&& code_vect_[i].ToString()[2] == 'm'
					&& code_vect_[i].ToString()[3] == 'p') {
				++i;
				while(i < code_vect_.size() && 
						(code_vect_[i].ToString()[0] == '\t' 
							|| code_vect_[i].ToString()[0] == ';')){
					code_vect_[i] = Code(";"+code_vect_[i].ToString());
					++i;
				}
			}
		}
	}

private:
    CodeList(){}

	std::vector<Code> code_vect_;
	std::vector<int> mark_list_;

};
}

