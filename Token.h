#pragma once
//Token:20130625
#include <iostream>
#include <memory>
#include "Symbolia/Depth.h"
#include "Symbolia/Offset.h"
#include "Symbolia/NumOfParameters.h"
#include "Kind.h"

namespace tiger_cat
{
class TokenType{
public:
	TokenType() : token_type_str(){}
	TokenType(const std::string& token_type_str) : 
		token_type_str(token_type_str){}
	auto ToString()const -> const std::string { return token_type_str; }
	auto operator==(const TokenType& pair)const -> const bool { 
		return this->token_type_str == pair.token_type_str; 
	}
	auto operator!=(const TokenType& pair)const -> const bool { 
		return !(*this == pair); 
	}
private:
	std::string token_type_str;
};
auto operator<<(std::ostream& os, const TokenType& type) -> std::ostream& {
	os << type.ToString();
	return os;
}

class Word{
public:
	Word() : word_str_(){}
    Word(const std::string& word_str) : word_str_(word_str){}
	auto ToString()const -> std::string { return word_str_; }
private:
	std::string word_str_;
};
auto operator<<(std::ostream& os, const Word& word) -> std::ostream& {
	os << word.ToString();
	return os;
}

class Token{
public:
	using Ptr = std::shared_ptr<Token>;

	static auto Create(const TokenType& type, const Word& word) -> Ptr {
		return Ptr(new Token(type, word));	
	}

	auto SetKind(const Kind& kind) -> void {
		kind_ = kind;	
	}

	auto SetDepth(const symbolia::Depth& depth) -> void {
		depth_ = depth;	
	}

	auto SetOffset(const symbolia::Offset& offset) -> void {
		offset_ = offset;	
	}
	
	auto SetNumOfParameters(
			const symbolia::NumOfParameters& num_of_params) -> void {
		num_of_params_ = num_of_params;	
	}

	auto GetType()const -> const TokenType { return type_; }
	auto GetWord()const -> const Word { return word_; }

	auto GetKind()const -> const Kind { return kind_; }
	auto GetDepth()const -> const symbolia::Depth { return depth_; }
	auto GetOffset()const -> const symbolia::Offset { return offset_; }
	auto GetNumOfParameters()const -> const symbolia::NumOfParameters {
		return num_of_params_;
	}

private:
	Token(const TokenType& type, const Word& word) 
		: type_(type), word_(word), kind_(), depth_(), offset_(){}
	
	TokenType type_;
	Word word_;

	Kind kind_;
	symbolia::Depth depth_;
	symbolia::Offset offset_;
	symbolia::NumOfParameters num_of_params_;
};
auto operator<<(std::ostream& os, const Token& token) -> std::ostream& {
	os << "Type:" << token.GetType() << " Word:" << token.GetWord();
	if(token.GetKind().IsUninitialized()){
		os << "Kind:" << token.GetKind() << 
		" Depth: " << token.GetDepth() << 
		" Offset:" << token.GetOffset();	
	}
	return os;
}
/*
auto operator<<(std::ostream& os, const Token::Ptr& token_ptr) -> std::ostream& {
	if(token_ptr){
		os << token_ptr << " " << *token_ptr;
	}
	else {
		os << token_ptr;	
	}
	return os;
}
*/
}

