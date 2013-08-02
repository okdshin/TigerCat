#ifndef LEXER_H
#define LEXER_H
//Lexer:20130416
#include <iostream>
#include <string>
#include <exception>
#include <boost/regex.hpp>
#include <boost/format.hpp>

namespace lexia
{
class TokenType{
public:
	static auto LEXIA_UNINITIALIZED_TOKEN_TYPE() -> TokenType { 
		return TokenType("LEXIA_UNINITIALIZED_TOKEN"); }
	static auto LEXIA_EOF_TOKEN_TYPE() -> TokenType { return TokenType("LEXIA_EOF_TOKEN"); }
#ifndef LEXER_UNIT_TEST
	static auto INT() -> TokenType { return TokenType("INT"); }
	static auto IF() -> TokenType { return TokenType("IF"); }
	static auto ELSE() -> TokenType { return TokenType("ELSE"); }
	static auto RETURN() -> TokenType { return TokenType("RETURN"); }
	static auto WHILE() -> TokenType { return TokenType("WHILE"); }
	static auto SEMICOLON() -> TokenType { return TokenType("SEMICOLON"); }
	static auto COMMA() -> TokenType { return TokenType("COMMA"); }
	static auto PLUS() -> TokenType { return TokenType("PLUS"); }
	static auto MINUS() -> TokenType { return TokenType("MINUS"); }
	static auto ASTERISK() -> TokenType { return TokenType("ASTERISK"); }
	static auto SLASH() -> TokenType { return TokenType("SLASH"); }
	static auto PERCENT() -> TokenType { return TokenType("PERCENT"); }
	static auto EQUAL() -> TokenType { return TokenType("EQUAL"); }
	static auto EQUALEQUAL() -> TokenType { return TokenType("EQUALEQUAL"); }
	static auto NOT_EQUAL() -> TokenType { return TokenType("NOT_EQUAL"); }
	static auto AND() -> TokenType { return TokenType("AND"); }
	static auto OR() -> TokenType { return TokenType("OR"); }
	static auto LOWER_THAN() -> TokenType { return TokenType("LOWER_THAN"); }
	static auto HIGHER_THAN() -> TokenType { return TokenType("HIGHER_THAN"); }
	static auto LOWER_EQUAL() -> TokenType { return TokenType("LOWER_EQUAL"); }
	static auto HIGHER_EQUAL() -> TokenType { return TokenType("HIGHER_EQUAL"); }
	static auto LEFT_PARENTHESIS() -> TokenType { return TokenType("LEFT_PARENTHESIS"); }
	static auto RIGHT_PARENTHESIS() -> TokenType { return TokenType("RIGHT_PARENTHESIS"); }
	static auto LEFT_BRACE() -> TokenType { return TokenType("LEFT_BRACE"); }
	static auto RIGHT_BRACE() -> TokenType { return TokenType("RIGHT_BRACE"); }
	static auto CONSTANT() -> TokenType { return TokenType("CONSTANT"); }
	static auto IDENTIFIER() -> TokenType { return TokenType("IDENTIFIER"); }
	static auto IF_WITH_ELSE() -> TokenType { return TokenType("IF_WITH_ELSE"); }
	static auto IF_OF_IF_WITH_ELSE() -> TokenType { return TokenType("IF_OF_IF_WITH_ELSE"); }
	static auto CONS() -> TokenType { return TokenType("CONS"); }
	static auto BLOCK() -> TokenType { return TokenType("BLOCK"); }
	static auto VARIABLE_DECLARATION() -> TokenType { return TokenType("VARIABLE_DECLARATION"); }
	static auto FUNCTION_DECLARATION() -> TokenType { return TokenType("FUNCTION_DECLARATION"); }
	static auto PARAMETER_DECLARATION() -> TokenType { return TokenType("PARAMETER_DECLARATION"); }
	static auto VARIABLE_REFERENCE() -> TokenType { return TokenType("VARIABLE_REFERENCE"); }
	static auto FUNCTION_CALL() -> TokenType { return TokenType("FUNCTION_CALL"); }
	static auto PARAMETER_LIST() -> TokenType { return TokenType("PARAMETER_LIST"); }
	static auto PARAMETER() -> TokenType { return TokenType("PARAMETER"); }
	static auto ASSIGN() -> TokenType { return TokenType("ASSIGN"); }
	static auto LOGICAL_OR() -> TokenType { return TokenType("LOGICAL_OR"); }
	static auto LOGICAL_AND() -> TokenType { return TokenType("LOGICAL_AND"); }
	static auto EQUALITY() -> TokenType { return TokenType("EQUALITY"); }
	static auto NOT_EQUALITY() -> TokenType { return TokenType("NOT_EQUALITY"); }
	static auto RELATIONAL_HIGHER_THAN() -> TokenType { return TokenType("RELATIONAL_HIGHER_THAN"); }
	static auto RELATIONAL_LOWER_THAN() -> TokenType { return TokenType("RELATIONAL_LOWER_THAN"); }
	static auto RELATIONAL_HIGHER_EQUAL() -> TokenType { return TokenType("RELATIONAL_HIGHER_EQUAL"); }
	static auto RELATIONAL_LOWER_EQUAL() -> TokenType { return TokenType("RELATIONAL_LOWER_EQUAL"); }
	static auto ADD() -> TokenType { return TokenType("ADD"); }
	static auto SUB() -> TokenType { return TokenType("SUB"); }
	static auto MUL() -> TokenType { return TokenType("MUL"); }
	static auto DIV() -> TokenType { return TokenType("DIV"); }
	static auto REM() -> TokenType { return TokenType("REM"); }
	static auto UNARY_MINUS() -> TokenType { return TokenType("UNARY_MINUS"); }
#else
	static auto INT() -> TokenType { return TokenType("INT"); }
	static auto SEMICOLON() -> TokenType { return TokenType("SEMICOLON"); }
	static auto EQUAL() -> TokenType { return TokenType("EQUAL"); }
	static auto CONSTANT() -> TokenType { return TokenType("CONSTANT"); }
	static auto IDENTIFIER() -> TokenType { return TokenType("IDENTIFIER"); }	
#endif

	auto ToString()const -> std::string {
		return token_type_str;	
	}

	auto operator==(const TokenType& pair)const -> bool { 
		return this->token_type_str == pair.token_type_str; 
	}

	auto operator!=(const TokenType& pair)const -> bool { 
		return !(*this == pair); 
	}

private:
	TokenType(const std::string& token_type_str) : token_type_str(token_type_str){}
	std::string token_type_str;
};
auto operator<<(std::ostream& os, const TokenType& token_type) -> std::ostream& {
	os << "TokenType:" << token_type.ToString();	
	return os;	
}

class Word{
public:
	Word() : word_str_(){}
    Word(const std::string& word_str) : word_str_(word_str){}

	auto ToString()const -> std::string {
		return word_str_;	
	}

private:
	std::string word_str_;
};
auto operator<<(std::ostream& os, const Word& word) -> std::ostream& {
	os << "Word:" << "\"" << word.ToString() << "\"";
	return os;
}

class Token{
public:
	Token():type_(TokenType::LEXIA_UNINITIALIZED_TOKEN_TYPE()), word_(){}
	Token(const TokenType& type, const Word& word) 
		: type_(type), word_(word){}
	
	static auto EOF_TOKEN() -> Token{
		return Token(TokenType::LEXIA_EOF_TOKEN_TYPE(), Word("EOF_CHARACTOR"));	
	}
	
#ifndef LEXER_UNIT_TEST
	static auto IF_WITH_ELSE_TOKEN() -> Token{
		return Token(TokenType::IF_WITH_ELSE(), Word("IF_WITH_ELSE"));
	}
	static auto IF_OF_IF_WITH_ELSE_TOKEN() -> Token{
		return Token(TokenType::IF_OF_IF_WITH_ELSE(), Word("IF_OF_IF_WITH_ELSE"));
	}
	static auto CONS_TOKEN() -> Token{
		return Token(TokenType::CONS(), Word("CONS"));
	}
	static auto BLOCK_TOKEN() -> Token{
		return Token(TokenType::BLOCK(), Word("BLOCK"));
	}
	static auto VARIABLE_DECLARATION_TOKEN() -> Token{
		return Token(TokenType::VARIABLE_DECLARATION(), Word("VARIABLE_DECLARATION"));
	}
	static auto FUNCTION_DECLARATION_TOKEN() -> Token{
		return Token(TokenType::FUNCTION_DECLARATION(), Word("FUNCTION_DECLARATION"));
	}
	static auto PARAMETER_DECLARATION_TOKEN() -> Token{
		return Token(TokenType::PARAMETER_DECLARATION(), Word("PARAMETER_DECLARATION"));
	}
	static auto VARIABLE_REFERENCE_TOKEN() -> Token{
		return Token(TokenType::VARIABLE_REFERENCE(), Word("VARIABLE_REFERENCE"));
	}
	static auto FUNCTION_CALL_TOKEN() -> Token{
		return Token(TokenType::FUNCTION_CALL(), Word("FUNCTION_CALL"));
	}
	static auto PARAMETER_LIST_TOKEN() -> Token{
		return Token(TokenType::PARAMETER_LIST(), Word("PARAMETER_LIST"));
	}
	static auto PARAMETER_TOKEN() -> Token{
		return Token(TokenType::PARAMETER(), Word("PARAMETER"));
	}
	static auto ASSIGN_TOKEN() -> Token{
		return Token(TokenType::ASSIGN(), Word("ASSIGN"));
	}
	static auto LOGICAL_OR_TOKEN() -> Token{
		return Token(TokenType::LOGICAL_OR(), Word("LOGICAL_OR"));
	}
	static auto LOGICAL_AND_TOKEN() -> Token{
		return Token(TokenType::LOGICAL_AND(), Word("LOGICAL_AND"));
	}
	static auto EQUALITY_TOKEN() -> Token{
		return Token(TokenType::EQUALITY(), Word("EQUALITY"));
	}
	static auto NOT_EQUALITY_TOKEN() -> Token{
		return Token(TokenType::NOT_EQUALITY(), Word("NOT_EQUALITY"));
	}
	static auto RELATIONAL_HIGHER_THAN_TOKEN() -> Token{
		return Token(TokenType::RELATIONAL_HIGHER_THAN(), Word("RELATIONAL_HIGHER_THAN"));
	}
	static auto RELATIONAL_LOWER_THAN_TOKEN() -> Token{
		return Token(TokenType::RELATIONAL_LOWER_THAN(), Word("RELATIONAL_LOWER_THAN"));
	}
	static auto RELATIONAL_HIGHER_EQUAL_TOKEN() -> Token{
		return Token(TokenType::RELATIONAL_HIGHER_EQUAL(), Word("RELATIONAL_HIGHER_EQUAL"));
	}
	static auto RELATIONAL_LOWER_EQUAL_TOKEN() -> Token{
		return Token(TokenType::RELATIONAL_LOWER_EQUAL(), Word("RELATIONAL_LOWER_EQUAL"));
	}
	static auto ADD_TOKEN() -> Token{
		return Token(TokenType::ADD(), Word("ADD"));
	}
	static auto SUB_TOKEN() -> Token{
		return Token(TokenType::SUB(), Word("SUB"));
	}
	static auto MUL_TOKEN() -> Token{
		return Token(TokenType::MUL(), Word("MUL"));
	}
	static auto DIV_TOKEN() -> Token{
		return Token(TokenType::DIV(), Word("DIV"));
	}
	static auto REM_TOKEN() -> Token{
		return Token(TokenType::REM(), Word("REM"));
	}
	static auto UNARY_MINUS_TOKEN() -> Token{
		return Token(TokenType::UNARY_MINUS(), Word("UNARY_MINUS"));
	}
#endif
	auto IsEof()const -> bool {
		return type_ == TokenType::LEXIA_EOF_TOKEN_TYPE();		
	}
	
	auto IsUninitialized()const -> bool {
		return type_ == TokenType::LEXIA_UNINITIALIZED_TOKEN_TYPE();		
	}

	auto GetType()const -> TokenType {
		return type_;
	}

	auto GetWord()const -> Word {
		return word_;
	}
	
private:
	TokenType type_;
	Word word_;
};
auto operator<<(std::ostream& os, const Token& token) -> std::ostream& {
	os << "Token:" << token.GetType() << ", " << token.GetWord();
	return os;	
}

class InvalidCharactorError : public std::exception {
public:
	InvalidCharactorError(const std::string& message) : 
		message_(message) {}
	virtual ~InvalidCharactorError() throw() {}
	virtual const char* what() const throw() {
		return message_.c_str();
	}
private:
	const std::string message_;
};

class Lexer{
public:
    Lexer(const std::string& code) : 
		code_(code){}


	auto GetNextToken() -> Token {	
		std::cout << code_ << std::endl;
		{
#ifndef LEXER_UNIT_TEST
			boost::regex ignore_reg("\\A[ \\t\\n]*");
#else
			boost::regex ignore_reg("\\A[ \\t\\n]*");
#endif
			boost::smatch matched;
			boost::regex_search(code_, matched, ignore_reg);
			auto matched_str = matched.str();
			std::cout << matched_str.length() << std::endl;
			code_.erase(0, matched_str.length());
		}
		if(code_.empty()){
			return Token::EOF_TOKEN();
		}

		std::vector<Token> regular_expression_token_list;
#ifndef LEXER_UNIT_TEST
		regular_expression_token_list.push_back(
			Token(TokenType::INT(), Word("\\Aint")));
		regular_expression_token_list.push_back(
			Token(TokenType::IF(), Word("\\Aif")));
		regular_expression_token_list.push_back(
			Token(TokenType::ELSE(), Word("\\Aelse")));
		regular_expression_token_list.push_back(
			Token(TokenType::RETURN(), Word("\\Areturn")));
		regular_expression_token_list.push_back(
			Token(TokenType::WHILE(), Word("\\Awhile")));
		regular_expression_token_list.push_back(
			Token(TokenType::SEMICOLON(), Word("\\A;")));
		regular_expression_token_list.push_back(
			Token(TokenType::COMMA(), Word("\\A,")));
		regular_expression_token_list.push_back(
			Token(TokenType::PLUS(), Word("\\A\\+")));
		regular_expression_token_list.push_back(
			Token(TokenType::MINUS(), Word("\\A-")));
		regular_expression_token_list.push_back(
			Token(TokenType::ASTERISK(), Word("\\A\\*")));
		regular_expression_token_list.push_back(
			Token(TokenType::SLASH(), Word("\\A/")));
		regular_expression_token_list.push_back(
			Token(TokenType::PERCENT(), Word("\\A%")));
		regular_expression_token_list.push_back(
			Token(TokenType::EQUAL(), Word("\\A=")));
		regular_expression_token_list.push_back(
			Token(TokenType::EQUALEQUAL(), Word("\\A==")));
		regular_expression_token_list.push_back(
			Token(TokenType::NOT_EQUAL(), Word("\\A!=")));
		regular_expression_token_list.push_back(
			Token(TokenType::AND(), Word("\\A&&")));
		regular_expression_token_list.push_back(
			Token(TokenType::OR(), Word("\\A\\|\\|")));
		regular_expression_token_list.push_back(
			Token(TokenType::LOWER_THAN(), Word("\\A<")));
		regular_expression_token_list.push_back(
			Token(TokenType::HIGHER_THAN(), Word("\\A>")));
		regular_expression_token_list.push_back(
			Token(TokenType::LOWER_EQUAL(), Word("\\A<=")));
		regular_expression_token_list.push_back(
			Token(TokenType::HIGHER_EQUAL(), Word("\\A>=")));
		regular_expression_token_list.push_back(
			Token(TokenType::LEFT_PARENTHESIS(), Word("\\A\\(")));
		regular_expression_token_list.push_back(
			Token(TokenType::RIGHT_PARENTHESIS(), Word("\\A\\)")));
		regular_expression_token_list.push_back(
			Token(TokenType::LEFT_BRACE(), Word("\\A\\{")));
		regular_expression_token_list.push_back(
			Token(TokenType::RIGHT_BRACE(), Word("\\A\\}")));
		regular_expression_token_list.push_back(
			Token(TokenType::CONSTANT(), Word("\\A[0-9]+")));
		regular_expression_token_list.push_back(
			Token(TokenType::IDENTIFIER(), Word("\\A[a-z_][a-z0-9_]*")));
#else
		std::cout << "!!!UNIT TEST SAMPLE!!!" << std::endl;
		regular_expression_token_list.push_back(
			Token(TokenType::INT(), Word("\\Aint")));
		regular_expression_token_list.push_back(
			Token(TokenType::SEMICOLON(), Word("\\A;")));
		regular_expression_token_list.push_back(
			Token(TokenType::EQUAL(), Word("\\A=")));
		regular_expression_token_list.push_back(
			Token(TokenType::CONSTANT(), Word("\\A[0-9]+")));
		regular_expression_token_list.push_back(
			Token(TokenType::IDENTIFIER(), Word("\\A[a-zA-Z_][a-zA-Z0-9_]*")));	
#endif
		std::vector<Token> matched_token_list;
		for(const auto reg_token : regular_expression_token_list){
			const boost::regex reg(reg_token.GetWord().ToString());
			boost::smatch matched;
			if(boost::regex_search(code_, matched, reg)){
				std::cout << "matched:" << matched.str() 
					<< " as " << reg_token.GetType() << std::endl;
				matched_token_list.push_back(
					Token(reg_token.GetType(), matched.str()));	
			}
		}

		if(matched_token_list.empty()){
			const auto invalid_char = std::string(1, code_.front());
			auto message_format = boost::format("InvalidCharactorError: \"%1%\"(CharCode: %2%) is invalid charactor.") % invalid_char % int(code_.front());
			throw InvalidCharactorError(message_format.str());
		}

		const auto longest_matched_token = 
			*std::max_element(matched_token_list.begin(), matched_token_list.end(),
				[](const Token& left, const Token& right) -> bool {
					return left.GetWord().ToString().length() 
						< right.GetWord().ToString().length();
				}
			);
		
		std::cout << "before:" << code_ << std::endl;
		code_.erase(0, longest_matched_token.GetWord().ToString().length());
		std::cout << "after:" << code_ << std::endl;
		return longest_matched_token; 
	}

private:
	std::string code_;
};
}
#endif