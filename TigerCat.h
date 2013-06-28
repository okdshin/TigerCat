#pragma once
//TigerCat:20130624
//#define PARSIA_TOKEN_BUFFER_DEBUG_ON
//#define LEXIA_LEXER_DEBUG_ON
#include <iostream>
#include "Lexia/Lexer.h"
#include "Parsia/BasicParser.h"
#include "Parsia/BasicSyntaxRule.h"
#include "Semantia/BasicTree.h"
#include "Semantia/Token.h"
#include "Semantia/SyntaxTreeStream.h"
#include "Symbolia/SymbolTable.h"
#include "Symbolia/BaseScope.h"
#include "Symbolia/GlobalScope.h"
#include "Symbolia/VariableSymbol.h"
#include "Symbolia/FunctionSymbol.h"

namespace tiger_cat
{

using Ast = semantia::BasicTree<semantia::Token>;

auto ParsiaToken(const lexia::Token& lexia_token) -> const parsia::Token {
	return parsia::Token(
		lexia_token.GetType().ToString(),
		parsia::Word(lexia_token.GetWord().ToString())
	);	
}

auto ParsiaToken(const semantia::Token& semantia_token) -> const parsia::Token {
	return parsia::Token(
		semantia_token.GetType().ToString(),
		parsia::Word(semantia_token.GetWord().ToString())
	);	
}

auto SymboliaWord(const semantia::Word& word) -> const symbolia::Word {
	return symbolia::Word(word.ToString());	
}

auto CreateAst(const lexia::Token& token) -> const Ast::Ptr {
	semantia::Token semantia_token(
		semantia::TokenType(token.GetType().ToString()), 
		semantia::Word(token.GetWord().ToString()));
	return Ast::Create(semantia_token);
}

auto CreateAst(const parsia::Token& token) -> const Ast::Ptr {
	semantia::Token semantia_token(
		semantia::TokenType(token.GetType().ToString()), 
		semantia::Word(token.GetWord().ToString()));
	return Ast::Create(semantia_token);
}

auto CreateAst(const semantia::Token& token) -> const Ast::Ptr {
	return Ast::Create(token);
}

auto operator==(const parsia::TokenType& left, const lexia::TokenType& right) -> bool {
	return left.ToString() == right.ToString();	
}

auto operator!=(const parsia::TokenType& left, const lexia::TokenType& right) -> bool {
	return !(left == right);	
}

auto operator==(const parsia::TokenType& left, const semantia::TokenType& right) -> bool {
	return left.ToString() == right.ToString();	
}

auto operator!=(const parsia::TokenType& left, const semantia::TokenType& right) -> bool {
	return !(left == right);	
}

auto Match(const parsia::TokenBuffer::Ptr& buffer, 
		const lexia::TokenType& type) -> const semantia::Token {
	auto parsia_token = buffer->Match(parsia::TokenType(type.ToString()));
	return semantia::Token(
		semantia::TokenType(parsia_token.GetType().ToString()),
		semantia::Word(parsia_token.GetWord().ToString()));
}

auto Match(const parsia::TokenBuffer::Ptr& buffer, 
		const semantia::TokenType& type) -> const semantia::Token {
	auto parsia_token = buffer->Match(parsia::TokenType(type.ToString()));
	return semantia::Token(
		semantia::TokenType(parsia_token.GetType().ToString()),
		semantia::Word(parsia_token.GetWord().ToString()));
}

auto Match(const parsia::TokenBuffer::Ptr& buffer, 
		const parsia::TokenType& type) -> const semantia::Token {
	auto parsia_token = buffer->Match(parsia::TokenType(type.ToString()));
	return semantia::Token(
		semantia::TokenType(parsia_token.GetType().ToString()),
		semantia::Word(parsia_token.GetWord().ToString()));
}

class TigerCat{
public:
    TigerCat(const std::string& code) : 
		lexer_(code),
		language_parser_(),
		ast_root_(),
		ast_stream_(),
		ast_parser_(){}
    ~TigerCat(){}

	auto Define() -> void {
		DefineLanguageSyntax();
		DefineTreePattern();
	}

	auto InitLanguageTokenBuffer() -> void {
		language_parser_.InitTokenBuffer(parsia::TokenBuffer::NextTokenGetter(
				[this]() -> parsia::Token {
			return ParsiaToken(lexer_.GetNextToken());
		}));
	}

	auto InitAstTokenBuffer() -> void {
		ast_parser_.InitTokenBuffer(parsia::TokenBuffer::NextTokenGetter(
				[this]() -> parsia::Token {
			const auto token = ParsiaToken(ast_stream_->GetNextToken());
			//std::cout << token << std::endl;
			return token;
		}));
	}

	auto InitAstStream() -> void {
		ast_stream_ = semantia::SyntaxTreeStream::Create(ast_root_);	
	}

	auto MakeAbstractSyntaxTree() -> const Ast::Ptr {
		try{
			ast_root_ = language_parser_.ProcessRule("program");
		}
		catch(const parsia::SyntaxError& e){
			std::cout << e.what() << std::endl;
		}
		return ast_root_;
	}

	auto MakeSymbolTable() -> void {
		const auto global_scope = symbolia::GlobalScope::Create();
		symbol_table_.PushScope(global_scope);
		try{
			ast_parser_.ProcessRule("pattern");	
		}
		catch(const parsia::SyntaxError& e){
			std::cout << e.what() << std::endl;
		}
	}

private:
	using LanguageParser = parsia::BasicParser<Ast::Ptr>;
	using AstParser = parsia::BasicParser<Ast::Ptr>;

	lexia::Lexer lexer_;
	LanguageParser language_parser_;
	Ast::Ptr ast_root_;
	semantia::SyntaxTreeStream::Ptr ast_stream_;
	AstParser ast_parser_;
	symbolia::SymbolTable symbol_table_;

private:
	auto DefineTreePattern() -> void {
		ast_parser_.DefineSyntaxRule("pattern")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				Match(buffer, lexia::TokenType::VARIABLE_REFERENCE());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				const auto var_token = 
					Match(buffer, lexia::TokenType::IDENTIFIER());
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::VARIABLE_REFERENCE());
				if(!buffer->IsSpeculating()){
					std::cout << "##variable reference!!" << std::endl;
					std::cout << symbolia::SymbolTable::Resolve(symbol_table_, SymboliaWord(var_token.GetWord())) << std::endl;
				}
				processor("pattern");
				return Ast::Ptr();
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				Match(buffer, lexia::TokenType::VARIABLE_DECLARATION());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::INT());
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::VARIABLE_DECLARATION());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				auto token = 
					Match(buffer, lexia::TokenType::IDENTIFIER());
				if(!buffer->IsSpeculating()){
					std::cout << "##variable declaration!!" << std::endl;
					auto variable_symbol = symbolia::VariableSymbol::Create(
						SymboliaWord(token.GetWord()), symbolia::Offset(4)
						);
					symbolia::SymbolTable::Define(symbol_table_, variable_symbol);
				}
				processor("pattern");
				return Ast::Ptr();
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				Match(buffer, lexia::TokenType::FUNCTION_DECLARATION());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::CONS());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::INT());
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::CONS());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				const auto func_name = 
					Match(buffer, lexia::TokenType::IDENTIFIER());
				if(!buffer->IsSpeculating()){
					std::cout << "##function declaration!!" << std::endl;
					const auto function_symbol = symbolia::FunctionSymbol::Create(
						SymboliaWord(func_name.GetWord()),
						symbolia::NumOfParameters(2),
						symbol_table_.GetCurrentScope());
					symbolia::SymbolTable::Define(symbol_table_, function_symbol);
					symbol_table_.PushScope(function_symbol);
					symbolia::SymbolTable::DebugPrint(symbol_table_);
				}
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::CONS());
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::FUNCTION_DECLARATION());
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::CONS());
				while(buffer->LookAheadTokenType(1) 
						!= semantia::TokenType::STEP_UP_TOKEN_TYPE()){
					Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
					Match(buffer, lexia::TokenType::PARAMETER_DECLARATION());
					Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
					Match(buffer, lexia::TokenType::INT());
					Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
					Match(buffer, lexia::TokenType::PARAMETER_DECLARATION());
					Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
					auto argument_name = 
						Match(buffer, lexia::TokenType::IDENTIFIER());
					if(!buffer->IsSpeculating()){
						std::cout << "##parameter declaration!!" << std::endl;
						const auto variable_symbol = 
							symbolia::VariableSymbol::Create(
								SymboliaWord(argument_name.GetWord()), symbolia::Offset(4));
						symbolia::SymbolTable::Define(
							symbol_table_, variable_symbol);
					}
					Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
					Match(buffer, lexia::TokenType::PARAMETER_DECLARATION());
					Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
					Match(buffer, lexia::TokenType::CONS());
				}
				processor("pattern");
				return Ast::Ptr();
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::BLOCK());
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				if(!buffer->IsSpeculating()){
					std::cout << "##empty block!!" << std::endl;
				}
				processor("pattern");
				return Ast::Ptr();
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::BLOCK());
				if(!buffer->IsSpeculating()){
					std::cout << "##enter block!!" << std::endl;
					const auto base_scope = symbolia::BaseScope::Create(
						symbol_table_.GetCurrentScope());
					symbol_table_.PushScope(base_scope);
					symbolia::SymbolTable::DebugPrint(symbol_table_);
				}
				processor("pattern");
				return Ast::Ptr();
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				if(buffer->LookAheadTokenType(1)
						== semantia::TokenType::SEMANTIA_EOF_TOKEN_TYPE()){
					return Ast::Ptr();
				}else{
					Match(buffer, lexia::TokenType::BLOCK());
					Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
					if(!buffer->IsSpeculating()){
						std::cout << "##exit block!!" << std::endl;
						symbol_table_.PopScope();
						symbolia::SymbolTable::DebugPrint(symbol_table_);
					}
					processor("pattern");
					return Ast::Ptr();
				}
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> Ast::Ptr {
				if(!buffer->IsSpeculating()){
					//std::cout << "pass token" << std::endl;
				}
				if(buffer->LookAheadTokenType(1)
						== semantia::TokenType::SEMANTIA_EOF_TOKEN_TYPE()){
					return Ast::Ptr();
				}else{
					buffer->Match(buffer->LookAheadTokenType(1));
					processor("pattern");
					return Ast::Ptr();
				}
			}))
			;
	}

	auto DefineLanguageSyntax() -> void {
		language_parser_.DefineSyntaxRule("program")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("external_declaration"));	
				while(buffer->LookAheadTokenType(1) 
						!= lexia::TokenType::LEXIA_EOF_TOKEN_TYPE()){
					cons->AddChild(processor("external_declaration"));	
				}
				return cons;
			}));

		language_parser_.DefineSyntaxRule("external_declaration")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##external_declaration1");
				return processor("declaration");
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##external_declaration2");
				return processor("function_definition");
			}));

		language_parser_.DefineSyntaxRule("declaration")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##declaration");
				auto cons = 
					CreateAst(lexia::Token::VARIABLE_DECLARATION_TOKEN());
				cons->AddChild(Ast::Create(Match(buffer, lexia::TokenType::INT())));
				cons->AddChild(processor("declarator_list"));
				Match(buffer, lexia::TokenType::SEMICOLON());
				return cons;
			}));

		language_parser_.DefineSyntaxRule("declarator_list")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##declarator_list");
				auto dec = processor("declarator");
				if(buffer->LookAheadTokenType(1) 
						!= lexia::TokenType::COMMA()){
					return dec;
				}
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(dec);
				while(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::COMMA()){
					Match(buffer, lexia::TokenType::COMMA());
					cons->AddChild(processor("declarator"));
				}
				return cons;
			}));	

		language_parser_.DefineSyntaxRule("declarator")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##declarator");
				return Ast::Create(Match(buffer, lexia::TokenType::IDENTIFIER()));	
			}));

		language_parser_.DefineSyntaxRule("function_definition")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##function_definition1");
				auto cons1 = 
					CreateAst(lexia::Token::FUNCTION_DECLARATION_TOKEN());
				auto cons2 = CreateAst(lexia::Token::CONS_TOKEN());
				cons1->AddChild(cons2);
				cons2->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::INT())));
				cons2->AddChild(processor("declarator"));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				cons1->AddChild(processor("compound_statement"));
				return cons1;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##function_definition2");
				auto cons1 = 
					CreateAst(lexia::Token::FUNCTION_DECLARATION_TOKEN());
				auto cons2 = CreateAst(lexia::Token::CONS_TOKEN());
				cons1->AddChild(cons2);
				cons2->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::INT())));
				cons2->AddChild(processor("declarator"));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				cons1->AddChild(processor("parameter_type_list"));
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				cons1->AddChild(processor("compound_statement"));
				return cons1;
			}))
			;	

		language_parser_.DefineSyntaxRule("parameter_type_list")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##parameter_type_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("parameter_declaration"));
				while(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::COMMA()){
					Match(buffer, lexia::TokenType::COMMA());
					cons->AddChild(processor("parameter_declaration"));
				}
				return cons;
			}));

		language_parser_.DefineSyntaxRule("parameter_declaration")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##parameter_declaration");
				auto cons = CreateAst(lexia::Token::PARAMETER_DECLARATION_TOKEN());
				cons->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::INT())));
				cons->AddChild(processor("declarator"));
				return cons;
			}));

		language_parser_.DefineSyntaxRule("statement")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement1");
				Match(buffer, lexia::TokenType::SEMICOLON());
				return CreateAst(lexia::Token::CONS_TOKEN());
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement2");
				auto exp = processor("expression");
				Match(buffer, lexia::TokenType::SEMICOLON());
				return exp;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement3");
				return processor("compound_statement");
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement4");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::IF())));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				Match(buffer, lexia::TokenType::ELSE());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement5");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::IF())));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement6");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::WHILE())));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement7");
				auto ret = Ast::Create(
					Match(buffer, lexia::TokenType::RETURN()));
				Match(buffer, lexia::TokenType::SEMICOLON());
				return ret;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement8");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::RETURN())));
				cons->AddChild(processor("expression"));
				Match(buffer, lexia::TokenType::SEMICOLON());
				return cons;
			}));

		language_parser_.DefineSyntaxRule("compound_statement")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##compound_statement1");
				Match(buffer, lexia::TokenType::LEFT_BRACE());
				Match(buffer, lexia::TokenType::RIGHT_BRACE());
				return CreateAst(lexia::Token::BLOCK_TOKEN());
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##compound_statement2");
				auto block = CreateAst(lexia::Token::BLOCK_TOKEN());
				Match(buffer, lexia::TokenType::LEFT_BRACE());
				block->AddChild(processor("declaration_list"));
				Match(buffer, lexia::TokenType::RIGHT_BRACE());
				return block;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##compound_statement3");
				auto block = CreateAst(lexia::Token::BLOCK_TOKEN());
				Match(buffer, lexia::TokenType::LEFT_BRACE());
				block->AddChild(processor("statement_list"));
				Match(buffer, lexia::TokenType::RIGHT_BRACE());
				return block;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##compound_statement4");
				auto block = CreateAst(lexia::Token::BLOCK_TOKEN());
				Match(buffer, lexia::TokenType::LEFT_BRACE());
				block->AddChild(processor("declaration_list"));
				block->AddChild(processor("statement_list"));
				Match(buffer, lexia::TokenType::RIGHT_BRACE());
				return block;
			}));
		
		language_parser_.DefineSyntaxRule("declaration_list")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##declaration_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("declaration"));
				while(buffer->LookAheadTokenType(1) == lexia::TokenType::INT()){
					cons->AddChild(processor("declaration"));
				}
				return cons;
			}));

		language_parser_.DefineSyntaxRule("statement_list")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##statement_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("statement"));
				while(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::SEMICOLON() || 
						buffer->LookAheadTokenType(1) 
							== lexia::TokenType::IDENTIFIER() || 
						buffer->LookAheadTokenType(1)
							== lexia::TokenType::MINUS() || 
						buffer->LookAheadTokenType(1) 
							== lexia::TokenType::LEFT_BRACE() || 
						buffer->LookAheadTokenType(1)
							== lexia::TokenType::IF() || 
						buffer->LookAheadTokenType(1)
							== lexia::TokenType::WHILE() || 
						buffer->LookAheadTokenType(1)
							== lexia::TokenType::RETURN()){
					cons->AddChild(processor("statement"));
				}
				return cons;
			}))
			;
		
		language_parser_.DefineSyntaxRule("expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const LanguageParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				buffer->DebugPrint("##expression");
				auto ass = processor("assign_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::COMMA()){
					return ass;
				}
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(ass);
				while(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::COMMA()){
					Match(buffer, lexia::TokenType::COMMA());
					cons->AddChild(processor("assign_expression"));
				}	
				return cons;
			}))
			;
		
		language_parser_.DefineSyntaxRule("assign_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##assign_expression1");
				auto left_id = Ast::Create(
					Match(buffer, lexia::TokenType::IDENTIFIER()));
				auto eq = Ast::Create(
					Match(buffer, lexia::TokenType::EQUAL()));
				auto right_exp = processor("assign_expression");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(eq);
				cons->AddChild(left_id);
				cons->AddChild(right_exp);
				return cons;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##assign_expression2");
				return processor("logical_or_expression");
			}));

		language_parser_.DefineSyntaxRule("logical_or_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##logical_or_expression");
				auto first_exp = processor("logical_and_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::OR()){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(buffer->LookAheadTokenType(1) == lexia::TokenType::OR()){
					inner_cons->AddChild(Ast::Create(
						Match(buffer, lexia::TokenType::OR())));
					inner_cons->AddChild(before_exp);
					before_exp = processor("logical_and_expression");
					if(buffer->LookAheadTokenType(1) == lexia::TokenType::OR()){
						auto new_inner_cons = CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
				}	
				inner_cons->AddChild(before_exp);
				return outer_cons;
			}));

		language_parser_.DefineSyntaxRule("logical_and_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##logical_and_expression");
				auto first_exp = processor("equality_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::AND()){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(buffer->LookAheadTokenType(1) == lexia::TokenType::AND()){
					inner_cons->AddChild(Ast::Create(
						Match(buffer, lexia::TokenType::AND())));
					inner_cons->AddChild(before_exp);
					before_exp = processor("equality_expression");
					if(buffer->LookAheadTokenType(1) == lexia::TokenType::AND()){
						auto new_inner_cons = CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
				}	
				inner_cons->AddChild(before_exp);
				return outer_cons;
			}));

		language_parser_.DefineSyntaxRule("equality_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##equality_expression");
				auto first_exp = processor("relational_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::EQUALEQUAL() 
						&& buffer->LookAheadTokenType(1) != lexia::TokenType::NOT_EQUAL()){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(Ast::Create(
						Match(buffer, buffer->LookAheadTokenType(1))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("relational_expression");
					if(buffer->LookAheadTokenType(1) == lexia::TokenType::EQUALEQUAL()
							|| buffer->LookAheadTokenType(1) == lexia::TokenType::NOT_EQUAL()){
						auto new_inner_cons = CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
					else {
						inner_cons->AddChild(before_exp);
						break;	
					}
				}	
				return outer_cons;
			}));

		language_parser_.DefineSyntaxRule("relational_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##relational_expression");
				auto first_exp = processor("add_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::LOWER_THAN() 
						&& buffer->LookAheadTokenType(1) 
							!= lexia::TokenType::HIGHER_THAN()
						&& buffer->LookAheadTokenType(1) 
							!= lexia::TokenType::LOWER_EQUAL()
						&& buffer->LookAheadTokenType(1) 
							!= lexia::TokenType::HIGHER_EQUAL()){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(Ast::Create(
						Match(buffer, buffer->LookAheadTokenType(1))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("add_expression");
					if(buffer->LookAheadTokenType(1) == lexia::TokenType::LOWER_THAN() 
							|| buffer->LookAheadTokenType(1) 
								== lexia::TokenType::HIGHER_THAN()
							|| buffer->LookAheadTokenType(1) 
								== lexia::TokenType::LOWER_EQUAL()
							|| buffer->LookAheadTokenType(1) 
								== lexia::TokenType::HIGHER_EQUAL()){
						auto new_inner_cons = CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
					else {
						inner_cons->AddChild(before_exp);
						break;	
					}
				}	
				return outer_cons;
			}));

		language_parser_.DefineSyntaxRule("add_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##add_expression");
				auto first_exp = processor("multiply_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::PLUS() 
						&& buffer->LookAheadTokenType(1) != lexia::TokenType::MINUS()){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(Ast::Create(
						Match(buffer, buffer->LookAheadTokenType(1))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("multiply_expression");
					if(buffer->LookAheadTokenType(1) == lexia::TokenType::PLUS() 
							|| buffer->LookAheadTokenType(1) == lexia::TokenType::MINUS()){
						auto new_inner_cons = CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
					else {
						inner_cons->AddChild(before_exp);
						break;	
					}
				}
				return outer_cons;
			}));
		
		language_parser_.DefineSyntaxRule("multiply_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##multiply_expression");
				auto first_exp = processor("unary_expression");
				if(buffer->LookAheadTokenType(1) != lexia::TokenType::MULTIPLY() 
						&& buffer->LookAheadTokenType(1) != lexia::TokenType::DIVIDE()){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(Ast::Create(
						Match(buffer, buffer->LookAheadTokenType(1))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("unary_expression");
					if(buffer->LookAheadTokenType(1) == lexia::TokenType::MULTIPLY() 
							|| buffer->LookAheadTokenType(1) == lexia::TokenType::DIVIDE()){
						auto new_inner_cons = CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
					else {
						inner_cons->AddChild(before_exp);
						break;	
					}
				}
				return outer_cons;
			}));
		
		language_parser_.DefineSyntaxRule("unary_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##unary_expression");
				if(buffer->LookAheadTokenType(1) == lexia::TokenType::MINUS()){
					auto cons = CreateAst(lexia::Token::CONS_TOKEN());
					cons->AddChild(Ast::Create(
						Match(buffer, lexia::TokenType::MINUS())));
					cons->AddChild(processor("unary_expression"));
					return cons;
				}
				else {
					return processor("postfix_expression");	
				}
			}));

		language_parser_.DefineSyntaxRule("postfix_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##postfix_expression1");
				auto func_call = 
					CreateAst(lexia::Token::FUNCTION_CALL_TOKEN());
				func_call->AddChild(CreateAst(
					Match(buffer, lexia::TokenType::IDENTIFIER())));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				return func_call;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##postfix_expression2");
				auto func_call = CreateAst(lexia::Token::FUNCTION_CALL_TOKEN());
				func_call->AddChild(Ast::Create(
					Match(buffer, lexia::TokenType::IDENTIFIER())));
				Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());
				func_call->AddChild(processor("argument_expression_list"));
				Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());
				return func_call;
			}))
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##postfix_expression3");
				return processor("primary_expression");	
			}));

		language_parser_.DefineSyntaxRule("primary_expression")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##primary_expression");
				if(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::IDENTIFIER()){	
					auto var_ref = 
						CreateAst(lexia::Token::VARIABLE_REFERENCE_TOKEN());
					var_ref->AddChild(CreateAst(
						Match(buffer, lexia::TokenType::IDENTIFIER())));
					return var_ref;
				}
				else if(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::CONSTANT()){	
					return CreateAst(
						Match(buffer, lexia::TokenType::CONSTANT()));	
				}
				else {
					Match(buffer, lexia::TokenType::LEFT_PARENTHESIS());	
					auto exp = processor("expression");
					Match(buffer, lexia::TokenType::RIGHT_PARENTHESIS());	
					return exp;
				}
			}));

		language_parser_.DefineSyntaxRule("argument_expression_list")
			->AddChoice(LanguageParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer, 
					const LanguageParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				buffer->DebugPrint("##argument_expression_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("assign_expression"));
				while(buffer->LookAheadTokenType(1) 
						== lexia::TokenType::COMMA()){ 
					Match(buffer, buffer->LookAheadTokenType(1));
					cons->AddChild(processor("assign_expression"));
				}	
				return cons;
			}));
		
	}

};
}

