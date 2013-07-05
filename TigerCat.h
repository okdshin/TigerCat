#pragma once
//TigerCat:20130624
//#define SEMANTIA_BASIC_TREE_WALKER_DEBUG_ON
#define PARSIA_TOKEN_BUFFER_DEBUG_ON
//#define LEXIA_LEXER_DEBUG_ON
#include <iostream>
#include "Lexia/Lexer.h"
#include "Parsia/BasicParser.h"
#include "Semantia/BasicTree.h"
#include "Semantia/Token.h"
#include "Semantia/SyntaxTreeStream.h"
#include "Symbolia/SymbolTable.h"
#include "Symbolia/BaseScope.h"
#include "Symbolia/GlobalScope.h"
#include "Symbolia/VariableSymbol.h"
#include "Symbolia/FunctionSymbol.h"
#include "Token.h"

namespace tiger_cat
{
using Ast = semantia::BasicTree<Token>;
using LangToken = lexia::Token;
using LangTokenType = lexia::TokenType;
using LangParser = parsia::BasicParser<LangToken, LangTokenType, Ast::Ptr>;
using LangTokenBuffer = LangParser::TokenBuffer;
/*
using AstParser = 
	parsia::BasicParser<semantia::Token, semantia::TokenType, void*>;
auto SymboliaWord(const Word& word) -> const symbolia::Word {
	return symbolia::Word(word.ToString());	
}
*/
auto CreateToken(const lexia::Token& lexia_token) -> const Token {
	return Token(TokenType(lexia_token.GetType().ToString()),
		Word(lexia_token.GetWord().ToString()));
}

auto CreateAst(const lexia::Token& lexia_token) -> const Ast::Ptr {
	return Ast::Create(CreateToken(lexia_token));	
}

auto GetType(const LangToken& token) -> const LangTokenType {
	return token.GetType();
}

auto IsTokenTypeSame(const LangToken& token, const LangTokenType& type) -> const bool {
	return GetType(token) == type;
}

class TigerCat{
public:

    TigerCat(const std::string& code) : 
		lexer_(code),
		lang_parser_(),
		ast_root_()/*,
		ast_stream_(),
		ast_parser_()*/{}
    ~TigerCat(){}

	auto Define() -> void {
		DefineLanguageSyntax();
		//DefineTreePatternForSymbolTable();
	}

	auto InitLangTokenBuffer() -> void {
		lang_parser_.InitTokenBuffer(
			LangTokenBuffer::NextTokenGetter([this]() -> const LangToken {
				return lexer_.GetNextToken();
			}),
			LangTokenBuffer::IsTokenTypeSameDecider([](
					const LangToken& token, const LangTokenType& type) -> const bool {
				return token.GetType() == type;
			}),	
			LangTokenBuffer::TokenOutputter([](
					std::ostream& os, const LangToken& token) -> void {
				os << "[" << token.GetWord().ToString() << "]";
			}),
			LangTokenBuffer::TokenTypeOutputter([](
					std::ostream& os, const LangTokenType& type) -> void {
				os << "\"" << type.ToString() << "\"";
			})
		);
	}

	/*
	auto InitAstTokenBuffer() -> void {
		ast_parser_.InitTokenBuffer(TokenBuffer::NextTokenGetter(
				[this]() -> parsia::Token {
			const auto token = ParsiaToken(ast_stream_->GetNextToken());
			//std::cout << token << std::endl;
			return token;
		}));
	}
	*/
	/*
	auto InitAstStream() -> void {
		ast_stream_ = semantia::SyntaxTreeStream::Create(ast_root_);	
	}
	*/

	auto MakeAbstractSyntaxTree() -> const Ast::Ptr {
		ast_root_ = lang_parser_.ProcessRule("program");
		return ast_root_;
	}

	/*
	auto MakeSymbolTable() -> void {
		const auto global_scope = symbolia::GlobalScope::Create();
		symbol_table_.PushScope(global_scope);
		ast_parser_.ProcessRule("pattern");	
	}
	*/

private:
	lexia::Lexer lexer_;
	LangParser lang_parser_;
	Ast::Ptr ast_root_;
	//semantia::SyntaxTreeStream::Ptr ast_stream_;
	//AstParser ast_parser_;
	//symbolia::SymbolTable symbol_table_;

private:
	/*
	auto DefineTreePatternForSymbolTable() -> void {
		ast_parser_.DefineSyntaxRule("pattern")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
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
				return nullptr;
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
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
				return nullptr;
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
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
				return nullptr;
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
				Match(buffer, semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				Match(buffer, lexia::TokenType::BLOCK());
				Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
				if(!buffer->IsSpeculating()){
					std::cout << "##empty block!!" << std::endl;
				}
				processor("pattern");
				return nullptr;
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
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
				return nullptr;
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
				if(buffer->LookAheadTokenType(1)
						== semantia::TokenType::SEMANTIA_EOF_TOKEN_TYPE()){
					return nullptr;
				}else{
					Match(buffer, lexia::TokenType::BLOCK());
					Match(buffer, semantia::TokenType::STEP_UP_TOKEN_TYPE());
					if(!buffer->IsSpeculating()){
						std::cout << "##exit block!!" << std::endl;
						symbol_table_.PopScope();
						symbolia::SymbolTable::DebugPrint(symbol_table_);
					}
					processor("pattern");
					return nullptr;
				}
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					const parsia::TokenBuffer::Ptr& buffer,
					const AstParser::SyntaxRule::RuleProcessor& processor
					) -> void* {
				if(!buffer->IsSpeculating()){
					//std::cout << "pass token" << std::endl;
				}
				if(buffer->LookAheadTokenType(1)
						== semantia::TokenType::SEMANTIA_EOF_TOKEN_TYPE()){
					return nullptr;
				}else{
					buffer->Match(buffer->LookAheadTokenType(1));
					processor("pattern");
					return nullptr;
				}
			}))
			;
	}
	*/
	auto DefineLanguageSyntax() -> void {
		lang_parser_.DefineSyntaxRule("program")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##program");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("external_declaration"));	
				while(!IsTokenTypeSame(looker(1), 
						lexia::TokenType::LEXIA_EOF_TOKEN_TYPE())){
					cons->AddChild(processor("external_declaration"));	
				}
				return cons;
			}));

		lang_parser_.DefineSyntaxRule("external_declaration")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##external_declaration1");
				return processor("declaration");
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##external_declaration2");
				return processor("function_definition");
			}));

		lang_parser_.DefineSyntaxRule("declaration")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##declaration");
				auto cons = 
					CreateAst(lexia::Token::VARIABLE_DECLARATION_TOKEN());
				cons->AddChild(CreateAst(matcher(lexia::TokenType::INT())));
				cons->AddChild(processor("declarator_list"));
				matcher(lexia::TokenType::SEMICOLON());
				return cons;
			}));

		lang_parser_.DefineSyntaxRule("declarator_list")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##declarator_list");
				auto dec = processor("declarator");
				if(!IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){
					return dec;
				}
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(dec);
				while(IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){
					matcher(lexia::TokenType::COMMA());
					cons->AddChild(processor("declarator"));
				}
				return cons;
			}));	

		lang_parser_.DefineSyntaxRule("declarator")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##declarator");
				return CreateAst(matcher(lexia::TokenType::IDENTIFIER()));	
			}));

		lang_parser_.DefineSyntaxRule("function_definition")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##function_definition1");
				auto cons1 = 
					CreateAst(lexia::Token::FUNCTION_DECLARATION_TOKEN());
				auto cons2 = CreateAst(lexia::Token::CONS_TOKEN());
				cons1->AddChild(cons2);
				cons2->AddChild(CreateAst(matcher(lexia::TokenType::INT())));
				cons2->AddChild(processor("declarator"));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons1->AddChild(processor("compound_statement"));
				return cons1;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##function_definition2");
				auto cons1 = 
					CreateAst(lexia::Token::FUNCTION_DECLARATION_TOKEN());
				auto cons2 = CreateAst(lexia::Token::CONS_TOKEN());
				cons1->AddChild(cons2);
				cons2->AddChild(CreateAst(matcher(lexia::TokenType::INT())));
				cons2->AddChild(processor("declarator"));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				cons1->AddChild(processor("parameter_type_list"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons1->AddChild(processor("compound_statement"));
				return cons1;
			}))
			;	

		lang_parser_.DefineSyntaxRule("parameter_type_list")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##parameter_type_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("parameter_declaration"));
				while(IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){
					matcher(lexia::TokenType::COMMA());
					cons->AddChild(processor("parameter_declaration"));
				}
				return cons;
			}));

		lang_parser_.DefineSyntaxRule("parameter_declaration")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##parameter_declaration");
				auto cons = CreateAst(lexia::Token::PARAMETER_DECLARATION_TOKEN());
				cons->AddChild(CreateAst(matcher(lexia::TokenType::INT())));
				cons->AddChild(processor("declarator"));
				return cons;
			}));
		
		lang_parser_.DefineSyntaxRule("statement")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement1");
				matcher(lexia::TokenType::SEMICOLON());
				return CreateAst(lexia::Token::CONS_TOKEN());
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement2");
				auto exp = processor("expression");
				matcher(lexia::TokenType::SEMICOLON());
				return exp;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement3");
				return processor("compound_statement");
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement4");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(CreateAst(matcher(lexia::TokenType::IF())));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				matcher(lexia::TokenType::ELSE());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement5");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(CreateAst(matcher(lexia::TokenType::IF())));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement6");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(CreateAst(matcher(lexia::TokenType::WHILE())));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement7");
				auto ret = CreateAst(matcher(lexia::TokenType::RETURN()));
				matcher(lexia::TokenType::SEMICOLON());
				return ret;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement8");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(CreateAst(matcher(lexia::TokenType::RETURN())));
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::SEMICOLON());
				return cons;
			}));
		
		lang_parser_.DefineSyntaxRule("compound_statement")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##compound_statement1");
				matcher(lexia::TokenType::LEFT_BRACE());
				matcher(lexia::TokenType::RIGHT_BRACE());
				return CreateAst(lexia::Token::BLOCK_TOKEN());
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##compound_statement2");
				auto block = CreateAst(lexia::Token::BLOCK_TOKEN());
				matcher(lexia::TokenType::LEFT_BRACE());
				block->AddChild(processor("declaration_list"));
				matcher(lexia::TokenType::RIGHT_BRACE());
				return block;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##compound_statement3");
				auto block = CreateAst(lexia::Token::BLOCK_TOKEN());
				matcher(lexia::TokenType::LEFT_BRACE());
				block->AddChild(processor("statement_list"));
				matcher(lexia::TokenType::RIGHT_BRACE());
				return block;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##compound_statement4");
				auto block = CreateAst(lexia::Token::BLOCK_TOKEN());
				matcher(lexia::TokenType::LEFT_BRACE());
				block->AddChild(processor("declaration_list"));
				block->AddChild(processor("statement_list"));
				matcher(lexia::TokenType::RIGHT_BRACE());
				return block;
			}));

		lang_parser_.DefineSyntaxRule("declaration_list")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##declaration_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("declaration"));
				while(IsTokenTypeSame(looker(1), lexia::TokenType::INT())){
					cons->AddChild(processor("declaration"));
				}
				return cons;
			}));

		lang_parser_.DefineSyntaxRule("statement_list")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("statement"));
				while(IsTokenTypeSame(looker(1), 
							lexia::TokenType::SEMICOLON()) || 
						IsTokenTypeSame(looker(1), 
							lexia::TokenType::IDENTIFIER()) || 
						IsTokenTypeSame(looker(1), 
							lexia::TokenType::MINUS()) || 
						IsTokenTypeSame(looker(1), 
							lexia::TokenType::LEFT_BRACE()) || 
						IsTokenTypeSame(looker(1), 
							lexia::TokenType::IF()) || 
						IsTokenTypeSame(looker(1), 
							lexia::TokenType::WHILE()) || 
						IsTokenTypeSame(looker(1), 
							lexia::TokenType::RETURN())){
					cons->AddChild(processor("statement"));
				}
				return cons;
			}))
			;
	
		lang_parser_.DefineSyntaxRule("expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##expression");
				auto ass = processor("assign_expression");
				if(!IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){
					return ass;
				}
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(ass);
				while(IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){
					matcher(lexia::TokenType::COMMA());
					cons->AddChild(processor("assign_expression"));
				}	
				return cons;
			}))
			;
		
		lang_parser_.DefineSyntaxRule("assign_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##assign_expression1");
				auto left_id = CreateAst(matcher(lexia::TokenType::IDENTIFIER()));
				auto eq = CreateAst(matcher(lexia::TokenType::EQUAL()));
				auto right_exp = processor("assign_expression");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(eq);
				cons->AddChild(left_id);
				cons->AddChild(right_exp);
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##assign_expression2");
				return processor("logical_or_expression");
			}));

		lang_parser_.DefineSyntaxRule("logical_or_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##logical_or_expression");
				auto first_exp = processor("logical_and_expression");
				if(!IsTokenTypeSame(looker(1), lexia::TokenType::OR())){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(IsTokenTypeSame(looker(1), lexia::TokenType::OR())){
					inner_cons->AddChild(
						CreateAst(matcher(lexia::TokenType::OR())));
					inner_cons->AddChild(before_exp);
					before_exp = processor("logical_and_expression");
					if(IsTokenTypeSame(looker(1), lexia::TokenType::OR())){
						auto new_inner_cons = 
							CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
				}	
				inner_cons->AddChild(before_exp);
				return outer_cons;
			}));

		lang_parser_.DefineSyntaxRule("logical_and_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##logical_and_expression");
				auto first_exp = processor("equality_expression");
				if(!IsTokenTypeSame(looker(1), lexia::TokenType::AND())){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(IsTokenTypeSame(looker(1), lexia::TokenType::AND())){
					inner_cons->AddChild(
						CreateAst(matcher(lexia::TokenType::AND())));
					inner_cons->AddChild(before_exp);
					before_exp = processor("equality_expression");
					if(IsTokenTypeSame(looker(1), lexia::TokenType::AND())){
						auto new_inner_cons = 
							CreateAst(lexia::Token::CONS_TOKEN());
						inner_cons->AddChild(new_inner_cons);
						inner_cons = new_inner_cons;
					}
				}	
				inner_cons->AddChild(before_exp);
				return outer_cons;
			}));

		lang_parser_.DefineSyntaxRule("equality_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##equality_expression");
				auto first_exp = processor("relational_expression");
				const auto next_token = looker(1);
				if(!IsTokenTypeSame(next_token, 
							lexia::TokenType::EQUALEQUAL()) && 
						!IsTokenTypeSame(next_token, 
							lexia::TokenType::NOT_EQUAL())){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(CreateAst(matcher(GetType(looker(1)))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("relational_expression");
					const auto next_token = looker(1);
					if(IsTokenTypeSame(next_token, 
							lexia::TokenType::EQUALEQUAL()) || 
						IsTokenTypeSame(next_token, 
							lexia::TokenType::NOT_EQUAL())){
						auto new_inner_cons = 
							CreateAst(lexia::Token::CONS_TOKEN());
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

		lang_parser_.DefineSyntaxRule("relational_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##relational_expression");
				auto first_exp = processor("add_expression");
				const auto next_token = looker(1);
				if(!IsTokenTypeSame(next_token, 
							lexia::TokenType::LOWER_THAN()) && 
						!IsTokenTypeSame(next_token, 
							lexia::TokenType::HIGHER_THAN()) && 
						!IsTokenTypeSame(next_token, 
							lexia::TokenType::LOWER_EQUAL()) && 
						!IsTokenTypeSame(next_token, 
							lexia::TokenType::HIGHER_EQUAL())){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(CreateAst(matcher(GetType(looker(1)))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("add_expression");
					const auto next_token = looker(1);
					if(IsTokenTypeSame(next_token, 
								lexia::TokenType::LOWER_THAN()) ||
							IsTokenTypeSame(next_token, 
								lexia::TokenType::HIGHER_THAN()) || 
							IsTokenTypeSame(next_token, 
								lexia::TokenType::LOWER_EQUAL()) ||
							IsTokenTypeSame(next_token, 
								lexia::TokenType::HIGHER_EQUAL())){
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

		lang_parser_.DefineSyntaxRule("add_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##add_expression");
				auto first_exp = processor("multiply_expression");
				auto is_add = [](const LangToken& token) -> const bool {
					return IsTokenTypeSame(token, 
							lexia::TokenType::PLUS()) ||
						IsTokenTypeSame(token, 
							lexia::TokenType::MINUS());
				};
				if(!is_add(looker(1))){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(CreateAst(matcher(GetType(looker(1)))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("multiply_expression");
					if(is_add(looker(1))){
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
		
		lang_parser_.DefineSyntaxRule("multiply_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##multiply_expression");
				auto first_exp = processor("unary_expression");
				auto is_multiply = [](const LangToken& token) -> const bool {
					return IsTokenTypeSame(token, 
							lexia::TokenType::MULTIPLY()) ||
						IsTokenTypeSame(token, 
							lexia::TokenType::DIVIDE());
				};
				if(!is_multiply(looker(1))){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::CONS_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(true){
					inner_cons->AddChild(CreateAst(matcher(GetType(looker(1)))));
					inner_cons->AddChild(before_exp);
					before_exp = processor("unary_expression");
					if(is_multiply(looker(1))){
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
		
		lang_parser_.DefineSyntaxRule("unary_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##unary_expression");
				if(IsTokenTypeSame(looker(1), lexia::TokenType::MINUS())){
					auto cons = CreateAst(lexia::Token::CONS_TOKEN());
					cons->AddChild(
						CreateAst(matcher(lexia::TokenType::MINUS())));
					cons->AddChild(processor("unary_expression"));
					return cons;
				}
				else {
					return processor("postfix_expression");	
				}
			}));

		lang_parser_.DefineSyntaxRule("postfix_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##postfix_expression1");
				auto func_call = 
					CreateAst(lexia::Token::FUNCTION_CALL_TOKEN());
				func_call->AddChild(CreateAst(
					matcher(lexia::TokenType::IDENTIFIER())));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				return func_call;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##postfix_expression2");
				auto func_call = CreateAst(lexia::Token::FUNCTION_CALL_TOKEN());
				func_call->AddChild(CreateAst(
					matcher(lexia::TokenType::IDENTIFIER())));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				func_call->AddChild(processor("argument_expression_list"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				return func_call;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##postfix_expression3");
				return processor("primary_expression");	
			}));

		lang_parser_.DefineSyntaxRule("primary_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##primary_expression");
				const auto next_token = looker(1);
				if(IsTokenTypeSame(next_token, 
						lexia::TokenType::IDENTIFIER())){	
					auto var_ref = 
						CreateAst(lexia::Token::VARIABLE_REFERENCE_TOKEN());
					var_ref->AddChild(
						CreateAst(matcher(lexia::TokenType::IDENTIFIER())));
					return var_ref;
				}
				else if(IsTokenTypeSame(next_token, 
						lexia::TokenType::CONSTANT())){	
					return CreateAst(
						matcher(lexia::TokenType::CONSTANT()));	
				}
				else {
					matcher(lexia::TokenType::LEFT_PARENTHESIS());	
					auto exp = processor("expression");
					matcher(lexia::TokenType::RIGHT_PARENTHESIS());	
					return exp;
				}
			}));

		lang_parser_.DefineSyntaxRule("argument_expression_list")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor
					) -> const Ast::Ptr {
				lang_parser_.DebugPrint("##argument_expression_list");
				auto cons = CreateAst(lexia::Token::CONS_TOKEN());
				cons->AddChild(processor("assign_expression"));
				while(IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){ 
					matcher(lexia::TokenType::COMMA());
					cons->AddChild(processor("assign_expression"));
				}	
				return cons;
			}));
		
	}

};
}

