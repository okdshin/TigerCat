#pragma once
//TigerCat:20130624
//#define SEMANTIA_BASIC_TREE_WALKER_DEBUG_ON
//#define PARSIA_TOKEN_BUFFER_DEBUG_ON
//#define LEXIA_LEXER_DEBUG_ON
#define PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS \
const AstParser::SyntaxRule::TokenMatcher& matcher,\
const AstParser::SyntaxRule::AheadTokenLooker& looker,\
const AstParser::SyntaxRule::RuleProcessor& processor,\
const AstParser::SyntaxRule::IsSpeculatingDecider& decider
#include <iostream>
#include "Lexia/Lexer.h"
#include "Parsia/BasicParser.h"
#include "Semantia/BasicTree.h"
#include "Semantia/BasicTreeStream.h"
#include "Semantia/BasicToken.h"
#include "Symbolia/SymbolTable.h"
#include "Symbolia/BaseScope.h"
#include "Symbolia/GlobalScope.h"
#include "Symbolia/VariableSymbol.h"
#include "Symbolia/FunctionSymbol.h"
#include "Token.h"
#include "Codia/Code.h"
#include "Codia/CodeList.h"
#include "Codia/LabelStack.h"
#include "Codia/BasicStack.h"

namespace tiger_cat
{
using LangToken = lexia::Token;
using LangTokenType = lexia::TokenType;
using Ast = semantia::BasicTree<Token::Ptr>;
using LangParser = parsia::BasicParser<LangToken, LangTokenType, Ast::Ptr>;

using AstToken = semantia::BasicToken<Token::Ptr>;
using AstTokenType = semantia::TokenType;
using NoneReturnType = void*;
using AstParser = parsia::BasicParser<AstToken, AstTokenType, NoneReturnType>;

using AstStream = semantia::BasicTreeStream<Token::Ptr>;

using AsmCode = codia::Code; 
using AsmCodeList = codia::CodeList; 

auto SymboliaWord(const Word& word) -> const symbolia::Word {
	return symbolia::Word(word.ToString());	
}
auto CreateToken(const lexia::Token& lexia_token) -> const Token::Ptr {
	return Token::Create(TokenType(lexia_token.GetType().ToString()),
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

auto LookNodeType(
		const AstParser::SyntaxRule::AheadTokenLooker& looker, int i) -> TokenType {
	if(looker(i).GetType() == AstTokenType::NODE_TOKEN_TYPE()){
		return looker(i).GetValue()->GetType();
	}
	throw parsia::SyntaxError("SyntaxError");
}

auto MatchNode(const AstParser::SyntaxRule::AheadTokenLooker& looker, 
		const AstParser::SyntaxRule::TokenMatcher& matcher, 
		const lexia::TokenType& type) -> Token::Ptr {
	if(looker(1).GetType() == AstTokenType::NODE_TOKEN_TYPE()){
		if(looker(1).GetValue()->GetType().ToString() == type.ToString()){
			return matcher(looker(1).GetType()).GetValue();	
		}
	}
	throw parsia::SyntaxError("SyntaxError");
}

auto MatchTree(const AstParser::SyntaxRule::AheadTokenLooker& looker, 
		const AstParser::SyntaxRule::TokenMatcher& matcher, 
		const lexia::TokenType& type) -> Ast::Ptr {
	if(looker(1).GetType() == AstTokenType::NODE_TOKEN_TYPE()){
		if(looker(1).GetValue()->GetType().ToString() == type.ToString()){
			return matcher(looker(1).GetType()).GetTree();	
		}
	}
	throw parsia::SyntaxError("SyntaxError");
}

auto Expect(const AstParser::SyntaxRule::AheadTokenLooker& looker,
		const AstTokenType& type) -> void {
	if(looker(1).GetType() != type){
		throw parsia::SyntaxError("ChoiceError");
	}	
}

auto CommentAboutTemp(const codia::CodeList::Ptr& code_list, 
		const symbolia::SymbolTable& table, 
		const std::string& message) -> void {
	code_list->Pushback(AsmCode(";"+message+" temp [ebp"
		+symbolia::OffsetToString(
			symbolia::SymbolTable::TopTempOffset(table))+
		"]"));			
}

class TigerCat{
public:

    TigerCat(const std::string& code) : 
		lexer_(code),
		lang_parser_(),
		ast_root_(),
		ast_stream_(),
		ast_parser_(),
		asm_code_list_(AsmCodeList::Create()),
		while_label_stack_(codia::Label("Lwhile")),
		if_label_stack_(codia::Label("Lif")),
		else_label_stack_(codia::Label("Lelse")),
		logical_and_label_stack_(codia::Label("Land")),
		logical_or_label_stack_(codia::Label("Lor")){}

	auto Define() -> void {
		DefineLanguageSyntax();
		DefineTreePatternForSymbolTable();
	}

	auto InitLangTokenBuffer() -> void {
		lang_parser_.InitTokenBuffer(
			LangParser::TokenBuffer::NextTokenGetter([this]() -> const LangToken {
				return lexer_.GetNextToken();
			}),
			LangParser::TokenBuffer::IsTokenTypeSameDecider([](
					const LangToken& token, const LangTokenType& type) -> const bool {
				return token.GetType() == type;
			}),	
			LangParser::TokenBuffer::TokenOutputter([](
					std::ostream& os, const LangToken& token) -> void {
				os << "[" << token.GetWord().ToString() << "]";
			}),
			LangParser::TokenBuffer::TokenTypeOutputter([](
					std::ostream& os, const LangTokenType& type) -> void {
				os << "\"" << type.ToString() << "\"";
			})
		);
	}

	auto MakeAbstractSyntaxTree() -> const Ast::Ptr {
		ast_root_ = lang_parser_.ProcessRule("program");
		return ast_root_;
	}

	auto InitAstStream() -> void {
		ast_stream_ = AstStream::Create(ast_root_);	
	}

	auto InitAstTokenBuffer() -> void {
		ast_parser_.InitTokenBuffer(
			AstParser::TokenBuffer::NextTokenGetter([this]() -> AstToken {
				return ast_stream_->GetNextToken();
			}),
			AstParser::TokenBuffer::IsTokenTypeSameDecider([](
					const AstToken& token, 
					const AstTokenType& type) -> const bool {
				return token.GetType() == type;
			}),	
			AstParser::TokenBuffer::TokenOutputter([](
					std::ostream& os, const AstToken& token) -> void {
				os << token;
				if(token.GetValue()){
					os << " " << *token.GetValue();
				}
			}),
			AstParser::TokenBuffer::TokenTypeOutputter([](
					std::ostream& os, const AstTokenType& type) -> void {
				os << "\"" << type << "\"";
			})
		);
	}

	auto Compile() -> void {
		const auto global_scope = symbolia::GlobalScope::Create();
		symbol_table_.PushScope(global_scope);
		ast_parser_.ProcessRule("pattern");	
	}

	auto RemoveUselessJumpCommand() -> void {
		asm_code_list_->RemoveUselessJumpCommand();
	}

	auto OutputResult(std::ostream& os)const -> void {
		asm_code_list_->Output(os);
	}

private:
	lexia::Lexer lexer_;
	LangParser lang_parser_;
	Ast::Ptr ast_root_;

	AstStream::Ptr ast_stream_;
	AstParser ast_parser_;

	symbolia::SymbolTable symbol_table_;

	AsmCodeList::Ptr asm_code_list_;

	codia::BasicStack<symbolia::SymbolWithDepth> 
		current_assigned_symbol_stack_;
	codia::BasicStack<symbolia::Symbol::Ptr> current_called_function_symbol_stack_;
	
	symbolia::Symbol::Ptr current_decralated_function_symbol_;

	codia::LabelStack while_label_stack_;
	codia::LabelStack if_label_stack_;
	codia::LabelStack else_label_stack_;
	codia::LabelStack logical_and_label_stack_;
	codia::LabelStack logical_or_label_stack_;

public:
	auto DefineTreePatternForSymbolTable() -> void {
		ast_parser_.DefineSyntaxRule("variable_declaration")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "##variable declaration!!" << std::endl;
				}
				MatchNode(looker, matcher, 
					lexia::TokenType::VARIABLE_DECLARATION());
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::INT());
				matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, 
					lexia::TokenType::VARIABLE_DECLARATION());
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				std::vector<Token::Ptr> var_token_list;
				assert(looker(1).GetType() == semantia::TokenType::NODE_TOKEN_TYPE());
				if(looker(1).GetValue()->GetType().ToString() 
						== lexia::TokenType::CONS().ToString()){
					MatchNode(looker, matcher, lexia::TokenType::CONS());
					while(looker(1).GetType()
							== semantia::TokenType::STEP_DOWN_TOKEN_TYPE()){
						matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
						auto var_token = MatchNode(looker, matcher, 
							lexia::TokenType::IDENTIFIER());
						var_token_list.push_back(var_token);	
						matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
						MatchNode(looker, matcher, lexia::TokenType::CONS());
					}
				}
				else{
					auto var_token = 
						MatchNode(looker, matcher, lexia::TokenType::IDENTIFIER());
					var_token_list.push_back(var_token);
				}
				if(!decider()){
					for(const auto& var_token : var_token_list){
						var_token->SetKind(Kind::VARIABLE_DECLARATION());
						var_token->SetDepth(
							symbol_table_.GetCurrentScope()->GetDepth());
						asm_code_list_->Pushback(
							AsmCode(";variable_declaration"));
						try{
							if(var_token->GetDepth() == 0){
								asm_code_list_->Pushback(AsmCode("\tcommon\t"+
									var_token->GetWord().ToString()+"\t4"));	
								auto offset = symbolia::Offset(
									symbolia::SymbolTable::GetCurrentOffsetSum(
										symbol_table_));
								auto variable_symbol = 
									symbolia::VariableSymbol::Create(
										SymboliaWord(var_token->GetWord()), 
										offset);
								symbolia::SymbolTable::Define(symbol_table_,
									variable_symbol,
									symbolia::VariableSize(0));	
								var_token->SetOffset(offset);
							}
							else {
								auto offset = symbolia::Offset(
									symbolia::SymbolTable::GetCurrentOffsetSum(
										symbol_table_)-4);
								auto variable_symbol = 
									symbolia::VariableSymbol::Create(
										SymboliaWord(var_token->GetWord()), 
										offset);
								symbolia::SymbolTable::Define(symbol_table_,
									variable_symbol,
									symbolia::VariableSize(4));	
								var_token->SetOffset(offset);
							}
						}
						catch(const symbolia::DuplicateDeclarationError& e){
							std::cout << "\033[1;31m" 
								<< e.what() << "\033[0;39m" << std::endl;
						}
					}
				}
				processor("pattern");
				return nullptr;
			}));

		ast_parser_.DefineSyntaxRule("function_declaration")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << ">>>>function declaration!!" << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::FUNCTION_DECLARATION());
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::CONS());
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::INT());
				matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::CONS());
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				const auto func_token = 
					MatchNode(looker, matcher, lexia::TokenType::IDENTIFIER());
				symbolia::FunctionSymbol::Ptr function_symbol;
				if(!decider()){
					func_token->SetKind(Kind::FUNCTION_DECLARATION());
					func_token->SetDepth(
						symbol_table_.GetCurrentScope()->GetDepth());
					func_token->SetOffset(symbolia::Offset(0));
					function_symbol = symbolia::FunctionSymbol::Create(
						SymboliaWord(func_token->GetWord()),
						symbol_table_.GetCurrentScope());
					current_decralated_function_symbol_ = function_symbol;
					symbolia::SymbolTable::Define(symbol_table_, 
						function_symbol, symbolia::VariableSize(0));
					symbol_table_.PushScope(function_symbol);
					symbolia::SymbolTable::DebugPrint(symbol_table_);
					asm_code_list_->Pushback(
						AsmCode(";function_declaration"));
					asm_code_list_->Pushback(
						AsmCode("\tGLOBAL "+func_token->GetWord().ToString()));
					asm_code_list_->Pushback(
						AsmCode(func_token->GetWord().ToString()+":\tpush\tebp"));
					asm_code_list_->Pushback(
						AsmCode("\tmov\tebp, esp"));
					asm_code_list_->Pushback(
						AsmCode("\tsub\tesp, Nlocal"));
					asm_code_list_->PushCurrentCode();
				}
				matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::CONS());
				matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, 
					lexia::TokenType::FUNCTION_DECLARATION());
				int param_count = 0;
				if(looker(2).GetValue()->GetType().ToString() 
						== lexia::TokenType::CONS().ToString()){
					matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
					MatchNode(looker, matcher, lexia::TokenType::CONS());
					while(looker(1).GetType() != 
							semantia::TokenType::STEP_UP_TOKEN_TYPE()){
						if(!decider()){
							std::cout << "##parameter declaration!!" << std::endl;
						}
						matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
						MatchNode(looker, matcher, 
							lexia::TokenType::PARAMETER_DECLARATION());
						matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
						MatchNode(looker, matcher, lexia::TokenType::INT());
						matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
						MatchNode(looker, matcher, 
							lexia::TokenType::PARAMETER_DECLARATION());
						matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
						auto param_token = 
							MatchNode(looker, matcher, 
								lexia::TokenType::IDENTIFIER());
						if(!decider()){
							++param_count;
							const auto offset = symbolia::Offset(4+4*param_count);
							const auto param_symbol = 
								symbolia::VariableSymbol::Create(
									SymboliaWord(param_token->GetWord()), 
									offset);
							symbolia::SymbolTable::Define(symbol_table_, 
								param_symbol, symbolia::VariableSize(0));
							param_token->SetKind(Kind::VARIABLE_DECLARATION());
							param_token->SetDepth(
								symbol_table_.GetCurrentScope()->GetDepth());
							param_token->SetOffset(offset);
						}
						matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
						MatchNode(looker, matcher, 
							lexia::TokenType::PARAMETER_DECLARATION());
						matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
						MatchNode(looker, matcher, lexia::TokenType::CONS());
					}
				}
				if(!decider()){
					function_symbol->SetNumOfParameters(
						symbolia::NumOfParameters(param_count));
				}
				processor("pattern");
				return nullptr;
			}));
		
		ast_parser_.DefineSyntaxRule("exit_function_declaration")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "<<<<<<<<exit func_dec: " << std::endl;
				}
				MatchNode(looker, matcher, 
					lexia::TokenType::FUNCTION_DECLARATION());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					const auto min_offset_sum = 
						symbolia::SymbolTable::GetMinOffsetSum(symbol_table_);
					asm_code_list_->ChangeAndPopTopCode(AsmCode("\tsub\tesp, "+
						boost::lexical_cast<std::string>(-min_offset_sum)));
					asm_code_list_->Pushback(AsmCode(
						";exit_func_declaration:Nlocal="+
						boost::lexical_cast<std::string>(-min_offset_sum)));
					symbol_table_.PopScope();
					const std::string return_label(
						current_decralated_function_symbol_->GetWord().ToString()
						+"_ret");
					asm_code_list_->Pushback(
						AsmCode(return_label+":\tmov\tesp, ebp"));
					asm_code_list_->Pushback(AsmCode("\tpop\tebp"));
					asm_code_list_->Pushback(AsmCode("\tret"));
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_function_call")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "##enter function call!!" << std::endl;
				}
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::FUNCTION_CALL());
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				const auto fcall_token = 
					MatchNode(looker, matcher, lexia::TokenType::IDENTIFIER());
				matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
				//MatchNode(looker, matcher, lexia::TokenType::FUNCTION_CALL());
				int param_count = 0;
				if(looker(2).GetType() == 
						semantia::TokenType::STEP_DOWN_TOKEN_TYPE()){
					MatchNode(looker, matcher, lexia::TokenType::FUNCTION_CALL());
					matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
					auto param_list_tree = MatchTree(looker, matcher, 
						lexia::TokenType::PARAMETER_LIST());
					param_count = param_list_tree->GetNumOfChildren();
				}
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(
						";enter_function_call\n;resolve func"));
					try{
						const auto dec = symbolia::SymbolTable::Resolve(
							symbol_table_, SymboliaWord(fcall_token->GetWord()));
						std::cout << dec << std::endl;
						if(dec.GetSymbol()->GetNumOfParameters().ToInt() 
								!= param_count){
							std::cout << boost::format(
								"\033[1;31mNumOfParametersError: " 
								"function \"%1%\" needs just %2% parameters, " 
								"but you provided %3% parameters.\033[0;39m") 
								% dec.GetSymbol()->GetWord().ToString()
								% dec.GetSymbol()->GetNumOfParameters().ToInt()
								% param_count
							<<std::endl;
							//assert(!"ERROR:num of parameters error");
						}
						fcall_token->SetKind(Kind::FUNCTION_CALL());
						fcall_token->SetDepth(dec.GetDepth());
						//fcall_token->SetOffset(dec.GetSymbol()->GetOffset());
						current_called_function_symbol_stack_.Push(dec.GetSymbol());
					}
					catch(const symbolia::NoDeclarationError& e){
						asm_code_list_->Pushback(AsmCode(";not found"));
						asm_code_list_->Pushback(AsmCode("\textern\t"+
							fcall_token->GetWord().ToString()));
						auto func_sym = 
							symbolia::FunctionSymbol::Create(symbolia::Word(
								fcall_token->GetWord().ToString()));
						func_sym->SetNumOfParameters(
							symbolia::NumOfParameters(param_count));
						current_called_function_symbol_stack_.Push(func_sym);
					}
				}
				processor("pattern");
				return nullptr;
			}));
		
		ast_parser_.DefineSyntaxRule("exit_parameter")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "##exit parameter!!" << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::PARAMETER());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_parameter"));
					asm_code_list_->Pushback(AsmCode("\tpush\teax"));
				}
				processor("pattern");
				return nullptr;
			}));

		ast_parser_.DefineSyntaxRule("exit_function_call")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "##exit function call!!" << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::FUNCTION_CALL());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_function_call"));
					asm_code_list_->Pushback(AsmCode("\tcall\t"+
						current_called_function_symbol_stack_.Top()
							->GetWord().ToString()
					));
					asm_code_list_->Pushback(AsmCode("\tadd\tesp, "+
						boost::lexical_cast<std::string>(
							current_called_function_symbol_stack_.Top()
								->GetNumOfParameters().ToInt()*4
					)));

					current_called_function_symbol_stack_.Pop();
				}
				processor("pattern");
				return nullptr;
			}));

		ast_parser_.DefineSyntaxRule("constant")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "constant" << std::endl; 
				}
				auto constant = MatchNode(looker, matcher, lexia::TokenType::CONSTANT());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";constant"));
					asm_code_list_->Pushback(AsmCode(
						"\tmov\teax, "+constant->GetWord().ToString()));
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("variable_reference")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##variable reference!!" << std::endl; }
				//MatchNode(looker, matcher, lexia::TokenType::VARIABLE_REFERENCE());
				//matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				const auto var_token = 
					MatchNode(looker, matcher, lexia::TokenType::IDENTIFIER());
				if(!decider()){
					try{
						const auto dec = symbolia::SymbolTable::Resolve(
							symbol_table_, SymboliaWord(var_token->GetWord()));
						std::cout << dec << std::endl;
						var_token->SetKind(Kind::VARIABLE_REFERENCE());
						var_token->SetDepth(dec.GetDepth());
						var_token->SetOffset(symbolia::Offset(4));
						asm_code_list_->Pushback(
							AsmCode(";variable_reference"));
						if(var_token->GetDepth() == 0){
							asm_code_list_->Pushback(AsmCode(
								"\tmov\teax, ["+
								dec.GetSymbol()->GetWord().ToString()+"]"));
						}
						else{
							asm_code_list_->Pushback(AsmCode(
								"\tmov\teax, [ebp"
								+symbolia::OffsetToString(dec.GetSymbol()->
									GetOffset())+"]"));
						}
					}
					catch(const symbolia::NoDeclarationError& e){
						std::cout << "\033[1;31m" 
							<< e.what() << "\033[0;39m" << std::endl;
					}
				}
				matcher(semantia::TokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::VARIABLE_REFERENCE());
				processor("pattern");
				return nullptr;
			}));

		ast_parser_.DefineSyntaxRule("exit_return")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "exit return" << std::endl; 
				}
				MatchNode(looker, matcher, lexia::TokenType::RETURN());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_return"));
					const std::string ret_label(
						current_decralated_function_symbol_->GetWord().ToString()+"_ret");
					asm_code_list_->Pushback(AsmCode("\tjmp\t"+ret_label));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_while")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "enter while" << std::endl; 
				}
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::WHILE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter_while"));
					while_label_stack_.Push();
					asm_code_list_->Pushback(
						AsmCode(while_label_stack_.Top().ToString()+"_start:"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_while")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "pass while" << std::endl; 
				}
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::WHILE());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_while"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 0"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						while_label_stack_.Top().ToString()+"_end"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_while")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "exit while" << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::WHILE());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_while"));
					asm_code_list_->Pushback(AsmCode("\tjmp\t"+
						while_label_stack_.Top().ToString()+"_start"));
					asm_code_list_->Pushback(AsmCode(
						while_label_stack_.Top().ToString()+"_end:"));
					while_label_stack_.Pop();
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_if")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "enter if" << std::endl; 
				}
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::IF());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter_if"));
					if_label_stack_.Push();
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_if")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "pass if" << std::endl; 
				}
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::IF());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_if"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 0"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						if_label_stack_.Top().ToString()+"_end"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_if")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "exit if" << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::IF());
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_if"));
					asm_code_list_->Pushback(AsmCode(
						if_label_stack_.Top().ToString()+"_end:"));
					if_label_stack_.Pop();
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_if_with_else")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "enter if with else" << std::endl; 
				}
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::IF_WITH_ELSE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter_if_with_else"));
					if_label_stack_.Push();
					else_label_stack_.Push();
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_if_of_if_with_else")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "pass if of if with else" << std::endl; 
				}
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::IF_OF_IF_WITH_ELSE());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_if_of_if_with_else"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 0"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						else_label_stack_.Top().ToString()+"_start"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_else")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "enter else" << std::endl; 
				}
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::ELSE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter_else"));
					asm_code_list_->Pushback(AsmCode("\tjmp\t"+
						if_label_stack_.Top().ToString()+"_end"));
					asm_code_list_->Pushback(AsmCode(
						else_label_stack_.Top().ToString()+"_start:"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_if_with_else")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "exit if with else" << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::IF_WITH_ELSE());
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_if_with_else"));
					asm_code_list_->Pushback(AsmCode(
						if_label_stack_.Top().ToString()+"_end:"));
					if_label_stack_.Pop();
					else_label_stack_.Pop();;
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_assign")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter assign" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::ASSIGN());
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::VARIABLE_REFERENCE());
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				const auto var_token = 
					MatchNode(looker, matcher, lexia::TokenType::IDENTIFIER());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter assign\n;resolve var"));
					current_assigned_symbol_stack_.Push(symbolia::SymbolWithDepth(
						symbolia::SymbolTable::Resolve(symbol_table_, 
							SymboliaWord(var_token->GetWord())).GetSymbol(),
							var_token->GetDepth()));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_assign")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit assign" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::ASSIGN());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit assign:"));
					const auto dec = symbolia::SymbolTable::Resolve(
						symbol_table_, 
						current_assigned_symbol_stack_.Top().GetSymbol()->
							GetWord());
					if(dec.GetDepth() == 0){
						asm_code_list_->Pushback(AsmCode("\tmov\t["
							+current_assigned_symbol_stack_.Top().GetSymbol()->
								GetWord().ToString()
							+"], eax"));
					}
					else{
						asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
							+symbolia::OffsetToString(
								current_assigned_symbol_stack_.Top().GetSymbol()->
									GetOffset())
							+"], eax"));
					}
					current_assigned_symbol_stack_.Pop();
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_add")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter add" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::ADD());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_add:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_add")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass add" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::ADD());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_add:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_add")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit add" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::ADD());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_add:"));
					asm_code_list_->Pushback(AsmCode("\tadd\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_sub")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter sub" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::SUB());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter_sub:"));
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_sub")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass sub" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::SUB());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_sub:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_sub")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit sub" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::SUB());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_sub:"));
					asm_code_list_->Pushback(AsmCode("\tsub\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"], eax"));
					asm_code_list_->Pushback(AsmCode("\tmov\teax, [ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"]"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_unary_minus")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit unary minus" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::UNARY_MINUS());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_unary_minus:"));
					asm_code_list_->Pushback(AsmCode("\tneg\teax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_mul")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter mul" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::MUL());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_mul:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_mul")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass mul" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::MUL());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_mul:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_mul")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit mul" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::MUL());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_mul:"));
					asm_code_list_->Pushback(AsmCode("\timul\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_div")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter div" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::DIV());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";enter_div:"));
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_div")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass div" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::DIV());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_div:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_div")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit div" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::DIV());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_div:"));
					asm_code_list_->Pushback(AsmCode("\tmov\tebx, [ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"]"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"], eax"));
					asm_code_list_->Pushback(AsmCode("\tmov\teax, ebx"));
					asm_code_list_->Pushback(AsmCode("\tcdq"));
					asm_code_list_->Pushback(AsmCode("\tidiv\tdword [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_relational_lower_equal")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##enter relational lower equal" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_LOWER_EQUAL());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_relational_lower_equal:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_relational_lower_equal")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##pass relational lower equal" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_LOWER_EQUAL());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_relational_lower_equal:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_relational_lower_equal")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##exit relational lower equal" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_LOWER_EQUAL());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_relational_lower_equal:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					asm_code_list_->Pushback(AsmCode("\tsetge\tal"));
					asm_code_list_->Pushback(AsmCode("\tmovzx\teax, al"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_relational_lower_than")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##enter relational lower than" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_LOWER_THAN());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_relational_lower_than:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_relational_lower_than")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##pass relational lower than" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_LOWER_THAN());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_relational_lower_than:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_relational_lower_than")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##exit relational lower than" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_LOWER_THAN());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_relational_lower_than:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					asm_code_list_->Pushback(AsmCode("\tsetg\tal"));
					asm_code_list_->Pushback(AsmCode("\tmovzx\teax, al"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_relational_higher_equal")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##enter relational higher equal" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_HIGHER_EQUAL());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_relational_higher_equal:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_relational_higher_equal")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##pass relational higher equal" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_HIGHER_EQUAL());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_relational_higher_equal:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_relational_higher_equal")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##exit relational higher equal" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_HIGHER_EQUAL());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_relational_higher_equal:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					asm_code_list_->Pushback(AsmCode("\tsetle\tal"));
					asm_code_list_->Pushback(AsmCode("\tmovzx\teax, al"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_relational_higher_than")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##enter relational higher than" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_HIGHER_THAN());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_relational_higher_than:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_relational_higher_than")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##pass relational higher than" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_HIGHER_THAN());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_relational_higher_than:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_relational_higher_than")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << 
					"##exit relational higher than" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::RELATIONAL_HIGHER_THAN());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_relational_higher_than:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					asm_code_list_->Pushback(AsmCode("\tsetl\tal"));
					asm_code_list_->Pushback(AsmCode("\tmovzx\teax, al"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_equality")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter equality" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::EQUALITY());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_equality:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_equality")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass equality" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::EQUALITY());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_equality:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_equality")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit equality" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::EQUALITY());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_equality:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					asm_code_list_->Pushback(AsmCode("\tsete\tal"));
					asm_code_list_->Pushback(AsmCode("\tmovzx\teax, al"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_not_equality")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter not equality" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::NOT_EQUALITY());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_not_equality:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_not_equality")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass equality" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::NOT_EQUALITY());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_not_equality:"));
					asm_code_list_->Pushback(AsmCode("\tmov\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], eax"));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_not_equality")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit not equality" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::NOT_EQUALITY());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_not_equality:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, [ebp"+
						symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))+
						"]"
					));
					asm_code_list_->Pushback(AsmCode("\tsetne\tal"));
					asm_code_list_->Pushback(AsmCode("\tmovzx\teax, al"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_logical_and")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter logical and" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::LOGICAL_AND());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_logical_and:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
					asm_code_list_->Pushback(AsmCode("\tmov dword\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], 0"));
					logical_and_label_stack_.Push();
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_logical_and")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass logical_and" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::LOGICAL_AND());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_logical_and:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 0"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						logical_and_label_stack_.Top().ToString()));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_logical_and")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit logical and" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::LOGICAL_AND());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_logical_and:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 0"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						logical_and_label_stack_.Top().ToString()));
					asm_code_list_->Pushback(AsmCode("\tmov dword\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"], 1"));
					asm_code_list_->Pushback(
						AsmCode(logical_and_label_stack_.Top().ToString()+":"));
					asm_code_list_->Pushback(AsmCode("\tmov\teax, [ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"]"));	
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("enter_logical_or")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##enter logical or" << std::endl; }
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::LOGICAL_OR());
				if(!decider()){
					symbolia::SymbolTable::PushTempOffset(symbol_table_, 
						symbolia::VariableSize(4));
					asm_code_list_->Pushback(AsmCode(";enter_logical_or:"));
					CommentAboutTemp(asm_code_list_, symbol_table_, "allocate");
					asm_code_list_->Pushback(AsmCode("\tmov dword\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
					+"], 1"));
					logical_or_label_stack_.Push();
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("pass_logical_or")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##pass logical_or" << std::endl; }
				matcher(AstTokenType::STEP_UP_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::LOGICAL_OR());
				Expect(looker, AstTokenType::STEP_DOWN_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";pass_logical_or:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 1"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						logical_or_label_stack_.Top().ToString()));
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("exit_logical_or")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){ std::cout << "##exit logical or" << std::endl; }
				MatchNode(looker, matcher, lexia::TokenType::LOGICAL_OR());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					asm_code_list_->Pushback(AsmCode(";exit_logical_or:"));
					asm_code_list_->Pushback(AsmCode("\tcmp\teax, 1"));
					asm_code_list_->Pushback(AsmCode("\tje\t"+
						logical_or_label_stack_.Top().ToString()));
					asm_code_list_->Pushback(AsmCode("\tmov dword\t[ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"], 0"));
					asm_code_list_->Pushback(
						AsmCode(logical_or_label_stack_.Top().ToString()+":"));
					asm_code_list_->Pushback(AsmCode("\tmov\teax, [ebp"
						+symbolia::OffsetToString(
							symbolia::SymbolTable::TopTempOffset(symbol_table_))
						+"]"));	
					CommentAboutTemp(asm_code_list_, symbol_table_, "release");
					symbolia::SymbolTable::PopTempOffset(symbol_table_); 
				}
				processor("pattern");
				return nullptr;	
			}));
		ast_parser_.DefineSyntaxRule("enter_block")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << ">>>>>>>enter block: " << std::endl; 
				}
				matcher(AstTokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::BLOCK());
				if(!decider()){
					symbol_table_.PushScope(symbolia::BaseScope::Create(
						symbol_table_.GetCurrentScope()));
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("exit_block")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << "<<<<<<<<exit block: " << std::endl;
				}
				MatchNode(looker, matcher, lexia::TokenType::BLOCK());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				if(!decider()){
					symbol_table_.PopScope();
				}
				processor("pattern");
				return nullptr;	
			}));

		ast_parser_.DefineSyntaxRule("empty_block")
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					std::cout << ">>><<<empty block!!" << std::endl;
				}
				matcher(semantia::TokenType::STEP_DOWN_TOKEN_TYPE());
				MatchNode(looker, matcher, lexia::TokenType::BLOCK());
				Expect(looker, AstTokenType::STEP_UP_TOKEN_TYPE());
				processor("pattern");
				return nullptr;	
			}));
		
		ast_parser_.DefineSyntaxRule("pass")
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				if(!decider()){
					if(looker(1).GetValue()){
						//std::cout << "pass token: " << looker(1) << " " 
							//<< looker(1).GetValue() << std::endl;
					}
					else {
						//std::cout << "pass token: " << looker(1) << std::endl;	
					}
				}
				if(looker(1).GetType()
						== semantia::TokenType::EOF_TOKEN_TYPE()){
					return nullptr;
				}else{
					matcher(looker(1).GetType());
					processor("pattern");
					return nullptr;
				}
			}));

		ast_parser_.DefineSyntaxRule("pattern")
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_function_call");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_parameter");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_function_call");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("function_declaration");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_function_declaration");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("constant");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("variable_declaration");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_assign");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_assign");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("variable_reference");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_add");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_add");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_add");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_sub");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_sub");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_sub");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_unary_minus");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_mul");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_mul");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_mul");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_div");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_div");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_div");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_relational_lower_equal");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_relational_lower_equal");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_relational_lower_equal");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_relational_lower_than");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_relational_lower_than");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_relational_lower_than");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_relational_higher_equal");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_relational_higher_equal");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_relational_higher_equal");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_relational_higher_than");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_relational_higher_than");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_relational_higher_than");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_equality");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_equality");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_equality");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_not_equality");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_not_equality");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_not_equality");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_logical_and");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_logical_and");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_logical_and");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_logical_or");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_logical_or");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_logical_or");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_return");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_while");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_while");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_while");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_if_with_else");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_else");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_if_of_if_with_else");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_if_with_else");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_if");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass_if");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_if");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("empty_block");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("enter_block");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([this](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("exit_block");
			}))
			->AddChoice(AstParser::SyntaxRule::Choice([](
					PP_AST_PARSER_SYNTAX_RULE_ARGUMENTS) -> void* {
				return processor("pass");
			}));
	}

	auto DefineLanguageSyntax() -> void {
		lang_parser_.DefineSyntaxRule("program")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider
					)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##external_declaration1");
				return processor("declaration");
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##external_declaration2");
				return processor("function_definition");
			}));

		lang_parser_.DefineSyntaxRule("declaration")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##declarator");
				return CreateAst(matcher(lexia::TokenType::IDENTIFIER()));	
			}));

		lang_parser_.DefineSyntaxRule("function_definition")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement1");
				matcher(lexia::TokenType::SEMICOLON());
				return CreateAst(lexia::Token::CONS_TOKEN());
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement2");
				auto exp = processor("expression");
				matcher(lexia::TokenType::SEMICOLON());
				return exp;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement3");
				return processor("compound_statement");
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement4");
				matcher(lexia::TokenType::IF());
				auto if_node = CreateAst(lexia::Token::IF_OF_IF_WITH_ELSE_TOKEN());
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				if_node->AddChild(processor("expression"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				if_node->AddChild(processor("statement"));
				auto else_node = CreateAst(matcher(lexia::TokenType::ELSE()));
				else_node->AddChild(processor("statement"));
				auto cons = CreateAst(lexia::Token::IF_WITH_ELSE_TOKEN());
				cons->AddChild(if_node);
				cons->AddChild(else_node);
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement5");
				auto cons = CreateAst(matcher(lexia::TokenType::IF()));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement6");
				auto cons = CreateAst(matcher(lexia::TokenType::WHILE()));
				matcher(lexia::TokenType::LEFT_PARENTHESIS());
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::RIGHT_PARENTHESIS());
				cons->AddChild(processor("statement"));
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement7");
				auto ret = CreateAst(matcher(lexia::TokenType::RETURN()));
				matcher(lexia::TokenType::SEMICOLON());
				return ret;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##statement8");
				auto cons = CreateAst(matcher(lexia::TokenType::RETURN()));
				cons->AddChild(processor("expression"));
				matcher(lexia::TokenType::SEMICOLON());
				return cons;
			}));
		
		lang_parser_.DefineSyntaxRule("compound_statement")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr { 
				lang_parser_.DebugPrint("##compound_statement1");
				matcher(lexia::TokenType::LEFT_BRACE());
				matcher(lexia::TokenType::RIGHT_BRACE());
				return CreateAst(lexia::Token::BLOCK_TOKEN());
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##assign_expression1");
				auto left_id = CreateAst(matcher(lexia::TokenType::IDENTIFIER()));
				matcher(lexia::TokenType::EQUAL());
				auto right_exp = processor("assign_expression");
				auto var_ref = CreateAst(lexia::Token::VARIABLE_REFERENCE_TOKEN());
				var_ref->AddChild(left_id);
				auto cons = CreateAst(lexia::Token::ASSIGN_TOKEN());
				cons->AddChild(var_ref);
				cons->AddChild(right_exp);
				return cons;
			}))
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##assign_expression2");
				return processor("logical_or_expression");
			}));

		lang_parser_.DefineSyntaxRule("logical_or_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##logical_or_expression");
				auto first_exp = processor("logical_and_expression");
				if(!IsTokenTypeSame(looker(1), lexia::TokenType::OR())){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::LOGICAL_OR_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(IsTokenTypeSame(looker(1), lexia::TokenType::OR())){
					matcher(lexia::TokenType::OR());
					inner_cons->AddChild(before_exp);
					before_exp = processor("logical_and_expression");
					if(IsTokenTypeSame(looker(1), lexia::TokenType::OR())){
						auto new_inner_cons = 
							CreateAst(lexia::Token::LOGICAL_OR_TOKEN());
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##logical_and_expression");
				auto first_exp = processor("equality_expression");
				if(!IsTokenTypeSame(looker(1), lexia::TokenType::AND())){
					return first_exp;
				}
				auto outer_cons = CreateAst(lexia::Token::LOGICAL_AND_TOKEN());
				auto inner_cons = outer_cons; 
				auto before_exp = first_exp;
				while(IsTokenTypeSame(looker(1), lexia::TokenType::AND())){
					matcher(lexia::TokenType::AND());
					inner_cons->AddChild(before_exp);
					before_exp = processor("equality_expression");
					if(IsTokenTypeSame(looker(1), lexia::TokenType::AND())){
						auto new_inner_cons = 
							CreateAst(lexia::Token::LOGICAL_AND_TOKEN());
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##equality_expression");
				auto first_exp = processor("relational_expression");
				Ast::Ptr outer_cons;
				if(IsTokenTypeSame(looker(1), lexia::TokenType::EQUALEQUAL())){
					outer_cons = CreateAst(lexia::Token::EQUALITY_TOKEN());	
				}else
				if(IsTokenTypeSame(looker(1), lexia::TokenType::NOT_EQUAL())){
					outer_cons = CreateAst(lexia::Token::NOT_EQUALITY_TOKEN());
				}
				else{
					return first_exp;	
				}
				outer_cons->AddChild(first_exp);
				matcher(looker(1).GetType());
				outer_cons->AddChild(processor("relational_expression"));
				return outer_cons;
			}));

		lang_parser_.DefineSyntaxRule("relational_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##relational_expression");
				auto first_exp = processor("add_expression");
				Ast::Ptr outer_cons;
				if(IsTokenTypeSame(looker(1), lexia::TokenType::LOWER_THAN())){	
					outer_cons = CreateAst(
						lexia::Token::RELATIONAL_LOWER_THAN_TOKEN());
				}else
				if(IsTokenTypeSame(looker(1), lexia::TokenType::HIGHER_THAN())){
					outer_cons = CreateAst(
						lexia::Token::RELATIONAL_HIGHER_THAN_TOKEN());
				}else
				if(IsTokenTypeSame(looker(1), lexia::TokenType::LOWER_EQUAL())){
					outer_cons = CreateAst(
						lexia::Token::RELATIONAL_LOWER_EQUAL_TOKEN());
				}else
				if(IsTokenTypeSame(looker(1), lexia::TokenType::HIGHER_EQUAL())){
					outer_cons = CreateAst(
						lexia::Token::RELATIONAL_HIGHER_EQUAL_TOKEN());
				}
				else {
					return first_exp;	
				}
				outer_cons->AddChild(first_exp);
				matcher(looker(1).GetType());
				outer_cons->AddChild(processor("add_expression"));
				
				return outer_cons;
			}));
		
		lang_parser_.DefineSyntaxRule("add_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##add_expression");
				auto first_exp = processor("multiply_expression");
				auto is_add = [](const LangToken& token) -> const bool {
						return IsTokenTypeSame(token, lexia::TokenType::PLUS());
					};
				auto is_sub = [](const LangToken& token) -> const bool {
						return IsTokenTypeSame(token, lexia::TokenType::MINUS());
					};
				Ast::Ptr parent;
				parent = first_exp;
				while(true){
					Ast::Ptr new_parent;
					if(is_add(looker(1))){
						new_parent = CreateAst(lexia::Token::ADD_TOKEN());
					}else
					if(is_sub(looker(1))){
						new_parent = CreateAst(lexia::Token::SUB_TOKEN());	
					}
					else {
						break;	
					}
					matcher(GetType(looker(1)));
					new_parent->AddChild(parent);
					new_parent->AddChild(processor("multiply_expression"));
					parent = new_parent;
				}
				return parent;
			}));

		lang_parser_.DefineSyntaxRule("multiply_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##multiply_expression");
				auto first_exp = processor("unary_expression");
				auto is_mul = [](const LangToken& token) -> const bool {
						return IsTokenTypeSame(token, 
							lexia::TokenType::ASTERISK());
					};
				auto is_div = [](const LangToken& token) -> const bool {
						return IsTokenTypeSame(token, lexia::TokenType::SLASH());
					};
				Ast::Ptr parent;
				parent = first_exp;
				while(true){
					Ast::Ptr new_parent;
					if(is_mul(looker(1))){
						new_parent = CreateAst(lexia::Token::MUL_TOKEN());
					}else
					if(is_div(looker(1))){
						new_parent = CreateAst(lexia::Token::DIV_TOKEN());	
					}
					else {
						break;	
					}
					matcher(GetType(looker(1)));
					new_parent->AddChild(parent);
					new_parent->AddChild(processor("unary_expression"));
					parent = new_parent;
				}
				return parent;
			}));
		lang_parser_.DefineSyntaxRule("unary_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##unary_expression");
				if(IsTokenTypeSame(looker(1), lexia::TokenType::MINUS())){
					matcher(lexia::TokenType::MINUS());
					auto cons = CreateAst(lexia::Token::UNARY_MINUS_TOKEN());
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##postfix_expression3");
				return processor("primary_expression");	
			}));

		lang_parser_.DefineSyntaxRule("primary_expression")
			->AddChoice(LangParser::SyntaxRule::Choice([this](
					const LangParser::SyntaxRule::TokenMatcher& matcher,
					const LangParser::SyntaxRule::AheadTokenLooker& looker,
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
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
					const LangParser::SyntaxRule::RuleProcessor& processor,
					const LangParser::SyntaxRule::IsSpeculatingDecider& decider)
					-> const Ast::Ptr {
				lang_parser_.DebugPrint("##argument_expression_list");
				auto param_list = CreateAst(lexia::Token::PARAMETER_LIST_TOKEN());
				auto parameter = CreateAst(lexia::Token::PARAMETER_TOKEN());
				param_list->AddChild(parameter);
				parameter->AddChild(processor("assign_expression"));
				while(IsTokenTypeSame(looker(1), lexia::TokenType::COMMA())){ 
					matcher(lexia::TokenType::COMMA());
					parameter = CreateAst(lexia::Token::PARAMETER_TOKEN());
					param_list->AddChild(parameter);
					parameter->AddChild(processor("assign_expression"));
				}	

				//REVERSE ARGUMENTS FOR CALLING FUNCTION!!!
				param_list->ReverseChildren();
				return param_list;
			}));
		
	}

};
}

