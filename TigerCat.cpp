#ifdef TIGERCAT_UNIT_TEST
#include "TigerCat.h"
#include <iostream>
#include <fstream>

using namespace tiger_cat;

int main(int argc, char* argv[])
{
#ifdef RELEASE_MODE
	if(argc!=2){
		std::cout << "Compiler: " << 
			"ArgumentError: Please input a source file name." << std::endl;
		return 0;	
	}
	std::ifstream ifs(argv[1]);
#else
	std::ifstream ifs("test4.tc");
#endif
	if(!ifs){
		std::cout << "Compiler: "
			"FileOpenError: Please check the source file existing." << std::endl;
		return 0;
	}
	std::string code(
		(std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
	try{
		TigerCat tiger_cat(code);
		tiger_cat.InitLangTokenBuffer();
		tiger_cat.DefineLanguageSyntax();
		auto ast_root = tiger_cat.MakeAbstractSyntaxTree();
		auto token2str = [](const Token::Ptr& token){
				if(token->GetKind().IsUninitialized()){
					return token->GetWord().ToString();
				}
				else {
					std::ostringstream oss;
					oss << token->GetWord() << ":" << token->GetDepth();
					if(token->GetDepth() != 0 && 
							token->GetKind() == Kind::VARIABLE_DECLARATION()){
						oss << ":" << token->GetOffset();
					}

					return oss.str();
				}
			};
		std::cout << ast_root->ToString(token2str) << std::endl;
		tiger_cat.InitAstStream();
		tiger_cat.InitAstTokenBuffer();
		tiger_cat.DefineTreePatternForSymbolTable();
		tiger_cat.MakeSymbolTable();
		std::cout << ast_root->ToString(token2str) << std::endl;
	}
	catch(const lexia::InvalidCharactorError& e){
		std::cout << "Lexer: " << e.what() << std::endl;
	}
	catch(const parsia::SyntaxError& e){	
		std::cout << "Parser: " << e.what() << std::endl;
	}

    return 0;
}

#endif
