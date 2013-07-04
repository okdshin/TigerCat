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
	std::ifstream ifs("test2.tc");
#endif
	if(!ifs){
		std::cout << "Compiler: "
			"FileOpenError: Please check the source file existing." << std::endl;
		return 0;
	}
	std::string code(
		(std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
	TigerCat tiger_cat(code);
	tiger_cat.Define();
	tiger_cat.InitLanguageTokenBuffer();
	try{
		auto ast_root = tiger_cat.MakeAbstractSyntaxTree();
		auto token2str = [](const semantia::Token& token){
				return token.GetWord().ToString();
			};
		std::cout << ast_root->ToString(token2str) << std::endl;
		tiger_cat.InitAstStream();
		tiger_cat.InitAstTokenBuffer();
		tiger_cat.MakeSymbolTable();
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
