#ifdef TIGERCAT_UNIT_TEST
#include "TigerCat.h"
#include <iostream>
#include <fstream>

using namespace tiger_cat;

int main(int argc, char* argv[])
{
	std::ifstream ifs("test1.tc");
	std::string code(
		(std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
	TigerCat tiger_cat(code);
	tiger_cat.Define();
	tiger_cat.InitLanguageTokenBuffer();
	auto ast_root = tiger_cat.MakeAbstractSyntaxTree();
	std::cout << ast_root->ToString([](const semantia::Token& token){
			return token.GetWord().ToString();
		}) << std::endl;
	tiger_cat.InitAstStream();
	tiger_cat.InitAstTokenBuffer();
	tiger_cat.MakeSymbolTable();

    return 0;
}

#endif
