#ifdef USERTEST_UNIT_TEST
#include "UserTest.h"
#include <iostream>
#include <fstream>
#include <streambuf>
#include "Lexer.h"

using namespace user_test;

int main(int argc, char* argv[])
{
	std::ifstream ifs(argv[1]);
	std::string line(
		(std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
	lexia::Lexer lexer(line);
	while(true){
		const auto token = lexer.GetNextToken(); 
		if(token.IsEof()){
			break;	
		}
		std::cout << "NextToken:" << token << "\n" << std::endl;
		//std::string str;
		//std::cin >> str;
	}

    return 0;
}

#endif
