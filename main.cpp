#include <iostream>
#include <fstream>
#include <filesystem>
#include "objc-parser.hpp"
#include "classes.h"

namespace fs = std::filesystem;

extern FILE* yyin;
extern int yyparse();
extern ProgramNode* root;

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: " << argv[0] << " <file_path>" << std::endl;
        return 1;
    }

    std::string inputFile = argv[1];

    yyin = fopen(inputFile.c_str(), "r");
    if (!yyin) {
        std::cerr << "Could not open input file: '" + inputFile + "'" << std::endl;
        return 1;
    }

    int parse_result = yyparse();

    if (parse_result != 0) {
        std::cerr << "Parsing failed with code: '" + std::to_string(parse_result) + "'" << std::endl;
        return 1;
    }

    if (!root) {
        std::cerr << "No parse tree generated" << std::endl;
        return 1;
    }

    std::cout << "digraph AST {\n";
    std::cout << root->toDot();
    std::cout << "}\n";

    return 0;
}