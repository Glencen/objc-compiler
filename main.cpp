#include <iostream>
#include <fstream>
#include <filesystem>
#include "objc-parser.hpp"
#include "classes.h"
#include "output_utils.h"

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
    
    std::string base_name = inputFile;
    size_t dot_pos = base_name.find_last_of('.');
    if (dot_pos != std::string::npos) {
        base_name = base_name.substr(0, dot_pos);
    }
    
    std::string token_file = base_name + "_tokens.txt";
    std::string ast_file = base_name + "_ast.dot";

    std::cout << "Input file: " << inputFile << std::endl;
    std::cout << "Token output: " << token_file << std::endl;
    std::cout << "AST output: " << ast_file << std::endl;

    TokenOutput::getInstance().initialize(token_file);

    yyin = fopen(inputFile.c_str(), "r");
    if (!yyin) {
        std::cerr << "Could not open input file: '" + inputFile + "'" << std::endl;
        TokenOutput::getInstance().close();
        return 1;
    }

    int parse_result = yyparse();

    TokenOutput::getInstance().close();

    if (parse_result != 0) {
        std::cerr << "Parsing failed with code: '" + std::to_string(parse_result) + "'" << std::endl;
        fclose(yyin);
        return 1;
    }

    if (!root) {
        std::cerr << "No parse tree generated" << std::endl;
        fclose(yyin);
        return 1;
    }

    fclose(yyin);

    std::ofstream ast_out(ast_file);
    if (!ast_out.is_open()) {
        std::cerr << "Could not open AST file for writing: '" + ast_file + "'" << std::endl;
        return 1;
    }

    ast_out << "digraph AST {\n";
    ast_out << root->toDot();
    ast_out << "}\n";
    ast_out.close();

    std::cout << "Tokens written to: " << token_file << std::endl;
    std::cout << "AST written to: " << ast_file << std::endl;

    // Опционально: генерация PNG из DOT файла
    // std::cout << "Generating PNG visualization..." << std::endl;
    // std::string png_file = base_name + "_ast.png";
    // std::string command = "dot -Tpng \"" + ast_file + "\" -o \"" + png_file + "\"";
    // int result = system(command.c_str());
    // if (result == 0) {
    //     std::cout << "PNG visualization generated: " << png_file << std::endl;
    // }

    return 0;
}