#include <iostream>
#include <fstream>
#include <filesystem>
#include "objc-parser.hpp"
#include "classes.h"
#include "tables.h"
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
    std::string ast_before_file = base_name + "_ast_before.dot";
    std::string ast_after_file = base_name + "_ast_after.dot";
    std::string tables_dir = base_name + "_tables/";

    std::cout << "Input file: " << inputFile << std::endl;
    std::cout << "Token output: " << token_file << std::endl;
    std::cout << "AST before semantics: " << ast_before_file << std::endl;
    std::cout << "AST after semantics: " << ast_after_file << std::endl;
    std::cout << "Tables output directory: " << tables_dir << std::endl;

    if (!fs::exists(tables_dir)) {
        if (!fs::create_directory(tables_dir)) {
            std::cerr << "Could not create directory for tables: '" + tables_dir + "'" << std::endl;
        }
    }

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

    std::ofstream ast_before_out(ast_before_file);
    if (!ast_before_out.is_open()) {
        std::cerr << "Could not open AST before file for writing: '" + ast_before_file + "'" << std::endl;
        return 1;
    }

    ast_before_out << "digraph AST {\n";
    ast_before_out << root->toDot();
    ast_before_out << "}\n";
    ast_before_out.close();

    std::cout << "Tokens written to: " << token_file << std::endl;
    std::cout << "AST before semantics written to: " << ast_before_file << std::endl;

    DebugLogger::getInstance().initialize("semantic_debug.log");

    try {
        ClassesTable::initRTL();
        root->fillTables();
        root->semanticTransform();
        ClassesTable::fillFieldRefs();
        ClassesTable::fillMethodRefs();
        ClassesTable::fillLiterals();
        FunctionsTable::fillFieldRefs();
        FunctionsTable::fillMethodRefs();
        FunctionsTable::fillLiterals();
        FunctionsTable::convertToClassProgramMethods();
        FunctionsTable::semanticTransform();
        ClassesTable::semanticTransform();
        
        std::ofstream ast_after_out(ast_after_file);
        if (!ast_after_out.is_open()) {
            std::cerr << "Could not open AST after file for writing: '" + ast_after_file + "'" << std::endl;
            return 1;
        }

        ast_after_out << "digraph AST {\n";
        ast_after_out << root->toDot();
        ast_after_out << "}\n";
        ast_after_out.close();
        
        std::cout << "\nAST after semantics written to: " << ast_after_file << std::endl;
        
        if (!tables_dir.empty() && tables_dir.back() != '/') {
            tables_dir += '/';
        }
        
        ClassesTable::toCSVFile(tables_dir, '|');
        
        std::cout << "CSV tables generated in directory: " << tables_dir << std::endl;
        
        std::cout << "\nGenerated tables:" << std::endl;
        std::cout << "- " << tables_dir << "ClassesTable.csv" << std::endl;
        
        if (!ClassesTable::items.empty()) {
            for (const auto& [className, classElement] : ClassesTable::items) {
                std::cout << "- " << tables_dir << className << "_ConstantsTable.csv" << std::endl;
                std::cout << "- " << tables_dir << className << "_FieldsTable.csv" << std::endl;
                std::cout << "- " << tables_dir << className << "_MethodsTable.csv" << std::endl;
                std::cout << "- " << tables_dir << className << "_PropertiesTable.csv" << std::endl;
            }
        }
        
        std::cout << "\nProcessing completed successfully!" << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "\nError during semantic analysis: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "\nUnknown error during semantic analysis!" << std::endl;
        return 1;
    }

    return 0;
}