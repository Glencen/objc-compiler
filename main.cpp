#include <iostream>
#include <fstream>
#include <filesystem>
#include <cstdlib>
#include <signal.h>
#include "objc-parser.hpp"
#include "classes.h"
#include "tables.h"
#include "output_utils.h"

namespace fs = std::filesystem;

extern FILE* yyin;
extern int yyparse();
extern ProgramNode* root;

void safeExit(int code) {
    if (DebugLogger::getInstance().isEnabled()) {
        DebugLogger::getInstance().close();
    }
    TokenOutput::getInstance().close();
    
    if (yyin) {
        fclose(yyin);
        yyin = nullptr;
    }
    
    exit(code);
}

void signalHandler(int signal) {
    string signalStr;
    switch (signal) {
        case 2:     signalStr = "SIGINT";
        case 15:    signalStr = "SIGTERM";
        case 11:    signalStr = "SIGSEGV";
    }
    std::cerr << "\nReceived signal: " << signal << " (" << signalStr << ")" << std::endl;
    safeExit(signal);
}

std::string safeString(const char* str) {
    return str ? std::string(str) : std::string("(null)");
}

int main(int argc, char* argv[])
{
    signal(SIGINT, signalHandler);
    signal(SIGTERM, signalHandler);
    signal(SIGSEGV, signalHandler);
    
    std::atexit([]() {
        DebugLogger::getInstance().close();
        TokenOutput::getInstance().close();
    });

    DebugLogger::getInstance().initialize("semantic_debug.log");
    
    if (argc != 2)
    {
        std::cerr << "Usage: " << argv[0] << " <file_path>" << std::endl;
        safeExit(1);
    }

    std::string inputFile = safeString(argv[1]);
    
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

    try {
        if (!fs::exists(tables_dir)) {
            if (!fs::create_directory(tables_dir)) {
                std::cerr << "Could not create directory for tables: '" + tables_dir + "'" << std::endl;
                safeExit(1);
            }
        }

        TokenOutput::getInstance().initialize(token_file);

        yyin = fopen(inputFile.c_str(), "r");
        if (!yyin) {
            std::cerr << "Could not open input file: '" + inputFile + "'" << std::endl;
            safeExit(1);
        }

        int parse_result = yyparse();

        TokenOutput::getInstance().close();

        if (parse_result != 0) {
            std::cerr << "Parsing failed with code: '" + std::to_string(parse_result) + "'" << std::endl;
            fclose(yyin);
            safeExit(1);
        }

        if (!root) {
            std::cerr << "No parse tree generated" << std::endl;
            fclose(yyin);
            safeExit(1);
        }

        fclose(yyin);
        yyin = nullptr;

        std::ofstream ast_before_out(ast_before_file);
        if (!ast_before_out.is_open()) {
            std::cerr << "Could not open AST before file for writing: '" + ast_before_file + "'" << std::endl;
            safeExit(1);
        }

        ast_before_out << "digraph AST {\n";
        try {
            ast_before_out << root->toDot();
        } catch (const std::exception& e) {
            throw;
        }
        ast_before_out << "}\n";
        ast_before_out.close();

        std::cout << "Tokens written to: " << token_file << std::endl;
        std::cout << "AST before semantics written to: " << ast_before_file << std::endl;

        try {
            ClassesTable::initRTL();
            DEBUG_LOG("DEBUG: RTL initialized");
            root->fillTables();
            DEBUG_LOG("DEBUG: root->fillTables() completed");
            root->semanticTransform();
            DEBUG_LOG("DEBUG: root->semanticTransform() completed");
            
            std::ofstream ast_after_out(ast_after_file);
            if (!ast_after_out.is_open()) {
                std::cerr << "Could not open AST after file for writing: '" + ast_after_file + "'" << std::endl;
                safeExit(1);
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
            
        } catch (const semantic_exception& e) {
            std::cerr << "\nError during semantic analysis: " << e.getFullMessage() << std::endl;
            safeExit(1);
        } catch (const runtime_error& e) {
            std::cerr << "\nError during semantic analysis: " << e.what() << std::endl;
            safeExit(1);
        } catch (const exception& e) {
            std::cerr << "\nError during semantic analysis: " << e.what() << std::endl;
            safeExit(1);
        } catch (...) {
            std::cerr << "\nUnknown error during semantic analysis!" << std::endl;
            safeExit(1);
        }

    } catch (const std::exception& e) {
        std::cerr << "\nUnexpected error: " << e.what() << std::endl;
        safeExit(1);
    }

    DebugLogger::getInstance().close();
    TokenOutput::getInstance().close();
    
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
    
    return 0;
}