#include "output_utils.h"

TokenOutput& TokenOutput::getInstance() {
    static TokenOutput instance;
    return instance;
}

void TokenOutput::initialize(const std::string& filename) {
    token_file.open(filename);
    if (!token_file.is_open()) {
        std::cerr << "Warning: Could not open token file: " << filename << std::endl;
    }
    tokens.clear();
    parser_error_occurred = false;
    current_line = 1;
}

void TokenOutput::setCurrentLine(int line) {
    current_line = line;
}

void TokenOutput::addToken(token_type_t type, const std::string& value) {
    tokens.emplace_back(type, value, current_line);
}

void TokenOutput::addTokenInt(token_type_t type, long value) {
    std::ostringstream oss;
    oss << value;
    tokens.emplace_back(type, oss.str(), current_line);
}

void TokenOutput::addTokenFloat(token_type_t type, double value) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(6) << value;
    tokens.emplace_back(type, oss.str(), current_line);
}

void TokenOutput::flushTokens() {
    if (token_file.is_open()) {
        for (const auto& token : tokens) {
            token_file << std::setw(6) << token.line << ": " 
                        << std::left << std::setw(20) << token_type_name(token.type) 
                        << " '" << token.value << "'" << std::endl;
        }
        tokens.clear();
        token_file.flush();
    } else {
        for (const auto& token : tokens) {
            std::cerr << std::setw(6) << token.line << ": " 
                        << std::left << std::setw(20) << token_type_name(token.type) 
                        << " '" << token.value << "'" << std::endl;
        }
        tokens.clear();
    }
}

void TokenOutput::setParserError() {
    parser_error_occurred = true;
    flushTokens();
}

void TokenOutput::close() {
    if (token_file.is_open()) {
        if (!parser_error_occurred) {
            flushTokens();
        }
        token_file.close();
    }
    else if (!tokens.empty()) {
        flushTokens();
    }
}

const char* TokenOutput::token_type_name(token_type_t type) {
    switch (type) {
        case TOK_KEYWORD: return "KEYWORD";
        case TOK_IDENTIFIER: return "IDENTIFIER";
        case TOK_CLASS_NAME: return "CLASS_NAME";
        case TOK_OPERATOR: return "OPERATOR";
        case TOK_DELIMITER: return "DELIMITER";
        case TOK_INTEGER: return "INTEGER";
        case TOK_FLOAT: return "FLOAT";
        case TOK_STRING: return "STRING";
        case TOK_CHAR: return "CHAR";
        case TOK_COMMENT: return "COMMENT";
        case TOK_OBJC_INTERFACE: return "OBJC_INTERFACE";
        case TOK_OBJC_IMPLEMENTATION: return "OBJC_IMPLEMENTATION";
        case TOK_OBJC_END: return "OBJC_END";
        case TOK_OBJC_PROPERTY: return "OBJC_PROPERTY";
        case TOK_UNKNOWN: return "UNKNOWN";
        case TOK_ERROR: return "ERROR";
        default: return "UNKNOWN";
    }
}

size_t TokenOutput::getTokenCount() const {
    return tokens.size();
}
