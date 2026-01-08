#ifndef OUTPUT_UTILS_H
#define OUTPUT_UTILS_H

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <iomanip>
#include <sstream>

typedef enum {
    TOK_KEYWORD,
    TOK_IDENTIFIER,
    TOK_CLASS_NAME,
    TOK_OPERATOR,
    TOK_DELIMITER,
    TOK_INTEGER,
    TOK_FLOAT,
    TOK_STRING,
    TOK_CHAR,
    TOK_COMMENT,
    TOK_OBJC_INTERFACE,
    TOK_OBJC_IMPLEMENTATION,
    TOK_OBJC_END,
    TOK_OBJC_PROPERTY,
    TOK_UNKNOWN,
    TOK_ERROR
} token_type_t;

class TokenOutput {
private:
    struct TokenInfo {
        token_type_t type;
        std::string value;
        int line;
        
        TokenInfo(token_type_t t, const std::string& v, int l) 
            : type(t), value(v), line(l) {}
    };
    
    std::vector<TokenInfo> tokens;
    std::ofstream token_file;
    bool parser_error_occurred;
    int current_line;
    
public:
    static TokenOutput& getInstance();
    void initialize(const std::string& filename);
    void setCurrentLine(int line);
    void addToken(token_type_t type, const std::string& value);
    void addTokenInt(token_type_t type, long value);
    void addTokenFloat(token_type_t type, double value);
    void flushTokens();
    void setParserError();
    static const char* token_type_name(token_type_t type);
    size_t getTokenCount() const;
    void close();
    
private:
    TokenOutput() = default;
    TokenOutput(const TokenOutput&) = delete;
    TokenOutput& operator=(const TokenOutput&) = delete;
};

#endif