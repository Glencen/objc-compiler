#ifndef SEMANTIC_EXCEPTIONS_H
#define SEMANTIC_EXCEPTIONS_H

#include <stdexcept>
#include <string>
#include <sstream>

class semantic_exception : public std::runtime_error {
protected:
    std::string context;
    int line = -1;
    int column = -1;
    std::string nodeInfo;

public:
    semantic_exception(const std::string& message, const std::string& context = "", 
                     int line = -1, int column = -1, const std::string& nodeInfo = "")
        : std::runtime_error(message), context(context), line(line), column(column), nodeInfo(nodeInfo) {}

    virtual std::string getFullMessage() const {
        std::stringstream ss;
        ss << "Semantic Error";
        if (!context.empty()) {
            ss << " in " << context;
        }
        if (line != -1) {
            ss << " at line " << line;
            if (column != -1) {
                ss << ":" << column;
            }
        }
        ss << ": " << what();
        if (!nodeInfo.empty()) {
            ss << " [" << nodeInfo << "]";
        }
        return ss.str();
    }

    virtual ~semantic_exception() = default;
};

class type_exception : public semantic_exception {
public:
    type_exception(const std::string& message, const std::string& context = "", 
                 int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

class symbol_exception : public semantic_exception {
public:
    symbol_exception(const std::string& message, const std::string& context = "", 
                   int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

class array_exception : public semantic_exception {
public:
    array_exception(const std::string& message, const std::string& context = "", 
                  int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

class class_exception : public semantic_exception {
public:
    class_exception(const std::string& message, const std::string& context = "", 
                  int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

class method_exception : public semantic_exception {
public:
    method_exception(const std::string& message, const std::string& context = "", 
                   int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

class field_exception : public semantic_exception {
public:
    field_exception(const std::string& message, const std::string& context = "", 
                  int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

class function_exception : public semantic_exception {
public:
    function_exception(const std::string& message, const std::string& context = "", 
                     int line = -1, int column = -1, const std::string& nodeInfo = "")
        : semantic_exception(message, context, line, column, nodeInfo) {}
};

#endif