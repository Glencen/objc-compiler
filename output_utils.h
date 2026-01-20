#ifndef OUTPUT_UTILS_H
#define OUTPUT_UTILS_H

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <iomanip>
#include <sstream>
#include <mutex>
#include <memory>
#include <queue>
#include <condition_variable>
#include <atomic>
#include <thread>

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

class DebugLogger {
private:
    struct LogEntry {
        std::chrono::steady_clock::time_point timestamp;
        std::string message;
        std::string function;
        std::string filename;
        int line;
        
        LogEntry(const std::string& msg, const std::string& func, 
                const std::string& file, int ln)
            : timestamp(std::chrono::steady_clock::now()), 
              message(msg), 
              function(func),
              filename(file),
              line(ln) {}
    };
    
    std::vector<LogEntry> logBuffer;
    std::ofstream logFile;
    std::mutex logMutex;
    std::atomic<bool> isInitialized;
    std::atomic<bool> isEnabledFlag;
    std::atomic<size_t> logCounter;
    size_t bufferLimit;
    bool autoFlush;
    
    std::string getCurrentTimestamp() const;
    std::string getShortFilename(const std::string& fullPath) const;
    void flushBufferToFile();
    
    DebugLogger();
    
public:
    DebugLogger(const DebugLogger&) = delete;
    DebugLogger& operator=(const DebugLogger&) = delete;
    
    ~DebugLogger();
    
    static DebugLogger& getInstance();
    
    void initialize(const std::string& filename = "debug.log", size_t bufferSize = 100, bool autoFlushEnabled = true);
    void log(const std::string& message, const std::string& function = "", const std::string& filename = "", int line = 0);
    void logInfo(const std::string& message, const std::string& function = "", const std::string& filename = "", int line = 0);
    void logError(const std::string& message, const std::string& function = "", const std::string& filename = "", int line = 0);
    void logWarning(const std::string& message, const std::string& function = "", const std::string& filename = "", int line = 0);
    void flush();
    void clear();
    void close();
    
    void enable(bool state = true);
    void disable();
    void setBufferLimit(size_t limit);
    void setAutoFlush(bool enabled);
    
    bool isEnabled() const;
    bool isInitializedState() const;
    bool isFileOpen() const;
    size_t getBufferSize() const;
    size_t getTotalLogsCount() const;
    std::string getLogFilePath() const;
    
    static std::string safeString(const char* str);

    #ifdef DEBUG_ENABLED
    #define DEBUG_LOG(msg) DebugLogger::getInstance().log(msg, __FUNCTION__, __FILE__, __LINE__)
    #define DEBUG_LOG_INFO(msg) DebugLogger::getInstance().logInfo(msg, __FUNCTION__, __FILE__, __LINE__)
    #define DEBUG_LOG_ERROR(msg) DebugLogger::getInstance().logError(msg, __FUNCTION__, __FILE__, __LINE__)
    #define DEBUG_LOG_WARNING(msg) DebugLogger::getInstance().logWarning(msg, __FUNCTION__, __FILE__, __LINE__)
    #define DEBUG_LOG_FMT(fmt, ...) do { \
        char buffer[1024]; \
        snprintf(buffer, sizeof(buffer), fmt, __VA_ARGS__); \
        DebugLogger::getInstance().log(buffer, __FUNCTION__, __FILE__, __LINE__); \
    } while(0)
    
    #else
    #define DEBUG_LOG(msg) ((void)0)
    #define DEBUG_LOG_INFO(msg) ((void)0)
    #define DEBUG_LOG_ERROR(msg) ((void)0)
    #define DEBUG_LOG_WARNING(msg) ((void)0)
    #define DEBUG_LOG_FMT(fmt, ...) ((void)0)
    #endif
};

#endif