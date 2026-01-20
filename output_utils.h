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
    
    struct TimestampedQueue {
        std::priority_queue<
            LogEntry, 
            std::vector<LogEntry>,
            bool(*)(const LogEntry&, const LogEntry&)
        > queue;
        std::mutex mutex;
        std::condition_variable cv;
        
        TimestampedQueue() : queue([](const LogEntry& a, const LogEntry& b) {
            return a.timestamp > b.timestamp;
        }) {}
    };
    
    std::unique_ptr<TimestampedQueue> logQueue;
    std::ofstream logFile;
    std::mutex fileMutex;
    std::atomic<bool> running;
    std::thread writerThread;
    std::string currentFile;
    
    DebugLogger();
    ~DebugLogger();
    
    void writerLoop();
    std::string getTimestamp() const;
    
public:
    static DebugLogger& getInstance();
    
    DebugLogger(const DebugLogger&) = delete;
    DebugLogger& operator=(const DebugLogger&) = delete;
    
    void initialize(const std::string& filename = "debug.log");
    
    void log(const std::string& message, 
            const std::string& function = "",
            const std::string& filename = "",
            int line = 0);
    
    void flush();
    void close();
    
    #ifdef DEBUG_ENABLED
    #define DEBUG_LOG(msg) DebugLogger::getInstance().log(msg, __FUNCTION__, __FILE__, __LINE__)
    #define DEBUG_LOG_FMT(...) do { \
        char buffer[1024]; \
        snprintf(buffer, sizeof(buffer), __VA_ARGS__); \
        DebugLogger::getInstance().log(buffer, __FUNCTION__, __FILE__, __LINE__); \
    } while(0)
    #else
    #define DEBUG_LOG(msg) ((void)0)
    #define DEBUG_LOG_FMT(...) ((void)0)
    #endif
};

#endif