#include <chrono>
#include "output_utils.h"

//--------------------------------------------------------------TokenOutput--------------------------------------------------------------

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

//--------------------------------------------------------------DebugLogger--------------------------------------------------------------

DebugLogger::DebugLogger() 
    : logQueue(std::make_unique<TimestampedQueue>()), 
      running(false) {}

DebugLogger::~DebugLogger() {
    close();
}

DebugLogger& DebugLogger::getInstance() {
    static DebugLogger instance;
    return instance;
}

void DebugLogger::initialize(const std::string& filename) {
    std::lock_guard<std::mutex> lock(fileMutex);
    
    if (running) {
        close();
    }
    
    currentFile = filename;
    logFile.open(filename, std::ios::app);
    if (!logFile.is_open()) {
        std::cerr << "Error: Could not open debug log file: " << filename << std::endl;
        return;
    }
    
    running = true;
    writerThread = std::thread(&DebugLogger::writerLoop, this);
    
    logFile << "=== Debug session started at " << getTimestamp() << " ===" << std::endl;
    logFile.flush();
}

std::string DebugLogger::getTimestamp() const {
    auto now = std::chrono::system_clock::now();
    auto time = std::chrono::system_clock::to_time_t(now);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()
    ) % 1000;
    
    std::stringstream ss;
    ss << std::put_time(std::localtime(&time), "%Y-%m-%d %H:%M:%S");
    ss << '.' << std::setfill('0') << std::setw(3) << ms.count();
    return ss.str();
}

void DebugLogger::log(const std::string& message, const std::string& function, const std::string& filename, int line) {
    if (!running) return;
    
    LogEntry entry(message, function, filename, line);
    
    {
        std::lock_guard<std::mutex> lock(logQueue->mutex);
        logQueue->queue.push(std::move(entry));
    }

    logQueue->cv.notify_one();
}

void DebugLogger::writerLoop() {
    while (running) {
        std::unique_lock<std::mutex> lock(logQueue->mutex);
        
        logQueue->cv.wait(lock, [this]() {
            return !logQueue->queue.empty() || !running;
        });
        
        std::queue<LogEntry> localQueue;
        while (!logQueue->queue.empty()) {
            localQueue.push(std::move(const_cast<LogEntry&>(logQueue->queue.top())));
            logQueue->queue.pop();
        }
        
        lock.unlock();
        
        while (!localQueue.empty()) {
            auto& entry = localQueue.front();
            
            std::lock_guard<std::mutex> fileLock(fileMutex);
            if (logFile.is_open()) {
                std::string shortFilename = entry.filename;
                size_t lastSlash = shortFilename.find_last_of("/\\");
                if (lastSlash != std::string::npos) {
                    shortFilename = shortFilename.substr(lastSlash + 1);
                }
                
                logFile << "[" << getTimestamp() << "] "
                        << "[" << shortFilename << ":" << entry.line << "] "
                        << "[" << entry.function << "] "
                        << entry.message << std::endl;
            }
            
            localQueue.pop();
        }
        
        if (logFile.is_open()) {
            logFile.flush();
        }
    }
}

void DebugLogger::flush() {
    if (!running) return;
    
    logQueue->cv.notify_one();

    std::unique_lock<std::mutex> lock(logQueue->mutex);
    logQueue->cv.wait(lock, [this]() {
        return logQueue->queue.empty();
    });
}

void DebugLogger::close() {
    if (running) {
        running = false;
        logQueue->cv.notify_one();
        
        if (writerThread.joinable()) {
            writerThread.join();
        }
        
        std::lock_guard<std::mutex> lock(fileMutex);
        if (logFile.is_open()) {
            logFile << "=== Debug session ended at " << getTimestamp() << " ===" << std::endl;
            logFile.close();
        }
    }
}