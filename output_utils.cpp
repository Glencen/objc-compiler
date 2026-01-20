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

DebugLogger::DebugLogger() : isInitialized(false), isEnabledFlag(true), logCounter(0), bufferLimit(100), autoFlush(true) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
}

DebugLogger::~DebugLogger() {
    close();
}

DebugLogger& DebugLogger::getInstance() {
    static DebugLogger instance;
    return instance;
}

std::string DebugLogger::getCurrentTimestamp() const {
    auto now = std::chrono::system_clock::now();
    auto time = std::chrono::system_clock::to_time_t(now);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()
    ) % 1000;
    
    std::tm tm_info;
    #ifdef _WIN32
        localtime_s(&tm_info, &time);
    #else
        localtime_r(&time, &tm_info);
    #endif
    
    char buffer[24];
    strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", &tm_info);
    
    std::stringstream ss;
    ss << buffer << '.' << std::setfill('0') << std::setw(3) << ms.count();
    return ss.str();
}

std::string DebugLogger::getShortFilename(const std::string& fullPath) const {
    if (fullPath.empty()) {
        return "unknown";
    }
    
    try {
        size_t pos = fullPath.find_last_of("/\\");
        if (pos != std::string::npos) {
            return fullPath.substr(pos + 1);
        }
        return fullPath;
    } catch (...) {
        return "error";
    }
}

void DebugLogger::flushBufferToFile() {
    if (!logFile.is_open() || logBuffer.empty()) {
        return;
    }
    
    try {
        for (const auto& entry : logBuffer) {
            std::string shortFile = getShortFilename(entry.filename);
            
            logFile << "[" << getCurrentTimestamp() << "] "
                   << "[" << shortFile << ":" << entry.line << "]";
            
            if (!entry.function.empty()) {
                logFile << "[" << entry.function << "]";
            }
            
            logFile << " " << entry.message << std::endl;
        }
        
        logFile.flush();
        logBuffer.clear();
        
    } catch (const std::exception& e) {
        std::cerr << "DEBUG LOGGER ERROR (flush): " << e.what() << std::endl;
        logBuffer.clear();
    }
}

void DebugLogger::initialize(const std::string& filename, 
                            size_t bufferSize, 
                            bool autoFlushEnabled) {
    std::lock_guard<std::mutex> lock(logMutex);
    
    if (isInitialized) {
        close();
    }
    
    try {
        logFile.open(filename, std::ios::app);
        
        if (!logFile.is_open()) {
            std::cerr << "Error: Could not open debug log file: " << filename << std::endl;
            isInitialized = false;
            isEnabledFlag = false;
            return;
        }
        
        bufferLimit = (bufferSize > 0) ? bufferSize : 100;
        autoFlush = autoFlushEnabled;
        isInitialized = true;
        isEnabledFlag = true;
        logCounter = 0;
        logBuffer.clear();
        
        logFile << "\n";
        logFile << "=== Debug session started at " << getCurrentTimestamp() << " ===" << std::endl;
        logFile << "=== Log file: " << filename << " ===" << std::endl;
        logFile << "=== Buffer size: " << bufferLimit << ", Auto flush: " 
               << (autoFlush ? "enabled" : "disabled") << " ===" << std::endl;
        logFile.flush();
        
    } catch (const std::exception& e) {
        std::cerr << "Error initializing DebugLogger: " << e.what() << std::endl;
        isInitialized = false;
        isEnabledFlag = false;
        if (logFile.is_open()) {
            logFile.close();
        }
    }
}

void DebugLogger::log(const std::string& message, const std::string& function, const std::string& filename, int line) {
    if (!isEnabledFlag || !isInitialized) {
        return;
    }
    
    std::lock_guard<std::mutex> lock(logMutex);
    
    if (!isEnabledFlag || !logFile.is_open()) {
        return;
    }
    
    try {
        size_t currentCount = ++logCounter;
        
        LogEntry entry(message, function, filename, line);
        logBuffer.push_back(entry);
        
        if (autoFlush && logBuffer.size() >= bufferLimit) {
            flushBufferToFile();
        }
        
        if (message.find("ERROR") != std::string::npos || 
            message.find("FATAL") != std::string::npos ||
            message.find("EXCEPTION") != std::string::npos) {
            
            std::string shortFile = getShortFilename(filename);
            logFile << "[" << getCurrentTimestamp() << "] "
                   << "[" << shortFile << ":" << line << "]";
            
            if (!function.empty()) {
                logFile << "[" << function << "]";
            }
            
            logFile << " [IMMEDIATE] " << message << std::endl;
            logFile.flush();
        }
        
    } catch (const std::exception& e) {
        std::cerr << "DEBUG LOGGER ERROR (log): " << e.what() << std::endl;
    }
}

void DebugLogger::logInfo(const std::string& message, 
                         const std::string& function,
                         const std::string& filename,
                         int line) {
    log("[INFO] " + message, function, filename, line);
}

void DebugLogger::logError(const std::string& message, 
                          const std::string& function,
                          const std::string& filename,
                          int line) {
    log("[ERROR] " + message, function, filename, line);
}

void DebugLogger::logWarning(const std::string& message, 
                            const std::string& function,
                            const std::string& filename,
                            int line) {
    log("[WARNING] " + message, function, filename, line);
}

void DebugLogger::flush() {
    std::lock_guard<std::mutex> lock(logMutex);
    flushBufferToFile();
}

void DebugLogger::clear() {
    std::lock_guard<std::mutex> lock(logMutex);
    logBuffer.clear();
}

void DebugLogger::close() {
    std::lock_guard<std::mutex> lock(logMutex);
    
    if (isInitialized && logFile.is_open()) {
        try {
            if (!logBuffer.empty()) {
                flushBufferToFile();
            }
            
            logFile << "=== Debug session ended at " << getCurrentTimestamp() << " ===" << std::endl;
            logFile << "=== Total logs written: " << logCounter.load() << " ===" << std::endl;
            logFile << "\n";
            logFile.flush();
            logFile.close();
            
        } catch (const std::exception& e) {
            std::cerr << "Error closing DebugLogger: " << e.what() << std::endl;
        }
    }
    
    isInitialized = false;
    isEnabledFlag = false;
    logBuffer.clear();
}

void DebugLogger::enable(bool state) {
    std::lock_guard<std::mutex> lock(logMutex);
    isEnabledFlag = state && isInitialized && logFile.is_open();
}

void DebugLogger::disable() {
    enable(false);
}

void DebugLogger::setBufferLimit(size_t limit) {
    std::lock_guard<std::mutex> lock(logMutex);
    bufferLimit = (limit > 0) ? limit : 1;
    
    if (autoFlush && logBuffer.size() >= bufferLimit) {
        flushBufferToFile();
    }
}

void DebugLogger::setAutoFlush(bool enabled) {
    std::lock_guard<std::mutex> lock(logMutex);
    autoFlush = enabled;
    
    if (enabled && !logBuffer.empty()) {
        flushBufferToFile();
    }
}

bool DebugLogger::isEnabled() const {
    return isEnabledFlag.load();
}

bool DebugLogger::isInitializedState() const {
    return isInitialized.load();
}

bool DebugLogger::isFileOpen() const {
    return logFile.is_open();
}

size_t DebugLogger::getBufferSize() const {
    return logBuffer.size();
}

size_t DebugLogger::getTotalLogsCount() const {
    return logCounter.load();
}

std::string DebugLogger::getLogFilePath() const {
    return "debug.log";
}

std::string DebugLogger::safeString(const char* str) {
    return (str != nullptr) ? std::string(str) : std::string("(null)");
}