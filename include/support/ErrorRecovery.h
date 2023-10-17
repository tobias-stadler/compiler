#pragma once

#include <stdexcept>
#include <string_view>

class FrameLogger {
  class logger_error : public std::runtime_error {};

  class LogFrame {
  public:
    LogFrame(FrameLogger &logger, std::string_view what) : logger(logger) {}
    LogFrame(const LogFrame &) = delete;
    LogFrame &operator=(const LogFrame &) = delete;
    LogFrame(LogFrame &&) noexcept = delete;
    LogFrame &operator=(LogFrame &&) noexcept = delete;
    ~LogFrame() {}

  private:
    FrameLogger &logger;
  };
};
