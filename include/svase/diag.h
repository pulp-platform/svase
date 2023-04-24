// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    Utilities for diagnostic output and logging

#pragma once

#include "svase/util.h"

#include "slang/ast/Symbol.h"
#include "slang/diagnostics/TextDiagnosticClient.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/util/OS.h"
#include <ctime>
#include <unordered_map>
#include <unordered_set>

namespace svase {

using namespace slang;
using namespace slang::ast;
using namespace slang::syntax;

using DiagSev = DiagnosticSeverity;

typedef std::tuple<size_t, DiagSev, bool, const std::string> UniqMsg;

/// An extended `TextDiagnosticClient` with additional methods for simple
/// logging and reporting.
class Diag : TextDiagnosticClient {
private:
  std::unique_ptr<const DiagnosticEngine> eng;
  std::clock_t startTime;
  DiagSev verbosity;
  // We index by byte location first, then differentiate strings, level 2
  // collisions are unlikely.
  std::unordered_map<size_t, std::unordered_set<std::string>> uniqMsgs;

  /// Reuse meta diagnostics code 0 for our purposes.
  static inline DiagCode getDiagCode() {
    return DiagCode(DiagSubsystem::Meta, 0);
  }

  /// Insert a unique message into our map and return whether it was present.
  bool testOrInsertUniqMsg(DiagSev sev, std::string_view msg,
                           size_t locHash = -1, bool showCode = false);

  /// Emit a log message with a location.
  void logLocation(DiagSev sev, std::string_view msg, Diagnostic od,
                   bool keepUnique, bool showCode);

public:
  Diag()
      : TextDiagnosticClient(), eng(nullptr), startTime(std::clock()),
        verbosity(DiagSev::Ignored) {
    showColors(OS::tryEnableColors());
  }

  /// Set the verbosity; any severity smaller will be ignored
  void setVerbosity(DiagSev sev);

  /// Get the time elapsed since the diagnostic engine was constructed; may be
  /// used for timekeeping.
  clock_t timeElapsed();

  /// Get a string representing the elapsed time.
  const std::string formatTimeElapsed();

  /// Register a diagnostic engine by providing a source manager needed to
  /// report source locations.
  void registerEngine(const SourceManager *sm);

  /// Log basic message without source location; does not need engine.
  void log(DiagSev sev, std::string_view msg, bool keepUnique = false);

  ///  Log diagnostic using a source location only.
  void log(DiagSev sev, std::string_view msg, const SourceLocation &loc,
           bool keepUnique = false, bool showCode = true);

  ///  Log diagnostic using a symbol to determine source location.
  void log(DiagSev sev, std::string_view msg, const Symbol &sym,
           bool keepUnique = false, bool showCode = true);

  ///  Log diagnostic using a syntax node to determine source location.
  void log(DiagSev sev, std::string_view msg, const SyntaxNode &syn,
           bool keepUnique = false, bool showCode = true);

  /// Log the start of a new stage in a highlighted fashion
  void logStage(std::string_view name);
};

} // namespace svase
