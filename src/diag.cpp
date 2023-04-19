// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    Utilities for diagnostic output and logging

#include "svase/diag.h"

#include "fmt/color.h"
#include "fmt/format.h"

namespace svase {

using namespace slang;
using namespace slang::ast;
using namespace slang::syntax;

bool Diag::testOrInsertUniqMsg(DiagSev sev, std::string_view msg,
                               size_t locHash, bool showCode) {
  auto insStr = fmt::format("{:0x}{:0x}|{}", showCode, uint(sev), msg);
  // Check in two stages whether message was already logged.
  if (uniqMsgs.count(locHash) && uniqMsgs[locHash].count(insStr))
    return true;
  // If it was not, insert into map and instruct caller to log message.
  uniqMsgs[locHash].insert(insStr);
  return false;
}

void Diag::logLocation(DiagSev sev, std::string_view msg, Diagnostic od,
                       bool keepUnique, bool showCode) {
  // Do not log messages below the desired
  if (sev < verbosity)
    return;
  if (keepUnique &&
      testOrInsertUniqMsg(sev, msg, getSourceLocIdx(od.location), showCode))
    return;
  if (!engine)
    throw new std::runtime_error(
        "Cannot log a source location without engine.");
  ReportedDiagnostic rd(od);
  rd.formattedMessage = msg;
  rd.severity = sev;
  rd.shouldShowIncludeStack = false;
  rd.location = od.location;
  showSourceLine(showCode);
  report(rd);
  OS::printE(getString());
  clear();
  // TODO: throw something more appropriate
  if (sev >= DiagSev::Fatal)
    ASSUME_UNREACHABLE;
}

void Diag::setVerbosity(DiagSev sev) { verbosity = sev; }

clock_t Diag::timeElapsed() { return std::clock() - startTime; }

const std::string Diag::formatTimeElapsed() {
  auto t = timeElapsed() * 1000 / CLOCKS_PER_SEC;
  auto ms = t % 1000;
  auto s = (t /= 1000) % 60;
  auto m = (t /= 60);
  return fmt::format("{:3}m {:2}.{:03}s", m, s, ms);
}

void Diag::registerEngine(const SourceManager *sm) {
  sourceManager = sm;
  eng = std::make_unique<DiagnosticEngine>(*sm);
  engine = eng.get();
}

void Diag::log(DiagSev sev, std::string_view msg, bool keepUnique) {
  if (sev < verbosity)
    return;
  if (keepUnique && testOrInsertUniqMsg(sev, msg))
    return;
  OS::printE(fg(getSeverityColor(sev)),
             fmt::format("{}: ", getSeverityString(sev)));
  if (sev != DiagSev::Note)
    OS::printE(fmt::text_style(fmt::emphasis::bold), fmt::format("{}\n", msg));
  else
    OS::printE(fmt::format("{}\n", msg));
  // TODO: throw something more appropriate
  if (sev >= DiagSev::Fatal)
    ASSUME_UNREACHABLE;
}

void Diag::log(DiagSev sev, std::string_view msg, const SourceLocation &loc,
               bool keepUnique, bool showCode) {
  Diagnostic od(getDiagCode(), loc);
  logLocation(sev, msg, od, keepUnique, showCode);
}

void Diag::log(DiagSev sev, std::string_view msg, const Symbol &sym,
               bool keepUnique, bool showCode) {
  Diagnostic od(sym, getDiagCode(), sym.location);
  logLocation(sev, msg, od, keepUnique, showCode);
}

void Diag::log(DiagSev sev, std::string_view msg, const SyntaxNode &syn,
               bool keepUnique, bool showCode) {
  Diagnostic od(getDiagCode(), syn.sourceRange().start());
  logLocation(sev, msg, od, keepUnique, showCode);
}

void Diag::logStage(std::string_view name) {
  OS::printE(fg(highlightColor),
             fmt::format("[{}] {}\n", formatTimeElapsed(), name));
}

} // namespace svase
