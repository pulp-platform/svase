// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    Main command line driver for SVase.

#include "cxxopts.hpp"
#include <cstdio>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include "svase/design.h"
#include "svase/diag.h"
#include "svase/preproc.h"
#include "svase/rewrite.h"

#include "slang/ast/symbols/CompilationUnitSymbols.h"
#include "slang/driver/Driver.h"
#include "slang/util/TimeTrace.h"

namespace svase {

cxxopts::Options genCmdOpts() {
  cxxopts::Options ret("svase",
                       "SVase: a source-to-source SystemVerilog elaborator");
  // TODO: make these mandatory somehow and make sure they show in help
  // TODO: get a better parser library...
  ret.parse_positional({"top", "out", "files"});
  ret.add_options()
      // Mandatory positionals
      ("top", "Top module of design to elaborate",
       cxxopts::value<std::string>())(
          "out", "The output file (- for stdout) or library",
          cxxopts::value<std::string>())(
          "files", "The source files to process",
          cxxopts::value<std::vector<std::string>>())
      // Optional arguments
      ("l,lib", "Output library of individual modules",
       cxxopts::value<bool>()->implicit_value("false")) // TODO
      ("z,compress", "Compress output file or library using Gzip",
       cxxopts::value<bool>()->implicit_value("false")) // TODO
      ("s,slang-argfile", "Argument file overriding Slang default options",
       cxxopts::value<std::string>())(
          "S,slang-args", "Argument string overriding Slang default options",
          cxxopts::value<std::string>())(
          "r,timetrace", "Time each stage and write chrome event trace to JSON",
          cxxopts::value<std::string>()) // TODO
      ("V,verbosity",
       "Verbosity of stderr diagnostics: 3(errors), 2(warnings), 1(notes)",
       cxxopts::value<uint>()->default_value("2"))
      // Informational arguments (cause early exit)
      ("h,help", "Print usage")("v,version", "Print version information");

  return ret;
}

template <typename Stream, typename String>
void writeToFile(Stream &os, std::string_view fileName, String contents) {
  os.write(contents.data(), contents.size());
  os.flush();
  if (!os)
    throw std::runtime_error(
        fmt::format("Unable to write AST to '{}'", fileName));
}

void writeToFile(std::string_view fileName, std::string_view contents) {
  if (fileName == "-") {
    writeToFile(std::cout, "stdout", contents);
  } else {
    std::ofstream file{std::string(fileName)};
    writeToFile(file, fileName, contents);
  }
}

// TODO: find a nicer way to wrap the timing/exception scopes
int driverMain(int argc, char **argv) {
  Diag diag;
  DiagSev verbosity;
  bool ok;

  // Parse and handle global args
  auto cmdOpts = genCmdOpts();
  cxxopts::ParseResult cmdOptsRes;
  try {
    cmdOptsRes = cmdOpts.parse(argc, argv);
  } catch (const cxxopts::exceptions::parsing &e) {
    diag.log(DiagSev::Fatal, e.what());
    return 1;
  }
  if (cmdOptsRes.count("help")) {
    const auto &helpOptions = cmdOpts.help();
    fmt::print("{}\n", helpOptions);
    return 0;
  }
  if (cmdOptsRes.count("timetrace"))
    TimeTrace::initialize();
  verbosity = (DiagSev)cmdOptsRes["verbosity"].as<uint>();
  diag.setVerbosity(verbosity);

  // Parse and handle Slang args using its driver
  slang::driver::Driver slangDriver;
  slangDriver.diagEngine.setIgnoreAllNotes(verbosity > DiagSev::Note);
  slangDriver.diagEngine.setIgnoreAllWarnings(verbosity > DiagSev::Warning);
  slangDriver.addStandardArgs();
  ok = true;
  auto builtinFlags =
      "--ignore-unknown-modules --allow-use-before-declare --single-unit -Wrange-width-oob -Wrange-oob"sv;
  if (cmdOptsRes.count("fslang"))
    ok &= slangDriver.processCommandFile(
        cmdOptsRes["slang-argfile"].as<std::string>(), true);
  std::string slangArgs = cmdOptsRes.count("slang-args")
                              ? cmdOptsRes["slang-args"].as<std::string>()
                              : "";
  auto slangCmd = fmt::format(
      "{} {} {} --top {} {}", argv[0], slangArgs, builtinFlags,
      cmdOptsRes["top"].as<std::string>(),
      fmt::join(cmdOptsRes["files"].as<std::vector<std::string>>(), " "));
  ok &= slangDriver.parseCommandLine(slangCmd);
  ok &= slangDriver.processOptions();
  diag.registerEngine(&slangDriver.sourceManager);
  if (!ok)
    return 2;

  // Preprocess buffers
  TypedBumpAllocator<std::string> preBuffers;
  try {
    TimeTraceScope timeScope("preproc"sv, ""sv);
    diag.logStage("PREPROCESS");
    Preprocessor preproc(slangDriver.buffers, preBuffers, diag);
    preproc.preprocess();
  } catch (const std::exception e) {
    diag.log(DiagSev::Fatal, e.what());
    ok = false;
  }
  if (!ok)
    return 3;

  // Parse using Slang
  try {
    diag.logStage("PARSE");
    TimeTraceScope timeScope("parse"sv, ""sv);
    ok = slangDriver.parseAllSources();
  } catch (const std::exception e) {
    diag.log(DiagSev::Fatal, e.what());
    ok = false;
  }
  if (!ok)
    return 4;

  // Compile using Slang
  std::unique_ptr<slang::ast::Compilation> compilation;
  try {
    diag.logStage("COMPILE");
    TimeTraceScope timeScope("compile"sv, ""sv);
    compilation = slangDriver.createCompilation();
    ok = slangDriver.reportCompilation(*compilation, true);

  } catch (const std::exception e) {
    diag.log(DiagSev::Fatal, e.what());
    ok = false;
  }
  if (!ok)
    return 5;

  // Rewrite sources using our passes
  // TODO: put rewriter-referenced resources in a shared class for convenience
  std::unique_ptr<Design> design;
  std::shared_ptr<SyntaxTree> synTree;
  BumpAllocator alloc;
  TypedBumpAllocator<std::string> strAlloc;
  // try {
  diag.logStage("REWRITE");
  // TimeTraceScope timeScope("rewrite"sv, ""sv);
  design =
      std::make_unique<Design>(*compilation->getRoot().topInstances.begin());
  synTree = compilation->getSyntaxTrees().back();
  // Run our passes (TODO: somehow handle boolean return?)
  synTree =
      UniqueModuleRewriter(*design, alloc, strAlloc, diag).transform(synTree);
  synTree =
      ParameterRewriter(*design, alloc, strAlloc, diag).transform(synTree);
  // synTree = GenerateRewriter(*design, alloc, strAlloc,
  // diag).transform(synTree);
  // synTree = DefaultAssignmentRewriter(*design, alloc, strAlloc, diag)
  //               .transform(synTree);
  // } catch (const std::exception e) {diag.log(DiagSev::Fatal, e.what()); ok =
  // false;}
  // if (!ok) return 6;

  // TODO: Postprocess syntax tree into writable buffers, one per root unit
  // member
  std::vector<std::pair<std::string, std::string>> postBuffers;
  try {
    diag.logStage("POSTPROCESS");
    TimeTraceScope timeScope("postproc"sv, ""sv);
    // TODO: proper lib handling
    postBuffers.emplace_back(cmdOptsRes["top"].as<std::string>(),
                             synTree->root().toString());
  } catch (const std::exception e) {
    diag.log(DiagSev::Fatal, e.what());
    ok = false;
  }
  if (!ok)
    return 7;

  // try {
  diag.logStage("WRITEOUT");
  TimeTraceScope timeScope("writeout"sv, ""sv);
  // TODO: library handling and such; guard that out exists, is legal
  // beforehand!
  writeToFile(cmdOptsRes["out"].as<std::string>(), postBuffers.back().second);
  //} catch (const std::exception e) {diag.log(DiagSev::Fatal, e.what()); ok =
  // false;} if (!ok) return 8;

  diag.logStage("DONE");

  // TODO: report on top, unique modules, and blackboxes here.

  // TODO: Timescope implementation!

  return 0;
}

} // namespace svase

int main(int argc, char **argv) { return svase::driverMain(argc, argv); }
