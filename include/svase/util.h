// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    Header for utility types and inline functions.

#pragma once

#include "fmt/format.h"
#include "slang/ast/symbols/BlockSymbols.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include "slang/ast/symbols/ParameterSymbols.h"
#include "slang/ast/types/Type.h"
#include "slang/ast/types/TypePrinter.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/util/BumpAllocator.h"
#include <cassert>
#include <regex>
#include <unordered_map>

namespace svase {

using namespace slang;
using namespace slang::ast;
using namespace slang::syntax;

/// Generates a string uniquely identifying the external parameterization of an
/// instance.
static inline std::string genParamString(const InstanceSymbol *const sym) {
  std::string hashString;
  auto typePrinter = TypePrinter();
  typePrinter.options.skipScopedTypeNames = true;
  typePrinter.options.fullEnumType = true;
  // Find all non-local Type and non-type parameters and uniquely stringify them
  // for hashing
  for (auto &param : sym->body.membersOfType<TypeParameterSymbol>())
    if (!param.isLocalParam()) {
      typePrinter.append(
          param.getTypeAlias().getDeclaredType()->getType().getCanonicalType());
      hashString += fmt::format("{}:{},", param.name, typePrinter.toString());
      typePrinter.clear();
    }
  for (auto &param : sym->body.membersOfType<ParameterSymbol>())
    if (!param.isLocalParam())
      hashString +=
          fmt::format("{}:{},", param.name,
                      param.getValue().toString(SVInt::MAX_BITS, true));
  return hashString;
}

/// Generates a reproducible (non-collision-free) hash representing only module
/// parameterization.
static inline size_t genParamHash(const InstanceSymbol *const inst) {
  return std::hash<std::string>()(genParamString(inst));
}

/// Check whether the module parameterization of two instances is exactly
/// identical.
static inline bool areParamEqual(const InstanceSymbol *const a,
                                 const InstanceSymbol *const b) {
  return genParamString(a) == genParamString(b);
}

/// Get the member of a scope by name if it exists or null.
static inline const Symbol *getScopeMember(const Scope &scope,
                                           std::string_view name) {
  auto find = scope.getNameMap().find(name);
  if (find == scope.getNameMap().end())
    return nullptr;
  else
    return find->second;
}

/// Get a fixed-sized array on a heap; useful whenever `SmallVector`s become too
/// large for the stack.
template <typename T>
static inline std::span<T> allocArray(const size_t size, BumpAllocator &alloc) {
  auto base = (T *)(void *)alloc.allocate(size * sizeof(T), sizeof(T));
  return std::span<T>(base, size);
}

/// Get a comparable index (without collisions) for unique source locations
/// across buffers. We avoid collisions by reserving half a `size_t` for the
/// buffer ID and offset, respectively.
static inline size_t getSourceLocIdx(const SourceLocation &loc) {
  constexpr size_t size_half_bits = sizeof(size_t) * 4;
  constexpr size_t size_hash_sup = size_t(1) << size_half_bits;
  size_t bufId = loc.buffer().getId();
  size_t bufOffs = loc.offset();
  assert(bufId < size_hash_sup);
  assert(bufOffs < size_hash_sup);
  return (bufId << size_half_bits) | bufOffs;
}

/// Get an index uniquely identifying byte offset of a syntax node.
static inline size_t getSynSourceLocIdx(const SyntaxNode &syn) {
  return getSourceLocIdx(syn.sourceRange().start());
}

/// Get an index uniquely identifying the  byte offset of the source syntax
/// associated with a symbol. May be used to identify the symbol iff original
/// syntax locations are preserved.
static inline size_t getSymSourceLocIdx(const Symbol &sym) {
  return getSourceLocIdx(sym.location);
}

/// Conditionally Resolve symbol of a specific type from its syntax and scope or
/// return null.
template <typename TSym>
static inline const TSym *synToSym(const SyntaxNode &syn, const Scope &scope) {
  for (auto &memSym : scope.membersOfType<TSym>()) {
    size_t memIdx = getSymSourceLocIdx(memSym);
    if constexpr (std::is_same_v<TSym, GenerateBlockSymbol> ||
                  std::is_same_v<TSym, GenerateBlockArraySymbol>) {
      // We account here for a difference that appears
      // The SyntaxNode start with "<name>"
      // The Symbol location is at the "begin : <name>"" so we need to skip the
      // "begin :" part. We do that by converting it to a SyntaxNode
      auto memSyn = memSym.getSyntax();
      // can be called for a loopGenerate and for that we do not want to move
      // the matching
      if (!memSyn) {
        printf("Warning: Symbol %s has no syntax.\n", memSym.name.data());
        continue;
      }

      if (memSyn->kind == syn.kind) {
        memIdx = getSynSourceLocIdx(*memSyn);
      } else {
        printf(
            "This type goes through %d\n",
            static_cast<std::underlying_type<SyntaxKind>::type>(memSyn->kind));
        printf("Comparision of %s and %s\n", memSyn->toString().c_str(),
               syn.toString().c_str());
        printf("Values %zu and %zu\n", memIdx, getSynSourceLocIdx(syn));

        if (memIdx != getSynSourceLocIdx(syn) &&
            memSyn->kind == SyntaxKind::LoopGenerate) {
          std::regex reBegin(R"(begin\s*:\s*)");
          std::smatch match;
          std::string symbolStr = syn.toString();
          auto start = symbolStr.cbegin();
          size_t beginLength = 0;
          while (std::regex_search(start, symbolStr.cend(), match, reBegin)) {
            if (beginLength == 0) {
              beginLength = match.length();
              break;
            }
          }
          if (memIdx - beginLength == getSynSourceLocIdx(syn)) {
            return &memSym.template as<TSym>();
          }
        }
      }
    }

    if (memIdx == getSynSourceLocIdx(syn))
      return &memSym.template as<TSym>();
  }

  // None of the source locations matched, so the scope does not contain this
  // syntax
  return nullptr;
}

/// If the passed member syntax is a `GenerateBlockSyntax` matching a
/// `GenerateBlockSymbol` in the passed scope, return it. We do this even if the
/// block is not instantiated.
static inline const GenerateBlockSymbol *
matchInstGenBlock(SyntaxNode *condBlkSyn, const Scope &scope) {
  if (condBlkSyn)
    return synToSym<GenerateBlockSymbol>(*condBlkSyn, scope);
  else
    return nullptr;
}

} // namespace svase
