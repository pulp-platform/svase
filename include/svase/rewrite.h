// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    A collection of syntax rewriters leveraging design information.

#pragma once

#include "svase/design.h"
#include "svase/diag.h"

#include "slang/ast/symbols/BlockSymbols.h"
#include "slang/ast/types/TypePrinter.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/syntax/SyntaxVisitor.h"

namespace svase {

using namespace slang;
using namespace slang::ast;
using namespace slang::syntax;
using namespace slang::parsing;

/// A `SyntaxRewriter` variant leveraging a `DesignCollection` to rewrite code
/// using elaboration info.
template <typename Subclass>
class DesignRewriter : public SyntaxRewriter<Subclass> {
protected:
  Design &design;
  BumpAllocator &alloc;
  TypedBumpAllocator<std::string> &strAlloc;
  Diag &diag;
  size_t uniqCounter; // Can be used to uniquify names during rewrote

public:
  DesignRewriter(Design &coll, BumpAllocator &alloc,
                 TypedBumpAllocator<std::string> &strAlloc, Diag &diag)
      : design(coll), alloc(alloc), strAlloc(strAlloc), diag(diag),
        uniqCounter(0) {}
};

/// Uniquify modules by creating one instantiation per unique parameter set and
/// using its unique name. Original and unused module declarations are
/// discarded.
class UniqueModuleRewriter : public DesignRewriter<UniqueModuleRewriter> {
public:
  using DesignRewriter::DesignRewriter;

  /// Duplicate module declarations to uniquify them, removing all other
  /// declarations
  void handle(const ModuleDeclarationSyntax &modSyn);
};

/// Replace parameters and type parameters, both in port maps and module bodies,
/// with their elaborated values. This requires uniquified modules as generated
/// by `UniqueModuleWriter`.
class ParameterRewriter : public DesignRewriter<ParameterRewriter> {
private:
  /// Make a equals token for a parameter initializer
  Token makeEquals();

  /// Get the unique module containing a given parameter declaration syntax or
  /// null.
  DesignUniqueModule *
  getUniqueModule(const ParameterDeclarationBaseSyntax &pd) const;

  // Obtain Scope containing the Symbols corresponding to the given SyntaxNode
  const Scope *getContainingScope(const SyntaxNode &synNode) const;
  /// Get the symbol for a parameter declaration syntax or leave it unchanged
  /// and return null.

  template <typename T>
  const Symbol *getParamSymOrBail(const T *pd,
                                  std::vector<std::string> &declStrs,
                                  const Scope *scope) const;

  /// Replace a parameter declaration syntax with a new syntax in string form if
  /// it can be parsed as such.
  template <typename T>
  void replaceParamDeclOrBail(std::string declStr, const T &pd);

  /// Walk a type syntax and mangle enumerated type value labels (inaccessible
  /// anyways) to prevent name collisions.
  DataTypeSyntax *mangleEnumTypes(DataTypeSyntax &typeSyn);

public:
  using DesignRewriter::DesignRewriter;

  void handle(const TypeParameterDeclarationSyntax &pd);

  void handle(const ParameterDeclarationSyntax &pd);
};

// Unroll generate constructs inside uniquified modules as needed by their
// parameter set. This does *not* create new instance symbols, keeping them
// intentionally coupled for later replacement.!
class GenerateRewriter : public DesignRewriter<GenerateRewriter> {
private:
  /// Make an empty member as a substitute for uninstantiated blocks
  EmptyMemberSyntax *makeEmptyMember();

  /// Make a colon token for labels
  Token makeColon();

  /// Make a trivial ifGenerate to wrap a `GenerateBlock` in, since standalone
  /// blocks are not LRM-legal.
  IfGenerateSyntax *makeDummyIfGen(std::string_view label = ""sv);

  /// Wrap an existing block in a trivial ifGenerate
  IfGenerateSyntax *wrapBlockInIfGen(GenerateBlockSyntax &blockSyn,
                                     MemberSyntax *genvar = nullptr);

  /// Wrap multiple members in a trivial ifGenerate with a block to make it a
  /// standalone member.
  IfGenerateSyntax *wrapMemberListInIfGen(SyntaxList<MemberSyntax> &members,
                                          std::string_view label = ""sv);

  /// Wrap one member in a trivial ifGenerate with a block to make it a
  /// standalone member.
  IfGenerateSyntax *wrapMemberInIfGen(MemberSyntax &membSyn,
                                      std::string_view label = ""sv,
                                      MemberSyntax *genvar = nullptr);

  /// Wrap a Pseudomember in a trivial ifGenerate, possibly with a block, to
  /// make it a standalone member.
  MemberSyntax *wrapInIfGen(MemberSyntax &membSyn,
                            MemberSyntax *genvar = nullptr);

  /// Get a `beginName` label from whatever may be the block's canonical name
  /// iff it exists or null.
  NamedBlockClauseSyntax *getBeginName(const GenerateBlockSyntax &blockSyn);

  /// Generate a block's `beginName` syntax from a string; is null iff `name` is
  /// empty.
  NamedBlockClauseSyntax *makeBlockBeginName(std::string_view name);

  /// Unroll a generic `MemberSyntax`, leaving those that are not generate
  /// constructs or instances as they are.
  MemberSyntax *unrollGenSyntax(MemberSyntax &membSyn, const Scope &scope,
                                NamedBlockClauseSyntax *beginName = nullptr,
                                const GenerateBlockSymbol *blockSym = nullptr,
                                const Scope *globalScope = nullptr);

  /// Unroll a `GenerateBlockSyntax`, looking up its symbol in the compilation.
  MemberSyntax *
  unrollGenBlockSyntax(const GenerateBlockSyntax &blockSyn,
                       const GenerateBlockSymbol &blockSym,
                       NamedBlockClauseSyntax *beginName = nullptr);

  /// Unroll an `IfGenerateSyntax`, looking up its symbol in the compilation.
  MemberSyntax *unrollGenSyntax(const IfGenerateSyntax &ifSyn,
                                const Scope &scope,
                                NamedBlockClauseSyntax *beginName = nullptr);

  /// Unroll a `CaseGenerateSyntax`, looking up its symbol in the compilation.
  MemberSyntax *unrollGenSyntax(const CaseGenerateSyntax &caseSyn,
                                const Scope &scope,
                                NamedBlockClauseSyntax *beginName = nullptr);

  /// Unroll a `LoopGenerateSyntax`, looking up its symbol in the compilation.
  MemberSyntax *unrollGenSyntax(const LoopGenerateSyntax &loopSyn,
                                const Scope &scope);

  /// Unroll a `GenerateRegionSyntax`; these have no real effect on the design,
  /// but must be walked.
  MemberSyntax *unrollGenSyntax(const GenerateRegionSyntax &regSyn,
                                const Scope &scope,
                                NamedBlockClauseSyntax *beginName = nullptr);

  /// "Unroll" an instance by giving it a unique type; best done here as we
  /// already resolved the Symbol hierarchy.
  /// TODO: what about instance arrays? are we even properly tracking those in
  /// `Design`?
  MemberSyntax *unrollGenSyntax(const HierarchyInstantiationSyntax &instSyn,
                                const Scope &scope,
                                const Scope *globalScope = nullptr);

  /// Handle a generic generate construct iff it is inside a module.
  template <typename T> void handleGenerate(const T &syn);

public:
  using DesignRewriter::DesignRewriter;

  void handle(const IfGenerateSyntax &syn);
  void handle(const LoopGenerateSyntax &syn);
  void handle(const CaseGenerateSyntax &syn);
  void handle(const GenerateRegionSyntax &syn);

  void handle(const HierarchyInstantiationSyntax &syn);
};

class TypedefDeclarationRewriter
    : public DesignRewriter<TypedefDeclarationRewriter> {
private:
  const Symbol *getTypeNameSymOrBail(const TypedefDeclarationSyntax *pd,
                                     DesignUniqueModule *uniqMod) const;

  template <typename T>
  void replaceTypeDeclOrBail(std::string declStr, const T &pd);

  DesignUniqueModule *getUniqueModule(const SyntaxNode &pd) const;

public:
  using DesignRewriter::DesignRewriter;

  // void handle(const ScopedNameSyntax &pd);
  void handle(const TypedefDeclarationSyntax &pd);
};

class AssignmentRewriter : public DesignRewriter<AssignmentRewriter> {
private:
  const ContinuousAssignSymbol *
  getLHSNameSymOrBail(const ContinuousAssignSyntax *pd,
                      DesignUniqueModule *uniqMod);
  // template <typename T>
  // void replaceAssignmentOrBail(std::string declStr, const T &pd);

  DesignUniqueModule *getUniqueModule(const SyntaxNode &pd) const;

public:
  using DesignRewriter::DesignRewriter;

  void handle(const ContinuousAssignSyntax &pd);
};

} // namespace svase
