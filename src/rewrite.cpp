// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    A collection of syntax rewriters leveraging design information.

#include "svase/rewrite.h"
#include "svase/util.h"

#include "fmt/format.h"
#include "slang/ast/symbols/ParameterSymbols.h"
#include "slang/ast/symbols/VariableSymbols.h"
#include "slang/ast/types/TypePrinter.h"

namespace svase {

using namespace slang;
using namespace slang::ast;
using namespace slang::syntax;
using namespace slang::parsing;

void UniqueModuleRewriter::handle(const ModuleDeclarationSyntax &modSyn) {
  // Look up the module to check if it needs to be uniquified
  auto hdrSyn = modSyn.header.get();
  auto genName = hdrSyn->name.rawText();
  auto genMod = design.getGenericModule(genName);
  // This module is instantiated, hence in use
  if (genMod) {
    for (auto uniqPair : *genMod) {
      auto uniqMod = uniqPair.second;
      // Change module name to unique version
      auto name = strAlloc.emplace(uniqMod.getUniqueName());
      auto nameToken = hdrSyn->name.withRawText(alloc, *name);
      // Assemble header
      auto uniqHdrSyn =
          ModuleHeaderSyntax(SyntaxKind::ModuleHeader, hdrSyn->moduleKeyword,
                             hdrSyn->lifetime, nameToken, hdrSyn->imports,
                             hdrSyn->parameters, hdrSyn->ports, hdrSyn->semi);
      // Assemble module (deepclone to decouple)
      auto uniqModSyn =
          deepClone(ModuleDeclarationSyntax(
                        SyntaxKind::ModuleDeclaration, modSyn.attributes,
                        uniqHdrSyn, modSyn.members, modSyn.endmodule, nullptr),
                    alloc);
      // Insert after generic module
      insertAfter(modSyn, *uniqModSyn);
    }
  }
  // Remove generic module iff it is a module and not a package or interface
  if (modSyn.kind == SyntaxKind::ModuleDeclaration)
    remove(modSyn);
}

DesignUniqueModule *ParameterRewriter::getUniqueModule(
    const ParameterDeclarationBaseSyntax &pd) const {
  // Obtain module instance from either port list or root (skip if in package or
  // in some other config)
  auto modNode = pd.parent;
  if (modNode->kind == SyntaxKind::ParameterPortList)
    modNode = modNode->parent->parent;
  else if (modNode->kind == SyntaxKind::ParameterDeclarationStatement)
    modNode = modNode->parent;
  if (modNode->kind == SyntaxKind::PackageDeclaration ||
      modNode->kind == SyntaxKind::InterfaceDeclaration ||
      modNode->kind == SyntaxKind::GenerateBlock)
    return nullptr;
  else if (modNode->kind != SyntaxKind::ModuleDeclaration) {
    diag.log(DiagSev::Warning,
             fmt::format("parameter declaration with unhandled parent kind "
                         "`{}`; left unchanged",
                         toString(modNode->kind)),
             pd, true);
    return nullptr;
  }
  // Obtain corresponding unique module
  auto &modSyn = modNode->as<ModuleDeclarationSyntax>();
  return design.getUniqueModule(modSyn.header->name.rawText());
}

template <typename T>
const Symbol *
ParameterRewriter::getParamSymOrBail(const T *pd,
                                     std::vector<std::string> &declStrs,
                                     DesignUniqueModule *uniqMod) const {
  auto memberSym =
      getScopeMember(uniqMod->getInstances().begin()->symbol->body.as<Scope>(),
                     pd->name.rawText());
  if (memberSym == nullptr) {
    diag.log(DiagSev::Note,
             "parameter declaration not found in compilation; left unchanged",
             *pd, true);
    declStrs.emplace_back(pd->toString());
  }
  return memberSym;
}

template <typename T>
void ParameterRewriter::replaceParamDeclOrBail(std::string declStr,
                                               const T &pd) {
  auto &newPdSyn = parse(declStr);
  if (newPdSyn.kind != SyntaxKind::ParameterDeclarationStatement) {
    diag.log(DiagSev::Error,
             fmt::format(
                 "misparsed parameter declaration as kind `{}`; left unchanged",
                 toString(newPdSyn.kind)),
             pd, true);
    return;
  }
  auto newPdStatement =
      clone(newPdSyn.as<ParameterDeclarationStatementSyntax>(), alloc);
  if (newPdStatement) {
    auto newPd = clone(newPdStatement->parameter.get()->as<T>(), alloc);
    replace(pd, *newPd);
  }
}

DataTypeSyntax *ParameterRewriter::mangleEnumTypes(DataTypeSyntax &typeSyn) {
  switch (typeSyn.kind) {
  case SyntaxKind::EnumType: {
    auto &enumSyn = typeSyn.as<EnumTypeSyntax>();
    size_t enumIdx = ++uniqCounter;
    auto newMembArr =
        allocArray<TokenOrSyntax>(2 * enumSyn.members.size() - 1, alloc);
    size_t i = 0;
    bool firstIter = true;
    for (auto member : enumSyn.members) {
      if (!firstIter) {
        newMembArr[i++] = *alloc.emplace<TokenOrSyntax>(makeComma());
      }
      firstIter = false;
      auto newNameStr = std::string_view(*strAlloc.emplace(
          fmt::format("{}_svase{}", member->name.rawText(), enumIdx)));
      Token newName(alloc, parsing::TokenKind::Identifier,
                    member->name.trivia(), newNameStr, member->name.location());
      newMembArr[i++] = *alloc.emplace<TokenOrSyntax>(clone(
          DeclaratorSyntax(newName, member->dimensions, member->initializer),
          alloc));
    }
    auto newMembers = SeparatedSyntaxList<DeclaratorSyntax>(newMembArr);
    return clone(EnumTypeSyntax(enumSyn.keyword, enumSyn.baseType,
                                enumSyn.openBrace, newMembers,
                                enumSyn.closeBrace, enumSyn.dimensions),
                 alloc);
  }
  case SyntaxKind::UnionType:
  case SyntaxKind::StructType: {
    // Resolve Syntax
    auto &ustrSyn = typeSyn.as<StructUnionTypeSyntax>();
    auto newMembArr =
        allocArray<StructUnionMemberSyntax *>(ustrSyn.members.size(), alloc);
    size_t i = 0;
    for (auto member : ustrSyn.members) {
      auto newTypeSyn = mangleEnumTypes(*member->type);
      newMembArr[i++] =
          clone(StructUnionMemberSyntax(member->attributes,
                                        member->randomQualifier, *newTypeSyn,
                                        member->declarators, member->semi),
                alloc);
    }
    return clone(StructUnionTypeSyntax(ustrSyn.kind, ustrSyn.keyword,
                                       ustrSyn.tagged, ustrSyn.packed,
                                       ustrSyn.signing, ustrSyn.openBrace,
                                       SyntaxList(newMembArr),
                                       ustrSyn.closeBrace, ustrSyn.dimensions),
                 alloc);
  }
  case SyntaxKind::VirtualInterfaceType: {
    diag.log(DiagSev::Warning,
             "Virtual interface type elaboration is not supported yet", typeSyn,
             true);
  }
  default:;
  }
  return &typeSyn;
}

void ParameterRewriter::handle(const TypeParameterDeclarationSyntax &pd) {
  auto uniqMod = getUniqueModule(pd);
  if (!uniqMod)
    return;
  std::vector<std::string> newDeclStrs;
  auto typePrinter = TypePrinter();
  typePrinter.options.skipScopedTypeNames = true;
  typePrinter.options.fullEnumType = true;
  for (auto decl : pd.declarators) {
    // Find parameter in compilation; if not found, leave as-is
    auto memberSym =
        getParamSymOrBail<TypeAssignmentSyntax>(decl, newDeclStrs, uniqMod);
    if (!memberSym)
      continue;
    auto &paramSym = memberSym->as<TypeParameterSymbol>();
    // Generate parseable string view of our type, then parse it as a
    // declaration
    auto &type = paramSym.getDeclaredType()->getType().getCanonicalType();
    typePrinter.append(type);
    auto &declSyn = this->parse(typePrinter.toString())
                        .template as<DataDeclarationSyntax>();
    typePrinter.clear();
    // Rebuild assignment
    auto newEquals = EqualsTypeClauseSyntax(decl->assignment->equals,
                                            *mangleEnumTypes(*declSyn.type));
    auto assignSyn = TypeAssignmentSyntax(decl->name, &newEquals);
    newDeclStrs.emplace_back(assignSyn.toString());
  }
  auto declStr =
      fmt::format("{}{}{}", pd.keyword.toString(), pd.typeKeyword.toString(),
                  fmt::join(newDeclStrs, ", "));
  replaceParamDeclOrBail<TypeParameterDeclarationSyntax>(declStr, pd);
}

void ParameterRewriter::handle(const ParameterDeclarationSyntax &pd) {
  auto uniqMod = getUniqueModule(pd);
  if (!uniqMod)
    return;
  std::vector<std::string> newDeclStrs;
  for (auto decl : pd.declarators) {
    // Find parameter in compilation; if not found, leave as-is
    auto memberSym =
        getParamSymOrBail<DeclaratorSyntax>(decl, newDeclStrs, uniqMod);
    if (!memberSym)
      continue;
    auto &paramSym = memberSym->as<ParameterSymbol>();
    //  Parse parameter value as expression (wrap in cast if enum)
    auto exprStr = paramSym.getValue().toString(SVInt::MAX_BITS, true, true);
    auto &paramType = memberSym->getDeclaredType()->getType();
    if (paramType.isEnum())
      exprStr = fmt::format("{}'({})", pd.type.get()->toString(), exprStr);
    auto &exprSyn = this->parse(exprStr).template as<ExpressionSyntax>();
    // Rebuild assignment
    auto newEquals =
        EqualsValueClauseSyntax(decl->initializer->equals, exprSyn);
    auto declSyn = DeclaratorSyntax(decl->name, decl->dimensions, &newEquals);
    newDeclStrs.emplace_back(declSyn.toString());
  }
  auto declStr = fmt::format("{}{}{}", pd.keyword.toString(),
                             pd.type->toString(), fmt::join(newDeclStrs, ", "));
  replaceParamDeclOrBail<ParameterDeclarationSyntax>(declStr, pd);
}

EmptyMemberSyntax *GenerateRewriter::makeEmptyMember() {
  return clone(parse("\n;\n").as<EmptyMemberSyntax>(), alloc);
}

Token GenerateRewriter::makeColon() {
  return makeToken(TokenKind::Colon, *strAlloc.emplace(":"));
}

IfGenerateSyntax *GenerateRewriter::makeDummyIfGen(std::string_view label) {
  std::string labelSuffix = label.empty() ? "" : fmt::format(" : {}", label);
  std::string retStr = fmt::format(
      "generate\n\nif (1) begin{}\nwire dummy;\nend\n\nendgenerate\n",
      labelSuffix);
  return clone((*parse(retStr).as<GenerateRegionSyntax>().members.begin())
                   ->as<IfGenerateSyntax>(),
               alloc);
}

IfGenerateSyntax *
GenerateRewriter::wrapBlockInIfGen(GenerateBlockSyntax &blockSyn,
                                   MemberSyntax *genvar) {
  auto ifSyn = makeDummyIfGen();
  auto newBlkSyn = &blockSyn;
  if (genvar) {
    // Create new span with genvar member at beginning
    auto newMembers =
        allocArray<MemberSyntax *>(blockSyn.members.size() + 1, alloc);
    newMembers[0] = genvar;
    std::copy(std::begin(blockSyn.members), std::end(blockSyn.members),
              std::begin(newMembers) + 1);
    newBlkSyn =
        clone(GenerateBlockSyntax(blockSyn.attributes, blockSyn.label,
                                  blockSyn.begin, blockSyn.beginName,
                                  newMembers, blockSyn.end, blockSyn.endName),
              alloc);
  }
  return clone(IfGenerateSyntax(ifSyn->attributes, ifSyn->keyword,
                                ifSyn->openParen, *ifSyn->condition,
                                ifSyn->closeParen, *newBlkSyn,
                                ifSyn->elseClause),
               alloc);
}

IfGenerateSyntax *
GenerateRewriter::wrapMemberListInIfGen(SyntaxList<MemberSyntax> &members,
                                        std::string_view label) {
  auto ifSyn = makeDummyIfGen(label);
  auto &blkSyn = ifSyn->block->as<GenerateBlockSyntax>();
  auto newBlkSyn =
      clone(GenerateBlockSyntax(blkSyn.attributes, blkSyn.label, blkSyn.begin,
                                blkSyn.beginName, members, blkSyn.end,
                                blkSyn.endName),
            alloc);
  return clone(IfGenerateSyntax(ifSyn->attributes, ifSyn->keyword,
                                ifSyn->openParen, *ifSyn->condition,
                                ifSyn->closeParen, *newBlkSyn,
                                ifSyn->elseClause),
               alloc);
}

IfGenerateSyntax *GenerateRewriter::wrapMemberInIfGen(MemberSyntax &membSyn,
                                                      std::string_view label,
                                                      MemberSyntax *genvar) {
  auto newMembSpan = allocArray<MemberSyntax *>(1 + bool(genvar), alloc);
  if (genvar)
    newMembSpan[0] = genvar;
  newMembSpan[bool(genvar)] = &membSyn;
  auto newMembers = clone(SyntaxList(newMembSpan), alloc);
  return wrapMemberListInIfGen(*newMembers, label);
}

MemberSyntax *GenerateRewriter::wrapInIfGen(MemberSyntax &membSyn,
                                            MemberSyntax *genvar) {
  // Do not wrap empty members to avoid unnecessary block spawning
  if (membSyn.kind == SyntaxKind::EmptyMember)
    return &membSyn;
  else if (membSyn.kind == SyntaxKind::GenerateBlock)
    return wrapBlockInIfGen(membSyn.as<GenerateBlockSyntax>(), genvar);
  else
    return wrapMemberInIfGen(membSyn, ""sv, genvar);
}

NamedBlockClauseSyntax *
GenerateRewriter::getBeginName(const GenerateBlockSyntax &blockSyn) {
  NamedBlockClauseSyntax *ret = blockSyn.beginName;
  if (!ret && blockSyn.label)
    ret =
        clone(NamedBlockClauseSyntax(makeColon(), blockSyn.label->name), alloc);
  return ret;
}

NamedBlockClauseSyntax *
GenerateRewriter::makeBlockBeginName(std::string_view name) {
  auto ifSyn = makeDummyIfGen(name);
  return ifSyn->block->as<GenerateBlockSyntax>().beginName;
}

MemberSyntax *
GenerateRewriter::unrollGenSyntax(MemberSyntax &membSyn, const Scope &scope,
                                  NamedBlockClauseSyntax *beginName,
                                  const GenerateBlockSymbol *blockSym,
                                  const Scope *globalScope) {
  // After generate, skip into its possible block scope unless an actual block
  // syntax exists.
  auto &genScope = blockSym ? *blockSym : scope;
  if (blockSym && blockSym->isUninstantiated)
    return makeEmptyMember();
  switch (membSyn.kind) {
  case SyntaxKind::IfGenerate:
    return unrollGenSyntax(membSyn.as<IfGenerateSyntax>(), genScope, beginName);
  case SyntaxKind::CaseGenerate:
    return unrollGenSyntax(membSyn.as<CaseGenerateSyntax>(), genScope,
                           beginName);
  case SyntaxKind::LoopGenerate:
    return unrollGenSyntax(membSyn.as<LoopGenerateSyntax>(), genScope);
  case SyntaxKind::GenerateRegion:
    return unrollGenSyntax(membSyn.as<GenerateRegionSyntax>(), genScope,
                           beginName);
  case SyntaxKind::HierarchyInstantiation:
    return unrollGenSyntax(membSyn.as<HierarchyInstantiationSyntax>(), genScope,
                           globalScope);
  case SyntaxKind::GenerateBlock: {
    // If the child is a block, the parent *should* set the block symbol
    if (!blockSym)
      diag.log(DiagSev::Fatal,
               "Could not find compilation symbol for generate block", membSyn);
    return unrollGenBlockSyntax(membSyn.as<GenerateBlockSyntax>(), *blockSym,
                                beginName);
  }
  default:
    return &membSyn;
  }
}

MemberSyntax *
GenerateRewriter::unrollGenBlockSyntax(const GenerateBlockSyntax &blockSyn,
                                       const GenerateBlockSymbol &blockSym,
                                       NamedBlockClauseSyntax *beginName) {
  // Allocate and unroll new members; do not propagate our begin label past this
  // stage.
  auto newMembers = allocArray<MemberSyntax *>(blockSyn.members.size(), alloc);
  size_t i = 0;
  for (auto memb : blockSyn.members)
    newMembers[i++] = unrollGenSyntax(*memb, blockSym);
  // Construct new block (Name is either passed down or taken from beginName or
  // label)
  if (!beginName)
    beginName = getBeginName(blockSyn);
  auto newMembersList = clone(SyntaxList(newMembers), alloc);
  return clone(GenerateBlockSyntax(blockSyn.attributes, nullptr, blockSyn.begin,
                                   beginName, *newMembersList, blockSyn.end,
                                   nullptr),
               alloc);
}

MemberSyntax *
GenerateRewriter::unrollGenSyntax(const IfGenerateSyntax &ifSyn,
                                  const Scope &scope,
                                  NamedBlockClauseSyntax *beginName) {
  auto newSyn = unrollGenSyntax(*ifSyn.block, scope, beginName,
                                matchInstGenBlock(ifSyn.block, scope));
  if (newSyn->kind == SyntaxKind::EmptyMember && ifSyn.elseClause) {
    auto &elseSyn = ifSyn.elseClause->clause->as<MemberSyntax>();
    newSyn = unrollGenSyntax(elseSyn, scope, beginName,
                             matchInstGenBlock(&elseSyn, scope));
  }
  return wrapInIfGen(*newSyn);
}

MemberSyntax *
GenerateRewriter::unrollGenSyntax(const CaseGenerateSyntax &caseSyn,
                                  const Scope &scope,
                                  NamedBlockClauseSyntax *beginName) {
  // Iterate over case items and break once one contains an instantiated block
  for (auto &item : caseSyn.items) {
    SyntaxNode *caseNode;
    switch (item->kind) {
    case SyntaxKind::StandardCaseItem: {
      caseNode = item->as<StandardCaseItemSyntax>().clause;
      break;
    }
    case SyntaxKind::DefaultCaseItem: {
      caseNode = item->as<DefaultCaseItemSyntax>().clause;
      break;
    }
    default:
      diag.log(DiagSev::Fatal, "Encountered unexpected generate case item type",
               item);
    }
    auto &itemSyn = caseNode->as<MemberSyntax>();
    auto newSyn = unrollGenSyntax(itemSyn, scope, beginName,
                                  matchInstGenBlock(&itemSyn, scope));
    // We found the one instantiated (matching) case --> wrap and return
    // unrolled block or member
    if (newSyn->kind != SyntaxKind::EmptyMember)
      return wrapInIfGen(*newSyn);
  }
  // None of the cases are instantiated (i.e. matched) --> return *unwrapped*
  // empty member
  return makeEmptyMember();
}

MemberSyntax *
GenerateRewriter::unrollGenSyntax(const LoopGenerateSyntax &loopSyn,
                                  const Scope &scope) {
  // Get syntax and symbol
  auto topMembSyn = loopSyn.block;
  auto topArraySym = synToSym<GenerateBlockArraySymbol>(*topMembSyn, scope);
  if (!topArraySym)
    diag.log(DiagSev::Fatal,
             "Could not find symbol for loop generate construct", loopSyn);

  // Allocate member array. We will later wrap them in an if-generate block
  auto newMembers =
      allocArray<MemberSyntax *>(topArraySym->entries.size(), alloc);
  // If direct child is block, give top block its name and give it an index
  // name. Otherwise, give top block a generic name and leave subblocks
  // unchanged (guaranteed unique)
  bool renameSubs = (topMembSyn->kind == SyntaxKind::GenerateBlock);
  // Iterate over instantiated blocks and collect unrolled clones with defparams
  size_t i = 0;
  for (auto &entry : topArraySym->entries) {
    // Skip uninstantiated members
    if (entry->isUninstantiated) {
      newMembers[i++] = makeEmptyMember();
      continue;
    }
    // Emit defparam assignment before member
    auto arrayIdxStr = entry->arrayIndex->toString();
    auto loopGenvarDeclStr = fmt::format(
        "\nlocalparam {} = {};\n", loopSyn.identifier.toString(), arrayIdxStr);
    auto loopGenvarSyn =
        clone(parse(loopGenvarDeclStr).as<MemberSyntax>(), alloc);
    // Emit member, with overriden name if requested.
    // All children should consume the block used to store the genvar.
    NamedBlockClauseSyntax *subBlockName = nullptr;
    if (renameSubs)
      subBlockName = makeBlockBeginName(fmt::format("__{}", arrayIdxStr));
    newMembers[i++] =
        wrapInIfGen(*unrollGenSyntax(*topMembSyn, *topArraySym, subBlockName,
                                     entry, topArraySym->getParentScope()),
                    loopGenvarSyn);
  }
  // Determine top block name
  std::string topBlockName = fmt::format("genfor{}", ++uniqCounter);
  if (renameSubs) {
    auto topBeginName = getBeginName(topMembSyn->as<GenerateBlockSyntax>());
    if (topBeginName)
      topBlockName = topBeginName->name.toString();
  }
  // Return clones of top member in new generate-block pair
  auto newMembersList = clone(SyntaxList(newMembers), alloc);
  return wrapMemberListInIfGen(*newMembersList, topBlockName);
}

MemberSyntax *
GenerateRewriter::unrollGenSyntax(const GenerateRegionSyntax &regSyn,
                                  const Scope &scope,
                                  NamedBlockClauseSyntax *beginName) {
  // Allocate and unroll members
  auto newMembers = allocArray<MemberSyntax *>(regSyn.members.size(), alloc);
  size_t i = 0;
  for (auto memb : regSyn.members)
    newMembers[i++] = unrollGenSyntax(*memb, scope, beginName);
  // Construct new block (Name is either passed down or taken from beginName or
  // label)
  auto newMembersList = clone(SyntaxList(newMembers), alloc);
  return clone(GenerateRegionSyntax(regSyn.attributes, regSyn.keyword,
                                    *newMembersList, regSyn.endgenerate),
               alloc);
};

MemberSyntax *
GenerateRewriter::unrollGenSyntax(const HierarchyInstantiationSyntax &instSyn,
                                  const Scope &scope,
                                  const Scope *globalScope) {
  // We use the first instance symbol as a representative to get the unique
  // module
  auto instSymGeneric =
      getScopeMember(scope, (*instSyn.instances.begin())->decl->name.rawText());
  if (!instSymGeneric && globalScope)
    instSymGeneric = getScopeMember(
        *globalScope, (*instSyn.instances.begin())->decl->name.rawText());
  if (!instSymGeneric)
    diag.log(DiagSev::Fatal, "Could not find compilation symbol for instance",
             instSyn);
  // Check if this is a black box or non-module; if so, leave it be.
  // Uninstantiated modules are also left be, are pad or macros
  if (instSymGeneric->kind == SymbolKind::Unknown ||
      instSymGeneric->kind == SymbolKind::UninstantiatedDef) {
    return clone(instSyn, alloc);
  }
  auto &instSym = instSymGeneric->as<InstanceSymbol>();
  if (!instSym.isModule())
    return clone(instSyn, alloc);
  // Identify unique module
  auto uniqMod = design.getUniqueModule(instSymGeneric->as<InstanceSymbol>());
  if (!uniqMod)
    diag.log(DiagSev::Fatal, "Could not find unique module for instance",
             instSyn);
  // Create a new type token
  auto uniqNameStr = strAlloc.emplace(uniqMod->getUniqueName() + " ");
  auto uniqTypeTok =
      instSyn.type.withRawText(alloc, std::string_view(*uniqNameStr));
  // Return reconstructed instantiation
  return clone(HierarchyInstantiationSyntax(instSyn.attributes, uniqTypeTok,
                                            nullptr, instSyn.instances,
                                            instSyn.semi),
               alloc);
}

template <typename T> void GenerateRewriter::handleGenerate(const T &syn) {
  // The handler should tackle the uppermost block automatically --> handle only
  // if our parent is a module
  if (syn.parent->kind == SyntaxKind::ModuleDeclaration) {
    auto uniqMod = design.getUniqueModule(
        syn.parent->template as<ModuleDeclarationSyntax>()
            .header->name.rawText());
    auto &scope =
        uniqMod->getInstances().begin()->symbol->body.template as<Scope>();
    auto newSyn = unrollGenSyntax(syn, scope);
    replace(syn, *newSyn);
  }
}

void GenerateRewriter::handle(const IfGenerateSyntax &syn) {
  handleGenerate(syn);
}

void GenerateRewriter::handle(const LoopGenerateSyntax &syn) {
  handleGenerate(syn);
}

void GenerateRewriter::handle(const CaseGenerateSyntax &syn) {
  handleGenerate(syn);
}

void GenerateRewriter::handle(const GenerateRegionSyntax &syn) {
  handleGenerate(syn);
}

void GenerateRewriter::handle(const HierarchyInstantiationSyntax &syn) {
  handleGenerate(syn);
}

DesignUniqueModule *
TypedefDeclarationRewriter::getUniqueModule(const SyntaxNode &pd) const {
  // Obtain module instance from either port list or root (skip if in package or
  // in some other config)
  if (!pd.parent)
    return nullptr;
  auto modNode = pd.parent;
  while (modNode->kind != SyntaxKind::ModuleDeclaration && modNode->parent &&
         modNode->kind != SyntaxKind::Unknown) {
    modNode = modNode->parent;
  }
  if (modNode->kind == SyntaxKind::CompilationUnit)
    return nullptr;
  if (modNode->kind != SyntaxKind::ModuleDeclaration) {
    diag.log(DiagSev::Warning,
             fmt::format(
                 "type rewrite with no parent found with unhandled parent kind "
                 "`{}`; left unchanged",
                 toString(modNode->kind)),
             pd, true);
    return nullptr;
  }
  // Obtain corresponding unique module
  auto &modSyn = modNode->as<ModuleDeclarationSyntax>();
  return design.getUniqueModule(modSyn.header->name.rawText());
}

const Symbol *TypedefDeclarationRewriter::getTypeNameSymOrBail(
    const TypedefDeclarationSyntax *pd, DesignUniqueModule *uniqMod) const {
  auto memberSym =
      getScopeMember(uniqMod->getInstances().begin()->symbol->body.as<Scope>(),
                     pd->name.rawText());
  if (memberSym == nullptr) {
    diag.log(DiagSev::Note,
             "getTypeNameSymOrBail not found in compilation; left unchanged",
             *pd, true);
  }
  return memberSym;
}

template <typename T>
void TypedefDeclarationRewriter::replaceTypeDeclOrBail(std::string declStr,
                                                       const T &pd) {
  auto &newPdSyn = parse(declStr);
  if (newPdSyn.kind != SyntaxKind::TypedefDeclaration) {
    diag.log(DiagSev::Error,
             fmt::format(
                 "misparsed parameter declaration as kind `{}`; left unchanged",
                 toString(newPdSyn.kind)),
             pd, true);
    return;
  }
  auto newPdStatement = clone(newPdSyn.as<TypedefDeclarationSyntax>(), alloc);
  if (newPdStatement) {
    replace(pd, *newPdStatement);
  }
}

void TypedefDeclarationRewriter::handle(const TypedefDeclarationSyntax &pd) {
  auto uniqMod = getUniqueModule(pd);
  if (!uniqMod)
    return;
  auto memberSym = getTypeNameSymOrBail(&pd, uniqMod);
  if (!memberSym)
    return;

  auto typePrinter = TypePrinter();
  typePrinter.options.skipScopedTypeNames = true;
  typePrinter.options.fullEnumType = true;
  auto &paramSym = memberSym->as<TypeAliasType>();
  auto &type = paramSym.getDeclaredType()->getType().getCanonicalType();
  typePrinter.append(type);
  auto declStr = fmt::format("{} {} {};", pd.typedefKeyword.toString(),
                             typePrinter.toString(), pd.name.toString());
  typePrinter.clear();
  replaceTypeDeclOrBail<TypedefDeclarationSyntax>(declStr, pd);
}
} // namespace svase
