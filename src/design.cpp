// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    A collection of classes to manage a compiled design with a single
// root.

#include "svase/design.h"
#include "svase/util.h"

#include "fmt/format.h"
#include "slang/ast/Definition.h"
#include "slang/ast/symbols/BlockSymbols.h"
#include <unordered_map>

namespace svase {

using namespace slang;
using namespace slang::ast;

size_t DesignUniqueModule::addInstance(const InstanceSymbol *const symbol) {
  instances.emplace_back(symbol);
  return instances.size();
}

std::vector<DesignInstance> &DesignUniqueModule::getInstances() {
  return instances;
}

std::string_view DesignUniqueModule::getGenericName() const {
  return instances.back().symbol->getDefinition().name;
}

std::string DesignUniqueModule::getUniqueName() const {
  return fmt::format("{}__{}", getGenericName(), id);
}

void Design::insertInstance(const InstanceSymbol *const sym,
                            const size_t collisions) {
  // Generate ID and avoid collisions if necessary
  size_t id = genParamHash(sym) + collisions;
  // Construct our DesignUniqueModule *inside* the map *iff* the key is unused
  std::string_view genericName = sym->getDefinition().name;
  auto emplRet = uniqMods[genericName].emplace(
      std::piecewise_construct, std::make_tuple(id), std::make_tuple(sym, id));
  // Handle insertion as necessary
  auto containedMod = &emplRet.first->second;
  if (!emplRet.second) {
    // Insertion has failed due to collision
    if (!areParamEqual(containedMod->getInstances().back().symbol, sym))
      // Existing ID has different parameters: modify ID and retry insertion
      return this->insertInstance(sym, collisions + 1);
    else
      // Existing ID has same parameters: add instance to existing UniqueModule
      containedMod->addInstance(sym);
  } else {
    // Module Insertion successful: enter unique module into its maps
    uniqModByName.emplace(std::piecewise_construct,
                          std::make_tuple(containedMod->getUniqueName()),
                          std::make_tuple(genericName, id));
    uniqModByParamStr[genericName].emplace(std::piecewise_construct,
                                           std::make_tuple(genParamString(sym)),
                                           std::make_tuple(id));
  }
}

void Design::walkModuleInstances(const Symbol *sym) {
  switch (sym->kind) {
  // Walk and add instances and add modules only.
  case SymbolKind::Instance: {
    auto instSymb = static_cast<const InstanceSymbol *>(sym);
    if (instSymb->isModule())
      insertInstance(instSymb);
    for (auto &member : instSymb->body.members())
      walkModuleInstances(&member);
    break;
  }
  // Walk past structures that may occur between instance bodies and
  // instantiations.
  case SymbolKind::GenerateBlock: {
    auto genBlock = static_cast<const GenerateBlockSymbol *>(sym);
    if (!genBlock->isUninstantiated)
      for (auto &member : genBlock->members())
        walkModuleInstances(&member);
    break;
  }
  case SymbolKind::GenerateBlockArray: {
    auto genBlockArray = static_cast<const GenerateBlockArraySymbol *>(sym);
    if (!genBlockArray->isUninstantiated())
      for (auto &member : genBlockArray->members())
        walkModuleInstances(&member);
    break;
  }
  case SymbolKind::InstanceArray: {
    auto instArray = static_cast<const InstanceArraySymbol *>(sym);
    if (!instArray->isUninstantiated())
      for (auto &member : instArray->members())
        walkModuleInstances(&member);
    break;
  }
  default:;
  }
}

const std::unordered_map<size_t, DesignUniqueModule> *
Design::getGenericModule(std::string_view genericName) const {
  if (auto find = uniqMods.find(genericName); find == uniqMods.end())
    return nullptr;
  else
    return &find->second;
}

// Get a uniquified module by its generic name and ID if it exists or null.
DesignUniqueModule *Design::getUniqueModule(std::string_view genericName,
                                            size_t id) const {
  auto module = getGenericModule(genericName);
  if (!module)
    return nullptr;
  auto find = module->find(id);
  if (find == module->end())
    return nullptr;
  else
    return const_cast<DesignUniqueModule *>(&find->second);
}

DesignUniqueModule *Design::getUniqueModule(std::string uniqueName) const {
  if (auto find = uniqModByName.find(uniqueName); find == uniqModByName.end())
    return nullptr;
  else
    return getUniqueModule(find->second.first, find->second.second);
}

DesignUniqueModule *Design::getUniqueModule(std::string_view uniqueName) const {
  return getUniqueModule(std::string(uniqueName));
}

DesignUniqueModule *Design::getUniqueModule(std::string_view genericName,
                                            std::string paramString) const {
  if (auto find = uniqModByParamStr.find(genericName);
      find == uniqModByParamStr.end())
    return nullptr;
  else if (auto inFind = find->second.find(paramString);
           inFind == find->second.end())
    return nullptr;
  else
    return getUniqueModule(genericName, inFind->second);
}

DesignUniqueModule *
Design::getUniqueModule(const InstanceSymbol &instSym) const {
  return getUniqueModule(instSym.getDefinition().name,
                         genParamString(&instSym));
}

} // namespace svase
