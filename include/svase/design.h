// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    A collection of classes to manage a compiled design with a single
// root.

#pragma once

#include "slang/ast/Symbol.h"
#include "slang/ast/symbols/InstanceSymbols.h"
#include <unordered_map>

namespace svase {

using namespace slang;
using namespace slang::ast;

/// Represents a hierarchical instance in a design, storing it's symbol and
/// providing useful methods
struct DesignInstance {
public:
  const InstanceSymbol *const symbol;

  DesignInstance() = delete;

  DesignInstance(const InstanceSymbol *const symbol) : symbol(symbol) {}
};

/// Collects DesignInstances that share a same unique module-parameterization
/// pair.
class DesignUniqueModule {
private:
  std::vector<DesignInstance> instances;

public:
  const size_t id;

  DesignUniqueModule() = delete;

  DesignUniqueModule(const InstanceSymbol *const symbol, size_t id)
      : instances(), id(id) {
    addInstance(symbol);
  }

  /// Add more instances to this unique module; used by an DesignCollection
  /// tracking this. Returns the new index.
  size_t addInstance(const InstanceSymbol *const symbol);

  /// Get modifiable view of the instances.
  std::vector<DesignInstance> &getInstances();

  /// Get the module name without parameter uniquification, i.e. its name in the
  /// original design.
  std::string_view getGenericName() const;

  /// Get the uniquified name for this module, i.e. suffixed with its unique
  /// parameter ID.
  std::string getUniqueName() const;
};

/// Describes and stores a compiled design hierarchy, grouping instances by
/// unique module parameterizations.
class Design {
private:
  std::unordered_map<std::string_view,
                     std::unordered_map<size_t, DesignUniqueModule>>
      uniqMods;
  std::unordered_map<std::string, std::pair<std::string_view, size_t>>
      uniqModByName;
  std::unordered_map<std::string_view, std::unordered_map<std::string, size_t>>
      uniqModByParamStr;

  /// Insert an instance into the collection, returning the hierarchical key to
  /// the reference. Instances of the unique module are ordered by insertion, so
  /// the created instance is the last entry.
  void insertInstance(const InstanceSymbol *const sym,
                      const size_t collisions = 0);

  /// Walk a compiled hierarchy, inserting all instances we find into our
  /// design.
  void walkModuleInstances(const Symbol *sym);

public:
  Design(const InstanceSymbol *root)
      : uniqMods(), uniqModByName(), uniqModByParamStr() {
    walkModuleInstances(root);
  }

  /// Get a pointer to a generic module if it exists or null.
  const std::unordered_map<size_t, DesignUniqueModule> *
  getGenericModule(std::string_view genericName) const;

  /// Get a uniquified module by its generic name and ID if it exists or null.
  DesignUniqueModule *getUniqueModule(std::string_view genericName,
                                      size_t id) const;

  /// Get an uniquified module by its unique name if it exists or null.
  DesignUniqueModule *getUniqueModule(std::string uniqueName) const;

  /// Get an uniquified module by its unique name if it exists or null.
  DesignUniqueModule *getUniqueModule(std::string_view uniqueName) const;

  /// Get a uniquified module by its generic name and full parameter string.
  DesignUniqueModule *getUniqueModule(std::string_view genericName,
                                      std::string paramString) const;

  /// Get a uniquified module by one of its instances' symbols.
  DesignUniqueModule *getUniqueModule(const InstanceSymbol &instSym) const;
};

} // namespace svase
