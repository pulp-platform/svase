// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    Preprocess syntax prior to parsing and compilation, *preserving*
// syntax offsets.

#pragma once

// TODO: appropriate headers

namespace svase {

class Preprocessor {
private:
  std::vector<SourceBuffer> &buffers;
  TypedBumpAllocator<std::string> &preBuffers;
  Diag &diag;

public:
  Preprocessor(std::vector<SourceBuffer> &buffers,
               TypedBumpAllocator<std::string> &preBuffers, Diag &diag)
      : buffers(buffers), preBuffers(preBuffers), diag(diag) {}

  void preprocess() {
    for (auto &buf : buffers) {
      auto strBuf = preBuffers.emplace(buf.data);
      // Add future per-buffer transforms here
      filterPragmaTranslate(*strBuf, buf.id);
      buf.data = std::string_view(strBuf->c_str());
    }
  }

  // TODO: glibcxx has known overflow bugs... use an alternative?
  void filterPragmaTranslate(std::string &strBuf, BufferID &id) {
    std::regex reOff(R"(//\s*pragma\s+translate[_ ]off)");
    std::regex reOn(R"(//\s*pragma\s+translate[_ ]on)");
    std::smatch match;
    // Overwrite these matches with whitespace of equal length
    auto start = strBuf.cbegin();
    while (std::regex_search(start, strBuf.cend(), match, reOff)) {
      auto begin = (char *)start.base() + match.position(0);
      start = match.suffix().first;
      std::regex_search(start, strBuf.cend(), match, reOn);
      auto end = (char *)match.suffix().first.base();
      diag.log(DiagSev::Note,
               "Filtering out `pragma_translate off` region before parsing",
               SourceLocation(id, begin - strBuf.cbegin().base()), false,
               false);
      std::fill(begin, end, ' ');
    }
  }
};

} // namespace svase
