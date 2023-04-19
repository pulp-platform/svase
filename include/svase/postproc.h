// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>
// Gist:    Postprocess rewritten syntax tree to yield desired output buffers.

#pragma once

// TODO: appropriate headers

namespace svase {

class Postprocessor {
private:
  s TypedBumpAllocator<std::string> &postBuffers;
  Diag &diag;

public:
  Postprocessor(std::vector<SourceBuffer> &buffers,
                TypedBumpAllocator<std::string> &postBuffers, Diag &diag)
      : buffers(buffers), postBuffers(postBuffers), diag(diag) {}

  void preprocess() {
    for (auto &buf : buffers) {
      auto strBuf = newBuffers.emplace(buf.data);
      // Add future per-buffer transforms here
      filterPragmaTranslate(*strBuf, buf.id);
      buf.data = std::string_view(strBuf->c_str());
    }
  }

  void filterPragmaTranslate(std::string &strBuf, BufferID &id) {
    std::regex re(
        R"(//\s*pragma\s+translate[_ ]off(.|\n)+?//\s*pragma\s+translate[_ ]on)");
    std::smatch match;
    // Overwrite these matches with whitespace of equal length
    auto start = strBuf.cbegin();
    while (std::regex_search(start, strBuf.cend(), match, re)) {
      auto begin = (char *)start.base() + match.position(0);
      auto end = begin + match.length(0);
      diag.log(DiagSev::Note,
               "Filtering out `pragma_translate off` region before parsing",
               SourceLocation(id, begin - strBuf.cbegin().base()), false,
               false);
      std::fill(begin, end, ' ');
      start = match.suffix().first;
    }
  }
};

} // namespace svase