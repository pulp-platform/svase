// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0
//
// Author:  Philippe Sauter <phsauter@student.ethz.ch>
//
// Test the following parameter propagation scenarios:
// - from port to module scope
// - from module to derived

module test #(
	parameter int unsigned PortParam1 = 32'd5,
	parameter int unsigned PortParam2 = 32'd8
) ( );
	localparam int unsigned TopParam1 = unsigned'($clog2(PortParam1));
	localparam int unsigned TopParam2 = unsigned'($clog2(PortParam2));

	localparam int unsigned DerivParam = 2**TopParam1;
endmodule