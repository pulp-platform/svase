// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0
//
// Author:  Philippe Sauter <phsauter@student.ethz.ch>
//
// Test the following parameter propagation scenarios:
// - to port of instantiated module

module test2 #(
	parameter int unsigned PortParam = 0,
	parameter type PortTypeParam = logic
) ( );
	localparam int unsigned Module2Param = PortParam;
	PortTypeParam used_type;
endmodule


module test #( ) ( );
	localparam int unsigned TopParam = 32'd5;
	localparam type TypeParam = logic[1:0][31:0];

	test2 #(
		.PortParam(TopParam),
		.PortTypeParam(TypeParam)
	) i_test2 ( );
endmodule