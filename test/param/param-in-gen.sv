// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0
//
// Author:  Philippe Sauter <phsauter@student.ethz.ch>
//
// Test the following parameter propagation scenarios:
// - into generate-if scope
// - into iterations of generate-loop

module test #( ) ( );
	localparam int unsigned IfElseParam = 2;
	localparam int unsigned ModuleParam = 32'd5;

	if (IfElseParam == unsigned'(1)) begin
		localparam int unsigned GenParam = unsigned'($clog2(ModuleParam));
	end else begin
		localparam int unsigned GenParam = unsigned'(ModuleParam);

		for (genvar k = 0; k < GenParam; k++) begin : gen_for_outer
			localparam int unsigned OuterForParam = 2**k;

			for (genvar l = 0; l < 2**k; l++) begin : gen_for_inner
				localparam int unsigned InnerForParam = 2**k+l;
			end
		end
	end
endmodule