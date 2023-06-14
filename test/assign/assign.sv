// Copyright (c) 2022 ETH Zurich and University of Bologna.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0
//
// Author:  Philippe Sauter <phsauter@student.ethz.ch>
//
// Test the following ...

module test #( ) ( );
	logic [31:0] bit5_hot;
	logic [31:0] bit12_hot;
	logic [31:0] bits;
	
	assign bits = 32'h0000aaee;
	assign bit5_hot = nth_bit(5);
	assign bit12_hot = nth_bit(12);

	function automatic logic[31:0] nth_bit(input int unsigned bit_pos);
		logic[31:0] shifted;
		shifted = (32'd01 << bit_pos);

		return shifted;
	endfunction
endmodule