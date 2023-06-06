module test2 ();

endmodule


module test #(
	parameter int unsigned IfElseParam = 2,
	parameter int unsigned ModuleParam = 32'd5
) ( );
	localparam int unsigned TopParam = unsigned'($clog2(ModuleParam));

	test2 i_test2();

	if (IfElseParam == unsigned'(1)) begin
		localparam int unsigned GenParam = unsigned'($clog2(ModuleParam));
	end else begin
		localparam int unsigned GenParam = unsigned'(ModuleParam);

		for (genvar level = 0; level < GenParam; level++) begin : gen_for_outer
			localparam int unsigned OuterForParam = 2**level;
			for (genvar l = 0; l < 2**level; l++) begin : gen_for_inner
				localparam int unsigned InnerForParam = 2**level-1+l;
			end
		end
	end
endmodule