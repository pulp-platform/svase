module test2 #(
	parameter int unsigned PortParam = 0
) ( );
	localparam int unsigned Module2Param = PortParam;
endmodule


module test #(
	parameter int unsigned IfElseParam = 2,
	parameter int unsigned ModuleParam = 32'd5
) ( );
	localparam int unsigned TopParam = unsigned'($clog2(ModuleParam));

	test2 #(.PortParam(TopParam)) i_test2 ( );

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