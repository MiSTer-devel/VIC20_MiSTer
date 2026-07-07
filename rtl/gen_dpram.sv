

// Bidirectional dual-port synchronous memory module for VIC20 memories
module gen_dpram #(
	parameter addr_width_g = 8,
	parameter data_width_g = 8
) (
	input  [addr_width_g-1:0] address_a, address_b,
	input                     clock_a, clock_b,
	input  [data_width_g-1:0] data_a, data_b,
	input                     enable_a, enable_b,
	input                     wren_a, wren_b,
	input                     cs_a, cs_b,
	output [data_width_g-1:0] q_a, q_b
);

/* verilator lint_off MULTIDRIVEN */
(* ramstyle = "M10K, no_rw_check" *) reg [data_width_g-1:0] ram[0:(1<<addr_width_g)-1];
/* verilator lint_on MULTIDRIVEN */

reg [data_width_g-1:0] ram_out_a, ram_out_b;

// Port A: Hardware-compliant synchronous read/write
always @(posedge clock_a) begin
	if (enable_a && wren_a) ram[address_a] <= data_a;
	ram_out_a <= ram[address_a];
end

// Port B: Hardware-compliant synchronous read/write
always @(posedge clock_b) begin
	if (enable_b && wren_b) ram[address_b] <= data_b;
	ram_out_b <= ram[address_b];
end

// Combinational Chip Select and bus pull-up simulation
assign q_a = cs_a ? ram_out_a : {data_width_g{1'b1}};
assign q_b = cs_b ? ram_out_b : {data_width_g{1'b1}};

endmodule
