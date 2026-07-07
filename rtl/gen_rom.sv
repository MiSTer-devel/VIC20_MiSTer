// -- altera message_off 10306

// ROM generator with conditional Quartus init file support and download write port
/* verilator lint_off UNUSEDPARAM */
/* verilator lint_off UNUSEDSIGNAL */
module gen_rom #(
	parameter INIT_FILE = "",
	parameter ADDR_WIDTH = 14,
	parameter [2:0] START_AI = 3'b000
) (
	input                   wrclock,
	input            [15:0] wraddress,
	input             [7:0] data,
	input                   wren,

	input                   rdclock,
	input  [ADDR_WIDTH-1:0] rdaddress,
	output            [7:0] q,
	input                   cs
);

reg [7:0] q0;
wire conf_en_s = (wraddress[15:15-$bits(START_AI)+1] == START_AI);

// Conditional compilation based on INIT_FILE parameter presence (VHDL-equivalent behavior)
generate
	if (INIT_FILE == "") begin : gen_rom_empty
		// Instantiates the RAM without any init file attribute if the string is empty
		reg [7:0] ram[0:(1<<ADDR_WIDTH)-1];

		always @(posedge wrclock) begin
			if (wren && conf_en_s) begin
				ram[wraddress[ADDR_WIDTH-1:0]] <= data;
			end
		end

		always @(posedge rdclock) begin
			q0 <= ram[rdaddress];
		end
	end else begin : gen_rom_init
		// Instantiates the RAM WITH the init file attribute only if a path is provided
		(* ram_init_file = INIT_FILE *) reg [7:0] ram[0:(1<<ADDR_WIDTH)-1];

		always @(posedge wrclock) begin
			if (wren && conf_en_s) begin
				ram[wraddress[ADDR_WIDTH-1:0]] <= data;
			end
		end

		always @(posedge rdclock) begin
			q0 <= ram[rdaddress];
		end
	end
endgenerate

assign q = cs ? q0 : 8'hFF;

endmodule
/* verilator lint_on UNUSEDPARAM */
/* verilator lint_on UNUSEDSIGNAL */
