// ----------------------------------------------------------------------------------
// --
// -- Description:
// -- ROM with generic size, with ext. load function: sync. write, sync. read
// --
// -- (c) copyright 2011...2013 by Wolfgang Scherr
// -- http://www.pin4.at - ws_arcade <at> pin4.at
// --
// -- All Rights Reserved
// --
// -- Version 1.0
// -- SVN: $Id: generic_rom.vhd 647 2014-05-17 11:47:04Z wolfgang.scherr $
// --
// -------------------------------------------------------------------------------
// -- Redistribution and use in source or synthesized forms are permitted
// -- provided that the following conditions are met (or a prior written
// -- permission was given otherwise):
// --
// -- * Redistributions of source code must retain this original header
// --   incl. author, contributors, conditions, copyright and disclaimer.
// --
// -- * Redistributions in synthesized (binary) form must also contain
// --   the soure code according to this conditions to keep it "open".
// --
// -- * Neither the name of the author nor the names of contributors may
// --   be used to endorse or promote products derived from this code.
// --
// -- * This code is only allowed to be used on:
// --   - Replay hardware (from fpgaarcade.com)
// --
// -- * Feedback or bug reports are welcome, but please check on the 
// --   web sites given in the header first for any updates available.
// --
// -- * You are responsible for any legal issues arising from your use
// --   or your own distribution of this code.
// ----------------------------------------------------------------------
// -- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// -- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// -- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// -- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// -- AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// -- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// -- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// -- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// -- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// -- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// -- ARISING IN ANY WAY OUT OF THE USE OF THIS  CODE OR ANY WORK
// -- PRODUCTS, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// ----------------------------------------------------------------------

// Generic ROM module with configuration write port and synthesis init attribute
/* verilator lint_off UNUSEDPARAM */
/* verilator lint_off UNUSEDSIGNAL */
module generic_rom #(
	parameter ADDR_WIDTH = 11,
	parameter DATA_WIDTH = 8,
	parameter [15:0] START_AI = 16'h0000,
	parameter FILE_NAME = ""
) (
	input                   CLK,
	input                   ENA,
	input  [ADDR_WIDTH-1:0] ADDR,
	output reg [DATA_WIDTH-1:0] DATA,

	input                   CONF_WR,
	input            [15:0] CONF_AI,
	input  [DATA_WIDTH-1:0] CONF_DI
);

// Memory array declaration with synthesis attribute for init file loading
reg [DATA_WIDTH-1:0] rom[0:(1<<ADDR_WIDTH)-1] /* synthesis ram_init_file = FILE_NAME */;

wire conf_match = (CONF_AI[15:ADDR_WIDTH] == START_AI[15:ADDR_WIDTH]);

// Synchronous read process for standard memory access
always @(posedge CLK) begin
	DATA <= rom[ADDR];
end

// Synchronous write process for configuration bus loading
always @(posedge CLK) begin
	if (CONF_WR && conf_match) begin
		rom[CONF_AI[ADDR_WIDTH-1:0]] <= CONF_DI;
	end
end

endmodule
/* verilator lint_on UNUSEDPARAM */
/* verilator lint_on UNUSEDSIGNAL */
