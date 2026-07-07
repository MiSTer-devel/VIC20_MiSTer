// ----------------------------------------------------------------------------------
// --
// -- Description:
// -- RAM 1024 x 8 bit: sync. write, sync. read, configurable
// --
// -- (c) copyright 2011...2013 by Wolfgang Scherr
// -- http://www.pin4.at - ws_arcade <at> pin4.at
// --
// -- All Rights Reserved
// --
// -- Version 1.0
// -- SVN: $Id: ram_conf_1024x8.vhd 647 2014-05-17 11:47:04Z wolfgang.scherr $
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

// Configurable 1024x8 RAM module with setup bus interface
module ram_conf_1024x8 #(
	parameter [5:0] START_AI = 6'b000000
) (
	input         CLK,
	input         CLK_EN,

	input         ENn,
	input         WRn,
	input  [9:0]  ADDR,
	input  [7:0]  DIN,
	output [7:0]  DOUT,

	input         CONF_CLK,
	input         CONF_WR,
	input  [15:0] CONF_AI,
	input  [7:0]  CONF_DI
);

wire conf_en_s = (CONF_AI[15:10] == START_AI);

/* verilator lint_off PINCONNECTEMPTY */
// Dual port RAM instantiation for system access and configuration loading
gen_dpram #(
	.addr_width_g(10),
	.data_width_g(8)
) ram_inst (
	.clock_a   (CLK),
	.enable_a  (CLK_EN & ~ENn),
	.address_a (ADDR),
	.data_a    (DIN),
	.wren_a    (~WRn),
	.q_a       (DOUT),
	.cs_a      (1'b1),

	.clock_b   (CONF_CLK),
	.enable_b  (1'b1),
	.address_b (CONF_AI[9:0]),
	.data_b    (CONF_DI),
	.wren_b    (CONF_WR & conf_en_s),
	.q_b       (),
	.cs_b      (1'b1)
);
/* verilator lint_on PINCONNECTEMPTY */

endmodule
