// --
// -- A simulation model of VIC20 hardware (clocking)
// -- 
// -- All rights reserved
// -- (c) copyright 2003-2009 by MikeJ (Mike Johnson)
// -- http://www.FPGAArcade.com - mikej <at> fpgaarcade <dot> com
// -- (c) copyright 2011...2015 by WoS (Wolfgang Scherr)
// -- http://www.pin4.at - WoS <at> pin4 <dot> at
// --
// -- $Id: vic20_clocks.vhd 1328 2015-05-22 19:29:53Z wolfgang.scherr $
// --
// ----------------------------------------------------------------------------
// --
// -- Redistribution and use in source and synthezised forms, with or without
// -- modification, are permitted provided that the following conditions are met:
// --
// -- Redistributions of source code must retain the above copyright notice,
// -- this list of conditions and the following disclaimer.
// --
// -- Redistributions in synthesized form must reproduce the above copyright
// -- notice, this list of conditions and the following disclaimer in the
// -- documentation and/or other materials provided with the distribution.
// --
// -- Neither the name of the author nor the names of other contributors may
// -- be used to endorse or promote products derived from this software without
// -- specific prior written permission; any commercial use is forbidden as well.
// --
// -- This code must be run on Replay hardware only.
// --
// -- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// -- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
// -- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// -- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
// -- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// -- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// -- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// -- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// -- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// -- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// -- POSSIBILITY OF SUCH DAMAGE.
// --
// -- You are responsible for any legal issues arising from your use of this code.
// --
// -- The latest version of this file can be found at: www.fpgaarcade.com
// --
// -- Email vic20@fpgaarcade.com
// --

// --
// -- A simulation model of VIC20 hardware (clocking)
// -- 
// -- All rights reserved
// -- (c) copyright 2003-2009 by MikeJ (Mike Johnson)
// -- http://www.FPGAArcade.com - mikej <at> fpgaarcade <dot> com
// -- (c) copyright 2011...2015 by WoS (Wolfgang Scherr)
// -- http://www.pin4.at - WoS <at> pin4 <dot> at
// --
// -- $Id: vic20_clocks.vhd 1328 2015-05-22 19:29:53Z wolfgang.scherr $
// --
// ----------------------------------------------------------------------------
// --
// -- Redistribution and use in source and synthezised forms, with or without
// -- modification, are permitted provided that the following conditions are met:
// --
// -- Redistributions of source code must retain the above copyright notice,
// -- this list of conditions and the following disclaimer.
// --
// -- Redistributions in synthesized form must reproduce the above copyright
// -- notice, this list of conditions and the following disclaimer in the
// -- documentation and/or other materials provided with the distribution.
// --
// -- Neither the name of the author nor the names of other contributors may
// -- be used to endorse or promote products derived from this software without
// -- specific prior written permission; any commercial use is forbidden as well.
// --
// -- This code must be run on Replay hardware only.
// --
// -- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// -- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
// -- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// -- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
// -- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// -- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// -- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// -- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// -- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// -- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// -- POSSIBILITY OF SUCH DAMAGE.
// --
// -- You are responsible for any legal issues arising from your use of this code.
// --

// Clock generation and reset delay controller for VIC20 core
/* verilator lint_off DECLFILENAME */

module VIC20_CLOCKS (
	input  I_SYSCLK,
	input  I_SYSCLK_EN,
	input  I_RESET_L,

	output reg O_ENA,
	output reg O_RESET_L
);
/* verilator lint_on DECLFILENAME */

// Delay counter and clock divider state registers
reg [11:0] delay_count = 12'd0;
reg        div_cnt = 1'b0;

// Reset delay counter with asynchronous clear on I_RESET_L
always @(posedge I_SYSCLK or negedge I_RESET_L) begin
	if (!I_RESET_L) begin
		delay_count <= 12'd0;
		O_RESET_L   <= 1'b0;
	end else if (I_SYSCLK_EN) begin
		if (&delay_count) begin
			O_RESET_L <= 1'b1;
		end else begin
			delay_count <= delay_count + 12'd1;
			O_RESET_L   <= 1'b0;
		end
	end
end

// Clock enable divider toggling on valid clock enable pulse
always @(posedge I_SYSCLK) begin
	O_ENA <= 1'b0;
	if (I_SYSCLK_EN) begin
		div_cnt <= ~div_cnt;
		O_ENA   <= div_cnt;
	end
end

endmodule
