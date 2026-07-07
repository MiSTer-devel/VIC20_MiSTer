// --
// -- A simulation model of VIC20 hardware
// --
// -- All rights reserved
// -- (c) copyright 2003-2009 by MikeJ (Mike Johnson)
// -- http://www.FPGAArcade.com - mikej <at> fpgaarcade <dot> com
// -- (c) copyright 2011...2015 by WoS (Wolfgang Scherr)
// -- http://www.pin4.at - WoS <at> pin4 <dot> at
// --
// -- $Id: vic20.vhd 2205 2017-08-04 19:28:32Z mikej $
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
// -- A simulation model of VIC20 hardware
// --
// -- All rights reserved
// -- (c) copyright 2003-2009 by MikeJ (Mike Johnson)
// -- http://www.FPGAArcade.com - mikej <at> fpgaarcade <dot> com
// -- (c) copyright 2011...2015 by WoS (Wolfgang Scherr)
// -- http://www.pin4.at - WoS <at> pin4 <dot> at
// --
// -- $Id: vic20.vhd 2205 2017-08-04 19:28:32Z mikej $
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

// VIC20 hardware emulation top-level module
/* verilator lint_off UNUSEDSIGNAL */
/* verilator lint_off PINCONNECTEMPTY */
module vic20_core (
	input  wire        i_sysclk,
	input  wire        i_sysclk_en,
	input  wire        i_reset,
	output wire        o_p2h,
	output wire        atn_o,
	output wire        clk_o,
	input  wire        clk_i,
	output wire        data_o,
	input  wire        data_i,
	input  wire [3:0]  i_joy,
	input  wire        i_fire,
	input  wire [7:0]  i_potx,
	input  wire [7:0]  i_poty,
	input  wire [4:0]  i_ram_ext_ro,
	input  wire [4:0]  i_ram_ext,
	input  wire        i_extmem_en,
	output wire        o_extmem_sel,
	output wire        o_extmem_r_wn,
	output wire [15:0] o_extmem_addr,
	input  wire [7:0]  i_extmem_data,
	output wire [7:0]  o_extmem_data,
	output wire        o_io2_sel,
	output wire        o_io3_sel,
	output wire        o_blk123_sel,
	output wire        o_blk5_sel,
	output wire        o_ram123_sel,
	output wire        o_ce_pix,
	output reg  [3:0]  o_video_r,
	output reg  [3:0]  o_video_g,
	output reg  [3:0]  o_video_b,
	output reg         o_hsync,
	output reg         o_vsync,
	output wire        o_hblank,
	output wire        o_vblank,
	input  wire [1:0]  i_center,
	input  wire        i_pal,
	input  wire        i_wide,
	input  wire [10:0] ps2_key,
	output wire        tape_play,
	output wire [5:0]  o_audio,
	output wire        cass_write,
	input  wire        cass_read,
	output wire        cass_motor,
	input  wire        cass_sw,
	input  wire        rom_std,
	input  wire        conf_clk,
	input  wire        conf_wr,
	input  wire [15:0] conf_ai,
	input  wire [7:0]  conf_di
);

// Serial IEC bus synchronization flip-flops
reg [2:0] iec_data_sr;
reg [2:0] iec_clk_sr;
always @(posedge i_sysclk) begin
	iec_data_sr <= {iec_data_sr[1:0], data_i};
	iec_clk_sr  <= {iec_clk_sr[1:0], clk_i};
end
wire iec_data = iec_data_sr[2];
wire iec_clk  = iec_clk_sr[2];

// Reset generation and serial/tape bus output assignments
wire reset_key;
wire reset   = i_reset | reset_key;
wire reset_l = ~reset;
wire ena_4;
wire motor;
wire [7:0] via1_pa_out;
wire serial_atn_out_l  = via1_pa_out[7];
wire serial_clk_out_l;
wire serial_data_out_l;
assign o_ce_pix   = ena_4;
assign cass_motor = motor;
assign atn_o      = ~serial_atn_out_l;
assign clk_o      = ~serial_clk_out_l;
assign data_o     = ~serial_data_out_l;

// Serial bus attention line and VIA #1 Port A input bus
wire serial_clk_in  = ~serial_clk_out_l & iec_clk;
wire serial_data_in = ~serial_data_out_l & iec_data;
wire serial_atn_in  = ~serial_atn_out_l;

// Clock generator module instantiation
wire reset_l_sampled;
VIC20_CLOCKS u_clocks (
	.I_SYSCLK(i_sysclk),
	.I_SYSCLK_EN(i_sysclk_en),
	.I_RESET_L(reset_l),
	.O_ENA(ena_4),
	.O_RESET_L(reset_l_sampled)
);

// Clock enable and bidirectional CPU data bus multiplexing
wire        ena_1mhz;
wire        c_rw_l;
wire [7:0]  c_dout;
reg  [7:0]  c_din;
wire        c_ena   = ena_1mhz & ena_4;
wire [7:0]  c_din_s = ~c_rw_l ? c_dout : c_din;
wire [23:0] c_addr;
wire        c_irq_l;
wire        c_nmi_l;

// 6502 CPU core instantiation
T65 cpu (
	.Mode(2'b00),
	.BCD_en(1'b1),
	.Res_n(reset_l_sampled),
	.Enable(c_ena),
	.Clk(i_sysclk),
	.Rdy(1'b1),
	.Abort_n(1'b1),
	.IRQ_n(c_irq_l),
	.NMI_n(c_nmi_l),
	.SO_n(1'b1),
	.R_W_n(c_rw_l),
	.Sync(),
	.EF(),
	.MF(),
	.XF(),
	.ML_n(),
	.VP_n(),
	.VDA(),
	.VPA(),
	.A(c_addr),
	.DI(c_din_s),
	.DO(c_dout),
	.Regs(),
	.DEBUG(),
	.NMI_ack()
);

// Video memory and bus control wires
wire        p2_h;
wire        p2_h_rise;
wire        p2_h_fall;
wire        v_rw_l;
wire [13:0] v_addr;
wire [13:0] vic_addr;
wire [11:0] vic_din;
wire [7:0]  vic_dout;
wire        vic_oe_l;
wire [3:0]  video_r;
wire [3:0]  video_g;
wire [3:0]  video_b;
wire        hsync;
wire        vsync;
assign      o_p2h = p2_h;

// VIC-I video and audio controller instantiation
M6561 vic (
	.I_CLK(i_sysclk),
	.I_ENA_4(ena_4),
	.I_RESET_L(reset_l),
	.O_ENA_1MHZ(ena_1mhz),
	.O_P2_H(p2_h),
	.O_P2_H_RISE(p2_h_rise),
	.O_P2_H_FALL(p2_h_fall),
	.I_RW_L(v_rw_l),
	.I_ADDR(v_addr),
	.O_ADDR(vic_addr),
	.I_DATA(vic_din),
	.O_DATA(vic_dout),
	.O_DATA_OE_L(vic_oe_l),
	.O_AUDIO(o_audio),
	.O_VIDEO_R(video_r),
	.O_VIDEO_G(video_g),
	.O_VIDEO_B(video_b),
	.O_HSYNC(hsync),
	.O_VSYNC(vsync),
	.O_COMP_SYNC_L(),
	.O_HBLANK(o_hblank),
	.O_VBLANK(o_vblank),
	.I_CENTER(i_center),
	.I_PAL(i_pal),
	.I_WIDE(i_wide),
	.I_LIGHT_PEN(i_fire),
	.I_POTX(i_potx),
	.I_POTY(i_poty)
);

// IO block select signals ($9000-$9FFF)
wire [3:0] io_sel_l = (c_addr[15:12] == 4'b1001) ? ~(4'b0001 << c_addr[11:10]) : 4'b1111;

// Block select decoding ($0000-$FFFF in 8KB blocks)
wire [7:0] blk_sel_l    = ~(8'b00000001 << c_addr[15:13]);
assign     o_blk123_sel = ~c_addr[15] & (c_addr[14] | c_addr[13]);
assign     o_blk5_sel   = ~blk_sel_l[5];

// Video and CPU shared memory bus multiplexer
wire [7:0] v_data_read_mux;
wire [7:0] v_data         = p2_h ? c_dout : v_data_read_mux;
assign     v_rw_l         = p2_h ? c_rw_l : 1'b1;
assign     v_addr         = p2_h ? {blk_sel_l[4], c_addr[12:0]} : vic_addr;
wire       col_ram_sel_l  = p2_h ? io_sel_l[1] : 1'b0;

// Internal RAM block select decoding ($0000-$1FFF in 1KB blocks)
wire       ram_en       = (p2_h & ~blk_sel_l[0]) | (~p2_h & v_addr[13]);
wire [7:0] ram_sel_l    = ram_en ? ~(8'b00000001 << v_addr[12:10]) : 8'hFF;
assign     o_ram123_sel = ~(ram_sel_l[1] & ram_sel_l[2] & ram_sel_l[3]);

// VIA #1 Port A input bus and IRQ wire
wire [7:0] via1_dout;
wire [7:0] via1_pa_in = {serial_atn_in, cass_sw, i_fire, i_joy[2:0], serial_data_in, serial_clk_in};
wire       via1_nmi;
wire       keybd_restore;

// VIA #1 (System/User port and serial bus controller) instantiation
via6522 via1 (
	.clock(i_sysclk),
	.rising(p2_h_rise),
	.falling(p2_h_fall),
	.reset(~reset_l_sampled),
	.addr(c_addr[3:0]),
	.wen(c_addr[4] & ~io_sel_l[0] & ~c_rw_l),
	.ren(c_addr[4] & ~io_sel_l[0] & c_rw_l),
	.data_in(v_data),
	.data_out(via1_dout),
	.phi2_ref(),
	.port_a_o(via1_pa_out),
	.port_a_t(),
	.port_a_i(via1_pa_in),
	.port_b_o(),
	.port_b_t(),
	.port_b_i(8'h00),
	.ca1_i(keybd_restore),
	.ca2_o(motor),
	.ca2_i(motor),
	.ca2_t(),
	.cb1_o(),
	.cb1_i(1'b0),
	.cb1_t(),
	.cb2_o(),
	.cb2_i(1'b0),
	.cb2_t(),
	.irq(via1_nmi)
);

// VIA #2 Keyboard and cassette interface wires
wire [7:0] via2_dout;
wire [7:0] keybd_row_out;
wire [7:0] keybd_row_oe;
wire [7:0] keybd_row_in;
wire [7:0] keybd_col_out;
wire [7:0] keybd_col_oe;
wire [7:0] keybd_col_in;
wire       via2_irq;

// VIA #2 (Keyboard matrix and serial/cassette interface) instantiation
via6522 via2 (
	.clock(i_sysclk),
	.rising(p2_h_rise),
	.falling(p2_h_fall),
	.reset(~reset_l_sampled),
	.addr(c_addr[3:0]),
	.wen(c_addr[5] & ~io_sel_l[0] & ~c_rw_l),
	.ren(c_addr[5] & ~io_sel_l[0] & c_rw_l),
	.data_in(v_data),
	.data_out(via2_dout),
	.phi2_ref(),
	.port_a_o(keybd_row_out),
	.port_a_t(keybd_row_oe),
	.port_a_i(keybd_row_in),
	.port_b_o(keybd_col_out),
	.port_b_t(keybd_col_oe),
	.port_b_i({i_joy[3] & keybd_col_in[7], keybd_col_in[6:0]}),
	.ca1_i(cass_read),
	.ca2_o(serial_clk_out_l),
	.ca2_i(serial_clk_out_l),
	.ca2_t(),
	.cb1_o(),
	.cb1_i(1'b1),
	.cb1_t(),
	.cb2_o(serial_data_out_l),
	.cb2_i(serial_data_out_l),
	.cb2_t(),
	.irq(via2_irq)
);

// Keyboard matrix wiring and swapping
wire [7:0] keybd_row_out_s = keybd_row_out | ~keybd_row_oe;
wire [7:0] keybd_col_out_s = keybd_col_out | ~keybd_col_oe;
assign cass_write = keybd_col_out[3];
wire [7:0] kbd_pao;
wire [7:0] kbd_pbo;
assign keybd_row_in = {kbd_pao[0], kbd_pao[6:1], kbd_pao[7]};
assign keybd_col_in = {kbd_pbo[3], kbd_pbo[6:4], kbd_pbo[7], kbd_pbo[2:0]};

// Keyboard controller instantiation
fpga64_keyboard keyboard (
	.clk(i_sysclk),
	.reset(1'b0),
	.ps2_key(ps2_key),
	.pai({keybd_row_out_s[0], keybd_row_out_s[6:1], keybd_row_out_s[7]}),
	.pbi({keybd_col_out_s[3], keybd_col_out_s[6:4], keybd_col_out_s[7], keybd_col_out_s[2:0]}),
	.pao(kbd_pao),
	.pbo(kbd_pbo),
	.reset_key(reset_key),
	.restore_key(keybd_restore),
	.mod_key(),
	.tape_play(tape_play),
	.backwardsReadingEnabled(1'b1)
);

// Interrupt request line resolution
assign c_irq_l = ~via2_irq;
assign c_nmi_l = ~via1_nmi;

// Memory bus read data wires
wire [3:0] col_ram_dout;
wire [7:0] ram0_dout;
wire [7:0] ram45_dout;
wire [7:0] ram67_dout;
wire [7:0] ramex0_dout;
wire [7:0] ramex1_dout;
wire [7:0] ramex2_dout;
wire [7:0] ramex3_dout;
wire [7:0] cart_dout;
wire [7:0] char_rom_dout;
wire [7:0] basic_rom_dout;
wire [7:0] pal_rom_dout_dl;
wire [7:0] ntsc_rom_dout_dl;
wire [7:0] pal_rom_dout_o;
wire [7:0] ntsc_rom_dout_o;

// VIC-I data input multiplexer (includes color RAM nibble during VIC cycles)
assign vic_din = {p2_h ? v_data[3:0] : col_ram_dout, v_data};

// Video bus data read routing and floating bus hold register
reg [7:0] v_data_read_mux_comb;
reg       v_data_oe_l;
reg [7:0] v_data_read_muxr;
assign    v_data_read_mux = v_data_read_mux_comb;

// Video bus data read routing
always @* begin
	v_data_oe_l = 1'b0;
	if (!col_ram_sel_l && p2_h)
		v_data_read_mux_comb = {v_data_read_muxr[7:4], col_ram_dout};
	else if (!vic_oe_l)
		v_data_read_mux_comb = vic_dout;
	else if (!ram_sel_l[0])
		v_data_read_mux_comb = ram0_dout;
	else if (!ram_sel_l[4] || !ram_sel_l[5])
		v_data_read_mux_comb = ram45_dout;
	else if (!ram_sel_l[6] || !ram_sel_l[7])
		v_data_read_mux_comb = ram67_dout;
	else if (v_addr[13:12] == 2'b00)
		v_data_read_mux_comb = char_rom_dout;
	else begin
		v_data_read_mux_comb = v_data_read_muxr;
		v_data_oe_l          = 1'b1;
	end
end

// Floating data bus latch
always @(posedge i_sysclk) begin
	if (ena_4) begin
		v_data_read_muxr <= v_data_read_mux;
	end
end

// CPU data read bus multiplexer
always @* begin
	if (!p2_h)
		c_din = 8'h00;
	else if (!io_sel_l[0] && c_addr[4])
		c_din = via1_dout;
	else if (!io_sel_l[0] && c_addr[5])
		c_din = via2_dout;
	else if (!blk_sel_l[6])
		c_din = basic_rom_dout;
	else if (!blk_sel_l[7])
		c_din = i_pal ? (pal_rom_dout_dl & pal_rom_dout_o) : (ntsc_rom_dout_dl & ntsc_rom_dout_o);
	else if (!v_data_oe_l)
		c_din = v_data_read_mux;
	else if (extmem)
		c_din = i_extmem_data;
	else if (i_extmem_en)
		c_din = 8'hFF;
	else if (~(&ram_sel_l[3:1]) && i_ram_ext[0])
		c_din = ramex0_dout;
	else if (!blk_sel_l[1] && i_ram_ext[1])
		c_din = ramex1_dout;
	else if (!blk_sel_l[2] && i_ram_ext[2])
		c_din = ramex2_dout;
	else if (!blk_sel_l[3] && i_ram_ext[3])
		c_din = ramex3_dout;
	else if (!blk_sel_l[5] && i_ram_ext[4])
		c_din = cart_dout;
	else
		c_din = 8'hFF;
end

// External memory and IO expansion bus control signals
wire extmem = i_extmem_en & ((~(&ram_sel_l[3:1]) & i_ram_ext[0]) |
                             (~blk_sel_l[1]      & i_ram_ext[1]) |
                             (~blk_sel_l[2]      & i_ram_ext[2]) |
                             (~blk_sel_l[3]      & i_ram_ext[3]) |
                             (~blk_sel_l[5]      & i_ram_ext[4]) |
                             ~io_sel_l[2] | ~io_sel_l[3]);
assign o_extmem_sel  = extmem & p2_h;
assign o_extmem_r_wn = c_rw_l | ~blk_sel_l[6] | ~blk_sel_l[7] | (~blk_sel_l[5] & i_ram_ext_ro[4]);
assign o_extmem_addr = c_addr[15:0];
assign o_extmem_data = c_dout;
assign o_io2_sel     = ~io_sel_l[2];
assign o_io3_sel     = ~io_sel_l[3];

// Internal 1KB low RAM ($0000-$03FF) instantiation
ram_conf_1024x8 #(
	.START_AI(6'b000000)
) rams0 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn(ram_sel_l[0]),
	.WRn(v_rw_l),
	.ADDR(v_addr[9:0]),
	.DIN(v_data),
	.DOUT(ram0_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Internal 2KB main RAM block 1 ($1000-$17FF) instantiation
ram_conf_2048x8 #(
	.START_AI(5'b00010)
) rams45 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.EN1n(ram_sel_l[4]),
	.EN2n(ram_sel_l[5]),
	.WRn(v_rw_l),
	.ADDR(v_addr[10:0]),
	.DIN(v_data),
	.DOUT(ram45_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Internal 2KB main RAM block 2 ($1800-$1FFF) instantiation
ram_conf_2048x8 #(
	.START_AI(5'b00011)
) rams67 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.EN1n(ram_sel_l[6]),
	.EN2n(ram_sel_l[7]),
	.WRn(v_rw_l),
	.ADDR(v_addr[10:0]),
	.DIN(v_data),
	.DOUT(ram67_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Internal 1x4-bit color RAM ($9400-$97FF) instantiation
ram_conf_1024x4 #(
	.START_AI(6'b100101)
) col_ram (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn(col_ram_sel_l),
	.WRn(v_rw_l),
	.ADDR(v_addr[9:0]),
	.DIN(v_data[3:0]),
	.DOUT(col_ram_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Expansion 3KB RAM ($0400-$0FFF) instantiation
ram_conf_8192x8 #(
	.START_AI(3'b000)
) ramex0 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn((&ram_sel_l[3:1]) | ~i_ram_ext[0] | ~p2_h),
	.WRn(c_rw_l | i_ram_ext_ro[0]),
	.ADDR(c_addr[12:0]),
	.DIN(c_dout),
	.DOUT(ramex0_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Expansion 8KB RAM Block 1 ($2000-$3FFF) instantiation
ram_conf_8192x8 #(
	.START_AI(3'b001)
) ramex1 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn(blk_sel_l[1] | ~i_ram_ext[1] | ~p2_h),
	.WRn(c_rw_l | i_ram_ext_ro[1]),
	.ADDR(c_addr[12:0]),
	.DIN(c_dout),
	.DOUT(ramex1_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Expansion 8KB RAM Block 2 ($4000-$5FFF) instantiation
ram_conf_8192x8 #(
	.START_AI(3'b010)
) ramex2 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn(blk_sel_l[2] | ~i_ram_ext[2] | ~p2_h),
	.WRn(c_rw_l | i_ram_ext_ro[2]),
	.ADDR(c_addr[12:0]),
	.DIN(c_dout),
	.DOUT(ramex2_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Expansion 8KB RAM Block 3 ($6000-$7FFF) instantiation
ram_conf_8192x8 #(
	.START_AI(3'b011)
) ramex3 (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn(blk_sel_l[3] | ~i_ram_ext[3] | ~p2_h),
	.WRn(c_rw_l | i_ram_ext_ro[3]),
	.ADDR(c_addr[12:0]),
	.DIN(c_dout),
	.DOUT(ramex3_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Cartridge RAM/ROM emulation block ($A000-$BFFF) instantiation
ram_conf_8192x8 #(
	.START_AI(3'b101)
) cart (
	.CLK(i_sysclk),
	.CLK_EN(ena_4),
	.ENn(blk_sel_l[5] | ~i_ram_ext[4] | ~p2_h),
	.WRn(c_rw_l | i_ram_ext_ro[4]),
	.ADDR(c_addr[12:0]),
	.DIN(c_dout),
	.DOUT(cart_dout),
	.CONF_CLK(conf_clk),
	.CONF_WR(conf_wr),
	.CONF_AI(conf_ai),
	.CONF_DI(conf_di)
);

// Character ROM ($8000-$8FFF) instantiation
gen_rom #(
	.INIT_FILE("rtl/roms/characters.901460-03.mif"),
	.ADDR_WIDTH(12)
) char_rom (
	.wrclock(i_sysclk),
	.wraddress(16'd0),
	.data(8'd0),
	.wren(1'b0),
	.rdclock(i_sysclk),
	.rdaddress(v_addr[11:0]),
	.q(char_rom_dout),
	.cs(1'b1)
);

// BASIC ROM ($C000-$DFFF) instantiation
gen_rom #(
	.INIT_FILE("rtl/roms/basic.901486-01.mif"),
	.ADDR_WIDTH(13)
) basic_rom (
	.wrclock(i_sysclk),
	.wraddress(16'd0),
	.data(8'd0),
	.wren(1'b0),
	.rdclock(i_sysclk),
	.rdaddress(c_addr[12:0]),
	.q(basic_rom_dout),
	.cs(1'b1)
);

// KERNAL ROM PAL download/internal copy ($E000-$FFFF) instantiation
gen_rom #(
	.INIT_FILE("rtl/roms/kernal.901486-07.mif"),
	.ADDR_WIDTH(13),
	.START_AI(3'b110)
) kernal_rom_pal (
	.wrclock(conf_clk),
	.wraddress(conf_ai),
	.data(conf_di),
	.wren(conf_wr),
	.rdclock(i_sysclk),
	.rdaddress(c_addr[12:0]),
	.q(pal_rom_dout_dl),
	.cs(~rom_std)
);

// KERNAL ROM NTSC download/internal copy ($E000-$FFFF) instantiation
gen_rom #(
	.INIT_FILE("rtl/roms/kernal.901486-06.mif"),
	.ADDR_WIDTH(13),
	.START_AI(3'b111)
) kernal_rom_ntsc (
	.wrclock(conf_clk),
	.wraddress(conf_ai),
	.data(conf_di),
	.wren(conf_wr),
	.rdclock(i_sysclk),
	.rdaddress(c_addr[12:0]),
	.q(ntsc_rom_dout_dl),
	.cs(~rom_std)
);

// KERNAL ROM PAL standard ROM copy ($E000-$FFFF) instantiation
gen_rom #(
	.INIT_FILE("rtl/roms/kernal.901486-07.mif"),
	.ADDR_WIDTH(13)
) kernal_rom_pal_o (
	.wrclock(i_sysclk),
	.wraddress(16'd0),
	.data(8'd0),
	.wren(1'b0),
	.rdclock(i_sysclk),
	.rdaddress(c_addr[12:0]),
	.q(pal_rom_dout_o),
	.cs(rom_std)
);

// KERNAL ROM NTSC standard ROM copy ($E000-$FFFF) instantiation
gen_rom #(
	.INIT_FILE("rtl/roms/kernal.901486-06.mif"),
	.ADDR_WIDTH(13)
) kernal_rom_ntsc_o (
	.wrclock(i_sysclk),
	.wraddress(16'd0),
	.data(8'd0),
	.wren(1'b0),
	.rdclock(i_sysclk),
	.rdaddress(c_addr[12:0]),
	.q(ntsc_rom_dout_o),
	.cs(rom_std)
);

// Video RGB and inverted sync output registration
always @(posedge i_sysclk) begin
	if (i_sysclk_en) begin
		o_video_r <= video_r;
		o_video_g <= video_g;
		o_video_b <= video_b;
		o_hsync   <= ~hsync;
		o_vsync   <= ~vsync;
	end
end

endmodule
/* verilator lint_on DECLFILENAME */
/* verilator lint_on PINCONNECTEMPTY */
/* verilator lint_on UNUSEDSIGNAL */
