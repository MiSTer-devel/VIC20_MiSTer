//============================================================================
//  C16
//
//  Port to MiSTer
//  Copyright (C) 2017,2018 Sorgelig
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [44:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	output  [7:0] VIDEO_ARX,
	output  [7:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)
	input         TAPE_IN,

	// SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE
);

assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = 0;
 
assign LED_USER  = ioctl_download | led_disk;
assign LED_DISK  = 0;
assign LED_POWER = 0;

assign VIDEO_ARX = status[1] ? 8'd16 : 8'd4;
assign VIDEO_ARY = status[1] ? 8'd9  : 8'd3; 

wire [1:0] scale = status[3:2];

`include "build_id.v" 
parameter CONF_STR = {
	"VIC20;;",
	"-;",
	"F,PRG;",
	"F,CRT,Load Cart;",
	"F,CR4CR6CRACRB,Load Cart;",
	"S,D64;",
	"-;",
	"O1,Aspect ratio,4:3,16:9;",
	"O23,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%;",
	"OCD,Screen center,Both,None,Horz,Vert;",
	"-;",
	"O6,ExtRAM 1,Off,$0400(3KB);",
	"O78,ExtRAM 2,Off,$2000-$3FFF(8KB),$2000-$5FFF(16KB),$2000-$7FFF(24KB);",
	"O9,ExtRAM 3,Off,$A000(8KB);",
	"OB,Cart is writable,No,Yes;", 
	"R0,Reset;",
	"J,Fire;",
	"V,v1.00.",`BUILD_DATE
};

wire      extram1 = status[6];
reg [2:0] extram2;
wire      extram3 = status[9];

always_comb begin
	case(status[8:7])
		0: extram2 <= 0;
		1: extram2 <= 1;
		2: extram2 <= 3;
		3: extram2 <= 7;
	endcase
end

/////////////////  CLOCKS  ////////////////////////

wire clk_sys;
wire clk_v20;

pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_v20),
	.outclk_1(clk_sys)
);

reg v20_en = 0;
always @(negedge clk_v20) begin
	reg [1:0] div = 0;

	div <= div + 1'd1;
	v20_en <= !div;
end


/////////////////  HPS  ///////////////////////////

wire [31:0] status;
wire  [1:0] buttons;

wire [15:0] joya, joyb;
wire [10:0] ps2_key;

wire        ioctl_download;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        forced_scandoubler;

wire [31:0] sd_lba;
wire        sd_rd;
wire        sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire        img_mounted;
wire        img_readonly;

hps_io #(.STRLEN($size(CONF_STR)>>3)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.conf_str(CONF_STR),

	.buttons(buttons),
	.status(status),
	.forced_scandoubler(forced_scandoubler),

	.ps2_key(ps2_key),

	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),
	.img_mounted(img_mounted),
	.img_readonly(img_readonly),

	.joystick_0(joya),
	.joystick_1(joyb)
);

/////////////////  RESET  /////////////////////////

wire sys_reset = RESET | status[0] | buttons[1];
wire reset = sys_reset | cart_reset;

////////////////  LOADING  ////////////////////////

reg  [15:0] dl_addr;
reg   [7:0] dl_data;
reg         dl_wr;
reg         cart_reset = 0;
reg   [4:0] cart_blk = 0;

always @(posedge clk_sys) begin
	reg        old_download = 0;
	reg  [3:0] state = 0;
	reg [15:0] addr;

	dl_wr <= 0;
	old_download <= ioctl_download;

	if(ioctl_download && (ioctl_index == 1)) begin
		state <= 0;
		if(ioctl_wr) begin
			     if(ioctl_addr == 0) addr[7:0]  <= ioctl_dout;
			else if(ioctl_addr == 1) addr[15:8] <= ioctl_dout;
			else begin
				if(addr<'hA000) begin
					dl_addr <= addr;
					dl_data <= ioctl_dout;
					dl_wr   <= 1;
					addr    <= addr + 1'd1;
				end
			end
		end
	end

	if(old_download && ~ioctl_download && (ioctl_index == 1)) state <= 1;
	if(state) state <= state + 1'd1;

	case(state)
		 1: begin dl_addr <= 16'h2d; dl_data <= addr[7:0];  dl_wr <= 1; end
		 3: begin dl_addr <= 16'h2e; dl_data <= addr[15:8]; dl_wr <= 1; end
		 5: begin dl_addr <= 16'h2f; dl_data <= addr[7:0];  dl_wr <= 1; end
		 7: begin dl_addr <= 16'h30; dl_data <= addr[15:8]; dl_wr <= 1; end
		 9: begin dl_addr <= 16'h31; dl_data <= addr[7:0];  dl_wr <= 1; end
		11: begin dl_addr <= 16'h32; dl_data <= addr[15:8]; dl_wr <= 1; end
		13: begin dl_addr <= 16'hae; dl_data <= addr[7:0];  dl_wr <= 1; end
		15: begin dl_addr <= 16'haf; dl_data <= addr[15:8]; dl_wr <= 1; end
	endcase

	if(ioctl_download && !ioctl_index) begin
		state <= 0;
		if(ioctl_wr) begin
			if(ioctl_addr>='h4000 && ioctl_addr<'h8000) begin
				dl_addr <= ioctl_addr[15:0] + 16'h8000;
				dl_data <= ioctl_dout;
				dl_wr   <= 1;
			end
		end
	end

	if(ioctl_download && (ioctl_index[4:1] == 1)) begin
		if(ioctl_wr) begin
			if(ioctl_addr == 0 && ioctl_index == 2) begin
				addr[7:0]  <= ioctl_dout;
			end
			else if(ioctl_addr == 1 && ioctl_index == 2) begin
				addr[15:8] <= ioctl_dout;
			end
			else if(ioctl_addr == 0 && ioctl_index[4:0] == 3) begin
				case(ioctl_index[7:6])
					0: {dl_addr,addr} <= {16'h4000,16'h4001};
					1: {dl_addr,addr} <= {16'h6000,16'h6001};
					2: {dl_addr,addr} <= {16'hA000,16'hA001};
					3: {dl_addr,addr} <= {16'hB000,16'hB001};
				endcase
				dl_data <= ioctl_dout;
				dl_wr   <= 1;
			end
			else if(addr < 'hC000) begin
				if(addr[15:13] == 3'b000) cart_blk[0] <= 1;
				if(addr[15:13] == 3'b001) cart_blk[1] <= 1;
				if(addr[15:13] == 3'b010) cart_blk[2] <= 1;
				if(addr[15:13] == 3'b011) cart_blk[3] <= 1;
				if(addr[15:13] == 3'b101) cart_blk[4] <= 1;
				dl_addr <= addr;
				dl_data <= ioctl_dout;
				dl_wr   <= 1;
				addr    <= addr + 1'd1;
			end
		end
	end

	if(old_download && ~ioctl_download && (ioctl_index[4:0] == 2)) cart_reset <= 0;
	if(sys_reset) {cart_reset, cart_blk} <= 0;
end


///////////////////////////////////////////////////

wire [15:0] joy = joya | joyb;

reg [10:0] v20_key;
always @(posedge clk_v20) begin
	reg [10:0] key;
	
	key <= ps2_key;
	v20_key <= key;
end

VIC20 VIC20
(
	.i_sysclk(clk_v20),
	.i_sysclk_en(v20_en),
	.i_reset(reset),

	//IEC
	.atn_o(v20_iec_atn_o),
	.atn_i(!v20_iec_atn_i),
	.clk_o(v20_iec_clk_o),
	.clk_i(!v20_iec_clk_i),
	.data_o(v20_iec_data_o),
	.data_i(!v20_iec_data_i),

	.i_joy(~{joy[0],joy[1],joy[2],joy[3]}),
	.i_fire(~joy[4]),

	.i_ram_ext_ro(cart_blk & ~{5{status[11]}}),
	.i_ram_ext({extram3,extram2,extram1}|cart_blk),

	.o_ce_pix(ce_pix),
	.o_video_r(r),
	.o_video_g(g),
	.o_video_b(b),
	.o_hsync(hs),
	.o_vsync(vs),
	.o_hblank(hblank),
	.o_vblank(vblank),
	.i_center(status[13:12]+2'b11),

	.ps2_key(v20_key),

	.o_audio(audio),
	
	.conf_clk(clk_sys),
	.conf_ai(dl_addr),
	.conf_di(dl_data),
	.conf_wr(dl_wr)
);

wire v20_iec_atn_o;
wire v20_iec_data_o;
wire v20_iec_clk_o;

reg v20_iec_atn_i;
reg v20_iec_data_i; 
reg v20_iec_clk_i;

always @(posedge clk_v20) begin
	reg iec_atn_d1,iec_atn_d2;
	reg iec_data_d1,iec_data_d2;
	reg iec_clk_d1,iec_clk_d2;

	iec_atn_d1 <=iec_atn_i;
	iec_data_d1<=iec_data_i;
	iec_clk_d1 <=iec_clk_i;
	
	iec_atn_d2 <=iec_atn_d1;
	iec_data_d2<=iec_data_d1;
	iec_clk_d2 <=iec_clk_d1;

	v20_iec_atn_i <=iec_atn_d2;
	v20_iec_data_i<=iec_data_d2;
	v20_iec_clk_i <=iec_clk_d2;
end

wire [15:0] audio;

assign AUDIO_L = audio;
assign AUDIO_R = AUDIO_L;
assign AUDIO_MIX = 0;
assign AUDIO_S = 0;

wire hs, vs, hblank, vblank, ce_pix;
wire [3:0] r,g,b;

wire scandoubler = scale || forced_scandoubler;

wire ce_sd;
assign CLK_VIDEO = clk_v20;
assign CE_PIXEL = scandoubler ? ce_sd : v20_en;

video_mixer #(256, 1) mixer
(
	.clk_sys(CLK_VIDEO),
	
	.ce_pix(ce_pix),
	.ce_pix_out(ce_sd),

	.hq2x(scale == 1),
	.scanlines({scale==3, scale==2}),
	.scandoubler(scandoubler),

	.R(r),
	.G(g),
	.B(b),

	.mono(0),

	.HSync(~hs),
	.VSync(~vs),
	.HBlank(hblank),
	.VBlank(vblank),

	.VGA_R(VGA_R),
	.VGA_G(VGA_G),
	.VGA_B(VGA_B),
	.VGA_VS(VGA_VS),
	.VGA_HS(VGA_HS),
	.VGA_DE(VGA_DE)
);

///////////////////////////////////////////////////

wire led_disk;

wire c1541_iec_atn_o;
wire c1541_iec_data_o;
wire c1541_iec_clk_o;

wire iec_atn_i  = v20_iec_atn_o  | c1541_iec_atn_o;
wire iec_data_i = v20_iec_data_o | c1541_iec_data_o;
wire iec_clk_i  = v20_iec_clk_o  | c1541_iec_clk_o;

reg c1541_iec_atn_i;
reg c1541_iec_data_i; 
reg c1541_iec_clk_i;

always @(posedge clk_sys) begin
	reg iec_atn_d1,iec_atn_d2;
	reg iec_data_d1,iec_data_d2; 
	reg iec_clk_d1,iec_clk_d2;

	iec_atn_d1 <=iec_atn_i;
	iec_data_d1<=iec_data_i;
	iec_clk_d1 <=iec_clk_i;
	
	iec_atn_d2 <=iec_atn_d1;
	iec_data_d2<=iec_data_d1;
	iec_clk_d2 <=iec_clk_d1;

	c1541_iec_atn_i <=iec_atn_d2;
	c1541_iec_data_i<=iec_data_d2; 
	c1541_iec_clk_i <=iec_clk_d2;
end

c1541_sd c1541_sd
(
	.clk32 (clk_sys),
	.reset (reset),

	.c1541rom_clk(clk_sys),
	.c1541rom_addr(ioctl_addr[13:0]),
	.c1541rom_data(ioctl_dout),
	.c1541rom_wr(ioctl_wr && (ioctl_addr[24:14] == 0) && !ioctl_index),

   .disk_change ( img_mounted ),
	.disk_readonly ( img_readonly ),

	.iec_atn_i  ( c1541_iec_atn_i  ),
	.iec_data_i ( c1541_iec_data_i ),
	.iec_clk_i  ( c1541_iec_clk_i  ),

	.iec_atn_o  ( c1541_iec_atn_o  ),
	.iec_data_o ( c1541_iec_data_o ),
	.iec_clk_o  ( c1541_iec_clk_o  ),

   .led (led_disk),

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),

	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr)
);

endmodule
