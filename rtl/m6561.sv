// --
// -- A model of the 6561 PAL VIC chip
// --
// -- Fully functional and tested against a real chip.
// --
// -- POTX/Y not implemented
// -- light pen may not be correct
// -- 
// -- All rights reserved
// -- (c) copyright 2003-2009 by MikeJ (Mike Johnson)
// -- http://www.FPGAArcade.com - mikej <at> fpgaarcade <dot> com
// -- (c) copyright 2011...2015 by WoS (Wolfgang Scherr)
// -- http://www.pin4.at - WoS <at> pin4 <dot> at
// --
// -- $Id: m6561.vhd 1328 2015-05-22 19:29:53Z wolfgang.scherr $
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

// 6561 PAL/NTSC Video Interface Chip (VIC-I) module
/* verilator lint_off DECLFILENAME */
/* verilator lint_off UNUSEDSIGNAL */
module M6561 (
	input  wire        I_CLK,
	input  wire        I_ENA_4,
	input  wire        I_RESET_L,
	output wire        O_ENA_1MHZ,
	output wire        O_P2_H,
	output wire        O_P2_H_RISE,
	output wire        O_P2_H_FALL,
	input  wire        I_RW_L,
	input  wire [13:0] I_ADDR,
	output wire [13:0] O_ADDR,
	input  wire [11:0] I_DATA,
	output reg  [7:0]  O_DATA,
	output wire        O_DATA_OE_L,
	output reg  [5:0]  O_AUDIO,
	output reg  [3:0]  O_VIDEO_R,
	output reg  [3:0]  O_VIDEO_G,
	output reg  [3:0]  O_VIDEO_B,
	output wire        O_HSYNC,
	output wire        O_VSYNC,
	output wire        O_COMP_SYNC_L,
	output reg         O_HBLANK,
	output reg         O_VBLANK,
	input  wire [1:0]  I_CENTER,
	input  wire        I_PAL,
	input  wire        I_WIDE,
	input  wire        I_LIGHT_PEN,
	input  wire [7:0]  I_POTX,
	input  wire [7:0]  I_POTY
);

// Video timing parameters based on PAL/NTSC selection
wire [8:0] CLOCKS_PER_LINE_M1 = I_PAL ? 9'd283 : 9'd259;
wire [8:0] TOTAL_LINES_M1     = I_PAL ? 9'd311 : 9'd260;
wire [8:0] H_START_M          = I_PAL ? 9'd47  : 9'd43;
wire [8:0] H_END_M            = I_PAL ? 9'd275 : 9'd247;
wire [8:0] V_START            = I_PAL ? 9'd28  : 9'd16;
wire [8:0] H_START_OFF        = I_PAL ? 9'd20  : 9'd0;
wire [8:0] H_END_OFF          = I_PAL ? 9'd20  : 9'd0;
wire [8:0] H_START_M1         = I_WIDE ? (H_START_M + H_START_OFF) : H_START_M;
wire [8:0] H_END_M1           = I_WIDE ? (H_END_M - H_END_OFF) : H_END_M;

// Internal timing registers and clocking strobes
reg [8:0] hcnt;
reg [8:0] vcnt;
reg [8:0] vcnt_c;
wire p2_h_int     = ~hcnt[1];
wire ena_1mhz_int = hcnt[0] & p2_h_int;

// Clock generation and strobe output assignments
assign O_P2_H_RISE = I_ENA_4 & (&hcnt[1:0]);
assign O_P2_H_FALL = I_ENA_4 & (hcnt[1:0] == 2'b01);
assign O_ENA_1MHZ  = ena_1mhz_int;
assign O_P2_H      = p2_h_int;

// CPU read/write chip select and data output enable
wire cs = (I_ADDR[13:8] == 6'b010000) & p2_h_int;
assign O_DATA_OE_L = ~(I_RW_L & cs);

// Internal CPU-accessible control and offset registers
reg       r_interlaced;
reg [6:0] r_x_offset;
reg [7:0] r_y_offset;
reg [6:0] r_num_cols;
reg [6:0] r_num_cols_latch;
reg [5:0] r_num_rows;
reg [5:0] r_num_rows_latch;
reg       r_charsize;
reg [4:0] r_screen_mem;
reg [3:0] r_char_mem;
reg [7:0] r_x_lightpen;
reg [7:0] r_y_lightpen;
reg [6:0] r_bass_freq;
reg [6:0] r_alto_freq;
reg [6:0] r_soprano_freq;
reg [6:0] r_noise_freq;
reg       r_bass_enabled;
reg       r_alto_enabled;
reg       r_soprano_enabled;
reg       r_noise_enabled;
reg [3:0] r_amplitude;
reg [3:0] r_aux_colour;
reg [2:0] r_border_colour;
reg       r_reverse_mode;
reg [3:0] r_backgnd_colour;
reg [6:0] c_x_offset;
reg [7:0] c_y_offset;

// Combinatorial calculations for centered horizontal and vertical offsets
wire [8:0] calc_x_diff = (H_END_M1 - H_START_M1) - {I_DATA[5:0], 3'b000};
wire [8:0] calc_x_temp = H_START_M1 + {1'b0, calc_x_diff[8:1]} - 9'd24;
wire [8:0] calc_y_rows = {I_DATA[6:1], 3'b000} + (I_DATA[0] ? {I_DATA[6:1], 3'b000} : 9'd0);
wire [8:0] calc_y_diff = (TOTAL_LINES_M1 - V_START) - calc_y_rows;
wire [8:0] calc_y_temp = V_START + {1'b0, calc_y_diff[8:1]};

// CPU register write operations
always @(posedge I_CLK or negedge I_RESET_L) begin
	if (!I_RESET_L) begin
		r_interlaced      <= 1'b0;
		r_x_offset        <= 7'd12;
		r_y_offset        <= 8'd38;
		r_num_cols        <= 7'd22;
		r_num_rows        <= 6'd23;
		r_charsize        <= 1'b0;
		r_screen_mem      <= 5'b11111;
		r_char_mem        <= 4'b0000;
		r_bass_freq       <= 7'd0;
		r_alto_freq       <= 7'd0;
		r_soprano_freq    <= 7'd0;
		r_noise_freq      <= 7'd0;
		r_bass_enabled    <= 1'b0;
		r_alto_enabled    <= 1'b0;
		r_soprano_enabled <= 1'b0;
		r_noise_enabled   <= 1'b0;
		r_amplitude       <= 4'd0;
		r_aux_colour      <= 4'd0;
		r_border_colour   <= 3'b011;
		r_reverse_mode    <= 1'b1;
		r_backgnd_colour  <= 4'b0001;
	end else if (I_ENA_4 && ena_1mhz_int && !I_RW_L && cs) begin
		case (I_ADDR[3:0])
			4'h0: begin
				r_interlaced <= I_DATA[7];
				r_x_offset   <= I_DATA[6:0];
			end
			4'h1: r_y_offset <= I_DATA[7:0];
			4'h2: begin
				r_screen_mem[0] <= I_DATA[7];
				r_num_cols      <= I_DATA[6:0];
				c_x_offset      <= calc_x_temp[8:2];
			end
			4'h3: begin
				r_num_rows <= I_DATA[6:1];
				r_charsize <= I_DATA[0];
				c_y_offset <= calc_y_temp[8:1];
			end
			4'h5: begin
				r_screen_mem[4:1] <= I_DATA[7:4];
				r_char_mem[3:0]   <= I_DATA[3:0];
			end
			4'hA: begin
				r_bass_enabled <= I_DATA[7];
				r_bass_freq    <= I_DATA[6:0];
			end
			4'hB: begin
				r_alto_enabled <= I_DATA[7];
				r_alto_freq    <= I_DATA[6:0];
			end
			4'hC: begin
				r_soprano_enabled <= I_DATA[7];
				r_soprano_freq    <= I_DATA[6:0];
			end
			4'hD: begin
				r_noise_enabled <= I_DATA[7];
				r_noise_freq    <= I_DATA[6:0];
			end
			4'hE: begin
				r_aux_colour <= I_DATA[7:4];
				r_amplitude  <= I_DATA[3:0];
			end
			4'hF: begin
				r_backgnd_colour <= I_DATA[7:4];
				r_reverse_mode   <= I_DATA[3];
				r_border_colour  <= I_DATA[2:0];
			end
			default: ;
		endcase
	end
end

// CPU register read multiplexer
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		case (I_ADDR[3:0])
			4'h0: O_DATA <= {r_interlaced, r_x_offset};
			4'h1: O_DATA <= r_y_offset;
			4'h2: O_DATA <= {r_screen_mem[0], r_num_cols};
			4'h3: O_DATA <= {vcnt[0], r_num_rows, r_charsize};
			4'h4: O_DATA <= vcnt[8:1];
			4'h5: O_DATA <= {r_screen_mem[4:1], r_char_mem[3:0]};
			4'h6: O_DATA <= r_x_lightpen;
			4'h7: O_DATA <= r_y_lightpen;
			4'h8: O_DATA <= I_POTX;
			4'h9: O_DATA <= I_POTY;
			4'hA: O_DATA <= {r_bass_enabled, r_bass_freq};
			4'hB: O_DATA <= {r_alto_enabled, r_alto_freq};
			4'hC: O_DATA <= {r_soprano_enabled, r_soprano_freq};
			4'hD: O_DATA <= {r_noise_enabled, r_noise_freq};
			4'hE: O_DATA <= {r_aux_colour, r_amplitude};
			4'hF: O_DATA <= {r_backgnd_colour, r_reverse_mode, r_border_colour};
			default: ;
		endcase
	end
end

// Video line and frame boundary indicators
wire h_cnt_last = (hcnt == CLOCKS_PER_LINE_M1);
wire v_cnt_last = (vcnt == TOTAL_LINES_M1);
wire [8:0] hcnt_next = h_cnt_last ? 9'd0 : (hcnt + 9'd1);
wire [8:0] vcnt_next = (h_cnt_last & v_cnt_last) ? 9'd0 : (h_cnt_last ? (vcnt + 9'd1) : vcnt);

// Combinatorial calculation for vertical centering counter
wire [8:0] calc_vcnt_raw = vcnt_next + {c_y_offset, 1'b0} - {r_y_offset, 1'b0};
wire [8:0] calc_vcnt_next = (calc_vcnt_raw > 9'd412) ? (calc_vcnt_raw + TOTAL_LINES_M1 + 9'd1) :
                            ((calc_vcnt_raw > TOTAL_LINES_M1) ? (calc_vcnt_raw - TOTAL_LINES_M1 - 9'd1) : calc_vcnt_raw);

// Horizontal and vertical video counter progression
always @(posedge I_CLK or negedge I_RESET_L) begin
	if (!I_RESET_L) begin
		hcnt   <= 9'd0;
		vcnt   <= 9'd0;
		vcnt_c <= 9'd0;
	end else if (I_ENA_4) begin
		hcnt <= hcnt_next;
		vcnt <= vcnt_next;
		if (h_cnt_last) begin
			vcnt_c <= calc_vcnt_next;
		end
	end
end

// Vertical synchronization signals for standard and centered modes
wire vsync   = ~|vcnt[8:2];
wire vsync_c = ~|vcnt_c[8:2];

// Composite and vertical sync outputs
assign O_HSYNC       = hsync;
assign O_VSYNC       = I_CENTER[1] ? vsync_c : vsync;
assign O_COMP_SYNC_L = ~O_VSYNC & ~hsync;

// Internal sync and blanking flip-flops
reg hblank;
reg vblank;
reg vblank_c;
reg hsync;

// Horizontal and vertical sync and blanking generator
always @(posedge I_CLK or negedge I_RESET_L) begin
	if (!I_RESET_L) begin
		hblank   <= 1'b1;
		hsync    <= 1'b1;
		vblank   <= 1'b1;
		vblank_c <= 1'b1;
	end else if (I_ENA_4) begin
		if (hcnt == H_END_M1)
			hblank <= 1'b1;
		else if (hcnt == H_START_M1)
			hblank <= 1'b0;

		if (h_cnt_last)
			hsync <= 1'b1;
		else if (hcnt == 9'd19)
			hsync <= 1'b0;

		if (h_cnt_last) begin
			if (v_cnt_last)
				vblank <= 1'b1;
			else if (vcnt == V_START)
				vblank <= 1'b0;

			if (vcnt_c == TOTAL_LINES_M1)
				vblank_c <= 1'b1;
			else if (vcnt_c == V_START)
				vblank_c <= 1'b0;
		end
	end
end

// Internal video timing and matrix registers
reg [6:0] num_cols;
reg [8:0] h_char_cnt;
reg [8:0] h_char_cnt_r;
reg [3:0] row_count;
reg [3:0] row_count_r;
reg [5:0] row_char;
reg [5:0] row_char_r;
reg       v_active;
reg       v_active_r;
reg       h_active;
reg       h_active_r;
reg       h_row_active;
reg       h_row_active_r;

// Combinatorial boundaries and character row flags
wire start_h     = I_CENTER[0] ? (hcnt[8:2] == c_x_offset) : ((hcnt[8:2] - (I_PAL ? 7'd0 : 7'd3)) == r_x_offset);
wire end_h       = (h_char_cnt_r == {num_cols[5:0], 3'b000});
wire start_v     = (vcnt[8:1] == r_y_offset) & ~p2_h_int;
wire end_v       = (row_char_r == r_num_rows_latch);
wire v_char_last = r_charsize ? (&row_count_r[3:0]) : (&row_count_r[2:0]);

// Combinatorial character grid state evaluation
always @* begin
	v_active     = v_active_r;
	row_count    = row_count_r;
	row_char     = row_char_r;
	h_active     = h_active_r;
	h_char_cnt   = h_char_cnt_r;
	h_row_active = h_row_active_r;
	num_cols     = r_num_cols_latch;

	if (~|hcnt[8:1]) begin
		if (!I_RW_L && cs && (I_ADDR[3:0] == 4'h2))
			num_cols = I_DATA[6:0];
		else
			num_cols = r_num_cols;
	end

	if (start_v && !v_active_r) begin
		v_active  = 1'b1;
		row_count = 4'd0;
		row_char  = 6'd0;
	end
	if (end_v || v_cnt_last) begin
		v_active  = 1'b0;
		row_count = 4'd0;
		row_char  = 6'd0;
	end

	if (h_active_r) begin
		if (end_h || (~|hcnt)) begin
			h_active   = 1'b0;
			h_char_cnt = 9'd0;
		end else begin
			h_char_cnt = h_char_cnt_r + 9'd1;
		end
	end

	if (h_row_active_r && (~|hcnt)) begin
		if (v_char_last) begin
			row_count = 4'd0;
			row_char  = row_char_r + 6'd1;
		end else begin
			row_count = row_count_r + 4'd1;
		end
		h_row_active = 1'b0;
	end

	if ((hcnt[1:0] == 2'b00) && start_h && v_active && !h_active_r) begin
		h_active     = 1'b1;
		h_row_active = 1'b1;
		h_char_cnt   = 9'd0;
	end
end

// Synchronous registration of character grid counters and flags
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		r_num_cols_latch <= num_cols;
		if ((hcnt[8:1] == 8'd3) && (~|vcnt)) begin
			r_num_rows_latch <= r_num_rows;
		end
		v_active_r     <= v_active;
		h_active_r     <= h_active;
		h_row_active_r <= h_row_active;
		h_char_cnt_r   <= h_char_cnt;
		row_count_r    <= row_count;
		row_char_r     <= row_char;
	end
end

// Pipeline registers for matrix and character fetch timing
reg [13:0] matrix_cnt;
reg [13:0] last_matrix_cnt;
reg [3:0]  din_reg_cell;
reg [7:0]  din_reg_char;
reg        doing_cell;
reg [13:0] cell_addr;
reg        h_activeD;
reg        h_activeD2;
reg        h_activeD3;
reg        h_activeD4;
reg        h_row_activeD;
reg        h_row_activeD2;
reg        v_activeD;
reg        v_activeD2;
reg        v_activeD3;
reg        start_hD;
reg        start_hD2;
reg        start_hD3;
reg        v_char_lastD;
reg        v_char_lastD2;

// Character data load strobe generator
wire char_load = h_activeD3 & v_activeD3 & (&hcnt[1:0]) & ~doing_cell;

// Video memory address multiplexer for screen and character data
assign O_ADDR = doing_cell ? (matrix_cnt + {r_screen_mem, 9'd0}) : ({r_char_mem, 10'd0} + cell_addr);

// Video matrix and character address counter pipeline
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		if (&hcnt[1:0]) begin
			h_activeD     <= h_active;
			h_activeD2    <= h_activeD;
			h_activeD3    <= h_activeD2;
			h_row_activeD <= h_row_active;
			h_row_activeD2<= h_row_activeD;
			v_activeD     <= v_active;
			v_activeD2    <= v_activeD;
			v_activeD3    <= v_activeD2;
		end

		if (~|hcnt[1:0]) begin
			start_hD      <= start_h;
			start_hD2     <= start_hD;
			start_hD3     <= start_hD2;
			v_char_lastD  <= v_char_last;
			v_char_lastD2 <= v_char_lastD;
		end

		h_activeD4 <= h_activeD3;

		if (v_cnt_last && h_cnt_last) begin
			last_matrix_cnt <= 14'd0;
		end else if ((&hcnt[1:0]) && v_char_lastD2 && h_row_activeD2) begin
			last_matrix_cnt <= matrix_cnt;
		end

		if (start_hD3 && v_activeD3 && !h_activeD4) begin
			matrix_cnt <= last_matrix_cnt;
			doing_cell <= 1'b1;
		end else if (char_load) begin
			matrix_cnt <= matrix_cnt + 14'd1;
		end

		if ((&hcnt[1:0]) && h_activeD3) begin
			doing_cell <= ~doing_cell;
			if (doing_cell) begin
				if (r_charsize)
					cell_addr <= {2'b00, I_DATA[7:0], row_count[3:0]};
				else
					cell_addr <= {3'b000, I_DATA[7:0], row_count[2:0]};
			end
		end

		if (&hcnt[1:0]) begin
			if (doing_cell)
				din_reg_cell <= I_DATA[11:8];
			else
				din_reg_char <= I_DATA[7:0];
		end
	end
end

// Character serializer and color shift register state
reg [3:0] op_cnt;
reg [3:0] op_cnt_r;
reg [7:0] op_reg;
reg       op_multi;
reg       op_multi_r;
reg [2:0] op_col;
reg [2:0] op_col_r;
reg       border_n;
reg       char_loadD;
reg       char_loadD2;
reg       char_loadD3;
reg       char_loadD4;

// Character generation and shift register pipeline
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		if (char_load)
			op_cnt_r <= 4'b1000;

		char_loadD  <= char_load;
		char_loadD2 <= char_loadD;
		char_loadD3 <= char_loadD2;
		char_loadD4 <= char_loadD3;

		if (char_loadD4) begin
			op_cnt     <= op_cnt_r;
			op_reg     <= din_reg_char;
			op_multi_r <= din_reg_cell[3];
			op_col_r   <= din_reg_cell[2:0];
		end else if (op_cnt[3]) begin
			op_cnt <= op_cnt + 4'd1;
		end
	end
end

// Internal pixel bit selection registers
reg       bit_sel;
reg [1:0] bit_sel_m;

// Character bit selection and multicolor pixel serialization
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		border_n <= op_cnt[3];
		op_multi <= op_multi_r;
		op_col   <= op_col_r;
		bit_sel  <= op_reg[~op_cnt[2:0]];
		case (op_cnt[2:1])
			2'b00: bit_sel_m <= op_reg[7:6];
			2'b01: bit_sel_m <= op_reg[5:4];
			2'b10: bit_sel_m <= op_reg[3:2];
			2'b11: bit_sel_m <= op_reg[1:0];
		endcase
	end
end

// Multicolor and reverse video color code decoder
wire [1:0] bit_sel_final = !border_n ? 2'b01 : (op_multi ? bit_sel_m : {(bit_sel ^ ~r_reverse_mode), 1'b0});

// Color index selection based on decoded pixel type
reg [3:0] col_mux_sel;
always @* begin
	case (bit_sel_final)
		2'b00: col_mux_sel = r_backgnd_colour;
		2'b01: col_mux_sel = {1'b0, r_border_colour};
		2'b10: col_mux_sel = {1'b0, op_col};
		2'b11: col_mux_sel = r_aux_colour;
	endcase
end

// VIC-I 16-color palette RGB mapping
reg [11:0] col_rgb;
always @* begin
	case (col_mux_sel)
		4'h0: col_rgb = 12'h000;
		4'h1: col_rgb = 12'hFFF;
		4'h2: col_rgb = 12'hB11;
		4'h3: col_rgb = 12'h5ED;
		4'h4: col_rgb = 12'hC3D;
		4'h5: col_rgb = 12'h4E3;
		4'h6: col_rgb = 12'h33C;
		4'h7: col_rgb = 12'hDE2;
		4'h8: col_rgb = 12'hC60;
		4'h9: col_rgb = 12'hEB8;
		4'hA: col_rgb = 12'hE99;
		4'hB: col_rgb = 12'hAFF;
		4'hC: col_rgb = 12'hEAE;
		4'hD: col_rgb = 12'hAFA;
		4'hE: col_rgb = 12'hA9E;
		4'hF: col_rgb = 12'hFFA;
	endcase
end

// Video color output registers and blanking synchronization
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		O_HBLANK  <= hblank;
		O_VBLANK  <= I_CENTER[1] ? vblank_c : vblank;
		O_VIDEO_R <= col_rgb[11:8];
		O_VIDEO_G <= col_rgb[7:4];
		O_VIDEO_B <= col_rgb[3:0];
	end
end

// Light pen edge detection registers
reg light_pen_in_t1;
reg light_pen_in_t2;

// Light pen input edge detector and coordinate capture
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		light_pen_in_t1 <= I_LIGHT_PEN;
		light_pen_in_t2 <= light_pen_in_t1;
		if (light_pen_in_t2 && !light_pen_in_t1) begin
			r_x_lightpen <= hcnt[8:1];
			r_y_lightpen <= vcnt[8:1];
		end
	end
end

// Audio frequency divider registers and strobes
reg [5:0] audio_div;
reg       audio_div_64;
reg       audio_div_32;
reg       audio_div_16;
reg       audio_div_8;

// Audio frequency divider registers
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		audio_div    <= audio_div + 6'd1;
		audio_div_64 <= ~|audio_div[5:0];
		audio_div_32 <= ~|audio_div[4:0];
		audio_div_16 <= ~|audio_div[3:0];
		audio_div_8  <= ~|audio_div[2:0];
	end
end

// Audio tone generator shift registers and counters
reg       bass_sg;
reg [6:0] bass_sg_cnt;
reg [7:0] bass_sg_sreg;
reg       alto_sg;
reg [6:0] alto_sg_cnt;
reg [7:0] alto_sg_sreg;
reg       soprano_sg;
reg [6:0] soprano_sg_cnt;
reg [7:0] soprano_sg_sreg;
reg       noise_sg;
reg [6:0] noise_sg_cnt;
reg [7:0] noise_sg_sreg;
reg [15:0] noise_LFSR;

// Tone and LFSR noise generators
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		if (audio_div_64) begin
			if (&bass_sg_cnt) begin
				bass_sg_cnt  <= r_bass_freq + 7'd1;
				bass_sg_sreg <= {bass_sg_sreg[6:0], ~bass_sg_sreg[7] & r_bass_enabled};
			end else begin
				bass_sg_cnt <= bass_sg_cnt + 7'd1;
			end
		end
		bass_sg <= bass_sg_sreg[0];

		if (audio_div_32) begin
			if (&alto_sg_cnt) begin
				alto_sg_cnt  <= r_alto_freq + 7'd1;
				alto_sg_sreg <= {alto_sg_sreg[6:0], ~alto_sg_sreg[7] & r_alto_enabled};
			end else begin
				alto_sg_cnt <= alto_sg_cnt + 7'd1;
			end
		end
		alto_sg <= alto_sg_sreg[0];

		if (audio_div_16) begin
			if (&soprano_sg_cnt) begin
				soprano_sg_cnt  <= r_soprano_freq + 7'd1;
				soprano_sg_sreg <= {soprano_sg_sreg[6:0], ~soprano_sg_sreg[7] & r_soprano_enabled};
			end else begin
				soprano_sg_cnt <= soprano_sg_cnt + 7'd1;
			end
		end
		soprano_sg <= soprano_sg_sreg[0];

		if (audio_div_8) begin
			if (&noise_sg_cnt) begin
				noise_sg_cnt <= r_noise_freq + 7'd1;
				if (noise_LFSR[0]) begin
					noise_sg_sreg <= {noise_sg_sreg[6:0], ~noise_sg_sreg[7] & r_noise_enabled};
				end
				noise_LFSR[15:1] <= noise_LFSR[14:0];
				noise_LFSR[0]    <= ~(~(noise_LFSR[3] ^ noise_LFSR[12] ^ noise_LFSR[14] ^ noise_LFSR[15]) & r_noise_enabled);
			end else begin
				noise_sg_cnt <= noise_sg_cnt + 7'd1;
			end
		end
		noise_sg <= noise_sg_sreg[0];
	end
end

// Audio voice contribution calculations
wire [5:0] wave_max = {2'b00, r_amplitude};
wire [5:0] wave_mid = {3'b000, r_amplitude[3:1]};
wire [5:0] val_bass    = r_bass_enabled    ? (bass_sg    ? wave_max : 6'd0) : wave_mid;
wire [5:0] val_alto    = r_alto_enabled    ? (alto_sg    ? wave_max : 6'd0) : wave_mid;
wire [5:0] val_soprano = r_soprano_enabled ? (soprano_sg ? wave_max : 6'd0) : wave_mid;
wire [5:0] val_noise   = noise_sg ? wave_max : (!r_noise_enabled ? wave_mid : 6'd0);

// Four-voice audio summation and output register
always @(posedge I_CLK) begin
	if (I_ENA_4) begin
		O_AUDIO <= val_bass + val_alto + val_soprano + val_noise;
	end
end

endmodule
/* verilator lint_on DECLFILENAME */
/* verilator lint_on UNUSEDSIGNAL */
