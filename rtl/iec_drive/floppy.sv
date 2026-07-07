

//
// floppy.sv
//
// Copyright (c) 2015 Till Harbaum <till@harbaum.org>
// SystemVerilog conversion 2026
//
// This source file is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// 

module floppy (
	input  logic        clk,
	input  logic        clk8m_en,
	input  logic        select,
	input  logic        motor_on,
	input  logic        step_in,
	input  logic        step_out,
	input  logic [10:0] sector_len,
	input  logic        sector_base,
	input  logic  [4:0] spt,
	input  logic  [9:0] sector_gap_len,
	input  logic        hd,
	input  logic        fm,
	output logic        dclk_en,
	output logic  [6:0] track,
	output logic  [4:0] sector,
	output logic        sector_hdr,
	output logic        sector_data,
	output logic        ready,
	output logic        index
);

	// Set clock speed parameter
	parameter CLK_EN = 8000;

	// Physical constants for disk rates and mechanics
	localparam RATESD          = 125000;
	localparam RATEDD          = 250000;
	localparam RATEHD          = 500000;
	localparam RPM             = 300;
	localparam STEPBUSY        = 18;
	localparam SPINUP          = 500;
	localparam SPINDOWN        = 300;
	localparam INDEX_PULSE_LEN = 5;
	localparam SECTOR_HDR_LEN  = 6;
	localparam TRACKS          = 85;

	// Calculate bytes per track based on density rates
	localparam BPTSD = RATESD*60/(8*RPM);
	localparam BPTDD = RATEDD*60/(8*RPM);
	localparam BPTHD = RATEHD*60/(8*RPM);

	// Sector states definition
	localparam SECTOR_STATE_GAP  = 2'd0;
	localparam SECTOR_STATE_HDR  = 2'd1;
	localparam SECTOR_STATE_DATA = 2'd2;

	// Drive signals and state variables
	logic [1:0]  sec_state;
	logic [9:0]  sec_byte_cnt;
	logic [4:0]  current_sector = 5'd1;
	logic [4:0]  start_sector = 5'd1;
	logic [18:0] index_pulse_cnt;
	logic [6:0]  current_track = 7'd0;
	logic        step_inD;
	logic        step_outD;
	logic [19:0] step_busy;
	logic [14:0] byte_cnt;
	logic        index_pulse_start;
	logic        byte_clk_en;
	logic [2:0]  clk_cnt2;
	logic [31:0] spin_up_counter;
	logic [31:0] rate = 32'd0;
	logic        motor_onD;
	logic        data_clk;
	logic        data_clk_en;
	logic [31:0] clk_cnt;

	// Assign output properties for drive status and sector
	assign ready       = select && (rate == (fm ? RATESD : hd ? RATEHD : RATEDD));
	assign sector_hdr  = (sec_state == SECTOR_STATE_HDR);
	assign sector_data = (sec_state == SECTOR_STATE_DATA);
	assign track       = current_track;
	assign sector      = current_sector;
	assign dclk_en     = byte_clk_en;

	/* verilator lint_off UNUSEDSIGNAL */
	wire [10:0] unused_sector_len = sector_len;
	/* verilator lint_on UNUSEDSIGNAL */

	// Calculate step busy cycles in system clocks
	localparam [19:0] STEP_BUSY_CLKS = 20'(CLK_EN * STEPBUSY);

	// Generate index pulse timed to index cycles
	localparam INDEX_PULSE_CYCLES = INDEX_PULSE_LEN * CLK_EN;
	always_ff @(posedge clk) begin
		if (clk8m_en) begin
			if (index_pulse_start && (index_pulse_cnt == INDEX_PULSE_CYCLES[18:0] - 19'd1)) begin
				index <= 1'b0;
				index_pulse_cnt <= 19'd0;
			end else if (index_pulse_cnt == INDEX_PULSE_CYCLES[18:0] - 19'd1) begin
				index <= 1'b1;
			end else begin
				index_pulse_cnt <= index_pulse_cnt + 19'd1;
			end
		end
	end

	// Handle track head stepping movement and settle times
	always_ff @(posedge clk) begin
		step_inD  <= step_in;
		step_outD <= step_out;
		if (clk8m_en && step_busy != 20'd0) begin
			step_busy <= step_busy - 20'd1;
		end
		if (select) begin
			if (step_in && !step_inD) begin
				if (current_track != 7'd0) begin
					current_track <= current_track - 7'd1;
				end
				step_busy <= STEP_BUSY_CLKS;
			end
			if (step_out && !step_outD) begin
				if (current_track != TRACKS[6:0] - 7'd1) begin
					current_track <= current_track + 7'd1;
				end
				step_busy <= STEP_BUSY_CLKS;
			end
		end
	end

	// Handle current sector positioning and sector transitions
	always_ff @(posedge clk) begin
		if (byte_clk_en) begin
			if (index_pulse_start) begin
				sec_byte_cnt   <= sector_gap_len - 10'd1;
				sec_state      <= SECTOR_STATE_GAP;
				current_sector <= start_sector;
			end else begin
				if (sec_byte_cnt == 10'd0) begin
					case (sec_state)
						SECTOR_STATE_GAP: begin
							sec_state    <= SECTOR_STATE_HDR;
							sec_byte_cnt <= SECTOR_HDR_LEN[9:0] - 10'd1;
						end
						SECTOR_STATE_HDR: begin
							sec_state    <= SECTOR_STATE_DATA;
							sec_byte_cnt <= sector_len[9:0] - 10'd1;
						end
						SECTOR_STATE_DATA: begin
							sec_state    <= SECTOR_STATE_GAP;
							sec_byte_cnt <= sector_gap_len - 10'd1;
							if (current_sector == 5'(sector_base) + spt - 5'd1) begin
								current_sector <= 5'(sector_base);
							end else begin
								current_sector <= current_sector + 5'd1;
							end
						end
						default: begin
							sec_state <= SECTOR_STATE_GAP;
						end
					endcase
				end else begin
					sec_byte_cnt <= sec_byte_cnt - 10'd1;
				end
			end
		end
	end

	// Drive byte clock sequencer timed to speed rates
	always_ff @(posedge clk) begin
		if (byte_clk_en) begin
			index_pulse_start <= 1'b0;
			if (byte_cnt == (15'(fm ? BPTSD : hd ? BPTHD : BPTDD) - 15'd1)) begin
				byte_cnt          <= 15'd0;
				index_pulse_start <= 1'b1;
			end else begin
				byte_cnt <= byte_cnt + 15'd1;
			end
		end
	end

	// Divide down data clock to produce byte enable rate
	always_ff @(posedge clk) begin
		byte_clk_en <= 1'b0;
		if (data_clk_en) begin
			clk_cnt2 <= clk_cnt2 + 3'd1;
			if (clk_cnt2 == 3'b011) begin
				byte_clk_en <= 1'b1;
			end
		end
	end

	// Simulate motor spinning up and down with select timing
	wire motor_on_sel = motor_on && select;
	localparam SPIN_UP_CLKS   = CLK_EN * SPINUP;
	localparam SPIN_DOWN_CLKS = CLK_EN * SPINDOWN;
	always_ff @(posedge clk) begin
		motor_onD <= motor_on_sel;
		if (motor_onD != motor_on_sel) begin
			spin_up_counter <= 32'd0;
		end else if (clk8m_en) begin
			spin_up_counter <= spin_up_counter + 32'(fm ? RATESD : hd ? RATEHD : RATEDD);
			if (motor_on_sel) begin
				if (spin_up_counter > SPIN_UP_CLKS[31:0]) begin
					if (rate < 32'(fm ? RATESD : hd ? RATEHD : RATEDD)) begin
						rate <= rate + 32'd1;
					end
					spin_up_counter <= spin_up_counter - 32'(SPIN_UP_CLKS - (fm ? RATESD : hd ? RATEHD : RATEDD));
				end
			end else begin
				if (spin_up_counter > SPIN_DOWN_CLKS[31:0]) begin
					if (rate > 32'd0) begin
						rate <= rate - 32'd1;
					end
					spin_up_counter <= spin_up_counter - 32'(SPIN_DOWN_CLKS - (fm ? RATESD : hd ? RATEHD : RATEDD));
				end
			end
		end
	end

	// Generate bit data clock from motor speeds
	always_ff @(posedge clk) begin
		data_clk_en <= 1'b0;
		if (clk8m_en) begin
			if (clk_cnt + rate > 32'(CLK_EN * 1000 / 2)) begin
				clk_cnt     <= clk_cnt - 32'(CLK_EN * 1000 / 2 - rate);
				data_clk    <= !data_clk;
				if (~data_clk) begin
					data_clk_en <= 1'b1;
				end
			end else begin
				clk_cnt <= clk_cnt + rate;
			end
		end
	end

endmodule
