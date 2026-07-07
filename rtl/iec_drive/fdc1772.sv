

//
// fdc1772.sv
//
// Copyright (c) 2015 Till Harbaum <till@harbaum.org>
// SystemVerilog conversion 2026
//

/* verilator lint_off DECLFILENAME */
/* verilator lint_off WIDTHTRUNC */
/* verilator lint_off WIDTHEXPAND */
/* verilator lint_off UNUSEDSIGNAL */
/* verilator lint_off UNUSEDPARAM */

module fdc1772 (
	input  logic        clkcpu,
	input  logic        clk8m_en,
	input  logic  [W:0] floppy_drive,
	input  logic        floppy_side,
	input  logic        floppy_reset,
	output logic        floppy_step,
	input  logic        floppy_motor,
	output logic        floppy_ready,
	output logic        irq,
	output logic        drq,
	input  logic  [1:0] cpu_addr,
	input  logic        cpu_sel,
	input  logic        cpu_rw,
	input  logic  [7:0] cpu_din,
	output logic  [7:0] cpu_dout,
	input  logic  [W:0] img_mounted,
	input  logic  [W:0] img_wp,
	input  logic        img_ds,
	input  logic [31:0] img_size,
	output logic [31:0] sd_lba,
	output logic  [W:0] sd_rd,
	output logic  [W:0] sd_wr,
	input  logic        sd_ack,
	input  logic  [8:0] sd_buff_addr,
	input  logic  [7:0] sd_dout,
	output logic  [7:0] sd_din,
	input  logic        sd_dout_strobe
);

	// Clock and hardware model configuration parameters
	parameter CLK_EN           = 16'd8000;
	parameter FD_NUM           = 2;
	parameter MODEL            = 2;
	parameter EXT_MOTOR        = 1'b0;
	parameter INVERT_HEAD_RA   = 1'b0;

	// Image format types
	localparam IMG_ARCHIE      = 0;
	localparam IMG_ST          = 1;
	localparam IMG_BBC         = 2;
	localparam IMG_TI99        = 3;

	parameter  IMG_TYPE        = IMG_ARCHIE;

	localparam W    = FD_NUM - 1;
	localparam WIDX = $clog2(FD_NUM);

	// Floppy state structures per supported drive
	logic [10:0] fdn_sector_len[FD_NUM];
	logic  [4:0] fdn_spt[FD_NUM];
	logic  [9:0] fdn_gap_len[FD_NUM];
	logic        fdn_doubleside[FD_NUM];
	logic        fdn_hd[FD_NUM];
	logic        fdn_fm[FD_NUM];
	logic        fdn_present[FD_NUM];

	// Active virtual disk geometry parameters
	logic [11:0] image_sectors;
	logic [11:0] image_sps;
	logic  [4:0] image_spt;
	logic  [9:0] image_gap_len;
	logic        image_doubleside;
	wire         image_hd = img_size[20];
	logic        image_fm;

	// Sector layout code mapping size formats
	logic  [1:0] sector_size_code;
	logic [10:0] sector_size;
	logic        sector_base;

	// Active target floppy drive select index
	logic [WIDX:0] fdn;

	// Floppy drive demux signals
	wire       fd_any         = ~&floppy_drive;
	wire       fd_index       = fd_any ? fdn_index[fdn[WIDX:0]]       : 1'b0;
	wire       fd_ready       = fd_any ? fdn_ready[fdn[WIDX:0]]       : 1'b0;
	wire [6:0] fd_track       = fd_any ? fdn_track[fdn[WIDX:0]]       : 7'd0;
	wire [4:0] fd_sector      = fd_any ? fdn_sector[fdn[WIDX:0]]      : 5'd0;
	wire       fd_sector_hdr  = fd_any ? fdn_sector_hdr[fdn[WIDX:0]]  : 1'b0;
	wire       fd_dclk_en     = fd_any ? fdn_dclk[fdn[WIDX:0]]        : 1'b0;
	wire       fd_present     = fd_any ? fdn_present[fdn[WIDX:0]]     : 1'b0;
	wire       fd_writeprot   = fd_any ? img_wp[fdn[WIDX:0]]          : 1'b1;
	wire       fd_doubleside  = fdn_doubleside[fdn[WIDX:0]];
	wire [4:0] fd_spt         = fdn_spt[fdn[WIDX:0]];

	// Command register state flags
	logic [7:0] cmd;
	wire cmd_type_1 = (cmd[7] == 1'b0);
	wire cmd_type_2 = (cmd[7:6] == 2'b10);
	wire cmd_type_3 = (cmd[7:5] == 3'b111) || (cmd[7:4] == 4'b1100);
	wire cmd_type_4 = (cmd[7:4] == 4'b1101);

	// Sector target state registers
	logic [7:0] track;
	logic [7:0] sector;
	logic [7:0] data_in;
	logic [7:0] data_out;
	logic       step_dir;
	logic       motor_on = 1'b0;
	logic       data_lost;
	logic       s_odd;

	// Calculate sector LBA and map image dimensions based on host system
	always_comb begin
		case (IMG_TYPE)
			IMG_ARCHIE: begin
				sector_size_code = 2'd3;
				sector_base = 0;
				sd_lba = 32'({(16'd0 + (16'(fd_spt)*16'(track[6:0])) << fd_doubleside) + (floppy_side ? 16'd0 : 16'(fd_spt)) + 16'(sector[4:0]), s_odd});
				image_fm = 1'b0;
				image_sectors = img_size[21:10];
				image_doubleside = 1'b1;
				image_spt = image_hd ? 5'd10 : 5'd5;
				image_gap_len = 10'd220;
			end
			IMG_ST: begin
				sector_size_code = 2'd2;
				sector_base = 1;
				sd_lba = 32'(((32'(fd_spt)*32'(track[6:0])) << fd_doubleside) + (floppy_side ? 32'd0 : 32'(fd_spt)) + 32'(sector[4:0]) - 32'd1);
				image_fm = 1'b0;
				image_sectors = img_size[20:9];
				image_doubleside = 1'b0;
				image_sps = image_sectors;
				if (image_sectors > 12'(85*12)) begin
					image_doubleside = 1'b1;
					image_sps = image_sectors >> 1'b1;
				end
				if (image_hd) begin
					image_sps = image_sps >> 1'b1;
				end
				case (image_sps)
					12'd711,12'd720,12'd729,12'd738,12'd747,12'd756,12'd765 : image_spt = 5'd9;
					12'd790,12'd800,12'd810,12'd820,12'd830,12'd840,12'd850 : image_spt = 5'd10;
					12'd948,12'd960,12'd972,12'd984,12'd996,12'd1008,12'd1020: image_spt = 5'd12;
					default: image_spt = 5'd11;
				endcase
				if (image_hd) begin
					image_spt = image_spt << 1'b1;
				end
				case (image_spt)
					5'd9, 5'd18 : image_gap_len = 10'd176;
					5'd10,5'd20 : image_gap_len = 10'd107;
					5'd11,5'd22 : image_gap_len = 10'd50;
					default     : image_gap_len = 10'd2;
				endcase
			end
			IMG_BBC, IMG_TI99: begin
				sector_size_code = 2'd1;
				sector_base = 0;
				if (IMG_TYPE == IMG_BBC) begin
					sd_lba = 32'((((32'(fd_spt)*32'(track[6:0])) << fd_doubleside) + (floppy_side ? 32'd0 : 32'(fd_spt)) + 32'(sector[4:0])) >> 1);
					image_spt = 5'd10;
				end else begin
					sd_lba = 32'(((32'(fd_spt)*(floppy_side ? 32'(track[5:0]) : 32'(79-track[5:0]))) + 32'(sector[4:0])) >> 1);
					image_spt = 5'd9;
				end
				image_fm = 1'b1;
				image_sectors = img_size[19:8];
				image_doubleside = img_ds;
				if (img_ds) begin
					image_sps = image_sectors >> 1'b1;
				end else begin
					image_sps = image_sectors;
				end
				image_gap_len = 10'd50;
			end
			default: begin
				sector_size_code = 2'd0;
				sector_base = 0;
				sd_lba = 32'd0;
				image_fm = 1'b0;
				image_sectors = 12'd0;
				image_doubleside = 1'b0;
				image_spt = 5'd0;
				image_gap_len = 10'd0;
				image_sps = 12'd0;
			end
		endcase
		sector_size = 11'd128 << sector_size_code;
	end

	// Handle external virtual disk mounts onto drives
	always_ff @(posedge clkcpu) begin
		logic [W:0] img_mountedD;
		img_mountedD <= img_mounted;
		for (integer idx = 0; idx < FD_NUM; idx = idx + 1) begin
			if (~img_mountedD[idx[WIDX:0]] && img_mounted[idx[WIDX:0]]) begin
				fdn_present[idx]    <= |img_size;
				fdn_sector_len[idx] <= sector_size;
				fdn_spt[idx]         <= image_spt;
				fdn_gap_len[idx]     <= image_gap_len;
				fdn_doubleside[idx]  <= image_doubleside;
				fdn_hd[idx]          <= image_hd;
				fdn_fm[idx]          <= image_fm;
			end
		end
	end

	// Sample registers for CPU bus read and write cycles
	logic cpu_selD;
	logic cpu_rwD;
	always_ff @(posedge clkcpu) begin
		cpu_rwD  <= cpu_sel & ~cpu_rw;
		cpu_selD <= cpu_sel;
	end

	wire cpu_we = cpu_sel & ~cpu_rw & ~cpu_rwD;

	// Reset or set interrupt requests on host command actions
	logic irq_set;
	logic cpu_rw_cmdstatus;
	always_ff @(posedge clkcpu) begin
		cpu_rw_cmdstatus <= ~cpu_selD && cpu_sel && cpu_addr == FDC_REG_CMDSTATUS;
	end

	wire irq_clr = !floppy_reset || cpu_rw_cmdstatus;
	always_ff @(posedge clkcpu) begin
		if (irq_clr) irq <= 1'b0;
		else if (irq_set) irq <= 1'b1;
	end

	// Reset or set data request interrupt flags
	logic drq_set;
	logic cpu_rw_data;
	always_ff @(posedge clkcpu) begin
		cpu_rw_data <= ~cpu_selD && cpu_sel && cpu_addr == FDC_REG_DATA;
	end

	wire drq_clr = !floppy_reset || cpu_rw_data;
	always_ff @(posedge clkcpu) begin
		if (drq_clr) drq <= 1'b0;
		else if (drq_set) drq <= 1'b1;
	end

	// Core floppy virtual drive interface signals
	wire       fdn_index[FD_NUM];
	wire       fdn_ready[FD_NUM];
	wire [6:0] fdn_track[FD_NUM];
	wire [4:0] fdn_sector[FD_NUM];
	wire       fdn_sector_hdr[FD_NUM];
	wire       fdn_sector_data[FD_NUM];
	wire       fdn_dclk[FD_NUM];

	// Instantiate multiple floppy virtual controllers
	generate
		genvar i;
		for (i = 0; i < FD_NUM; i = i + 1) begin : fdd
			floppy #(.CLK_EN(CLK_EN)) floppy_inst (
				.clk            ( clkcpu                 ),
				.clk8m_en       ( clk8m_en               ),
				.select         ( fd_any && (fdn == WIDX'(i)) ),
				.motor_on       ( fd_motor               ),
				.step_in        ( step_in                ),
				.step_out       ( step_out               ),
				.sector_len     ( fdn_sector_len[i]      ),
				.spt            ( fdn_spt[i]             ),
				.sector_gap_len ( fdn_gap_len[i]         ),
				.sector_base    ( sector_base            ),
				.hd             ( fdn_hd[i]              ),
				.fm             ( fdn_fm[i]              ),
				.dclk_en        ( fdn_dclk[i]            ),
				.track          ( fdn_track[i]           ),
				.sector         ( fdn_sector[i]          ),
				.sector_hdr     ( fdn_sector_hdr[i]      ),
				.sector_data    ( fdn_sector_data[i]     ),
				.ready          ( fdn_ready[i]           ),
				.index          ( fdn_index[i]           )
			);
		end
	endgenerate

	// Dynamically select target virtual floppy drive index
	always_comb begin
		fdn = '0;
		for (integer idx = FD_NUM - 1; idx >= 0; idx = idx - 1) begin
			if (!floppy_drive[idx[WIDX:0]]) begin
				fdn = idx[WIDX:0];
			end
		end
	end

	assign floppy_ready = fd_ready && fd_present;

	// Track motor state, timeout rotation counters, and spin speedups
	localparam MOTOR_IDLE_COUNTER = 4'd10;
	logic [3:0] motor_timeout_index;
	logic       indexD;
	logic       busy;
	logic       step_in;
	logic       step_out;
	logic [3:0] motor_spin_up_sequence;

	wire fd_motor = EXT_MOTOR ? floppy_motor : motor_on;
	wire motor_spin_up_done = (!motor_on) || (motor_on && (motor_spin_up_sequence == 4'd0));

	// Calculate and track floppy physical step timing constants
	localparam STEP_PULSE_LEN = 16'd1;
	localparam STEP_PULSE_CLKS = STEP_PULSE_LEN * CLK_EN;
	logic [15:0] step_pulse_cnt;

	wire [15:0] step_rate_clk = 
		(cmd[1:0] == 2'b00)               ? 16'(16'd6  * CLK_EN - 16'd1) :
		(cmd[1:0] == 2'b01)               ? 16'(16'd12 * CLK_EN - 16'd1) :
		(MODEL == 2 && cmd[1:0] == 2'b10) ? 16'(16'd2  * CLK_EN - 16'd1) :
		(cmd[1:0] == 2'b10)               ? 16'(16'd20 * CLK_EN - 16'd1) :
		(MODEL == 2)                      ? 16'(16'd3  * CLK_EN - 16'd1) :
		                                    16'(16'd30 * CLK_EN - 16'd1);

	logic [15:0] step_rate_cnt;
	logic [23:0] delay_cnt;

	assign floppy_step = step_in | step_out;

	wire step_busy = (step_rate_cnt != 16'd0);
	wire delaying = (delay_cnt != 24'd0);
	wire fd_track0 = (fd_track == 7'd0);

	logic [7:0]  step_to;
	logic        RNF;
	logic        sector_inc_strobe;
	logic        track_inc_strobe;
	logic        track_dec_strobe;
	logic        track_clear_strobe;

	// Sequence state-machine events on system clock posedge
	always_ff @(posedge clkcpu) begin
		logic [1:0] seek_state;
		logic       notready_wait;
		logic       sector_not_found;
		logic       irq_at_index;
		logic [1:0] data_transfer_state;

		sector_inc_strobe   <= 1'b0;
		track_inc_strobe    <= 1'b0;
		track_dec_strobe    <= 1'b0;
		track_clear_strobe  <= 1'b0;
		irq_set             <= 1'b0;

		if (!floppy_reset) begin
			motor_on               <= 1'b0;
			busy                   <= 1'b0;
			step_in                <= 1'b0;
			step_out               <= 1'b0;
			sd_card_read           <= 1'b0;
			sd_card_write          <= 1'b0;
			data_transfer_start    <= 1'b0;
			seek_state             <= 2'b00;
			notready_wait          <= 1'b0;
			sector_not_found       <= 1'b0;
			irq_at_index           <= 1'b0;
			data_transfer_state    <= 2'b00;
			RNF                    <= 1'b0;
			step_pulse_cnt         <= 16'd0;
			step_rate_cnt          <= 16'd0;
			delay_cnt              <= 24'd0;
			motor_spin_up_sequence <= 4'd0;
		end else if (clk8m_en) begin
			sd_card_read        <= 1'b0;
			sd_card_write       <= 1'b0;
			data_transfer_start <= 1'b0;

			if (step_pulse_cnt != 16'd0) begin
				step_pulse_cnt <= step_pulse_cnt - 16'd1;
			end else begin
				step_in  <= 1'b0;
				step_out <= 1'b0;
			end

			if (step_rate_cnt != 16'd0) begin
				step_rate_cnt <= step_rate_cnt - 16'd1;
			end

			if (delay_cnt != 24'd0) begin
				delay_cnt <= delay_cnt - 24'd1;
			end

			if (cmd_rx) begin
				busy                <= 1'b1;
				notready_wait       <= 1'b0;
				sector_not_found    <= 1'b0;
				data_transfer_state <= 2'b00;

				if (cmd_type_1 || cmd_type_2 || cmd_type_3) begin
					RNF      <= 1'b0;
					motor_on <= 1'b1;
					if (!motor_on && !cmd[3]) begin
						motor_spin_up_sequence <= 4'd6;
					end
				end

				if (cmd_type_4) begin
					busy <= 1'b0;
					if (cmd[3]) irq_set <= 1'b1;
					if (cmd[3:2] == 2'b01) irq_at_index <= 1'b1;
					if (!busy) motor_on <= 1'b1;
				end
			end

			if (cmd_type_1 && busy) begin
				if (cmd[7:4] == 4'b0000) begin
					if (fd_track0) begin
						track_clear_strobe <= 1'b1;
						busy               <= 1'b0;
						irq_set            <= 1'b1;
					end else if (!step_busy) begin
						step_in       <= 1'b1;
						step_pulse_cnt  <= STEP_PULSE_CLKS[15:0] - 16'd1;
						step_rate_cnt <= step_rate_clk;
					end
				end

				if (cmd[7:4] == 4'b0001) begin
					if (track == step_to) begin
						busy    <= 1'b0;
						irq_set <= 1'b1;
					end else if (!step_busy) begin
						if (track < step_to) begin
							step_out         <= 1'b1;
							track_inc_strobe <= 1'b1;
							step_pulse_cnt   <= STEP_PULSE_CLKS[15:0] - 16'd1;
							step_rate_cnt    <= step_rate_clk;
						end else begin
							step_in          <= 1'b1;
							track_dec_strobe <= 1'b1;
							step_pulse_cnt   <= STEP_PULSE_CLKS[15:0] - 16'd1;
							step_rate_cnt    <= step_rate_clk;
						end
					end
				end

				if (cmd[7:5] == 3'b001) begin
					if (!step_busy) begin
						if (step_dir) begin
							step_out         <= 1'b1;
							track_inc_strobe <= cmd[4];
							step_pulse_cnt   <= STEP_PULSE_CLKS[15:0] - 16'd1;
							step_rate_cnt    <= step_rate_clk;
						end else begin
							step_in          <= 1'b1;
							track_dec_strobe <= cmd[4];
							step_pulse_cnt   <= STEP_PULSE_CLKS[15:0] - 16'd1;
							step_rate_cnt    <= step_rate_clk;
						end
						busy    <= 1'b0;
						irq_set <= 1'b1;
					end
				end

				if (cmd[7:5] == 3'b010) begin
					if (!step_busy) begin
						step_out         <= 1'b1;
						step_dir         <= 1'b1;
						track_inc_strobe <= cmd[4];
						step_pulse_cnt   <= STEP_PULSE_CLKS[15:0] - 16'd1;
						step_rate_cnt    <= step_rate_clk;
						busy             <= 1'b0;
						irq_set          <= 1'b1;
					end
				end

				if (cmd[7:5] == 3'b011) begin
					if (!step_busy) begin
						step_in          <= 1'b1;
						step_dir         <= 1'b0;
						track_dec_strobe <= cmd[4];
						step_pulse_cnt   <= STEP_PULSE_CLKS[15:0] - 16'd1;
						step_rate_cnt    <= step_rate_clk;
						busy             <= 1'b0;
						irq_set          <= 1'b1;
					end
				end
			end

			if (cmd_type_2 && busy && motor_spin_up_done) begin
				if (cmd[4]) begin
					if (!fd_ready) begin
						notready_wait <= 1'b1;
					end else if (notready_wait) begin
						RNF     <= 1'b1;
						busy    <= 1'b0;
						irq_set <= 1'b1;
					end
				end

				if (fd_ready) begin
					if ((sector - 8'(sector_base)) >= 8'(fd_spt)) begin
						sector_not_found <= 1'b1;
					end
					if (sector_not_found && ~indexD && fd_index) begin
						RNF     <= 1'b1;
						busy    <= 1'b0;
						irq_set <= 1'b1;
					end

					if (cmd[7:5] == 3'b100) begin
						case (data_transfer_state)
							2'b00: begin
								if (fd_ready && fd_sector_hdr && (8'(fd_sector) == sector)) begin
									sd_card_read        <= 1'b1;
									data_transfer_state <= 2'b01;
								end
							end
							2'b01: begin
								if (sd_state == SD_IDLE) begin
									data_transfer_start <= 1'b1;
									data_transfer_state <= 2'b10;
								end
							end
							2'b10: begin
								if (data_transfer_done) begin
									if (cmd[4]) begin
										sector_inc_strobe   <= 1'b1;
										data_transfer_state <= 2'b00;
									end else begin
										busy    <= 1'b0;
										irq_set <= 1'b1;
									end
								end
							end
							default: ;
						endcase
					end

					if (cmd[7:5] == 3'b101) begin
						case (data_transfer_state)
							2'b00: begin
								if (fifo_cpuptr == 11'd0 && fd_ready && fd_sector_hdr && (8'(fd_sector) == sector)) begin
									data_transfer_start <= 1'b1;
									data_transfer_state <= 2'b01;
								end
							end
							2'b01: begin
								if (data_transfer_done) begin
									sd_card_write       <= 1'b1;
									data_transfer_state <= 2'b10;
								end
							end
							2'b10: begin
								if (sd_state == SD_IDLE) begin
									if (cmd[4]) begin
										sector_inc_strobe   <= 1'b1;
										data_transfer_state <= 2'b00;
									end else begin
										busy    <= 1'b0;
										irq_set <= 1'b1;
									end
								end
							end
							default: ;
						endcase
					end
				end
			end

			if (cmd_type_3 && busy && motor_spin_up_done) begin
				if (cmd[7:4] == 4'b1100) begin
					case (data_transfer_state)
						2'b00: begin
							if (fd_ready && fd_sector_hdr) begin
								data_transfer_start <= 1'b1;
								data_transfer_state <= 2'b10;
							end
							if (~indexD && fd_index) begin
								sector_not_found <= 1'b1;
							end
							if (sector_not_found && ~indexD && fd_index) begin
								RNF     <= 1'b1;
								busy    <= 1'b0;
								irq_set <= 1'b1;
							end
						end
						2'b10: begin
							if (data_transfer_done) begin
								busy    <= 1'b0;
								irq_set <= 1'b1;
							end
						end
						default: ;
					endcase
				end

				if (cmd[7:4] == 4'b1110) begin
					busy    <= 1'b0;
					irq_set <= 1'b1;
				end

				if (cmd[7:4] == 4'b1111) begin
					busy    <= 1'b0;
					irq_set <= 1'b1;
				end
			end

			if (irq_at_index && ~indexD && fd_index) begin
				irq_set      <= 1'b1;
				irq_at_index <= 1'b0;
			end
		end
	end

	// Drive floppy motor spin timer limits and down periods
	always_ff @(posedge clkcpu) begin
		indexD <= fd_index;
		if (!floppy_reset) begin
			motor_timeout_index <= 4'd0;
		end else if (clk8m_en) begin
			if (indexD && !fd_index) begin
				if (motor_timeout_index != 4'd0) begin
					if (!busy) begin
						motor_timeout_index <= motor_timeout_index - 4'd1;
					end
				end else if (motor_on) begin
					motor_timeout_index <= MOTOR_IDLE_COUNTER;
				end
				if (motor_timeout_index == 4'd1) begin
					motor_on <= 1'b0;
				end
			end
			if (motor_spin_up_sequence != 4'd0) begin
				motor_spin_up_sequence <= motor_spin_up_sequence - 4'd1;
			end
		end
		if (busy) begin
			motor_timeout_index <= 4'd0;
		end
	end

	// State machine synchronization flags
	logic data_transfer_start;
	logic data_transfer_done;

	// Dual-port sector buffer memory pointers
	logic [10:0] fifo_cpuptr;
	logic [9:0]  fifo_cpuptr_adj;
	wire  [7:0]  fifo_q;
	logic [9:0]  fifo_sdptr;

	always_comb begin
		if (sector_size_code == 2'd3) begin
			fifo_sdptr = {s_odd, sd_buff_addr};
		end else begin
			fifo_sdptr = {1'b0, sd_buff_addr};
		end
		if (sector_size_code == 2'd1) begin
			fifo_cpuptr_adj = {1'b0, (fd_spt[0] & (track[0] ^ !floppy_side)) ^ sector[0], fifo_cpuptr[7:0]};
		end else begin
			fifo_cpuptr_adj = fifo_cpuptr[9:0];
		end
	end

	fdc1772_dpram #(8, 10) fifo (
		.clock     ( clkcpu           ),
		.address_a ( fifo_sdptr       ),
		.data_a    ( sd_dout          ),
		.wren_a    ( sd_dout_strobe & sd_ack ),
		.q_a       ( sd_din           ),
		.address_b ( fifo_cpuptr_adj  ),
		.data_b    ( data_in          ),
		.wren_b    ( data_in_strobe   ),
		.q_b       ( fifo_q           )
	);

	// SD memory read and write transfer cycles state parameters
	localparam SD_IDLE  = 2'd0;
	localparam SD_READ  = 2'd1;
	localparam SD_WRITE = 2'd2;

	logic [1:0] sd_state;
	logic       sd_card_write;
	logic       sd_card_read;

	// Coordinate sector SD access on system clock
	always_ff @(posedge clkcpu) begin
		logic sd_ackD;
		logic sd_card_readD;
		logic sd_card_writeD;

		sd_card_readD  <= sd_card_read;
		sd_card_writeD <= sd_card_write;
		sd_ackD        <= sd_ack;

		if (sd_ack) begin
			sd_rd <= '0;
			sd_wr <= '0;
		end

		case (sd_state)
			SD_IDLE: begin
				s_odd <= 1'b0;
				if (~sd_card_readD & sd_card_read) begin
					sd_rd[fdn[WIDX:0]] <= 1'b1;
					sd_state           <= SD_READ;
				end else if (~sd_card_writeD & sd_card_write) begin
					sd_wr[fdn[WIDX:0]] <= 1'b1;
					sd_state           <= SD_WRITE;
				end
			end
			SD_READ: begin
				if (sd_ackD & ~sd_ack) begin
					if (s_odd || sector_size_code != 2'd3) begin
						sd_state <= SD_IDLE;
					end else begin
						s_odd              <= 1'b1;
						sd_rd[fdn[WIDX:0]] <= 1'b1;
					end
				end
			end
			SD_WRITE: begin
				if (sd_ackD & ~sd_ack) begin
					if (s_odd || sector_size_code != 2'd3) begin
						sd_state <= SD_IDLE;
					end else begin
						s_odd              <= 1'b1;
						sd_wr[fdn[WIDX:0]] <= 1'b1;
					end
				end
			end
			default: begin
				sd_state <= SD_IDLE;
			end
		endcase
	end

	// CPU interface buffer and status timing logic
	logic data_in_strobe;
	logic data_in_valid;

	// Calculate 16-bit floppy data block CRC checksums
	function automatic [15:0] crc(input [15:0] curcrc, input [7:0] val);
		logic [15:0] temp_crc;
		temp_crc = {curcrc[15:8] ^ val, 8'h00};
		for (integer idx = 0; idx < 8; idx = idx + 1) begin
			if (temp_crc[15]) begin
				temp_crc = temp_crc << 1;
				temp_crc = temp_crc ^ 16'h1021;
			end else begin
				temp_crc = temp_crc << 1;
			end
		end
		crc = {curcrc[7:0] ^ temp_crc[15:8], curcrc[7:0]};
	endfunction

	// Align CPU register load and data requests
	always_ff @(posedge clkcpu) begin
		logic        data_transfer_startD;
		logic [10:0] data_transfer_cnt;
		logic [15:0] crcval;
		logic        crc_en;

		crc_en <= 1'b0;
		if (crc_en) begin
			crcval <= crc(crcval, data_out);
		end

		if (cpu_we && cpu_addr == FDC_REG_DATA) begin
			data_out      <= data_in;
			data_in_valid <= 1'b1;
		end

		if (cmd_rx || sector_inc_strobe) begin
			data_in_valid     <= 1'b0;
			data_transfer_cnt <= 11'd0;
			fifo_cpuptr       <= 11'd0;
		end

		drq_set <= 1'b0;
		if (clk8m_en) begin
			data_transfer_done <= 1'b0;
		end
		data_transfer_startD <= data_transfer_start;

		if (~data_transfer_startD & data_transfer_start) begin
			if (cmd[7:4] == 4'b1100) begin
				crcval            <= 16'hB230;
				data_transfer_cnt <= 11'd7;
			end
			if (cmd[7:6] == 2'b10) begin
				data_transfer_cnt <= sector_size + 11'd1;
			end
			if (cmd[7:5] == 3'b101) begin
				drq_set <= !data_in_valid;
			end
		end

		data_in_strobe <= 1'b0;
		if (cmd[7:5] == 3'b101 && data_in_strobe) begin
			fifo_cpuptr <= fifo_cpuptr + 11'd1;
		end

		if (fd_dclk_en) begin
			if (data_transfer_cnt != 11'd0) begin
				if (data_transfer_cnt != 11'd1) begin
					data_lost <= 1'b0;
					if (drq) begin
						data_lost <= 1'b1;
					end
					if (cmd[7:5] != 3'b101 || data_transfer_cnt != 11'd2) begin
						drq_set <= 1'b1;
					end

					if (cmd[7:4] == 4'b1100) begin
						case (data_transfer_cnt)
							11'd7: begin data_out <= {1'b0, fd_track}; crc_en <= 1'b1; end
							11'd6: begin data_out <= {7'b0000000, (INVERT_HEAD_RA != 0) ^ floppy_side}; crc_en <= 1'b1; end
							11'd5: begin data_out <= {3'b000, fd_sector}; crc_en <= 1'b1; end
							11'd4: begin data_out <= {6'b000000, sector_size_code[1:0]}; crc_en <= 1'b1; end
							11'd3: data_out <= crcval[15:8];
							11'd2: data_out <= crcval[7:0];
							default: ;
						endcase
					end

					if (cmd[7:5] == 3'b100 && fifo_cpuptr != sector_size) begin
						data_out    <= fifo_q;
						fifo_cpuptr <= fifo_cpuptr + 11'd1;
					end
					if (cmd[7:5] == 3'b101 && fifo_cpuptr != sector_size) begin
						data_in_strobe <= 1'b1;
						data_in_valid  <= 1'b0;
					end
				end

				data_transfer_cnt <= data_transfer_cnt - 11'd1;
				if (data_transfer_cnt == 11'd1) begin
					data_transfer_done <= 1'b1;
				end
			end
		end
	end

	// Status byte construction
	wire [7:0] status = {
		((MODEL == 1 || MODEL == 3) ? !floppy_ready : motor_on),
		((cmd[7:5] == 3'b101 || cmd[7:4] == 4'b1111 || cmd_type_1) && fd_writeprot),
		(cmd_type_1 ? motor_spin_up_done : 1'b0),
		RNF,
		1'b0,
		(cmd_type_1 ? fd_track0 : data_lost),
		(cmd_type_1 ? ~fd_index : drq),
		busy
	};

	localparam FDC_REG_CMDSTATUS    = 0;
	localparam FDC_REG_TRACK        = 1;
	localparam FDC_REG_SECTOR       = 2;
	localparam FDC_REG_DATA         = 3;

	// Synchronize CPU data output reads
	always_comb begin
		cpu_dout = 8'h00;
		if (cpu_sel && cpu_rw) begin
			case (cpu_addr)
				2'd0: cpu_dout = status;
				2'd1: cpu_dout = track;
				2'd2: cpu_dout = sector;
				2'd3: cpu_dout = data_out;
			endcase
		end
	end

	// Handle CPU register writes and floppy controller command flags
	logic cmd_rx;
	logic cmd_rx_i;

	always_ff @(posedge clkcpu) begin
		if (!floppy_reset) begin
			cmd                <= 8'h00;
			track              <= 8'h00;
			sector             <= 8'h00;
			data_in            <= 8'h00;
			cmd_rx_i           <= 1'b0;
			cmd_rx             <= 1'b0;
			step_to            <= 8'd0;
			step_dir           <= 1'b0;
		end else begin
			cmd_rx <= cmd_rx_i;
			if ((!cmd_type_4 && busy) || (clk8m_en && cmd_type_4 && !busy)) begin
				cmd_rx_i <= 1'b0;
			end

			if (cpu_we) begin
				if (cpu_addr == 2'd0) begin
					cmd      <= cpu_din;
					cmd_rx_i <= 1'b1;
					if (cpu_din[7:4] == 4'b0000) begin
						step_to <= 8'd0;
						track   <= 8'hff;
					end
					if (cpu_din[7:4] == 4'b0001) begin
						step_to <= data_in;
					end
				end
				if (cpu_addr == 2'd1) begin
					track <= cpu_din;
				end
				if (cpu_addr == 2'd2) begin
					sector <= cpu_din;
				end
				if (cpu_addr == 2'd3) begin
					data_in <= cpu_din;
				end
			end

			if (sector_inc_strobe) begin
				sector <= sector + 8'd1;
			end
			if (track_inc_strobe) begin
				track <= track + 8'd1;
			end
			if (track_dec_strobe) begin
				track <= track - 8'd1;
			end
			if (track_clear_strobe) begin
				track <= 8'd0;
			end
		end
	end

endmodule

module fdc1772_dpram #(parameter DATAWIDTH=8, ADDRWIDTH=9) (
	input  logic                   clock,
	input  logic   [ADDRWIDTH-1:0] address_a,
	input  logic   [DATAWIDTH-1:0] data_a,
	input  logic                   wren_a,
	output logic   [DATAWIDTH-1:0] q_a,
	input  logic   [ADDRWIDTH-1:0] address_b,
	input  logic   [DATAWIDTH-1:0] data_b,
	input  logic                   wren_b,
	output logic   [DATAWIDTH-1:0] q_b
);

	// This dual-port sector buffer (typically 1Kx8) was being turned into
	// hundreds of registers instead of M10K blocks.
	(* ramstyle = "M10K, no_rw_check" *) logic [DATAWIDTH-1:0] ram[0:(1<<ADDRWIDTH)-1];

	always_ff @(posedge clock) begin
		if (wren_a) begin
			ram[address_a] <= data_a;
			q_a            <= data_a;
		end else begin
			q_a <= ram[address_a];
		end
	end

	always_ff @(posedge clock) begin
		if (wren_b) begin
			ram[address_b] <= data_b;
			q_b            <= data_b;
		end else begin
			q_b <= ram[address_b];
		end
	end

endmodule
