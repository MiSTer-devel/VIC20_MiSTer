// ---------------------------------------------------------------------------------
// -- Commodore 1530 to SD card host (read only) by Dar (darfpga@aol.fr) 25-Mars-2019
// -- http://darfpga.blogspot.fr
// -- also darfpga on sourceforge
// --
// -- tap/wav player 
// -- Converted to 8 bit FIFO - Slingshot
// ---------------------------------------------------------------------------------

// -------------------------------------------------------------------------------
// -- Commodore 1530 to SD card host (read only) by Dar (darfpga@aol.fr) 25-Mars-2019
// -- http://darfpga.blogspot.fr
// -- also darfpga on sourceforge
// --
// -- tap/wav player 
// -- Converted to 8 bit FIFO - Slingshot
// ---------------------------------------------------------------------------------

// Commodore 1530 tape and host FIFO interface for VIC20

module c1530 (
	input        clk32,
	input        restart_tape,
	input        wav_mode,
	input  [1:0] tap_version,
	input  [7:0] host_tap_in,
	input        host_tap_wrreq,
	output       tap_fifo_wrfull,
	output reg   tap_fifo_error,
	input        osd_play_stop_toggle,
	output       cass_sense,
	output reg   cass_read,
	input        cass_write,  // from keyboard matrix - not used for tape
	input        cass_motor,
	output       cass_run,
	input        ear_input
);

localparam FIFO_DEPTH = 64;

// Force Intel Cyclone V hardware memory block usage
(* ramstyle = "M10K, no_rw_check" *) reg [7:0] tap_mem[FIFO_DEPTH-1:0];
reg [5:0] rdptr;
reg [5:0] wrptr;
reg [6:0] usedw;


// Wire declarations for FIFO status
wire fifo_empty;
wire fifo_full;
wire fifo_we;
wire fifo_re;

assign fifo_empty = (usedw == 0);
assign fifo_full  = usedw[6];
assign fifo_we    = host_tap_wrreq && !fifo_full;
assign fifo_re    = tap_fifo_rdreq && !fifo_empty;
assign tap_fifo_wrfull = fifo_full;

// Registered output data
reg [7:0] tap_fifo_do;

// Main tape playback, OSD control, EAR detection and motor momentum process
reg  [5:0]  tap_player_tick_cnt;
reg  [11:0] wav_player_tick_cnt;
reg  [23:0] wave_cnt;
reg  [23:0] wave_len;
reg  [7:0]  start_bytes;
reg         get_24bits_len;
reg         initial_delay;
reg         skip_bytes;
reg         playing;
reg         osd_play_stop_toggleD;
reg         sense;
reg         ear_inputD;
reg         ear_input_detected;
reg  [28:0] ear_autostop_counter;
reg         cass_motor_D;
reg         motor;
reg  [23:0] motor_counter;
reg         tap_fifo_rdreq;

always @(posedge clk32 or posedge restart_tape) begin
	if (restart_tape) begin
		start_bytes <= 8'h00;
		skip_bytes <= 1'b1;
		tap_player_tick_cnt <= 6'd0;
		wav_player_tick_cnt <= 12'd0;
		wave_len <= 24'h000004;
		wave_cnt <= 24'd0;
		get_24bits_len <= 1'b0;
		osd_play_stop_toggleD <= 1'b0;
		initial_delay <= 1'b1;
		tap_fifo_rdreq <= 1'b0;
		tap_fifo_error <= 1'b0;
		sense <= 1'b1;
		motor <= 1'b1;
		cass_read <= 1'b1;
		// Reset FIFO
		rdptr <= 0;
		wrptr <= 0;
		usedw <= 0;
	end else begin
		osd_play_stop_toggleD <= osd_play_stop_toggle;
		if (!osd_play_stop_toggleD && osd_play_stop_toggle) sense <= ~sense;

		ear_inputD <= ear_input;
		if (ear_inputD != ear_input) begin
			ear_input_detected <= 1'b1;
			ear_autostop_counter <= 29'd160000000;
		end

		if (ear_input_detected) begin
			sense <= 1'b0;
			cass_read <= ~ear_input;
			if (~|ear_autostop_counter) begin
				ear_input_detected <= 1'b0;
				sense <= 1'b1;
			end else begin
				ear_autostop_counter <= ear_autostop_counter - 29'd1;
			end
		end

		cass_motor_D <= cass_motor;
		if (cass_motor_D != cass_motor) begin
			motor_counter <= 24'd320000;
		end else if (|motor_counter) begin
			motor_counter <= motor_counter - 24'd1;
		end else begin
			motor <= cass_motor;
		end

		playing <= !motor && !sense && !ear_input_detected;
		if (!playing && !ear_input_detected) cass_read <= 1'b1;
		tap_fifo_rdreq <= 1'b0;

		// FIFO Write
		if (fifo_we) begin
			tap_mem[wrptr] <= host_tap_in;
		end

		// FIFO Read - data available next cycle
		if (fifo_re) begin
			tap_fifo_do <= tap_mem[rdptr];
		end

		// Pointer and counter management
		if (fifo_we && !fifo_re) begin
			usedw <= usedw + 7'd1;
			wrptr <= wrptr + 6'd1;
		end else if (fifo_re && !fifo_we) begin
			usedw <= usedw - 7'd1;
			rdptr <= rdptr + 6'd1;
		end else if (fifo_we && fifo_re) begin
			wrptr <= wrptr + 6'd1;
			rdptr <= rdptr + 6'd1;
		end

		if (playing && wav_mode) begin
			wav_player_tick_cnt <= wav_player_tick_cnt + 12'd1;
			if (wav_player_tick_cnt == 12'h2F0) begin
				wav_player_tick_cnt <= 12'd0;
				if (fifo_empty) tap_fifo_error <= 1'b1;
				else tap_fifo_rdreq <= 1'b1;
			end
			// Use registered data from previous read
			cass_read <= ~tap_fifo_do[7];
		end

		tap_player_tick_cnt <= tap_player_tick_cnt + 6'd1;
		if (playing && !wav_mode) begin
			if ((tap_player_tick_cnt == 6'b011111) && !skip_bytes) begin
				if (!tap_version[1]) cass_read <= (wave_cnt > {14'd0,wave_len[10:1]});
				tap_player_tick_cnt <= 6'd0;
				wave_cnt <= wave_cnt + 24'd1;
				if (wave_cnt == wave_len - 24'd1) begin
					wave_cnt <= 24'd0;
					if (tap_version == 2'd2) cass_read <= ~cass_read;
					if (fifo_empty) begin
						tap_fifo_error <= 1'b1;
					end else begin
						tap_fifo_rdreq <= 1'b1;
						if (~|tap_fifo_do) begin
							wave_len <= 24'h000100;
							get_24bits_len <= |tap_version;
						end else begin
							wave_len <= {13'd0,tap_fifo_do,3'b000};
							initial_delay <= 1'b0;
						end
					end
				end
			end

			if (get_24bits_len && !skip_bytes && tap_player_tick_cnt[0]) begin
				if (tap_player_tick_cnt == 6'b000101) get_24bits_len <= 1'b0;
				if (fifo_empty) begin
					tap_fifo_error <= 1'b1;
				end else begin
					tap_fifo_rdreq <= 1'b1;
					wave_len <= {tap_fifo_do,wave_len[23:8]};
					if (initial_delay) wave_len <= 24'h000004;
				end
				if (!tap_version[1]) cass_read <= 1'b1;
			end

			if (skip_bytes && !fifo_empty && tap_player_tick_cnt[0]) begin
				tap_fifo_rdreq <= 1'b1;
				cass_read <= 1'b1;
				if (start_bytes < 8'h14) start_bytes <= start_bytes + 8'd1;
				else skip_bytes <= 1'b0;
			end
		end
	end
end

// Tape sense output follows the emulated PLAY/STOP state
assign cass_sense = sense;

// Motor run output follows the momentum-filtered motor state
assign cass_run = motor;

endmodule
