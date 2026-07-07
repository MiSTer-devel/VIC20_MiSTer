// -------------------------------------------------------------------------------
// -- Title      : VIA 6522
// -------------------------------------------------------------------------------
// -- Author     : Gideon Zweijtzer  <gideon.zweijtzer@gmail.com>
// -------------------------------------------------------------------------------
// -- Description: This module implements the 6522 VIA chip.
// --              A LOT OF REVERSE ENGINEERING has been done to make this module
// --              as accurate as it is now. Thanks to gyurco for ironing out some
// --              differences that were left unnoticed.
// -------------------------------------------------------------------------------
// -- License:     GPL 3.0 - Free to use, distribute and change to your own needs.
// --              Leaving a reference to the author will be highly appreciated.
// -------------------------------------------------------------------------------

// -------------------------------------------------------------------------------
// -- Title      : VIA 6522
// -------------------------------------------------------------------------------
// -- Author     : Gideon Zweijtzer  <gideon.zweijtzer@gmail.com>
// -------------------------------------------------------------------------------
// -- Description: This module implements the 6522 VIA chip.
// --              A LOT OF REVERSE ENGINEERING has been done to make this module
// --              as accurate as it is now. Thanks to gyurco for ironing out some
// --              differences that were left unnoticed.
// -------------------------------------------------------------------------------
// -- License:     GPL 3.0 - Free to use, distribute and change to your own needs.
// --              Leaving a reference to the author will be highly appreciated.
// -------------------------------------------------------------------------------

// VIA 6522 versatile interface adapter
module via6522 (
	input        clock,
	input        rising,
	input        falling,
	input        reset,
	input  [3:0] addr,
	input        wen,
	input        ren,
	input  [7:0] data_in,
	output reg [7:0] data_out,
	output reg   phi2_ref,
	output reg [7:0] port_a_o,
	output reg [7:0] port_a_t,
	input  [7:0] port_a_i,
	output [7:0] port_b_o,
	output [7:0] port_b_t,
	input  [7:0] port_b_i,
	input        ca1_i,
	output       ca2_o,
	input        ca2_i,
	output       ca2_t,
	output reg   cb1_o,
	input        cb1_i,
	output       cb1_t,
	output       cb2_o,
	input        cb2_i,
	output       cb2_t,
	output       irq
);

// Internal registers for ports, timers, interrupts and configuration
reg [7:0]  prb, ddrb;
reg [7:0]  port_a_c, port_b_c;
reg [7:0]  ira, irb;
reg [6:0]  irq_mask, irq_flags;
reg [15:0] timer_a_latch;
reg [7:0]  timer_b_latch;
reg [15:0] timer_a_count, timer_b_count;
reg        timer_a_reload, timer_a_out, timer_a_may_interrupt;
reg        timer_b_reload_lo, timer_b_oneshot_trig, timer_b_timeout, timer_b_tick;
reg [7:0]  acr, pcr;
reg [7:0]  shift_reg;
reg        shift_clock, shift_timer_tick, shift_active;
reg [2:0]  bit_cnt;
reg        ca1_c, ca2_c, cb1_c, cb2_c;
reg        ca1_d, ca2_d, cb1_d, cb2_d;
reg        ca2_handshake_o, ca2_pulse_o;
reg        cb2_handshake_o, cb2_pulse_o;
reg        pb6_c, pb6_d;
reg        ser_cb2_c;

// Event detection, control decode wires and continuous assignments for ports and handshake pins
wire       write_t1c_l = wen & falling & ((addr == 4'h4) | (addr == 4'h6));
wire       write_t1c_h = wen & falling & (addr == 4'h5);
wire       write_t2c_h = wen & falling & (addr == 4'h9);
wire       ca1_event   = (ca1_c ^ ca1_d) & (ca1_d ^ pcr[0]);
wire       ca2_event   = (ca2_c ^ ca2_d) & (ca2_d ^ pcr[2]);
wire       cb1_event   = (cb1_c ^ cb1_d) & (cb1_d ^ pcr[4]);
wire       cb2_event   = (cb2_c ^ cb2_d) & (cb2_d ^ pcr[6]);
wire       timer_a_event = rising & timer_a_reload & timer_a_may_interrupt;
wire       timer_b_event = rising & timer_b_timeout;
wire       shift_tick_r = ~cb1_o & shift_clock;
wire       serial_event = shift_tick_r & ~shift_active & rising & (|acr[4:2]);
wire [6:0] irq_events  = {timer_a_event, timer_b_event, cb1_event, cb2_event, serial_event, ca1_event, ca2_event};
wire [7:0] orb_read    = (prb & ddrb) | (irb & ~ddrb);
wire       shift_pulse = ~shift_active ? (~|acr[4:2] & shift_clock & ~cb1_o) : (~acr[3] ? shift_timer_tick : (acr[2] ? (shift_clock & ~cb1_o) : 1'b1));
assign     irq      = |(irq_flags & irq_mask);
assign     port_b_o = {acr[7] ? timer_a_out : prb[7], prb[6:0]};
assign     port_b_t = {acr[7] | ddrb[7], ddrb[6:0]};
assign     ca2_t    = pcr[3];
assign     ca2_o    = pcr[2] ? pcr[1] : (pcr[1] ? ca2_pulse_o : ca2_handshake_o);
assign     cb1_t    = ~&acr[3:2] & (|acr[4:2]);
assign     cb2_t    = (|acr[4:2]) ? acr[4] : pcr[7];
assign     cb2_o    = (|acr[4:2]) ? shift_reg[7] : (pcr[6] ? pcr[5] : (pcr[5] ? cb2_pulse_o : cb2_handshake_o));

// Reference clock phase generation synchronous process
always @(posedge clock) begin
	if (rising) phi2_ref <= 1'b1;
	else if (falling) phi2_ref <= 1'b0;
end

// Main VIA registers, handshake control and interrupts synchronous process
always @(posedge clock) begin
	if (reset) begin
		port_a_o <= 8'h00;
		port_a_t <= 8'h00;
		prb <= 8'h00;
		ddrb <= 8'h00;
		irq_mask <= 7'h00;
		irq_flags <= 7'h00;
		acr <= 8'h00;
		pcr <= 8'h00;
		ca2_handshake_o <= 1'b1;
		ca2_pulse_o <= 1'b1;
		cb2_handshake_o <= 1'b1;
		cb2_pulse_o <= 1'b1;
		timer_a_latch <= 16'h5550;
		timer_b_latch <= 8'h50;
		data_out <= 8'h00;
	end else begin
		ca1_c <= ca1_i;
		ca2_c <= ca2_i;
		cb1_c <= cb1_t ? cb1_o : cb1_i;
		cb2_c <= cb2_t ? cb2_o : cb2_i;
		ca1_d <= ca1_c;
		ca2_d <= ca2_c;
		cb1_d <= cb1_c;
		cb2_d <= cb2_c;
		port_a_c <= port_a_i;
		port_b_c <= port_b_i;
		if (!acr[0] | ca1_event) ira <= port_a_c;
		if (!acr[1] | cb1_event) irb <= port_b_c;

		if (ca1_event) ca2_handshake_o <= 1'b1;
		else if ((ren | wen) & (addr == 4'h1) & falling) ca2_handshake_o <= 1'b0;
		if (falling) ca2_pulse_o <= ~((ren | wen) & (addr == 4'h1));

		if (cb1_event) cb2_handshake_o <= 1'b1;
		else if ((ren | wen) & (addr == 4'h0) & falling) cb2_handshake_o <= 1'b0;
		if (falling) cb2_pulse_o <= ~((ren | wen) & (addr == 4'h0));

		irq_flags <= irq_flags | irq_events;

		if (wen & falling) begin
			case (addr)
				4'h0: begin
					prb <= data_in;
					if (!pcr[5]) irq_flags[3] <= 1'b0;
					irq_flags[4] <= 1'b0;
				end
				4'h1: begin
					port_a_o <= data_in;
					if (!pcr[1]) irq_flags[0] <= 1'b0;
					irq_flags[1] <= 1'b0;
				end
				4'h2: ddrb <= data_in;
				4'h3: port_a_t <= data_in;
				4'h4: timer_a_latch[7:0] <= data_in;
				4'h5: begin
					timer_a_latch[15:8] <= data_in;
					irq_flags[6] <= 1'b0;
				end
				4'h6: timer_a_latch[7:0] <= data_in;
				4'h7: begin
					timer_a_latch[15:8] <= data_in;
					irq_flags[6] <= 1'b0;
				end
				4'h8: timer_b_latch <= data_in;
				4'h9: irq_flags[5] <= 1'b0;
				4'hA: irq_flags[2] <= 1'b0;
				4'hB: acr <= data_in;
				4'hC: pcr <= data_in;
				4'hD: irq_flags <= (irq_flags | irq_events) & ~data_in[6:0];
				4'hE: irq_mask <= data_in[7] ? (irq_mask | data_in[6:0]) : (irq_mask & ~data_in[6:0]);
				4'hF: port_a_o <= data_in;
				default: ;
			endcase
		end

		case (addr)
			4'h0: data_out <= {acr[7] ? timer_a_out : orb_read[7], orb_read[6:0]};
			4'h1, 4'hF: data_out <= ira;
			4'h2: data_out <= ddrb;
			4'h3: data_out <= port_a_t;
			4'h4: data_out <= timer_a_count[7:0];
			4'h5: data_out <= timer_a_count[15:8];
			4'h6: data_out <= timer_a_latch[7:0];
			4'h7: data_out <= timer_a_latch[15:8];
			4'h8: data_out <= timer_b_count[7:0];
			4'h9: data_out <= timer_b_count[15:8];
			4'hA: data_out <= shift_reg;
			4'hB: data_out <= acr;
			4'hC: data_out <= pcr;
			4'hD: data_out <= {irq, irq_flags};
			4'hE: data_out <= {1'b0, irq_mask};
			default: data_out <= 8'h00;
		endcase

		if (ren & falling) begin
			case (addr)
				4'h0: begin
					if (!pcr[5]) irq_flags[3] <= 1'b0;
					irq_flags[4] <= 1'b0;
				end
				4'h1: begin
					if (!pcr[1]) irq_flags[0] <= 1'b0;
					irq_flags[1] <= 1'b0;
				end
				4'h4: irq_flags[6] <= 1'b0;
				4'h8: irq_flags[5] <= 1'b0;
				4'hA: irq_flags[2] <= 1'b0;
				default: ;
			endcase
		end
	end
end

// Timer A counter and output toggle synchronous process
always @(posedge clock) begin
	if (reset) begin
		timer_a_may_interrupt <= 1'b0;
		timer_a_out <= 1'b1;
		timer_a_count <= 16'h5550;
		timer_a_reload <= 1'b0;
	end else begin
		if (falling) begin
			if (timer_a_reload) begin
				timer_a_count <= {timer_a_latch[15:8], write_t1c_l ? data_in : timer_a_latch[7:0]};
				timer_a_reload <= 1'b0;
				timer_a_may_interrupt <= timer_a_may_interrupt & acr[6];
			end else begin
				if (~|timer_a_count) timer_a_reload <= 1'b1;
				timer_a_count <= timer_a_count - 16'd1;
			end
		end
		if (rising & timer_a_event & acr[7]) begin
			timer_a_out <= ~timer_a_out;
		end
		if (write_t1c_h) begin
			timer_a_may_interrupt <= 1'b1;
			timer_a_out <= ~acr[7];
			timer_a_count <= {data_in, timer_a_latch[7:0]};
			timer_a_reload <= 1'b0;
		end
	end
end

// Timer B counter and pulse detection synchronous process
always @(posedge clock) begin
	if (reset) begin
		timer_b_count <= 16'h5550;
		timer_b_reload_lo <= 1'b0;
		timer_b_oneshot_trig <= 1'b0;
		timer_b_timeout <= 1'b0;
		timer_b_tick <= 1'b0;
	end else begin
		if (rising) begin
			pb6_c <= port_b_i[6];
			pb6_d <= pb6_c;
		end
		if (falling) begin
			timer_b_timeout <= 1'b0;
			timer_b_tick <= 1'b0;
			if (~acr[5] | (pb6_d & ~pb6_c)) begin
				if (~|timer_b_count & timer_b_oneshot_trig) begin
					timer_b_oneshot_trig <= 1'b0;
					timer_b_timeout <= 1'b1;
				end
				if (~|timer_b_count[7:0] & ~acr[3] & (acr[4] | acr[2])) begin
					timer_b_reload_lo <= 1'b1;
					timer_b_tick <= 1'b1;
				end
				timer_b_count <= timer_b_count - 16'd1;
			end
			if (timer_b_reload_lo) begin
				timer_b_count[7:0] <= timer_b_latch;
				timer_b_reload_lo <= 1'b0;
			end
		end
		if (write_t2c_h) begin
			timer_b_count <= {data_in, timer_b_latch};
			timer_b_oneshot_trig <= 1'b1;
		end
	end
end

// Serial port clock generation and synchronization process
always @(posedge clock) begin
	if (reset) begin
		shift_clock <= 1'b1;
		cb1_o <= 1'b1;
	end else begin
		ser_cb2_c <= cb2_i;
		if (rising) begin
			if (~shift_active) shift_clock <= (|acr[4:2]) | cb1_i;
			else if (&acr[3:2]) shift_clock <= cb1_i;
			else if (shift_pulse) shift_clock <= ~shift_clock;
			cb1_o <= shift_clock;
		end
		if (falling) shift_timer_tick <= timer_b_tick;
	end
end

// Serial shift register synchronous process
always @(posedge clock) begin
	if (reset) begin
		shift_reg <= 8'hFF;
	end else if (falling) begin
		if (wen & (addr == 4'hA)) shift_reg <= data_in;
		else if (acr[4] & cb1_o & ~shift_clock) shift_reg <= {shift_reg[6:0], shift_reg[7]};
		else if (~acr[4] & ~cb1_o & shift_clock) shift_reg <= {shift_reg[6:0], ser_cb2_c};
	end
end

// Serial active flag and bit counter synchronous process
always @(posedge clock) begin
	if (reset) begin
		shift_active <= 1'b0;
		bit_cnt <= 3'd0;
	end else if (falling) begin
		if (~shift_active & (|acr[4:2])) begin
			if ((ren | wen) & (addr == 4'hA)) begin
				bit_cnt <= 3'd7;
				shift_active <= 1'b1;
			end
		end else begin
			if (~|acr[3:2]) begin
				shift_active <= acr[4];
			end else if (shift_pulse & shift_clock) begin
				if (~|bit_cnt) shift_active <= 1'b0;
				else bit_cnt <= bit_cnt - 3'd1;
			end
		end
	end
end

endmodule
