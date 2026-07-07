// -- -----------------------------------------------------------------------
// --
// --                                 FPGA 64
// --
// --     A fully functional commodore 64 implementation in a single FPGA
// --
// -- -----------------------------------------------------------------------
// -- Copyright 2005-2008 by Peter Wendrich (pwsoft@syntiac.com)
// -- http://www.syntiac.com/fpga64.html
// -- -----------------------------------------------------------------------
// -- 'Joystick emulation on keypad' additions by
// -- Mark McDougall (msmcdoug@iinet.net.au)
// -- -----------------------------------------------------------------------
// --
// -- VIC20/C64 Keyboard matrix
// --
// -- Hardware huh?
// --	In original machine if a key is pressed a contact is made.
// --	Bidirectional reading is possible on real hardware, which is difficult
// --	to emulate. (set backwardsReadingEnabled to '1' if you want this enabled).
// --	Then we have the joysticks, one of which is normally connected
// --	to a OUTPUT pin.
// --
// -- Emulation:
// --	All pins are high except when one is driven low and there is a
// --	connection. This is consistent with joysticks that force a line
// --	low too. CIA will put '1's when set to input to help this emulation.
// --
// -- -----------------------------------------------------------------------

// VIC20 and C64 PS/2 keyboard matrix interface
module fpga64_keyboard (
	input        clk,
	input        reset,
	input [10:0] ps2_key,
	input  [7:0] pai,
	input  [7:0] pbi,
	output reg [7:0] pao,
	output reg [7:0] pbo,
	output reg   reset_key,
	output reg   restore_key,
	output       mod_key,
	output reg   tape_play,
	input        backwardsReadingEnabled
);

// PS/2 decoded key state registers
reg key_del, key_return, key_left, key_right, key_F1, key_F2, key_F3, key_F4, key_F5, key_F6, key_F7, key_F8, key_up, key_down;
reg key_3, key_W, key_A, key_4, key_Z, key_S, key_E, key_shiftl;
reg key_5, key_R, key_D, key_6, key_C, key_F, key_T, key_X;
reg key_7, key_Y, key_G, key_8, key_B, key_H, key_U, key_V;
reg key_9, key_I, key_J, key_0, key_M, key_K, key_O, key_N;
reg key_plus, key_P, key_L, key_minus, key_dot, key_colon, key_at, key_comma;
reg key_pound, key_star, key_semicolon, key_home, key_shiftr, key_equal, key_arrowup, key_slash;
reg key_1, key_arrowleft, key_ctrl, key_2, key_space, key_commodore, key_Q, key_runstop;
reg mod_key1, mod_key2, key_inst, key_caps, ps2_stb, key_8s;
reg [18:0] delay_cnt;

// PS/2 helper wires and modifier outputs
wire pressed = ps2_key[9];
wire extended = ps2_key[8];
wire key_shift = key_shiftl | key_shiftr;
wire delay_end = ~|delay_cnt;
assign mod_key = mod_key1 | mod_key2;

// Keyboard matrix scan, key decode and reset process
always @(posedge clk) begin
	ps2_stb <= ps2_key[10];
	if (|delay_cnt) delay_cnt <= delay_cnt - 19'd1;

	pao[0] <= pai[0] & (!backwardsReadingEnabled | ((pbi[0] | !(key_del | key_inst)) & (pbi[1] | !key_return) & (pbi[2] | !(key_left | key_right)) & (pbi[3] | !(key_F7 | key_F8)) & (pbi[4] | !(key_F1 | key_F2)) & (pbi[5] | !(key_F3 | key_F4)) & (pbi[6] | !(key_F5 | key_F6)) & (pbi[7] | !(key_up | key_down))));
	pao[1] <= pai[1] & (!backwardsReadingEnabled | ((pbi[0] | !key_3) & (pbi[1] | !key_W) & (pbi[2] | !key_A) & (pbi[3] | !key_4) & (pbi[4] | !key_Z) & (pbi[5] | !key_S) & (pbi[6] | !key_E) & (pbi[7] | !(key_left | key_up | (key_shiftl & !key_8s) | key_caps | key_inst | key_F2 | key_F4 | key_F6 | key_F8))));
	pao[2] <= pai[2] & (!backwardsReadingEnabled | ((pbi[0] | !key_5) & (pbi[1] | !key_R) & (pbi[2] | !key_D) & (pbi[3] | !key_6) & (pbi[4] | !key_C) & (pbi[5] | !key_F) & (pbi[6] | !key_T) & (pbi[7] | !key_X)));
	pao[3] <= pai[3] & (!backwardsReadingEnabled | ((pbi[0] | !key_7) & (pbi[1] | !key_Y) & (pbi[2] | !key_G) & (pbi[3] | !key_8) & (pbi[4] | !key_B) & (pbi[5] | !key_H) & (pbi[6] | !key_U) & (pbi[7] | !key_V)));
	pao[4] <= pai[4] & (!backwardsReadingEnabled | ((pbi[0] | !key_9) & (pbi[1] | !key_I) & (pbi[2] | !key_J) & (pbi[3] | !key_0) & (pbi[4] | !key_M) & (pbi[5] | !key_K) & (pbi[6] | !key_O) & (pbi[7] | !key_N)));
	pao[5] <= pai[5] & (!backwardsReadingEnabled | ((pbi[0] | !key_plus) & (pbi[1] | !key_P) & (pbi[2] | !key_L) & (pbi[3] | !key_minus) & (pbi[4] | !key_dot) & (pbi[5] | !key_colon) & (pbi[6] | !key_at) & (pbi[7] | !key_comma)));
	pao[6] <= pai[6] & (!backwardsReadingEnabled | ((pbi[0] | !key_pound) & (pbi[1] | !(key_star | (key_8s & delay_end))) & (pbi[2] | !key_semicolon) & (pbi[3] | !key_home) & (pbi[4] | !(key_left | key_up | (key_shiftr & !key_8s) | key_caps | key_inst | key_F2 | key_F4 | key_F6 | key_F8)) & (pbi[5] | !key_equal) & (pbi[6] | !key_arrowup) & (pbi[7] | !key_slash)));
	pao[7] <= pai[7] & (!backwardsReadingEnabled | ((pbi[0] | !key_1) & (pbi[1] | !key_arrowleft) & (pbi[2] | !key_ctrl) & (pbi[3] | !key_2) & (pbi[4] | !key_space) & (pbi[5] | !(key_commodore | key_caps)) & (pbi[6] | !key_Q) & (pbi[7] | !key_runstop)));

	pbo[0] <= pbi[0] & (pai[0] | !(key_del | key_inst)) & (pai[1] | !key_3) & (pai[2] | !key_5) & (pai[3] | !key_7) & (pai[4] | !key_9) & (pai[5] | !key_plus) & (pai[6] | !key_pound) & (pai[7] | !key_1);
	pbo[1] <= pbi[1] & (pai[0] | !key_return) & (pai[1] | !key_W) & (pai[2] | !key_R) & (pai[3] | !key_Y) & (pai[4] | !key_I) & (pai[5] | !key_P) & (pai[6] | !(key_star | (key_8s & delay_end))) & (pai[7] | !key_arrowleft);
	pbo[2] <= pbi[2] & (pai[0] | !(key_left | key_right)) & (pai[1] | !key_A) & (pai[2] | !key_D) & (pai[3] | !key_G) & (pai[4] | !key_J) & (pai[5] | !key_L) & (pai[6] | !key_semicolon) & (pai[7] | !key_ctrl);
	pbo[3] <= pbi[3] & (pai[0] | !(key_F7 | key_F8)) & (pai[1] | !key_4) & (pai[2] | !key_6) & (pai[3] | !key_8) & (pai[4] | !key_0) & (pai[5] | !key_minus) & (pai[6] | !key_home) & (pai[7] | !key_2);
	pbo[4] <= pbi[4] & (pai[0] | !(key_F1 | key_F2)) & (pai[1] | !key_Z) & (pai[2] | !key_C) & (pai[3] | !key_B) & (pai[4] | !key_M) & (pai[5] | !key_dot) & (pai[6] | !(key_left | key_up | (key_shiftr & !key_8s) | key_caps | key_inst | key_F2 | key_F4 | key_F6 | key_F8)) & (pai[7] | !key_space);
	pbo[5] <= pbi[5] & (pai[0] | !(key_F3 | key_F4)) & (pai[1] | !key_S) & (pai[2] | !key_F) & (pai[3] | !key_H) & (pai[4] | !key_K) & (pai[5] | !key_colon) & (pai[6] | !key_equal) & (pai[7] | !(key_commodore | key_caps));
	pbo[6] <= pbi[6] & (pai[0] | !(key_F5 | key_F6)) & (pai[1] | !key_E) & (pai[2] | !key_T) & (pai[3] | !key_U) & (pai[4] | !key_O) & (pai[5] | !key_at) & (pai[6] | !key_arrowup) & (pai[7] | !key_Q);
	pbo[7] <= pbi[7] & (pai[0] | !(key_up | key_down)) & (pai[1] | !(key_left | key_up | (key_shiftl & !key_8s) | key_caps | key_inst | key_F2 | key_F4 | key_F6 | key_F8)) & (pai[2] | !key_X) & (pai[3] | !key_V) & (pai[4] | !key_N) & (pai[5] | !key_comma) & (pai[6] | !key_slash) & (pai[7] | !key_runstop);

	if (ps2_key[10] != ps2_stb) begin
		case (ps2_key[7:0])
			8'h05: key_F1 <= pressed;
			8'h06: key_F2 <= pressed;
			8'h04: key_F3 <= pressed;
			8'h0C: key_F4 <= pressed;
			8'h03: key_F5 <= pressed;
			8'h0B: key_F6 <= pressed;
			8'h83: key_F7 <= pressed;
			8'h0A: key_F8 <= pressed;
			8'h01: key_arrowup <= pressed;
			8'h09: key_equal <= pressed;
			8'h0D: key_commodore <= pressed;
			8'h0E: key_arrowleft <= pressed;
			8'h11: key_commodore <= pressed;
			8'h12: key_shiftl <= pressed;
			8'h14: key_ctrl <= pressed;
			8'h15: key_Q <= pressed;
			8'h16: key_1 <= pressed;
			8'h1A: key_Z <= pressed;
			8'h1B: key_S <= pressed;
			8'h1C: key_A <= pressed;
			8'h1D: key_W <= pressed;
			8'h1E: key_2 <= pressed;
			8'h1F: mod_key1 <= pressed;
			8'h21: key_C <= pressed;
			8'h22: key_X <= pressed;
			8'h23: key_D <= pressed;
			8'h24: key_E <= pressed;
			8'h25: key_4 <= pressed;
			8'h26: key_3 <= pressed;
			8'h27: mod_key2 <= pressed;
			8'h29: key_space <= pressed;
			8'h2A: key_V <= pressed;
			8'h2B: key_F <= pressed;
			8'h2C: key_T <= pressed;
			8'h2D: key_R <= pressed;
			8'h2E: key_5 <= pressed;
			8'h31: key_N <= pressed;
			8'h32: key_B <= pressed;
			8'h33: key_H <= pressed;
			8'h34: key_G <= pressed;
			8'h35: key_Y <= pressed;
			8'h36: begin key_7 <= pressed & key_shift; key_6 <= pressed & !key_shift; end
			8'h3A: key_M <= pressed;
			8'h3B: key_J <= pressed;
			8'h3C: key_U <= pressed;
			8'h3D: begin key_6 <= pressed & key_shift; key_7 <= pressed & !key_shift; end
			8'h3E: begin key_8s <= pressed & key_shift; key_8 <= pressed & !key_shift; delay_cnt <= 19'd300000; end
			8'h41: key_comma <= pressed;
			8'h42: key_K <= pressed;
			8'h43: key_I <= pressed;
			8'h44: key_O <= pressed;
			8'h45: begin key_9 <= pressed & key_shift; key_0 <= pressed & !key_shift; end
			8'h46: begin key_8 <= pressed & key_shift; key_9 <= pressed & !key_shift; end
			8'h49: key_dot <= pressed;
			8'h4A: key_slash <= pressed;
			8'h4B: key_L <= pressed;
			8'h4C: key_colon <= pressed;
			8'h4D: key_P <= pressed;
			8'h4E: key_minus <= pressed;
			8'h52: key_semicolon <= pressed;
			8'h54: key_at <= pressed;
			8'h55: key_plus <= pressed;
			8'h58: key_caps <= pressed;
			8'h59: key_shiftr <= pressed;
			8'h5A: key_return <= pressed;
			8'h5B: key_star <= pressed;
			8'h5D: key_pound <= pressed;
			8'h66: key_del <= pressed;
			8'h69: if (extended) key_equal <= pressed; else key_1 <= pressed;
			8'h6B: if (extended) key_left <= pressed; else key_4 <= pressed;
			8'h6C: if (extended) key_home <= pressed; else key_7 <= pressed;
			8'h70: if (extended) key_inst <= pressed; else key_0 <= pressed;
			8'h71: if (extended) key_del <= pressed; else key_dot <= pressed;
			8'h72: if (extended) key_down <= pressed; else key_2 <= pressed;
			8'h73: key_5 <= pressed;
			8'h74: if (extended) key_right <= pressed; else key_6 <= pressed;
			8'h75: if (extended) key_up <= pressed; else key_8 <= pressed;
			8'h76: key_runstop <= pressed;
			8'h79: key_plus <= pressed;
			8'h7A: if (extended) key_arrowup <= pressed; else key_3 <= pressed;
			8'h7B: key_minus <= pressed;
			8'h7C: key_star <= pressed;
			8'h7D: if (extended) tape_play <= pressed; else key_9 <= pressed;
			8'h78: begin
				if (pressed && key_ctrl) begin
					reset_key <= 1'b1;
				end else if (pressed && !key_ctrl) begin
					restore_key <= 1'b1;
				end else begin
					reset_key <= 1'b0;
					restore_key <= 1'b0;
				end
			end
			default: begin end
		endcase
	end

	if (reset) begin
		key_F1 <= 1'b0; key_F2 <= 1'b0; key_F3 <= 1'b0; key_F4 <= 1'b0; key_F5 <= 1'b0; key_F6 <= 1'b0; key_F7 <= 1'b0; key_F8 <= 1'b0;
		key_shiftr <= 1'b0; key_shiftl <= 1'b0; key_ctrl <= 1'b0; mod_key1 <= 1'b0; mod_key2 <= 1'b0; key_commodore <= 1'b0; key_runstop <= 1'b0;
		restore_key <= 1'b0; tape_play <= 1'b0; key_arrowup <= 1'b0; key_equal <= 1'b0; key_arrowleft <= 1'b0; key_space <= 1'b0;
		key_comma <= 1'b0; key_dot <= 1'b0; key_slash <= 1'b0; key_colon <= 1'b0; key_minus <= 1'b0; key_semicolon <= 1'b0; key_at <= 1'b0; key_plus <= 1'b0;
		key_caps <= 1'b0; key_return <= 1'b0; key_star <= 1'b0; key_pound <= 1'b0; key_del <= 1'b0; key_left <= 1'b0; key_home <= 1'b0; key_inst <= 1'b0;
		key_down <= 1'b0; key_right <= 1'b0; key_up <= 1'b0; key_1 <= 1'b0; key_2 <= 1'b0; key_3 <= 1'b0; key_4 <= 1'b0; key_5 <= 1'b0;
		key_6 <= 1'b0; key_7 <= 1'b0; key_8 <= 1'b0; key_8s <= 1'b0; key_9 <= 1'b0; key_0 <= 1'b0; key_Q <= 1'b0; key_Z <= 1'b0;
		key_S <= 1'b0; key_A <= 1'b0; key_W <= 1'b0; key_C <= 1'b0; key_X <= 1'b0; key_D <= 1'b0; key_E <= 1'b0; key_V <= 1'b0;
		key_F <= 1'b0; key_T <= 1'b0; key_R <= 1'b0; key_N <= 1'b0; key_B <= 1'b0; key_H <= 1'b0; key_G <= 1'b0; key_Y <= 1'b0;
		key_M <= 1'b0; key_J <= 1'b0; key_U <= 1'b0; key_K <= 1'b0; key_I <= 1'b0; key_O <= 1'b0; key_L <= 1'b0; key_P <= 1'b0;
	end
end

endmodule
