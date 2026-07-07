

// Implementation of the MegaCart for VIC20 FPGA
// Copyright (c) 2021 by Alastair M. Robinson
// 
// Modified for MiSTer (c) 2022 Alexey Melnikov
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
//

// MegaCart ROM/RAM/NVRAM controller for VIC20 — banked memory mapping with soft reset.
module megacart (
	input		clk,
	input		reset_n,
	input	[15:0]	vic_addr,
	input		vic_wr_n,
	input		vic_io2_sel,
	input		vic_io3_sel,
	input		vic_blk123_sel,
	input		vic_blk5_sel,
	input		vic_ram123_sel,
	input	[7:0]	vic_data,
	output	[22:0]	mc_addr,
	output		mc_wr_n,
	output		mc_nvram_sel,
	output reg	mc_soft_reset
);

// Bank enable, high/low bank select and NVRAM enable registers.
reg		bank_ena;
reg	[7:0]	bank_high;
reg	[7:0]	bank_low;
reg		nvram_ena_l;

// Effective bank indices and RAM-enable flags derived from bank registers.
wire	[6:0]	eff_bank_low  = bank_ena ? bank_low[6:0]  : 7'h7f;
wire	[6:0]	eff_bank_high = bank_ena ? bank_high[6:0] : 7'h7f;
wire		ram_low_ena   = bank_ena & bank_low[7];
wire		ram_high_ena  = bank_ena & bank_high[7];
wire		ram_wp_n      = bank_high[6];

// High ROM select: blk5 when RAM not enabled for high half.
wire	rom_high_sel = reset_n & ~ram_high_ena & vic_blk5_sel;

// Low ROM select: blk123 or blk5 when RAM not enabled for low half.
wire	rom_low_sel  = reset_n & ~ram_low_ena & (vic_blk5_sel | vic_blk123_sel);

// Cart RAM select: both RAM enables high and blk123 or blk5 active.
wire	mc_ram_sel   = ram_low_ena & ram_high_ena & (vic_blk123_sel | vic_blk5_sel);

// NVRAM select: io2/io3/ram123 blocks, gated by reset.
assign	mc_nvram_sel = reset_n & (vic_ram123_sel | vic_io2_sel | vic_io3_sel);

// Combined write enable: RAM writes when wp_n high, plus NVRAM writes when nvram enabled.
wire	mc_wr_en     = (mc_ram_sel & ram_wp_n) | (~nvram_ena_l & mc_nvram_sel);
assign	mc_wr_n      = ~reset_n | vic_wr_n | ~mc_wr_en;

// Address mux: high-ROM, low-ROM or cart-RAM mapping.
assign	mc_addr = rom_high_sel ? {3'b001, eff_bank_high, vic_addr[12:0]}
		: rom_low_sel  ? {3'b000, eff_bank_low,  vic_addr[12:0]}
		: {3'b100, 4'b0000, vic_addr[15:0]};

// IO3 register writes: bank selects, NVRAM enable and soft-reset toggle.
always @(posedge clk) begin
	mc_soft_reset <= 1'b0;
	if (!reset_n) begin
		nvram_ena_l <= 1'b0;
		bank_ena    <= 1'b0;
	end else if (vic_io3_sel & ~vic_wr_n) begin
		if (vic_addr[7]) bank_high <= vic_data;
		if (vic_addr[8]) bank_low  <= vic_data;
		if (vic_addr[8] & vic_addr[7]) nvram_ena_l <= vic_data[0];
		if (vic_addr[9]) begin
			mc_soft_reset <= 1'b1;
			bank_ena <= ~bank_ena;
		end
	end
end

endmodule
