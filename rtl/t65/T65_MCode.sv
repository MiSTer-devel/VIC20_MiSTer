// -- ****
// -- T65(b) core. In an effort to merge and maintain bug fixes ....
// --
// -- See list of changes in T65 top file (T65.vhd)...
// --
// -- ****
// -- 65xx compatible microprocessor core
// --
// -- FPGAARCADE SVN: $Id: T65_MCode.vhd 1234 2015-02-28 20:14:50Z wolfgang.scherr $
// --
// -- Copyright (c) 2002...2015
// --               Daniel Wallner (jesus <at> opencores <dot> org)
// --               Mike Johnson   (mikej <at> fpgaarcade <dot> com)
// --               Wolfgang Scherr (WoS <at> pin4 <dot> at>
// --               Morten Leikvoll ()
// --
// -- All rights reserved
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
// -- specific prior written permission.
// --
// -- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
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
// -- Please report bugs to the author(s), but before you do so, please
// -- make sure that this is not a derivative work and that
// -- you have the latest version of this file.
// --
// -- Limitations :
// --   See in T65 top file (T65.vhd)...

// Instruction microcode decoder for the T65 CPU core.
/* verilator lint_off IMPORTSTAR */
import T65_Pack::*;
module T65_MCode (
	input  [1:0]     Mode,
	input  [7:0]     IR,
	input  T_Lcycle  MCycle,
	input  [7:0]     P,
	input            Rdy_mod,
	output T_Lcycle       LCycle,
	output T_ALU_OP       ALU_Op,
	output T_Set_BusA_To  Set_BusA_To,
	output T_Set_Addr_To  Set_Addr_To,
	output T_Write_Data   Write_Data,
	output reg [1:0] Jump, BAAdd, BAQuirk,
	output reg       BreakAtNA, ADAdd, AddY, PCAdd,
	output reg       Inc_S, Dec_S,
	output reg       LDA, LDP, LDX, LDY, LDS,
	output reg       LDDI, LDALU, LDAD, LDBAL, LDBAH,
	output reg       SaveP, Write
);

// Branch condition decoder (bit selected by IR[7:6], polarity by IR[5]).
wire Branch = IR[5] ^ ~P[IR[7:6]==2'b00 ? Flag_N :
                        IR[7:6]==2'b01 ? Flag_V :
                        IR[7:6]==2'b10 ? Flag_C : Flag_Z];

// Extra undocumented opcode ALU phase flag.
reg ALUmore;

// Convenience: RMW/undoc combined write cycle predicate.
wire is_undoc_rmw = (Mode==2'b00 && IR[1]==1'b1 && IR[7:6]!=2'b10);

// Addressing microcycle and control signal decoder.
always_comb begin
	// Defaults
	LCycle      = Cycle_1;
	Set_BusA_To = Set_BusA_To_ABC;
	Set_Addr_To = Set_Addr_To_PBR;
	Write_Data  = Write_Data_DL;
	{Jump, BAAdd, BAQuirk} = '0;
	{BreakAtNA, ADAdd, AddY, PCAdd, Inc_S, Dec_S} = '0;
	{LDA, LDP, LDX, LDY, LDS, LDDI, LDALU, LDAD, LDBAL, LDBAH} = '0;
	{SaveP, Write, ALUmore} = '0;

	// -------- Set_BusA_To / Write_Data by column IR[7:5] --------
	case (IR[7:5])
	3'b100: begin
		case (IR[1:0])
		2'b00: begin
			Set_BusA_To = Set_BusA_To_Y;
			Write_Data  = (IR[4:2]==3'b111 && !Rdy_mod) ? Write_Data_YB : Write_Data_Y;
		end
		2'b10: begin
			Set_BusA_To = Set_BusA_To_X;
			Write_Data  = (IR[4:2]==3'b111 && !Rdy_mod) ? Write_Data_XB : Write_Data_X;
		end
		2'b11: begin
			if (IR[4:2]==3'b110) begin Set_BusA_To = Set_BusA_To_AAX; LDS = 1; end
			else                       Set_BusA_To = Set_BusA_To_ABC;
			Write_Data = ((IR[4:2]==3'b111 || IR[4:2]==3'b110 || IR[4:2]==3'b100) && !Rdy_mod)
			             ? Write_Data_AXB : Write_Data_AX;
		end
		default: Write_Data = Write_Data_ABC;
		endcase
	end
	3'b101: begin
		Set_BusA_To = Set_BusA_To_DI;
		case (IR[1:0])
		2'b00: if (IR[4] != 1'b1 || IR[2] != 1'b0) LDY = 1;
		2'b01: LDA = 1;
		2'b10: LDX = 1;
		default: begin
			LDX = 1; LDA = 1;
			if (IR[4:2]==3'b110) begin Set_BusA_To = Set_BusA_To_S; LDS = 1; end
		end
		endcase
	end
	3'b110: begin
		if (IR[1:0]==2'b00) begin
			if (!IR[4]) LDY = 1;
			Set_BusA_To = Set_BusA_To_Y;
		end
		else Set_BusA_To = Set_BusA_To_ABC;
	end
	3'b111: begin
		if (IR[1:0]==2'b00) begin
			if (!IR[4]) LDX = 1;
			Set_BusA_To = Set_BusA_To_X;
		end
		else Set_BusA_To = Set_BusA_To_ABC;
	end
	default: ;
	endcase

	// Immediate override for undocumented ops on col _B (0x_B) and col _2 handled below.
	if (IR[7:6] != 2'b10 && IR[1] == 1'b1 && (Mode==2'b00 || IR[0]==1'b0))
		Set_BusA_To = (IR==8'hEB) ? Set_BusA_To_ABC : Set_BusA_To_DI;

	// -------- Addressing / cycle decoder by IR[4:0] --------
	case (IR[4:0])
	5'b00000, 5'b01000, 5'b01010, 5'b11000, 5'b11010: begin
		case (IR)
		8'h00: begin // BRK
			LCycle = Cycle_6;
			case (MCycle)
			Cycle_1: begin Set_Addr_To=Set_Addr_To_SP; Write_Data=Write_Data_PCH; Write=1; end
			Cycle_2: begin Dec_S=1; Set_Addr_To=Set_Addr_To_SP; Write_Data=Write_Data_PCL; Write=1; end
			Cycle_3: begin Dec_S=1; Set_Addr_To=Set_Addr_To_SP; Write_Data=Write_Data_P;   Write=1; end
			Cycle_4: begin Dec_S=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_5: begin LDDI=1;  Set_Addr_To=Set_Addr_To_BA; end
			Cycle_6:       Jump = 2'b10;
			default: ;
			endcase
		end
		8'h20: begin // JSR
			LCycle = Cycle_5;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDDI=1; Set_Addr_To=Set_Addr_To_SP; end
			Cycle_2: begin Set_Addr_To=Set_Addr_To_SP; Write_Data=Write_Data_PCH; Write=1; end
			Cycle_3: begin Dec_S=1; Set_Addr_To=Set_Addr_To_SP; Write_Data=Write_Data_PCL; Write=1; end
			Cycle_4:       Dec_S = 1;
			Cycle_5:       Jump  = 2'b10;
			default: ;
			endcase
		end
		8'h40: begin // RTI
			LCycle = Cycle_5;
			case (MCycle)
			Cycle_1:       Set_Addr_To = Set_Addr_To_SP;
			Cycle_2: begin Inc_S=1; Set_Addr_To=Set_Addr_To_SP; end
			Cycle_3: begin Inc_S=1; Set_Addr_To=Set_Addr_To_SP; Set_BusA_To=Set_BusA_To_DI; end
			Cycle_4: begin LDP=1; Inc_S=1; LDDI=1; Set_Addr_To=Set_Addr_To_SP; end
			Cycle_5:       Jump = 2'b10;
			default: ;
			endcase
		end
		8'h60: begin // RTS
			LCycle = Cycle_5;
			case (MCycle)
			Cycle_1:       Set_Addr_To = Set_Addr_To_SP;
			Cycle_2: begin Inc_S=1; Set_Addr_To=Set_Addr_To_SP; end
			Cycle_3: begin Inc_S=1; LDDI=1; Set_Addr_To=Set_Addr_To_SP; end
			Cycle_4:       Jump = 2'b10;
			Cycle_5:       Jump = 2'b01;
			default: ;
			endcase
		end
		8'h08, 8'h48, 8'h5A, 8'hDA: begin // PHP/PHA/PHY/PHX
			LCycle = Cycle_2;
			if (Mode == 2'b00 && IR[1] == 1'b1) LCycle = Cycle_1;
			case (MCycle)
			Cycle_1: if (Mode!=2'b00 || IR[1]==1'b0) begin
				Write = 1;
				case (IR[7:4])
				4'b0000: Write_Data = Write_Data_P;
				4'b0100: Write_Data = Write_Data_ABC;
				4'b0101: if (Mode != 2'b00) Write_Data = Write_Data_Y; else Write = 0;
				4'b1101: if (Mode != 2'b00) Write_Data = Write_Data_X; else Write = 0;
				default: ;
				endcase
				Set_Addr_To = Set_Addr_To_SP;
			end
			Cycle_2: Dec_S = 1;
			default: ;
			endcase
		end
		8'h28, 8'h68, 8'h7A, 8'hFA: begin // PLP/PLA/PLY/PLX
			LCycle = Cycle_3;
			if (Mode == 2'b00 && IR[1] == 1'b1) LCycle = Cycle_1;
			case (IR[7:4])
			4'b0010: LDP = 1;
			4'b0110: LDA = 1;
			4'b0111: if (Mode != 2'b00) LDY = 1;
			4'b1111: if (Mode != 2'b00) LDX = 1;
			default: ;
			endcase
			case (MCycle)
			Cycle_sync: if (Mode != 2'b00 || IR[1] == 1'b0) SaveP = 1;
			Cycle_1:    if (Mode != 2'b00 || IR[1] == 1'b0) begin Set_Addr_To=Set_Addr_To_SP; LDP=0; end
			Cycle_2:    begin Inc_S=1; Set_Addr_To=Set_Addr_To_SP; LDP=0; end
			Cycle_3:    Set_BusA_To = Set_BusA_To_DI;
			default: ;
			endcase
		end
		8'hA0, 8'hC0, 8'hE0: case (MCycle) Cycle_1: Jump = 2'b01; default: ; endcase
		8'h88: begin LDY = 1; if (MCycle==Cycle_1) Set_BusA_To = Set_BusA_To_Y; end
		8'hCA: begin LDX = 1; if (MCycle==Cycle_1) Set_BusA_To = Set_BusA_To_X; end
		8'h1A, 8'h3A: begin
			if (Mode != 2'b00) LDA = 1; else LCycle = Cycle_1;
			if (MCycle==Cycle_1) Set_BusA_To = Set_BusA_To_S;
		end
		8'h0A, 8'h2A, 8'h4A, 8'h6A: begin LDA = 1; Set_BusA_To = Set_BusA_To_ABC; end
		8'h8A, 8'h98: LDA = 1;
		8'hAA, 8'hA8: if (MCycle==Cycle_1) Set_BusA_To = Set_BusA_To_ABC;
		8'h9A: LDS = 1;
		8'hBA: begin LDX = 1; if (MCycle==Cycle_1) Set_BusA_To = Set_BusA_To_S; end
		8'h80: if (MCycle==Cycle_1) Jump = 2'b01;
		default: ;
		endcase
	end

	5'b00001, 5'b00011: begin // ($nn,X)  and undoc variants
		LCycle = Cycle_5;
		if (IR[7:6] != 2'b10) begin
			LDA = 1;
			if (Mode==2'b00 && IR[1]==1'b1) LCycle = Cycle_7;
		end
		case (MCycle)
		Cycle_1: begin Jump=2'b01; LDAD=1; Set_Addr_To=Set_Addr_To_ZPG; end
		Cycle_2: begin ADAdd=1; Set_Addr_To=Set_Addr_To_ZPG; end
		Cycle_3: begin BAAdd=2'b01; LDBAL=1; Set_Addr_To=Set_Addr_To_ZPG; end
		Cycle_4: begin
			LDBAH = 1;
			if (IR[7:5]==3'b100) Write = 1;
			Set_Addr_To = Set_Addr_To_BA;
		end
		Cycle_5: if (is_undoc_rmw) begin Set_Addr_To=Set_Addr_To_BA; Write=1; LDDI=1; end
		Cycle_6: begin Write=1; LDALU=1; SaveP=1; Set_Addr_To=Set_Addr_To_BA; end
		Cycle_7: begin ALUmore=1; Set_BusA_To=Set_BusA_To_ABC; end
		default: ;
		endcase
	end

	5'b01001: begin
		if (IR[7:5]!=3'b100) LDA = 1;
		if (MCycle==Cycle_1) Jump = 2'b01;
	end

	5'b01011: if (Mode==2'b00) begin
		case (IR[7:5])
		3'b000, 3'b001, 3'b010, 3'b011: begin Set_BusA_To=Set_BusA_To_DA;  LDA=1; end
		3'b100:                         begin Set_BusA_To=Set_BusA_To_DAX; LDA=1; end
		3'b110:                         begin Set_BusA_To=Set_BusA_To_AAX; LDX=1; end
		3'b101:                         begin Set_BusA_To=Set_BusA_To_DAO; LDA=1; end
		default:                              LDA = 1;
		endcase
		if (MCycle==Cycle_1) Jump = 2'b01;
	end

	5'b00010, 5'b10010: if (MCycle==Cycle_1) begin
		if (IR == 8'hA2) begin Jump=2'b01; LDX=1; end
		else if (IR[7:4]==4'b1000 || IR[7:4]==4'b1100 || IR[7:4]==4'b1110) Jump = 2'b01;
	end

	5'b00100: begin
		LCycle = Cycle_2;
		case (MCycle)
		Cycle_sync: if (IR[7:5]==3'b001) SaveP = 1;
		Cycle_1: begin
			Jump = 2'b01; LDAD = 1;
			if (IR[7:5]==3'b100) Write = 1;
			Set_Addr_To = Set_Addr_To_ZPG;
		end
		default: ;
		endcase
	end

	5'b00101, 5'b00110, 5'b00111: begin
		if (IR[7:6] != 2'b10 && IR[1] == 1'b1 && (Mode==2'b00 || IR[0]==1'b0)) begin
			LCycle = Cycle_4;
			if (Mode==2'b00 && IR[0]==1'b1) LDA = 1;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDAD=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_2: begin LDDI=1; if (Mode==2'b00) Write=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_3: begin LDALU=1; SaveP=1; Write=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_4: if (Mode==2'b00 && IR[0]==1'b1) begin Set_BusA_To=Set_BusA_To_ABC; ALUmore=1; LDDI=1; end
			default: ;
			endcase
		end else begin
			LCycle = Cycle_2;
			if (IR[7:6] != 2'b10) LDA = 1;
			if (MCycle==Cycle_1) begin
				Jump = 2'b01; LDAD = 1;
				if (IR[7:5]==3'b100) Write = 1;
				Set_Addr_To = Set_Addr_To_ZPG;
			end
		end
	end

	5'b01100: begin
		if (IR[7:6] == 2'b01 && IR[4:0] == 5'b01100) begin
			if (!IR[5]) begin // JMP abs
				LCycle = Cycle_2;
				case (MCycle)
				Cycle_1: begin Jump=2'b01; LDDI=1; end
				Cycle_2: Jump = 2'b10;
				default: ;
				endcase
			end else begin // JMP (abs)  / (abs,X) on 65C02
				LCycle = Cycle_4;
				case (MCycle)
				Cycle_1: begin Jump=2'b01; LDDI=1; LDBAL=1; end
				Cycle_2: begin
					LDBAH = 1;
					if (Mode != 2'b00) Jump = 2'b10;
					else               Set_Addr_To = Set_Addr_To_BA;
				end
				Cycle_3: begin
					LDDI = 1;
					if (Mode == 2'b00) begin Set_Addr_To=Set_Addr_To_BA; BAAdd=2'b01; end
					else                     Jump = 2'b01;
				end
				Cycle_4: Jump = 2'b10;
				default: ;
				endcase
			end
		end else begin
			LCycle = Cycle_3;
			case (MCycle)
			Cycle_sync: if (IR[7:5]==3'b001) SaveP = 1;
			Cycle_1:    begin Jump=2'b01; LDBAL=1; end
			Cycle_2:    begin Jump=2'b01; LDBAH=1;
			                  if (IR[7:5]==3'b100) Write = 1;
			                  Set_Addr_To = Set_Addr_To_BA; end
			default: ;
			endcase
		end
	end

	5'b01101, 5'b01110, 5'b01111: begin
		if (IR[7:6] != 2'b10 && IR[1] == 1'b1 && (Mode==2'b00 || IR[0]==1'b0)) begin
			LCycle = Cycle_5;
			if (Mode==2'b00 && IR[0]==1'b1) LDA = 1;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDBAL=1; end
			Cycle_2: begin Jump=2'b01; LDBAH=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_3: begin LDDI=1; if (Mode==2'b00) Write=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_4: begin Write=1; LDALU=1; SaveP=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_5: if (Mode==2'b00 && IR[0]==1'b1) begin ALUmore=1; Set_BusA_To=Set_BusA_To_ABC; end
			default: ;
			endcase
		end else begin
			LCycle = Cycle_3;
			if (IR[7:6] != 2'b10) LDA = 1;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDBAL=1; end
			Cycle_2: begin Jump=2'b01; LDBAH=1;
			              if (IR[7:5]==3'b100) Write = 1;
			              Set_Addr_To = Set_Addr_To_BA; end
			default: ;
			endcase
		end
	end

	5'b10000: begin // Branches
		LCycle = Branch ? Cycle_3 : Cycle_1;
		case (MCycle)
		Cycle_1: begin Jump=2'b01; LDDI=1; end
		Cycle_2: begin Jump=2'b11; PCAdd=1; end
		default: ;
		endcase
	end

	5'b10001, 5'b10011: begin // ($nn),Y and undoc
		LCycle = Cycle_5;
		if (IR[7:6] != 2'b10) begin
			LDA = 1;
			if (Mode==2'b00 && IR[1]==1'b1) LCycle = Cycle_7;
		end
		case (MCycle)
		Cycle_1: begin Jump=2'b01; LDAD=1; Set_Addr_To=Set_Addr_To_ZPG; end
		Cycle_2: begin LDBAL=1; BAAdd=2'b01; Set_Addr_To=Set_Addr_To_ZPG; end
		Cycle_3: begin Set_BusA_To=Set_BusA_To_Y; BAAdd=2'b10; LDBAH=1; Set_Addr_To=Set_Addr_To_BA; end
		Cycle_4: begin
			BAAdd = 2'b11;
			if (IR[7:5]==3'b100) begin
				Write = 1;
				if (IR[3:0]==4'h3) BAQuirk = 2'b10;
			end
			else if (IR[1]==1'b0 || IR==8'hB3) BreakAtNA = 1;
			Set_Addr_To = Set_Addr_To_BA;
		end
		Cycle_5: if (is_undoc_rmw) begin Set_Addr_To=Set_Addr_To_BA; LDDI=1; Write=1; end
		Cycle_6: begin LDALU=1; SaveP=1; Write=1; Set_Addr_To=Set_Addr_To_BA; end
		Cycle_7: begin ALUmore=1; Set_BusA_To=Set_BusA_To_ABC; end
		default: ;
		endcase
	end

	5'b10100, 5'b10101, 5'b10110, 5'b10111: begin // $nn,X/Y
		if (IR[7:6] != 2'b10 && IR[1] == 1'b1 && (Mode==2'b00 || IR[0]==1'b0)) begin
			if (Mode==2'b00 && IR[0]==1'b1) LDA = 1;
			LCycle = Cycle_5;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDAD=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_2: begin ADAdd=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_3: begin LDDI=1; if (Mode==2'b00) Write=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_4: begin
				LDALU=1; SaveP=1; Write=1; Set_Addr_To=Set_Addr_To_ZPG;
				if (Mode==2'b00 && IR[0]==1'b1) LDDI = 1;
			end
			Cycle_5: if (Mode==2'b00 && IR[0]==1'b1) begin ALUmore=1; Set_BusA_To=Set_BusA_To_ABC; end
			default: ;
			endcase
		end else begin
			LCycle = Cycle_3;
			if (IR[7:6] != 2'b10 && IR[0]==1'b1) LDA = 1;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDAD=1; Set_Addr_To=Set_Addr_To_ZPG; end
			Cycle_2: begin
				ADAdd = 1;
				if (IR[3:1]==3'b011)   AddY  = 1;
				if (IR[7:5]==3'b100)   Write = 1;
				Set_Addr_To = Set_Addr_To_ZPG;
			end
			default: ;
			endcase
		end
	end

	5'b11001, 5'b11011: begin // abs,Y and undoc
		LCycle = Cycle_4;
		if (IR[7:6] != 2'b10) begin
			LDA = 1;
			if (Mode==2'b00 && IR[1]==1'b1) LCycle = Cycle_6;
		end
		case (MCycle)
		Cycle_1: begin Jump=2'b01; LDBAL=1; end
		Cycle_2: begin Jump=2'b01; Set_BusA_To=Set_BusA_To_Y; BAAdd=2'b10; LDBAH=1; Set_Addr_To=Set_Addr_To_BA; end
		Cycle_3: begin
			BAAdd = 2'b11;
			if (IR[7:5]==3'b100) begin
				Write = 1;
				if (IR[3:0]==4'hB) BAQuirk = 2'b01;
			end
			else if (IR[1]==1'b0 || IR==8'hBB) BreakAtNA = 1;
			Set_Addr_To = Set_Addr_To_BA;
		end
		Cycle_4: if (is_undoc_rmw) begin Set_Addr_To=Set_Addr_To_BA; LDDI=1; Write=1; end
		Cycle_5: begin Write=1; LDALU=1; Set_Addr_To=Set_Addr_To_BA; SaveP=1; end
		Cycle_6: begin ALUmore=1; Set_BusA_To=Set_BusA_To_ABC; end
		default: ;
		endcase
	end

	5'b11100, 5'b11101, 5'b11110, 5'b11111: begin // abs,X and undoc
		if (IR[7:6] != 2'b10 && IR[1] == 1'b1 && (Mode==2'b00 || IR[0]==1'b0)) begin
			LCycle = Cycle_6;
			if (Mode==2'b00 && IR[0]==1'b1) LDA = 1;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDBAL=1; end
			Cycle_2: begin Jump=2'b01; Set_BusA_To=Set_BusA_To_X; BAAdd=2'b10; LDBAH=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_3: begin BAAdd=2'b11; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_4: begin LDDI=1; if (Mode==2'b00) Write=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_5: begin LDALU=1; SaveP=1; Write=1; Set_Addr_To=Set_Addr_To_BA; end
			Cycle_6: if (Mode==2'b00 && IR[0]==1'b1) begin ALUmore=1; Set_BusA_To=Set_BusA_To_ABC; end
			default: ;
			endcase
		end else begin
			LCycle = Cycle_4;
			if (IR[7:6] != 2'b10 && (Mode!=2'b00 || IR[4]==1'b0 || IR[1:0]!=2'b00)) LDA = 1;
			case (MCycle)
			Cycle_1: begin Jump=2'b01; LDBAL=1; end
			Cycle_2: begin
				Jump = 2'b01;
				Set_BusA_To = (IR[7:6]==2'b10 && IR[4:1]==4'b1111) ? Set_BusA_To_Y : Set_BusA_To_X;
				BAAdd = 2'b10; LDBAH = 1; Set_Addr_To = Set_Addr_To_BA;
			end
			Cycle_3: begin
				BAAdd = 2'b11;
				if (IR[7:5]==3'b100) begin
					Write = 1;
					case (IR[1:0])
					2'b00, 2'b10: BAQuirk = 2'b01;
					2'b11:        BAQuirk = 2'b10;
					default: ;
					endcase
				end else BreakAtNA = 1;
				Set_Addr_To = Set_Addr_To_BA;
			end
			default: ;
			endcase
		end
	end

	default: ;
	endcase
end

// ---------------- ALU operation decoder ----------------
// Base ALU op for the RMW / bit-shift group (IR[1:0]==2'b10 and undoc 2'b11 ALUmore=0).
// The extra Mode!=0 override for op 0/1 with IR[4:2]==110 only applies in the
// 2'b10 path (see 65C02 TSB/TRB special-case in the original code).
function automatic T_ALU_OP alu_rmw(input [7:0] ir, input rmw_c02);
	case (ir[7:5])
	3'd0: alu_rmw = (rmw_c02 && ir[4:2]==3'b110 && Mode!=2'b00) ? ALU_OP_INC : ALU_OP_ASL;
	3'd1: alu_rmw = (rmw_c02 && ir[4:2]==3'b110 && Mode!=2'b00) ? ALU_OP_DEC : ALU_OP_ROL;
	3'd2: alu_rmw = ALU_OP_LSR;
	3'd3: alu_rmw = ALU_OP_ROR;
	3'd4: alu_rmw = ALU_OP_BIT;
	3'd5: alu_rmw = ALU_OP_EQ2;
	3'd6: alu_rmw = ALU_OP_DEC;
	default: alu_rmw = ALU_OP_INC;
	endcase
endfunction

// Standard ALU op for IR[1:0]==2'b01.
function automatic T_ALU_OP alu_std(input [2:0] c);
	case (c)
	3'd0: alu_std = ALU_OP_OR;
	3'd1: alu_std = ALU_OP_AND;
	3'd2: alu_std = ALU_OP_EOR;
	3'd3: alu_std = ALU_OP_ADC;
	3'd4: alu_std = ALU_OP_EQ1;
	3'd5: alu_std = ALU_OP_EQ2;
	3'd6: alu_std = ALU_OP_CMP;
	default: alu_std = ALU_OP_SBC;
	endcase
endfunction

always_comb begin
	case (IR[1:0])
	2'b00: begin
		case (IR[4:2])
		3'b000, 3'b001, 3'b011:
			case (IR[7:5])
			3'b110, 3'b111: ALU_Op = ALU_OP_CMP;
			3'b101:         ALU_Op = ALU_OP_EQ2;
			3'b001:         ALU_Op = ALU_OP_BIT;
			default:        ALU_Op = ALU_OP_EQ1;
			endcase
		3'b010:
			case (IR[7:5])
			3'b111, 3'b110: ALU_Op = ALU_OP_INC;
			3'b100:         ALU_Op = ALU_OP_DEC;
			default:        ALU_Op = ALU_OP_EQ2;
			endcase
		3'b110: ALU_Op = (IR[7:5]==3'b100) ? ALU_OP_EQ2 : ALU_OP_EQ1;
		default: ALU_Op = (IR[7:5]==3'b101) ? ALU_OP_EQ2 : ALU_OP_EQ1;
		endcase
	end
	2'b01: ALU_Op = alu_std(IR[7:5]);
	2'b10: ALU_Op = (IR[7:5]==3'd4) ? ((IR[4:2]==3'b010) ? ALU_OP_EQ2 : ALU_OP_EQ1)
	                                : alu_rmw(IR, 1'b1);
	default: begin // IR[1:0]==2'b11 (undocumented)
		if (IR[7:5]==3'd5)      ALU_Op = (IR==8'hBB) ? ALU_OP_AND : ALU_OP_EQ2;
		else if (IR==8'h6B)     ALU_Op = ALU_OP_ARR;
		else if (IR==8'h8B)     ALU_Op = ALU_OP_XAA;
		else if (IR==8'h0B || IR==8'h2B) ALU_Op = ALU_OP_ANC;
		else if (IR==8'hEB)     ALU_Op = ALU_OP_SBC;
		else if (ALUmore)       ALU_Op = alu_std(IR[7:5]);
		else if (IR[7:5]==3'd6 && IR[4:2]==3'b010) ALU_Op = ALU_OP_SAX;
		else                    ALU_Op = alu_rmw(IR, 1'b0);
	end
	endcase
end

endmodule
/* verilator lint_on IMPORTSTAR */
