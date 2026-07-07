// -- ****
// -- T65(b) core. In an effort to merge and maintain bug fixes ....
// --
// -- See list of changes in T65 top file (T65.vhd)...
// --
// -- ****
// -- 65xx compatible microprocessor core
// --
// -- FPGAARCADE SVN: $Id: T65_Pack.vhd 1234 2015-02-28 20:14:50Z wolfgang.scherr $
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

// Shared constants and types for the T65 CPU core.
package T65_Pack;
	localparam int Flag_C = 0;
	localparam int Flag_Z = 1;
	localparam int Flag_I = 2;
	localparam int Flag_D = 3;
	localparam int Flag_B = 4;
	localparam int Flag_1 = 5;
	localparam int Flag_V = 6;
	localparam int Flag_N = 7;

	typedef logic [2:0] T_Lcycle;
	localparam T_Lcycle Cycle_sync = 3'b000;
	localparam T_Lcycle Cycle_1    = 3'b001;
	localparam T_Lcycle Cycle_2    = 3'b010;
	localparam T_Lcycle Cycle_3    = 3'b011;
	localparam T_Lcycle Cycle_4    = 3'b100;
	localparam T_Lcycle Cycle_5    = 3'b101;
	localparam T_Lcycle Cycle_6    = 3'b110;
	localparam T_Lcycle Cycle_7    = 3'b111;

	typedef enum logic [3:0] {
		Set_BusA_To_DI,
		Set_BusA_To_ABC,
		Set_BusA_To_X,
		Set_BusA_To_Y,
		Set_BusA_To_S,
		Set_BusA_To_P,
		Set_BusA_To_DA,
		Set_BusA_To_DAO,
		Set_BusA_To_DAX,
		Set_BusA_To_AAX,
		Set_BusA_To_DONTCARE
	} T_Set_BusA_To;

	typedef enum logic [1:0] {
		Set_Addr_To_PBR,
		Set_Addr_To_SP,
		Set_Addr_To_ZPG,
		Set_Addr_To_BA
	} T_Set_Addr_To;

	typedef enum logic [3:0] {
		Write_Data_DL,
		Write_Data_ABC,
		Write_Data_X,
		Write_Data_Y,
		Write_Data_S,
		Write_Data_P,
		Write_Data_PCL,
		Write_Data_PCH,
		Write_Data_AX,
		Write_Data_AXB,
		Write_Data_XB,
		Write_Data_YB,
		Write_Data_DONTCARE
	} T_Write_Data;

	typedef enum logic [4:0] {
		ALU_OP_OR,
		ALU_OP_AND,
		ALU_OP_EOR,
		ALU_OP_ADC,
		ALU_OP_EQ1,
		ALU_OP_EQ2,
		ALU_OP_CMP,
		ALU_OP_SBC,
		ALU_OP_ASL,
		ALU_OP_ROL,
		ALU_OP_LSR,
		ALU_OP_ROR,
		ALU_OP_BIT,
		ALU_OP_DEC,
		ALU_OP_INC,
		ALU_OP_ARR,
		ALU_OP_ANC,
		ALU_OP_SAX,
		ALU_OP_XAA
	} T_ALU_OP;

	typedef struct packed {
		logic [7:0] I;
		logic [7:0] A;
		logic [7:0] X;
		logic [7:0] Y;
		logic [7:0] S;
		logic [7:0] P;
	} T_t65_dbg;
endpackage
