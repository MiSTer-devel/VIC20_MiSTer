-------------------------------------------------------------------------------
--
-- Basic RC low/highpass filter implemented as IIR filter for FPGA usage
--
-- (c) copyright 2013...2015 by WoS (Wolfgang Scherr)
-- http://www.pin4.at - WoS <at> pin4 <dot> at
--
-- All rights reserved. Use at your own risk.
--
-- This basic RC math can be found in similar fashion e.g. on the English Wiki
--                                http://en.wikipedia.org/wiki/Low-pass_filter
--
-------------------------------------------------------------------------------
-- Contributors:
--
-- Status: functional
--
-- SVN: $Id: rc_filter_1o.vhd 1328 2015-05-22 19:29:53Z wolfgang.scherr $
--
-- Change list:
--   Wolfgang:  initial set up
--
----------------------------------------------------------------------
-- Redistribution and use in source or synthesized forms are permitted
-- provided that the following conditions are met (or a prior written
-- permission was given otherwise):
--
-- * Redistributions of source code must retain this original header
--   incl. author, contributors, conditions, copyright and disclaimer.
--
-- * Redistributions in synthesized (binary) form must also contain
--   the soure code according to this conditions to keep it "open".
--
-- * Neither the name of the author nor the names of contributors may
--   be used to endorse or promote products derived from this code.
--
-- * This code is only allowed to be used on:
--   - Replay hardware (from fpgaarcade.com)
--
-- * Feedback or bug reports are welcome, but please check on the 
--   web sites given in the header first for any updates available.
--
-- * You are responsible for any legal issues arising from your use
--   or your own distribution of this code.
----------------------------------------------------------------------
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS  CODE OR ANY WORK
-- PRODUCTS, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
----------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity rc_filter_1o is
    generic ( dwidthi_g    : natural := 8;       -- bitwidth of the (digital) source
              dwidtho_g    : natural := 8;       -- output may use a different width (but <=dwidthi_g+cwidth_g)
              R_ohms_g     : natural := 1000;    -- 1kOhms  \  exemplary LP
              C_p_farads_g : natural := 10000;   -- 10 nF   /  with ~16kHz fg
              fclk_hz_g    : natural := 8867236; -- set accordingly to your clk_i+clken_i period 
              highpass_g   : boolean := false;   -- lowpass (unsigned out, gain=1.0) when false, highpass (signed out, gain=0.5) when true
              cwidth_g     : natural := 12);     -- adopt resolution for RC and fclk (-> alpha factor)
    Port ( clk_i   : in   std_logic;
           clken_i : in   std_logic;
           res_i   : in   std_logic;
           din_i   : in   std_logic_vector(dwidthi_g-1 downto 0);  -- range: 0...(2^dwidthi_g)-1
           dout_o  : out  std_logic_vector(dwidtho_g-1 downto 0)   -- range: 0...(2^dwidtho_g)-1
           );
end rc_filter_1o;

architecture RTL of rc_filter_1o is

-- alpha factor for the IIR filter, classic "textbook" implementation
constant tau         : real := real(R_ohms_g*C_p_farads_g)/1.0E12; -- s
constant tsamp       : real := 1.0/real(fclk_hz_g);                -- s
constant alpha_c     : real := tsamp/(tau+tsamp);

-- digitising the alpha factor, including some checks (shown as assert later on)
constant dig_alpha_c : integer := integer((2.0**cwidth_g)*alpha_c);
constant acheck_c    : real := real(dig_alpha_c)/(2.0**cwidth_g);
constant aerror_c    : real := 100.0*ABS(acheck_c-alpha_c)/acheck_c;

-- internal bitwidth depends on the FP decimals of the alpha factor and the input bit width
signal dlocal_s : signed (dwidthi_g+cwidth_g downto 0);

begin
  -- print actual digitised alpha value and its error
  assert false report "Using alpha: " & natural'image(dig_alpha_c) & " (= " & real'image(acheck_c) & ")" severity note;
  assert false report "Alpha error: " & real'image(aerror_c) & "%" severity note;

  -- the main IIC routine, classic "textbook" implementation
  iir_proc : process (res_i, clk_i) is
  begin
    if (res_i = '1') then
      dlocal_s <= (others => '0');
    elsif rising_edge(clk_i) then
      if (clken_i = '1') then
        dlocal_s <= dlocal_s + conv_signed(dig_alpha_c,cwidth_g)*(unsigned(din_i)-dlocal_s(dwidthi_g+cwidth_g downto cwidth_g));
      end if;
    end if;
  end process iir_proc;

  -- output with optional re-scaling of bitwidth
  output_map : process (din_i, dlocal_s) is
    -- helper signal for delta generation in HP mode
    variable din_resize_s : signed (dwidthi_g+cwidth_g downto 0);
    -- helper signal for final subtraction (HP mode)
    variable outsum_a_s   : signed (dwidtho_g downto 0);
    variable outsum_b_s   : signed (dwidtho_g downto 0);
    variable outsum_q_s   : signed (dwidtho_g downto 0);
  begin
    if (highpass_g) then
      -- helper signals to keep track of range/sign and decimals
      din_resize_s := "0" & signed(din_i) & conv_signed(0,cwidth_g);
      outsum_a_s   := "0" & din_resize_s(dwidthi_g+cwidth_g-1 downto cwidth_g-dwidtho_g+dwidthi_g);
      outsum_b_s   := "0" & dlocal_s(dwidthi_g+cwidth_g-1 downto cwidth_g-dwidtho_g+dwidthi_g);
      outsum_q_s   := outsum_a_s - outsum_b_s;
      -- output is signed, so the filter has a gain of 0.5 if input and output width is the same
      dout_o <= std_logic_vector(outsum_q_s(dwidtho_g downto 1));
    else
      -- output is unsigned, the filter has a gain of 1.0 if input and output width is the same
      dout_o <= std_logic_vector(dlocal_s(dwidthi_g+cwidth_g-1 downto cwidth_g-dwidtho_g+dwidthi_g));
    end if;
  end process output_map;

end RTL;
