----------------------------------------------------------------------------------
--
-- Description:
-- ROM with generic size, with ext. load function: sync. write, sync. read
--
-- (c) copyright 2011...2013 by Wolfgang Scherr
-- http://www.pin4.at - ws_arcade <at> pin4.at
--
-- All Rights Reserved
--
-- Version 1.0
-- SVN: $Id: generic_rom.vhd 647 2014-05-17 11:47:04Z wolfgang.scherr $
--
-------------------------------------------------------------------------------
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

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity generic_rom is
  generic (
    ADDR_WIDTH  : integer := 11;  -- address width (default 11 bit for 2k ROM)
    DATA_WIDTH  : integer := 8;   -- data width (default 8 bit)
    START_AI    : std_logic_vector(15 downto 0) := (others => '0'); -- allocated configuration bus address
    FILE_NAME   : string := "" -- assign in instance to binary filename to load in simulations
  );
  port (
    CLK         : in    std_logic;
    -- standard channel (read only)
    ENA         : in    std_logic;
    ADDR        : in    std_logic_vector(ADDR_WIDTH-1 downto 0);
    DATA        : out   std_logic_vector(DATA_WIDTH-1 downto 0);
    -- setup bus (write only, check also START_AI)
    CONF_WR     : in    std_logic;
    CONF_AI     : in    std_logic_vector(15 downto 0);
    CONF_DI     : in    std_logic_vector(DATA_WIDTH-1 downto 0)
    );
end;

architecture RTL of generic_rom is

  -- integer storage is more efficient in simulation than std_logic_vector
  type ROM_ARRAY is array(0 to (2**ADDR_WIDTH)-1) of integer range (2**DATA_WIDTH)-1 downto 0;
  signal ROM : ROM_ARRAY;

	attribute ram_init_file : string;
	attribute ram_init_file of ROM : signal is FILE_NAME;
  
begin

  p_rom : process (CLK) is
  begin
    if rising_edge(CLK) then
      --if (ENA = '1') then
         DATA <= std_logic_vector(to_unsigned(ROM(to_integer(unsigned(ADDR))),8));
      --end if;
    end if;
  end process p_rom;

  p_conf : process (CLK) is
  begin
    if rising_edge(CLK) then
      if (CONF_WR='1') and (CONF_AI(15 downto ADDR_WIDTH)=START_AI(15 downto ADDR_WIDTH)) then
        ROM(to_integer(unsigned(CONF_AI(ADDR_WIDTH-1 downto 0)))) <= to_integer(unsigned(CONF_DI));
      end if;
    end if;
  end process p_conf;
end RTL;
