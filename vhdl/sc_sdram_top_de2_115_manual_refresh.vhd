library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_config.all;
use work.sdram_controller_interface.all;

entity sc_sdram_top is
	port(
		clk             : in    std_logic;
		rst             : in    std_logic;

		pll_locked      : in    std_logic;
		dram_clk_skewed : in    std_logic;

		-- User interface
		M_Cmd           : in    std_logic_vector(2 downto 0);
		M_Addr          : in    std_logic_vector(26 downto 0);
		M_Data          : in    std_logic_vector(31 downto 0);
		M_DataValid     : in    std_logic;
		M_DataByteEn    : in    std_logic_vector(3 downto 0);
		S_Resp          : out   std_logic_vector(1 downto 0);
		S_Data          : out   std_logic_vector(31 downto 0);
		S_CmdAccept     : out   std_logic;
		S_DataAccept    : out   std_logic;
		M_CmdRefresh    : in    std_logic;
		S_RefreshAccept : out   std_logic;

		-- memory interface
		dram_CLK       : out   std_logic; -- Clock
		dram_CKE       : out   std_logic; -- Clock Enable
		dram_RAS_n     : out   std_logic; -- Row Address Strobe
		dram_CAS_n     : out   std_logic; -- Column Address Strobe
		dram_WE_n      : out   std_logic; -- Write Enable
		dram_CS_n      : out   std_logic; -- Chip Select
		dram_BA_0      : out   std_logic; -- Bank Address
		dram_BA_1      : out   std_logic; -- Bank Address
		dram_ADDR      : out   std_logic_vector(12 downto 0); -- SDRAM Address
		-- SDRAM interface lower chip
		dram0_UDQM      : out   std_logic; -- Data mask Upper Byte
		dram0_LDQM      : out   std_logic; -- Data mask Lower Byte
		-- SDRAM interface highier chip
		dram1_UDQM      : out   std_logic; -- Data mask Upper Byte
		dram1_LDQM      : out   std_logic; -- Data mask Lower Byte
		-- data bus from both chips
		dram_DQ         : inout std_logic_vector(31 downto 0) -- Data
	);
end sc_sdram_top;

architecture rtl of sc_sdram_top is
	constant BURST_LENGTH : natural           := 4;
	-- 100MHz, 2 cycles read data latency
	constant tCLK_PERIOD  : time              := 12.5 ns; --changed
	constant CAS_LATENCY  : natural           := 2;
	constant SDRAM        : sdram_config_type := GetSDRAMParameters(tCLK_PERIOD, CAS_LATENCY);

	-- Address Mapping => (bank & row & column)
	constant CS_WIDTH    : integer := 0; -- 1 rank
	constant COL_LOW_BIT : integer := 0;
	constant ROW_LOW_BIT : integer := COL_LOW_BIT + SDRAM.COL_WIDTH; -- 10
	constant BA_LOW_BIT  : integer := ROW_LOW_BIT + SDRAM.ROW_WIDTH; -- 10+13=23
	constant CS_LOW_BIT  : integer := BA_LOW_BIT + SDRAM.BA_WIDTH; -- 23+2=25

	signal dram_DQM_int   : std_logic_vector(3 downto 0);
	signal dram_BA_int    : std_logic_vector(1 downto 0);
	signal dram_ADDR_int  : std_logic_vector(12 downto 0);
	signal dram_CAS_N_int : std_logic;
	signal dram_RAS_N_int : std_logic;
	signal dram_CKE_int   : std_logic;
	signal dram_CS_N_int  : std_logic_vector(2 ** CS_WIDTH - 1 downto 0);
	signal dram_WE_N_int  : std_logic;

	attribute ALTERA_ATTRIBUTE : string;
	attribute ALTERA_ATTRIBUTE of dram_BA_0 : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_BA_1 : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_CAS_n : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_RAS_n : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_WE_n : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_CS_n : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_CKE : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram_ADDR : signal is "FAST_OUTPUT_REGISTER=ON";
	
	attribute ALTERA_ATTRIBUTE of dram0_UDQM : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram0_LDQM : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram1_UDQM : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of dram1_LDQM : signal is "FAST_OUTPUT_REGISTER=ON";

	attribute ALTERA_ATTRIBUTE of dram_DQ : signal is "FAST_INPUT_REGISTER=ON;FAST_OUTPUT_REGISTER=ON";
--attribute ALTERA_ATTRIBUTE of sdram_DQoe_r: port is "FAST_OUTPUT_REGISTER=ON";

	signal M_Addr_D1 : std_logic_vector(26 downto 0);
	signal M_Data_D1 : std_logic_vector(31 downto 0);
	signal M_DataValid_D1 : std_logic;
	signal M_DataByteEn_D1 : std_logic_vector(3 downto 0);
				
	signal M_Addr_D2 : std_logic_vector(26 downto 0);
	signal M_Data_D2 : std_logic_vector(31 downto 0);
	signal M_DataValid_D2 : std_logic;
	signal M_DataByteEn_D2 : std_logic_vector(3 downto 0);
	
	signal M_Addr_int : std_logic_vector(26 downto 0);
	signal M_Data_int : std_logic_vector(31 downto 0);
	signal M_DataValid_int : std_logic;
	signal M_DataByteEn_int : std_logic_vector(3 downto 0);
	
	signal S_CmdAccept_int : std_logic;
					
	signal M_Cmd_curr, M_Cmd_int, M_Cmd_hold : std_logic_vector(2 downto 0);
	
begin
	S_Resp(1) <= '0';

	dram_BA_0  <= dram_BA_int(0);
	dram_BA_1  <= dram_BA_int(1);
	dram0_UDQM  <= dram_DQM_int(1);
	dram0_LDQM  <= dram_DQM_int(0);
	dram1_UDQM  <= dram_DQM_int(3);
	dram1_LDQM  <= dram_DQM_int(2);
	dram_ADDR  <= dram_ADDR_int;
	dram_CAS_N <= dram_CAS_N_int;
	dram_CKE   <= dram_CKE_int;
	dram_CLK   <= dram_clk_skewed;
	dram_CS_N  <= dram_CS_N_int(0);
	dram_RAS_N <= dram_RAS_N_int;
	dram_WE_N  <= dram_WE_N_int;
	
	S_CmdAccept <= S_CmdAccept_int;

	sdr_sdram_inst : entity work.sdr_sdram
		generic map(
			USE_AUTOMATIC_REFRESH => SDRAM_USE_MANUAL_REFRESH,
			BURST_LENGTH          => BURST_LENGTH,
			SDRAM                 => SDRAM,
			CS_WIDTH              => CS_WIDTH,
			CS_LOW_BIT            => CS_LOW_BIT,
			BA_LOW_BIT            => BA_LOW_BIT,
			ROW_LOW_BIT           => ROW_LOW_BIT,
			COL_LOW_BIT           => COL_LOW_BIT)
		port map(
			rst                          => rst,
			clk                          => clk,
			pll_locked                   => pll_locked,

			-- User interface
			ocpMaster.MCmd               => M_Cmd_int, --   : in    SDRAM_controller_master_type;
			ocpMaster.MAddr              => M_Addr(26 downto 2),--M_Addr_int(26 downto 2), --
			ocpMaster.MData              => M_Data_int, --
			ocpMaster.MDataValid         => M_DataValid_int, --
			ocpMaster.MDataByteEn        => M_DataByteEn_int, --
			ocpMaster.MFlag_CmdRefresh   => M_CmdRefresh, --
			ocpSlave.SCmdAccept          => S_CmdAccept_int, --    : out   SDRAM_controller_slave_type;
			ocpSlave.SDataAccept         => S_DataAccept, --
			ocpSlave.SData               => S_Data, --
			ocpSlave.SResp               => S_Resp(0), --
			ocpSlave.SRespLast           => open, --
			ocpSlave.SFlag_RefreshAccept => S_RefreshAccept, --_next, --

			sdram_CKE                    => dram_CKE_int,
			sdram_RAS_n                  => dram_RAS_n_int,
			sdram_CAS_n                  => dram_CAS_n_int,
			sdram_WE_n                   => dram_WE_n_int,
			sdram_CS_n                   => dram_CS_n_int,
			sdram_BA                     => dram_BA_int,
			sdram_SA                     => dram_ADDR_int,
			sdram_DQ                     => dram_DQ,
			sdram_DQM                    => dram_DQM_int);

-- Logic added to complyt with the TDM tree almost OCP protocol
	delay_regs : process(clk) is
	begin
		if rising_edge(clk) then
			if (rst = '1') then
				M_Addr_D1 <= (others => '0');
				M_Data_D1 <= (others => '0');
				M_DataValid_D1 <= '0';
				M_DataByteEn_D1 <= (others => '0');
				
				M_Addr_D2 <= (others => '0');
				M_Data_D2 <= (others => '0');
				M_DataValid_D2 <= '0';
				M_DataByteEn_D2 <= (others => '0');
				
				M_Cmd_curr <= (others => '0');
				M_Cmd_hold <= (others => '0');
			else
				M_Addr_D1 <= M_Addr;
				M_Data_D1 <= M_Data;
				M_DataValid_D1 <= M_DataValid;
				M_DataByteEn_D1 <= M_DataByteEn;
				
				M_Addr_D2 <= M_Addr_D1;
				M_Data_D2 <= M_Data_D1;
				M_DataValid_D2 <= M_DataValid_D1;
				M_DataByteEn_D2 <= M_DataByteEn_D1;
				
				if (M_Cmd /= "000") then -- IDLE command
					M_Cmd_curr <= M_Cmd;
					M_Cmd_hold <= M_Cmd;
				end if;
				
				if (S_CmdAccept_int = '1') then
					M_Cmd_hold <= (others => '0');
				end if;
				
		
			end if;
		end if;	
	end process delay_regs;

	
	delay_mux : process (M_Cmd, M_Cmd_curr, M_cmd_hold, M_Addr_D2, M_Data_D2, M_DataValid_D2, M_DataByteEn_D2, M_Addr, M_Data, M_DataValid, M_DataByteEn) is
   begin
      case M_Cmd is
         when "000"  => --idle
				M_Cmd_int <= M_cmd_hold;
				case M_Cmd_curr is
				when "001"  => --write
					M_Addr_int <= M_Addr_D2;
					M_Data_int <= M_Data_D2;
					M_DataValid_int <= M_DataValid_D2;
					M_DataByteEn_int <= M_DataByteEn_D2;
				when "010"  => --read
					M_Addr_int <= M_Addr;
					M_Data_int <= M_Data;
					M_DataValid_int <= M_DataValid;
					M_DataByteEn_int <= M_DataByteEn;
				when others => 
					M_Addr_int <= M_Addr;
					M_Data_int <= M_Data;
					M_DataValid_int <= M_DataValid;
					M_DataByteEn_int <= M_DataByteEn;
				end case;
				
         when "001"  => --write
				M_Cmd_int <= M_cmd;
				M_Addr_int <= M_Addr_D2;
				M_Data_int <= M_Data_D2;
				M_DataValid_int <= M_DataValid_D2;
				M_DataByteEn_int <= M_DataByteEn_D2;
         when "010"  => --read
				M_Cmd_int <= M_cmd;
				M_Addr_int <= M_Addr;
				M_Data_int <= M_Data;
				M_DataValid_int <= M_DataValid;
				M_DataByteEn_int <= M_DataByteEn;
         when others => 
				M_Cmd_int <= M_cmd;
				M_Addr_int <= M_Addr;
				M_Data_int <= M_Data;
				M_DataValid_int <= M_DataValid;
				M_DataByteEn_int <= M_DataByteEn;
      end case;
   end process delay_mux;
			
			
	-- Registers
--	reg : process(clk) is
--	begin
--		if rising_edge(clk) then
--			if (rst = '1') then
--				S_RefreshAccept <= '0';
--			else
--				S_RefreshAccept <= S_RefreshAccept_next;
--			end if;
--		end if;	
--	end process reg;

end rtl;
