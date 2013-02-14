library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sc_pack.all;

entity sc_mem_if is
    --  generic (addr_bits : integer);

    port(
        clk, rst        : in    std_logic;

        pll_locked      : in    std_logic;
        dram_clk_skewed : in    std_logic;

        sc_mem_out      : in    sc_out_type;
        sc_mem_in       : out   sc_in_type;

        -- memory interface
        -- SDRAM interface lower chip
        dram0_CLK       : out   std_logic; -- Clock
        dram0_CKE       : out   std_logic; -- Clock Enable
        dram0_RAS_n     : out   std_logic; -- Row Address Strobe
        dram0_CAS_n     : out   std_logic; -- Column Address Strobe
        dram0_WE_n      : out   std_logic; -- Write Enable
        dram0_CS_n      : out   std_logic; -- Chip Select
        dram0_BA_0      : out   std_logic; -- Bank Address
        dram0_BA_1      : out   std_logic; -- Bank Address
        dram0_ADDR      : out   std_logic_vector(12 downto 0); -- SDRAM Address
        dram0_UDQM      : out   std_logic; -- Data mask Upper Byte
        dram0_LDQM      : out   std_logic; -- Data mask Lower Byte
        -- SDRAM interface highier chip
        dram1_CLK       : out   std_logic; -- Clock
        dram1_CKE       : out   std_logic; -- Clock Enable
        dram1_RAS_n     : out   std_logic; -- Row Address Strobe
        dram1_CAS_n     : out   std_logic; -- Column Address Strobe
        dram1_WE_n      : out   std_logic; -- Write Enable
        dram1_CS_n      : out   std_logic; -- Chip Select
        dram1_BA_0      : out   std_logic; -- Bank Address
        dram1_BA_1      : out   std_logic; -- Bank Address
        dram1_ADDR      : out   std_logic_vector(12 downto 0); -- SDRAM Address
        dram1_UDQM      : out   std_logic; -- Data mask Upper Byte
        dram1_LDQM      : out   std_logic; -- Data mask Lower Byte
        -- data bus from both chips
        dram_DQ         : inout std_logic_vector(31 downto 0) -- Data
    );
end sc_mem_if;

use work.sdram_config.all;
use work.sdram_controller_interface.all;

architecture rtl of sc_mem_if is
    constant BURST_LENGTH : natural           := 1;
    -- 100MHz, 2 cycles read data latency
    constant tCLK_PERIOD  : time              := 10 ns;
    constant CAS_LATENCY  : natural           := 2;
    constant SDRAM        : sdram_config_type := GetSDRAMParameters(tCLK_PERIOD, CAS_LATENCY);

    -- Address Mapping => (bank & row & column)
    constant CS_WIDTH    : integer := 0; -- 1 rank
    constant COL_LOW_BIT : integer := 0;
    constant ROW_LOW_BIT : integer := COL_LOW_BIT + SDRAM.COL_WIDTH; -- 9
    constant BA_LOW_BIT  : integer := ROW_LOW_BIT + SDRAM.ROW_WIDTH; -- 9+13=22
    constant CS_LOW_BIT  : integer := BA_LOW_BIT + SDRAM.BA_WIDTH; -- 22+2=24

    --    signal res_cnt : std_logic_vector(2 downto 0) := "000";
    --    attribute altera_attribute : string;
    --    attribute altera_attribute of res_cnt : signal is "POWER_UP_LEVEL=LOW";
    --    signal rst_n : std_logic;

    signal dram_DQM   : std_logic_vector(3 downto 0);
    signal dram_BA    : std_logic_vector(1 downto 0);
    signal dram_ADDR  : std_logic_vector(12 downto 0);
    signal dram_CAS_N : std_logic;
    signal dram_RAS_N : std_logic;
    signal dram_CKE   : std_logic;
    signal dram_CS_N  : std_logic_vector(2 ** CS_WIDTH - 1 downto 0);
    signal dram_WE_N  : std_logic;

    signal ocpMaster : SDRAM_controller_master_type;
    signal ocpSlave  : SDRAM_controller_slave_type;

    -- attribute ALTERA_ATTRIBUTE : string;
    attribute ALTERA_ATTRIBUTE of dram0_BA_0 : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_BA_1 : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_CAS_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_RAS_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_WE_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_CS_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_CKE : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_ADDR : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_UDQM : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram0_LDQM : signal is "FAST_OUTPUT_REGISTER=ON";

    attribute ALTERA_ATTRIBUTE of dram1_BA_0 : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_BA_1 : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_CAS_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_RAS_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_WE_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_CS_n : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_CKE : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_ADDR : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_UDQM : signal is "FAST_OUTPUT_REGISTER=ON";
    attribute ALTERA_ATTRIBUTE of dram1_LDQM : signal is "FAST_OUTPUT_REGISTER=ON";

    attribute ALTERA_ATTRIBUTE of dram_DQ : signal is "FAST_INPUT_REGISTER=ON;FAST_OUTPUT_REGISTER=ON";
--attribute ALTERA_ATTRIBUTE of sdram_DQoe_r: port is "FAST_OUTPUT_REGISTER=ON";

begin

    --    process(sys_clk, pll_locked)
    --    begin
    --        if pll_locked = '0' then
    --            res_cnt  <= "000";
    --            rst  <= '1';
    --        elsif rising_edge(sys_clk) then
    --            if (res_cnt /= "111") then
    --                res_cnt <= std_logic_vector(unsigned(res_cnt) + 1);
    --            end if;
    --            rst <= not res_cnt(0) or not res_cnt(1) or not res_cnt(2);
    --        end if;
    --    end process;
    --    rst_n  <= not rst;

    --    pll : entity work.de2_70_sdram_pll
    --        port map(
    --            inclk0 => clk,
    --            c0     => sys_clk,
    --            c1     => dram_clk,
    --            c2     => dram_clk_skew,
    --            locked => pll_locked);

    sdr_sdram_inst : entity work.sdr_sdram
        generic map(
            USE_AUTOMATIC_REFRESH => SDRAM_USE_AUTOMATIC_REFRESH,
            BURST_LENGTH          => BURST_LENGTH,
            SDRAM                 => SDRAM,
            CS_WIDTH              => CS_WIDTH,
            CS_LOW_BIT            => CS_LOW_BIT,
            BA_LOW_BIT            => BA_LOW_BIT,
            ROW_LOW_BIT           => ROW_LOW_BIT,
            COL_LOW_BIT           => COL_LOW_BIT)
        port map(
            rst         => rst,
            clk         => clk,
            pll_locked  => pll_locked,
            ocpSlave    => ocpSlave,
            ocpMaster   => ocpMaster,
            sdram_CKE   => dram_CKE,
            sdram_RAS_n => dram_RAS_n,
            sdram_CAS_n => dram_CAS_n,
            sdram_WE_n  => dram_WE_n,
            sdram_CS_n  => dram_CS_n,
            sdram_BA    => dram_BA,
            sdram_SA    => dram_ADDR,
            sdram_DQ    => dram_DQ,
            sdram_DQM   => dram_DQM);

    -- Early ready acknowledgement
    --  process (rst, clk)
    --  begin  -- process
    --    if rst = '1' then
    --      rdy_cnt <= "0000";
    --    elsif clk'event and clk = '1' then  -- rising clock edge
    --      if sc_mem_out.wr = '1' then
    --         rdy_cnt <= SDRAM.RAS+SDRAM.CAC;		
    --		elsif sc_mem_out.rd = '1' then
    --         rdy_cnt <= SDRAM.RAS;				
    --		elsif rdy_cnt <> "0000" then
    --			rdy_cnt <= std_logic_vector(unsigned(rdy_cnt)-1);
    --		end if;
    --    end if;
    --  end process;
    --  sc_mem_in.rdy_cnt <= "11" when rdy_cnt(3 downto 2) <> "00" else
    --		rdy_cnt(1 downto 0);


    process(clk, rst)
    begin                               -- process
        if rst = '1' then
            sc_mem_in.rdy_cnt <= "11";
        elsif clk'event and clk = '1' then -- rising clock edge
            -- Hold Cmd and Address
            if sc_mem_out.wr = '1' or sc_mem_out.rd = '1' then
                ocpMaster.MCmd  <= '0' & sc_mem_out.wr & sc_mem_out.rd;
                ocpMaster.MAddr <= sc_mem_out.address(ocpMaster.MAddr'range);
            end if;
            -- Hold Write data
            if sc_mem_out.wr = '1' then
                ocpMaster.MData <= sc_mem_out.wr_data;
            end if;
            -- Hold Read result
            if ocpSlave.SResp = '1' then
                sc_mem_in.rd_data <= ocpSlave.SData;
            end if;
            if ocpMaster.MCmd /= "000" and ocpSlave.SCmdAccept = '1' then
                ocpMaster.MCmd <= "000";
            end if;
            -- Simple (late) ready acknowledgement for now.
            if sc_mem_out.wr = '1' or sc_mem_out.rd = '1' then
                sc_mem_in.rdy_cnt <= "11";
            elsif ocpSlave.SDataAccept = '1' or ocpSlave.SResp = '1' then
                sc_mem_in.rdy_cnt <= "00";
            end if;
        end if;
    end process;

    -- Write 
    ocpMaster.MDataByteEn <= (others => '1');

    dram0_BA_0  <= dram_BA(0);
    dram0_BA_1  <= dram_BA(1);
    dram1_BA_0  <= dram_BA(0);
    dram1_BA_1  <= dram_BA(1);
    dram0_UDQM  <= dram_DQM(1);
    dram0_LDQM  <= dram_DQM(0);
    dram1_UDQM  <= dram_DQM(3);
    dram1_LDQM  <= dram_DQM(2);
    dram0_ADDR  <= dram_ADDR;
    dram1_ADDR  <= dram_ADDR;
    dram0_CAS_N <= dram_CAS_N;
    dram1_CAS_N <= dram_CAS_N;
    dram0_CKE   <= dram_CKE;
    dram1_CKE   <= dram_CKE;
    dram0_CLK   <= dram_clk_skewed;
    dram1_CLK   <= dram_clk_skewed;
    dram0_CS_N  <= dram_CS_N(0);
    dram1_CS_N  <= dram_CS_N(0);
    dram0_RAS_N <= dram_RAS_N;
    dram1_RAS_N <= dram_RAS_N;
    dram0_WE_N  <= dram_WE_N;
    dram1_WE_N  <= dram_WE_N;

end rtl;
