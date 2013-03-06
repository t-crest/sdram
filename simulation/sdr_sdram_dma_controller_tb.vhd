library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sdr_sdram_dma_controller_tb is
end entity sdr_sdram_dma_controller_tb;

use std.textio.all;
use work.dma_controller_dtl_cmp_pkg.all;
use work.sdram_config.all;
use work.sdram_controller_interface.all;

architecture RTL of sdr_sdram_dma_controller_tb is
    function REFI_override(config : sdram_config_type; REFI: natural) return sdram_config_type is
      variable result : sdram_config_type;
    begin
      result := config;
      result.REFI := REFI;
      return result;
    end function REFI_override;

    constant BURST_LENGTH : natural := 8;
    -- 100MHz, 2 cycles read data latency
    constant tCLK_PERIOD  : time              := 20 ns;
    constant CAS_LATENCY  : natural           := 2;
    constant SDRAM_default: sdram_config_type := GetSDRAMParameters(tCLK_PERIOD, CAS_LATENCY);
    -- Make refresh frequent to see the interference with other commands
    constant SDRAM : sdram_config_type := REFI_override(SDRAM_default, 10);


    -- Address Mapping => (bank & row & column)
    constant CS_WIDTH    : integer := 0; -- 1 rank
    constant COL_LOW_BIT : integer := 0;
    constant ROW_LOW_BIT : integer := COL_LOW_BIT + SDRAM.COL_WIDTH; -- 9
    constant BA_LOW_BIT  : integer := ROW_LOW_BIT + SDRAM.ROW_WIDTH; -- 9+13=22
    constant CS_LOW_BIT  : integer := BA_LOW_BIT + SDRAM.BA_WIDTH; -- 22+2=24

    constant DQ_WIDTH         : integer := 8;
    constant MTL_MASK_WIDTH   : integer := 4;
    --	constant MTL_SIZE_WIDTH   : integer := 5;
    constant MTL_SIZE_WIDTH   : integer := 5;
    constant MTL_ADDR_WIDTH   : integer := SDRAM_ADDR_WIDTH;
    -- Request size in bytes
    --	constant GEN_REQUEST_SIZE : integer := 64;
    constant GEN_REQUEST_SIZE : integer := 4 * BURST_LENGTH;
    -- DMA control interface
    constant DMA_ADDR_WIDTH   : integer := 3;
    constant DMA_DATA_WIDTH   : integer := 32;

    constant DATA_WIDTH_BITS : integer := 4 * DQ_WIDTH;
    constant NWORDS_PER_CMD  : integer := GEN_REQUEST_SIZE * 8 / DATA_WIDTH_BITS;

    constant DEBUG_SHOW_MEMORY_TRANSACTIONS    : boolean := true;
    constant DEBUG_SHOW_CACHELINE_TRANSACTIONS : boolean := true;


    --===========================================================
    -- Timing parameters for IS42S16160B: speed grade -7, tCL=3
    --===========================================================
    constant tOH         : TIME    := 3.0 ns;
    constant tMRD_CYCLES : INTEGER := 2; -- 2 Clk Cycles
    constant tRAS        : TIME    := SDRAM.RAS * tCLK_PERIOD;
    constant tRC         : TIME    := SDRAM.RC * tCLK_PERIOD;
    constant tRCD        : TIME    := SDRAM.RCD * tCLK_PERIOD;
    constant tRP         : TIME    := SDRAM.RP * tCLK_PERIOD;
    constant tRRD        : TIME    := SDRAM.RRD * tCLK_PERIOD;
    constant tWRa        : TIME    := 6.0 ns - 6 ns; -- A2 Version - Auto precharge mode only (1 Clk + 6 ns)
    constant tWRp        : TIME    := 20 ns + 14.0 ns; -- A2 Version - Precharge mode only (12 ns)
    constant tCH         : TIME    := 2.5 ns;
    constant tCL         : TIME    := 2.5 ns;
    constant tCK         : TIME    := 10 ns;
    constant tAS         : TIME    := 2 ns;
    constant tDS         : TIME    := 2 ns;
    constant tCKS        : TIME    := tDS;
    constant tCMS        : TIME    := tDS;
    constant tDH         : TIME    := 0 ns; --1 ns; -- We use 0 delay behavioural model, so Hold violation checks are disabled
    constant tCKH        : TIME    := tDH;
    constant tCMH        : TIME    := tDH;

    subtype dma_word_t is std_logic_vector(DMA_DATA_WIDTH - 1 downto 0);

    signal clk                  : std_logic;
    signal rst_n                : std_logic;
    signal mtl_wr_valid_i       : std_logic;
    signal mtl_cmd_valid_i      : std_logic;
    signal mtl_cmd_accept_i     : std_logic;
    signal mtl_cmd_addr_i       : std_logic_vector(MTL_ADDR_WIDTH - 1 downto 0);
    signal mtl_cmd_read_i       : std_logic;
    signal mtl_cmd_block_size_i : std_logic_vector(MTL_SIZE_WIDTH - 1 downto 0);
    signal mtl_wr_last_i        : std_logic;
    signal mtl_flush_i          : std_logic;
    signal mtl_wr_accept_i      : std_logic;
    signal mtl_wr_data_i        : std_logic_vector(4 * DQ_WIDTH - 1 downto 0);
    signal mtl_wr_mask_i        : std_logic_vector(MTL_MASK_WIDTH - 1 downto 0);

    signal mtl_rd_last_i      : std_logic;
    signal mtl_rd_valid_i     : std_logic;
    signal mtl_rd_accept_i    : std_logic;
    signal mtl_rd_data_i      : std_logic_vector(4 * DQ_WIDTH - 1 downto 0);

    signal dma_addr_special_i : std_logic;
    signal dma_addr_i         : std_logic_vector(DMA_ADDR_WIDTH - 1 downto 0);
    signal dma_rd_i           : std_logic;
    signal dma_rd_data_i      : std_logic_vector(DMA_DATA_WIDTH - 1 downto 0);
    signal dma_wr_i           : std_logic;
    signal dma_wr_data_i      : std_logic_vector(DMA_DATA_WIDTH - 1 downto 0);

    signal end_of_sim : std_logic := '0';

    component mt48lc8m16a2
        PORT(
            Dq    : INOUT STD_LOGIC_VECTOR(15 DOWNTO 0) := (OTHERS => 'Z');
            Addr  : IN    STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
            Ba    : IN    STD_LOGIC_VECTOR              := "00";
            Clk   : IN    STD_LOGIC                     := '0';
            Cke   : IN    STD_LOGIC                     := '0';
            Cs_n  : IN    STD_LOGIC                     := '1';
            Ras_n : IN    STD_LOGIC                     := '0';
            Cas_n : IN    STD_LOGIC                     := '0';
            We_n  : IN    STD_LOGIC                     := '0';
            Dqm   : IN    STD_LOGIC_VECTOR(1 DOWNTO 0)  := (OTHERS => '0')
        );
    END component;

    shared variable L : line;

    signal clk_ram     : std_logic := '0';
    signal rst         : std_logic;
    signal sdram_sa    : std_logic_vector(SDRAM.SA_WIDTH - 1 downto 0);
    signal sdram_ba    : std_logic_vector(SDRAM.BA_WIDTH - 1 downto 0);
    signal sdram_cs_n  : std_logic_vector(2 ** CS_WIDTH - 1 downto 0);
    signal sdram_cke   : std_logic;
    signal sdram_ras_n : std_logic;
    signal sdram_cas_n : std_logic;
    signal sdram_we_n  : std_logic;
    signal sdram_dq    : std_logic_vector(SDRAM_DATA_WIDTH - 1 downto 0);
    signal sdram_dqm   : std_logic_vector(SDRAM_DATA_WIDTH / 8 - 1 downto 0);
    signal sdram_dq_out : STD_LOGIC_VECTOR(SDRAM_DATA_WIDTH - 1 DOWNTO 0);
    signal sdram_dq_dir : STD_LOGIC_VECTOR(3 downto 0);

    constant USE_DIMM_MODEL   : boolean := true;
    constant USE_AUTOMATIC_REFRESH : boolean := true;

    signal ocp_master : SDRAM_controller_master_type;
    signal ocp_slave  : SDRAM_controller_slave_type;
begin
    -- Wrapper
    dut : work.dma_controller_dtl_cmp_pkg.dma_controller_dtl
        generic map(
            DQ_WIDTH         => DQ_WIDTH,
            MTL_MASK_WIDTH   => MTL_MASK_WIDTH,
            MTL_SIZE_WIDTH   => MTL_SIZE_WIDTH,
            MTL_ADDR_WIDTH   => MTL_ADDR_WIDTH,
            GEN_REQUEST_SIZE => GEN_REQUEST_SIZE,
            DMA_ADDR_WIDTH   => DMA_ADDR_WIDTH,
            DMA_DATA_WIDTH   => DMA_DATA_WIDTH)
        port map(
            mtl_clk              => clk,
            mtl_rst_n            => rst_n,
            mtl_cmd_valid_i      => mtl_cmd_valid_i,
            mtl_cmd_accept_i     => mtl_cmd_accept_i,
            mtl_cmd_addr_i       => mtl_cmd_addr_i,
            mtl_cmd_read_i       => mtl_cmd_read_i,
            mtl_cmd_block_size_i => mtl_cmd_block_size_i,
            mtl_wr_last_i        => mtl_wr_last_i,
            mtl_wr_valid_i       => mtl_wr_valid_i,
            mtl_flush_i          => mtl_flush_i,
            mtl_wr_accept_i      => mtl_wr_accept_i,
            mtl_wr_data_i        => mtl_wr_data_i,
            mtl_wr_mask_i        => mtl_wr_mask_i,
            mtl_rd_last_i        => mtl_rd_last_i,
            mtl_rd_valid_i       => mtl_rd_valid_i,
            mtl_rd_accept_i      => mtl_rd_accept_i,
            mtl_rd_data_i        => mtl_rd_data_i,
            dma_addr_special_i   => dma_addr_special_i,
            dma_addr_i           => dma_addr_i,
            dma_rd_i             => dma_rd_i,
            dma_rd_data_i        => dma_rd_data_i,
            dma_wr_i             => dma_wr_i,
            dma_wr_data_i        => dma_wr_data_i);
    -- The SDRAM Controller
    sdr_sdram_inst : entity work.sdr_sdram
        generic map(SHORT_INITIALIZATION => true,
                    USE_AUTOMATIC_REFRESH => USE_AUTOMATIC_REFRESH,
                    BURST_LENGTH          => BURST_LENGTH,
                    SDRAM                 => SDRAM,
                    CS_WIDTH              => CS_WIDTH,
                    CS_LOW_BIT            => CS_LOW_BIT,
                    BA_LOW_BIT            => BA_LOW_BIT,
                    ROW_LOW_BIT           => ROW_LOW_BIT,
                    COL_LOW_BIT           => COL_LOW_BIT)
        port map(rst         => rst,
                 clk         => clk,
                 pll_locked  => '1',
                 ocpSlave    => ocp_slave,
                 ocpMaster   => ocp_master,
                 sdram_CKE   => sdram_CKE,
                 sdram_RAS_n => sdram_RAS_n,
                 sdram_CAS_n => sdram_CAS_n,
                 sdram_WE_n  => sdram_WE_n,
                 sdram_CS_n  => sdram_CS_n,
                 sdram_BA    => sdram_BA,
                 sdram_SA    => sdram_SA,
                 sdram_DQ    => sdram_DQ,
                 sdram_DQM   => sdram_DQM);

    -- CMD
    ocp_master.MFlag_CmdRefresh  <= '0';
    ocp_master.MCmd         <= '0' & (mtl_cmd_valid_i and (not mtl_cmd_read_i)) & (mtl_cmd_valid_i and mtl_cmd_read_i);
    ocp_master.MAddr        <= mtl_cmd_addr_i;
    mtl_cmd_accept_i <= ocp_slave.SCmdAccept;
    assert (to_integer(unsigned(mtl_cmd_block_size_i)) + 1) = BURST_LENGTH or mtl_cmd_valid_i /= '1' report "Unsupported block size" severity failure;
    -- Write 
    ocp_master.MData       <= mtl_wr_data_i;
    ocp_master.MDataByteEn <= mtl_wr_mask_i;
    mtl_wr_accept_i <= ocp_slave.SDataAccept;
    -- Read 
    mtl_rd_data_i   <= ocp_slave.SData;
    mtl_rd_valid_i  <= ocp_slave.SResp;
    mtl_rd_last_i   <= ocp_slave.SRespLast;
    assert mtl_rd_accept_i = '1' or mtl_rd_valid_i /= '1' report "Protocol error: wrapper not ready to accept data" severity failure;
    -- ocp_master.MRespAccept not used

    gen_delay_dq_out : for i in sdram_dq_dir'range generate
        sdram_dq((i+1)*8-1 downto i*8)  <= sdram_dq_out((i+1)*8-1 downto i*8)'delayed(tCLK_PERIOD/10) when sdram_dq_dir'delayed(tCLK_PERIOD/10)(i)='1' else (others => 'Z');
    end generate gen_delay_dq_out;

    
    -- The SDRAMs
    dimm : if USE_DIMM_MODEL generate
        chips : for i in 0 to 2 ** CS_WIDTH - 1 generate
            B0 : entity work.mt48lc2m32b2
                generic map(
                    tOH       => tOH,
                    tMRD      => tMRD_CYCLES,
                    tRAS      => tRAS,
                    tRC       => tRC,
                    tRCD      => tRCD,
                    tRP       => tRP,
                    tRRD      => tRRD,
                    tWRa      => tWRa,
                    tWRp      => tWRp,
                    tAS       => tAS,
                    tCH       => tCH,
                    tCL       => tCL,
                    tCK       => tCK,
                    tDH       => tDH,
                    tDS       => tDS,
                    tCKH      => tCKH,
                    tCKS      => tCKS,
                    tCMH      => tCMH,
                    tCMS      => tCMS,
                    addr_bits => SDRAM.ROW_WIDTH,
                    data_bits => SDRAM_DATA_WIDTH,
                    col_bits  => SDRAM.COL_WIDTH)
                port map(
                    Dq_in  => sdram_dq'delayed(tCLK_PERIOD/10),
                    Dq_out => sdram_dq_out,
                    Dq_dir => sdram_dq_dir,
                    Addr   => sdram_sa'delayed(tCLK_PERIOD/10),
                    Ba     => sdram_Ba'delayed(tCLK_PERIOD/10),
--                    Clk    => clk_ram'delayed(tCLK_PERIOD/2),
                    Clk    => clk_ram,
                    Cke    => sdram_cke'delayed(tCLK_PERIOD/10),
                    Cs_n   => sdram_cs_n(i)'delayed(tCLK_PERIOD/10),
                    Ras_n  => sdram_ras_n'delayed(tCLK_PERIOD/10),
                    Cas_n  => sdram_cas_n'delayed(tCLK_PERIOD/10),
                    We_n   => sdram_We_n'delayed(tCLK_PERIOD/10),
                    Dqm    => sdram_Dqm'delayed(tCLK_PERIOD/10)  
                );
        end generate chips;
    end generate dimm;
    not_dimm : if not USE_DIMM_MODEL generate
        chips : for i in 0 to 2 ** CS_WIDTH - 1 generate
            B00 : mt48lc8m16a2 port map(
                    Dq    => sdram_dq(15 downto 0),
                    Addr  => sdram_sa(11 downto 0),
                    Ba    => sdram_ba,
                    CLK   => clk_ram,
                    Cke   => sdram_cke,
                    Cs_n  => sdram_cs_n(i),
                    Cas_n => sdram_cas_n,
                    Ras_n => sdram_ras_n,
                    We_n  => sdram_we_n,
                    Dqm   => sdram_dqm(1 downto 0)
                );
            B01 : mt48lc8m16a2 port map(
                    Dq    => sdram_dq(31 downto 16),
                    Addr  => sdram_sa(11 downto 0),
                    Ba    => sdram_ba,
                    CLK   => clk_ram,
                    Cke   => sdram_cke,
                    Cs_n  => sdram_cs_n(i),
                    Cas_n => sdram_cas_n,
                    Ras_n => sdram_ras_n,
                    We_n  => sdram_we_n,
                    Dqm   => sdram_dqm(3 downto 2)
                );
        end generate chips;
    end generate not_dimm;

    clock_driver : process
    begin
        if end_of_sim = '0' then
            -- generate both clocks to have 0 delta_delay seperation
            clk <= '1';
            clk_ram  <= '1';
            wait for tCLK_PERIOD / 2;
            clk <= '0';
            clk_ram  <= '0';
            wait for tCLK_PERIOD / 2;
        end if;
    end process clock_driver;
    rst_n <= '0', '1' after tCLK_PERIOD * 2.2;
    rst  <= not rst_n;

    show_cache_line_transactions : if DEBUG_SHOW_CACHELINE_TRANSACTIONS generate
        cl_monitor : process
        begin
            wait until rising_edge(clk);
            if dma_rd_i = '1' and dma_addr_special_i = '0' then
                report "CL RD " & integer'image(to_integer(signed(dma_rd_data_i))) & " at " & integer'image(to_integer(unsigned(dma_addr_i)));
            end if;
            if dma_wr_i = '1' and dma_addr_special_i = '0' then
                report "CL WR " & integer'image(to_integer(signed(dma_wr_data_i))) & " at " & integer'image(to_integer(unsigned(dma_addr_i)));
            end if;
        end process cl_monitor;
    end generate show_cache_line_transactions;

    show_memory_transactions : if DEBUG_SHOW_MEMORY_TRANSACTIONS generate
        memory_monitor : process
            variable addr : integer;
        begin
            wait until rising_edge(clk);
            if mtl_cmd_accept_i = '1' and mtl_cmd_valid_i = '1' then
                addr := TO_INTEGER(unsigned(mtl_cmd_addr_i)) / 4; -- the mtl_cmd_addr_i is byte addressable
            end if;

            if mtl_rd_accept_i = '1' and mtl_rd_valid_i = '1' then
                report "MEM RD " & integer'image(to_integer(signed(mtl_rd_data_i))) & " at " & integer'image(addr) & " last=" & std_logic'image(mtl_rd_last_i);
                addr := addr + 1;
            end if;
            if mtl_wr_accept_i = '1' and mtl_wr_valid_i = '1' then
                report "MEM WR " & integer'image(to_integer(signed(mtl_wr_data_i))) & " at " & integer'image(addr) & " last=" & std_logic'image(mtl_wr_last_i);
                addr := addr + 1;
            end if;
        end process memory_monitor;
    end generate show_memory_transactions;

    control_test : process is
        type line_op_t is (loLoadLine, loStoreLine);

        procedure controllerRead(isSpecial : std_logic; addr : natural; result : out dma_word_t) is
        begin
            dma_addr_special_i <= isSpecial;
            dma_addr_i         <= std_logic_vector(to_unsigned(addr, dma_addr_i'length));
            dma_rd_i           <= '1';
            dma_wr_i           <= '0';
            wait until rising_edge(clk);

            result := dma_rd_data_i;
        end procedure controllerRead;
        procedure controllerRead(isSpecial : std_logic; addr : natural; result : out natural) is
            variable result_slv : dma_word_t;
        begin
            controllerRead(isSpecial, addr, result_slv);
            result := to_integer(unsigned(result_slv));
        end procedure controllerRead;

        procedure controllerWrite(isSpecial : std_logic; addr : natural; value : dma_word_t) is
        begin
            dma_addr_special_i <= isSpecial;
            dma_addr_i         <= std_logic_vector(to_unsigned(addr, dma_addr_i'length));
            dma_rd_i           <= '0';
            dma_wr_i           <= '1';
            dma_wr_data_i      <= value;
            wait until rising_edge(clk);
        end procedure controllerWrite;
        procedure controllerWrite(isSpecial : std_logic; addr : natural; value : natural) is
        begin
            controllerWrite(isSpecial, addr, std_logic_vector(to_unsigned(value, dma_wr_data_i'length)));
        end procedure controllerWrite;

        procedure waitReady is
            variable stat : natural;
        begin
            loop
                controllerRead('1', DMA_OFFSET_CMD_STAT, stat);
                exit when stat = DMA_STATUS_READY;
            end loop;
        end procedure waitReady;

        procedure memoryLineOp(lineAddr : natural; lineOp : line_op_t) is
        begin
            waitReady;
            controllerWrite('1', DMA_OFFSET_ADDR_REG, lineAddr * NWORDS_PER_CMD * 4);

            if lineOp = loLoadLine then
                controllerWrite('1', DMA_OFFSET_CMD_STAT, DMA_CMD_LOAD_LINE);
            else
                controllerWrite('1', DMA_OFFSET_CMD_STAT, DMA_CMD_STORE_LINE);
            end if;
            waitReady;
        end procedure memoryLineOp;

        variable value : natural;

        constant A2 : integer := NWORDS_PER_CMD / 2 - 1;
        constant A3 : integer := NWORDS_PER_CMD - 1;

    begin
        wait until rst_n = '1';
        waitReady;

	if (NWORDS_PER_CMD >=4) then
		controllerWrite('0', 0, 1);
		controllerWrite('0', A2, 6);
		controllerWrite('0', A3, 16);
		controllerRead('0', 0, value);
		assert value = 1 report "T1 cache word write/read error at 0" severity error;
		controllerRead('0', A2, value);
		assert value = 6 report "T1 cache word write/read error at middle" severity error;
		controllerRead('0', A3, value);
		assert value = 16 report "T1 cache word write/read error at last" severity error;

		memoryLineOp(0, loStoreLine);
		memoryLineOp(1, loLoadLine);
		controllerRead('0', 0, value);
		assert value = 0 report "T2 cache word write/read error at 0" severity error;
		controllerRead('0', A2, value);
		assert value = 0 report "T2 cache word write/read error at middle" severity error;
		controllerRead('0', A3, value);
		assert value = 0 report "T2 cache word write/read error at last" severity error;

		memoryLineOp(0, loLoadLine);
		controllerRead('0', 0, value);
		assert value = 1 report "T3 cache word write/read error at 0" severity error;
		controllerRead('0', A2, value);
		assert value = 6 report "T3 cache word write/read error at middle" severity error;
		controllerRead('0', A3, value);
		assert value = 16 report "T3 cache word write/read error at last" severity error;
	end if;
        
        report "Writing inc pattern";
        for i in 0 to NWORDS_PER_CMD*2-1 loop
            if (not USE_AUTOMATIC_REFRESH and (i mod 10 = 0)) then
                ocp_master.MFlag_CmdRefresh <= '1';
                wait until rising_edge(clk) and ocp_slave.SFlag_RefreshAccept = '1';
            end if;
            controllerWrite('0', (i mod NWORDS_PER_CMD), i);
            if (i mod NWORDS_PER_CMD) = NWORDS_PER_CMD-1 then
                memoryLineOp(i/NWORDS_PER_CMD, loStoreLine);
            end if;
        end loop;
        
        report "Reading inc pattern";
        for i in 0 to NWORDS_PER_CMD*2-1 loop
            if (not USE_AUTOMATIC_REFRESH and (i mod 10 = 0)) then
                ocp_master.MFlag_CmdRefresh <= '1';
                wait until rising_edge(clk) and ocp_slave.SFlag_RefreshAccept = '1';
            end if;
            if (i mod NWORDS_PER_CMD) = 0 then
                memoryLineOp(i/NWORDS_PER_CMD, loLoadLine);
            end if;
            controllerRead('0', i mod NWORDS_PER_CMD, value);
            assert value = i report "Word Read failed at "& natural'image(i) severity error;
        end loop;

        report "OK.  Test Finished!";
        end_of_sim <= '1';
        report "OK.  Test Finished!" severity failure;
    end process control_test;

end architecture RTL;
