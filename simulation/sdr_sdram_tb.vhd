library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sdr_sdram_tb is
end entity sdr_sdram_tb;

use std.textio.all;
use work.sdram_config.all;
use work.sdram_controller_interface.all;

architecture RTL of sdr_sdram_tb is
    function SDRAM_params_override(config : sdram_config_type) return sdram_config_type is
        variable result : sdram_config_type;
    begin
        result      := config;
        -- Make refresh frequent to see the interference with other commands
        result.REFI := 10;
        -- Use smaller memory for testing
        result.COL_WIDTH := 4;
        result.ROW_WIDTH := 2;
        return result;
    end function SDRAM_params_override;

    constant BURST_LENGTH  : natural           := 8;
    -- 100MHz, 2 cycles read data latency
    constant tCLK_PERIOD   : time              := 20 ns;
    constant CAS_LATENCY   : natural           := 2;
    constant SDRAM_default : sdram_config_type := GetSDRAMParameters(tCLK_PERIOD, CAS_LATENCY);
    constant SDRAM         : sdram_config_type := SDRAM_params_override(SDRAM_default);

    -- Address Mapping => (bank & row & column)
    constant CS_WIDTH    : integer := 0; -- 1 rank
    constant COL_LOW_BIT : integer := 0;
    constant ROW_LOW_BIT : integer := COL_LOW_BIT + SDRAM.COL_WIDTH;
    constant BA_LOW_BIT  : integer := ROW_LOW_BIT + SDRAM.ROW_WIDTH;
    constant CS_LOW_BIT  : integer := BA_LOW_BIT + SDRAM.BA_WIDTH;

    constant DEBUG_SHOW_CONTROLLER_DATA_TRANSFERS : boolean := true;

    constant TEST_MEM_SIZE : natural := BURST_LENGTH * 4;

    constant USE_DIMM_MODEL        : boolean := true;
    constant USE_AUTOMATIC_REFRESH : boolean := true;

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

    subtype sdram_word_t is std_logic_vector(SDRAM_DATA_WIDTH - 1 downto 0);

    signal clk   : std_logic;
    signal rst_n : std_logic;

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

    signal clk_ram      : std_logic := '0';
    signal rst          : std_logic;
    signal sdram_sa     : std_logic_vector(SDRAM.SA_WIDTH - 1 downto 0);
    signal sdram_ba     : std_logic_vector(SDRAM.BA_WIDTH - 1 downto 0);
    signal sdram_cs_n   : std_logic_vector(2 ** CS_WIDTH - 1 downto 0);
    signal sdram_cke    : std_logic;
    signal sdram_ras_n  : std_logic;
    signal sdram_cas_n  : std_logic;
    signal sdram_we_n   : std_logic;
    signal sdram_dq     : std_logic_vector(SDRAM_DATA_WIDTH - 1 downto 0);
    signal sdram_dqm    : std_logic_vector(SDRAM_DATA_WIDTH / 8 - 1 downto 0);
    signal sdram_dq_out : STD_LOGIC_VECTOR(SDRAM_DATA_WIDTH - 1 DOWNTO 0);
    signal sdram_dq_dir : STD_LOGIC_VECTOR(3 downto 0);

    signal ocp_master : SDRAM_controller_master_type;
    signal ocp_slave  : SDRAM_controller_slave_type;
begin
    -- The SDRAM Controller
    sdr_sdram_inst : entity work.sdr_sdram
        generic map(SHORT_INITIALIZATION  => true,
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

    gen_delay_dq_out : for i in sdram_dq_dir'range generate
        sdram_dq((i + 1) * 8 - 1 downto i * 8) <= sdram_dq_out((i + 1) * 8 - 1 downto i * 8)'delayed(tCLK_PERIOD / 10) when sdram_dq_dir'delayed(tCLK_PERIOD / 10)(i) = '1' else (others => 'Z');
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
                    addr_bits => SDRAM.SA_WIDTH,
                    data_bits => SDRAM_DATA_WIDTH,
                    col_bits  => SDRAM.COL_WIDTH)
                port map(
                    Dq_in  => sdram_dq'delayed(tCLK_PERIOD / 10),
                    Dq_out => sdram_dq_out,
                    Dq_dir => sdram_dq_dir,
                    Addr   => sdram_sa'delayed(tCLK_PERIOD / 10),
                    Ba     => sdram_Ba'delayed(tCLK_PERIOD / 10),
                    --                    Clk    => clk_ram'delayed(tCLK_PERIOD/2),

                    Clk    => clk_ram,
                    Cke    => sdram_cke'delayed(tCLK_PERIOD / 10),
                    Cs_n   => sdram_cs_n(i)'delayed(tCLK_PERIOD / 10),
                    Ras_n  => sdram_ras_n'delayed(tCLK_PERIOD / 10),
                    Cas_n  => sdram_cas_n'delayed(tCLK_PERIOD / 10),
                    We_n   => sdram_We_n'delayed(tCLK_PERIOD / 10),
                    Dqm    => sdram_Dqm'delayed(tCLK_PERIOD / 10)
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
            clk     <= '1';
            clk_ram <= '1';
            wait for tCLK_PERIOD / 2;
            clk     <= '0';
            clk_ram <= '0';
            wait for tCLK_PERIOD / 2;
        end if;
    end process clock_driver;
    rst_n <= '0', '1' after tCLK_PERIOD * 2.2;
    rst   <= not rst_n;

    show_controller_transfers : if DEBUG_SHOW_CONTROLLER_DATA_TRANSFERS generate
        memory_monitor : process
            variable addr : integer;
        begin
            wait until rising_edge(clk);
            if ocp_master.MCmd /= "000" and ocp_slave.SCmdAccept = '1' then
                addr := TO_INTEGER(unsigned(ocp_master.MAddr));
            end if;

            if ocp_slave.SResp = '1' then
                report "MEM RD " & integer'image(to_integer(signed(ocp_slave.SData))) & " at " & integer'image(addr) & " last=" & std_logic'image(ocp_slave.SRespLast);
                addr := addr + 1;
            end if;
            if ocp_slave.SDataAccept = '1' then
                report "MEM WR " & integer'image(to_integer(signed(ocp_master.MData))) & " at " & integer'image(addr);
                addr := addr + 1;
            end if;
        end process memory_monitor;
    end generate show_controller_transfers;

    control_test : process is
        type burst_data_t is array (0 to BURST_LENGTH - 1) of sdram_word_t;

        function generate_test_pattern return burst_data_t is
            variable data : burst_data_t;
        begin
            for i in data'range loop
                data(i) := std_logic_vector(to_unsigned(BURST_LENGTH - i, data(i)'length));
            end loop;

            return data;
        end function generate_test_pattern;

        constant TEST_BURST_ZERO    : burst_data_t := (others => (others => '0'));
        constant TEST_BURST_PATTERN : burst_data_t := generate_test_pattern;

        variable errors : integer := 0;

        impure function check(assertion : boolean) return boolean is
        begin
            if not assertion then
                errors := errors + 1;
            end if;
            return assertion;
        end function check;

        procedure do_read(addr : natural; result : out burst_data_t) is
            variable i : integer := 0;
        begin
            ocp_master.MAddr <= std_logic_vector(to_unsigned(addr, ocp_master.MAddr'length));
            ocp_master.MCmd  <= "001";

            loop
                wait until rising_edge(clk);

                if ocp_slave.SCmdAccept = '1' then
                    ocp_master.MCmd <= "000";
                end if;

                if ocp_slave.SResp = '1' then
                    assert check((ocp_slave.SRespLast = '0' and i /= BURST_LENGTH - 1) or (ocp_slave.SRespLast = '1' and i = BURST_LENGTH - 1)) report "SRespLast error during " & integer'image(i) & "'th word read at " & integer'image(addr) severity error;
                    result(i) := ocp_slave.SData;
                    i         := i + 1;
                end if;

                exit when i = BURST_LENGTH;
            end loop;
        end procedure do_read;

        procedure do_write(addr : natural; data : burst_data_t) is
            variable i : integer := 0;
        begin
            ocp_master.MAddr <= std_logic_vector(to_unsigned(addr, ocp_master.MAddr'length));
            ocp_master.MCmd  <= "010";

            loop
                ocp_master.MData <= data(i);
                wait until rising_edge(clk);

                if ocp_slave.SCmdAccept = '1' then
                    ocp_master.MCmd <= "000";
                end if;

                if ocp_slave.SDataAccept = '1' then
                    i := i + 1;
                end if;

                exit when i = BURST_LENGTH;
            end loop;
        end procedure do_write;

        variable burst_data : burst_data_t;
        variable value      : sdram_word_t;

    begin
        ocp_master.MCmd             <= "000";
        ocp_master.MFlag_CmdRefresh <= '0';
        ocp_master.MDataByteEn      <= (others => '1');

        wait until rst_n = '1';

        report "Simple test:";
        do_write(0, TEST_BURST_PATTERN);
        do_write(1*BURST_LENGTH, TEST_BURST_ZERO);

        do_read(0, burst_data);
        for i in burst_data'range loop
            assert check(burst_data(i) = TEST_BURST_PATTERN(i)) report "burst write/read error at " & integer'image(i) & " word" severity error;
        end loop;

        do_read(1*BURST_LENGTH, burst_data);
        assert check(burst_data = TEST_BURST_ZERO) report "block 1 is not zero" severity error;

        report "Writing inc pattern:";
        for i in 0 to TEST_MEM_SIZE - 1 loop
            if (not USE_AUTOMATIC_REFRESH and (i mod 10 = 0)) then
                ocp_master.MFlag_CmdRefresh <= '1';
                wait until rising_edge(clk) and ocp_slave.SFlag_RefreshAccept = '1';
                ocp_master.MFlag_CmdRefresh <= '0';
            end if;
            burst_data(i mod BURST_LENGTH) := std_logic_vector(to_unsigned(i, burst_data(0)'length));
            if (i mod BURST_LENGTH) = BURST_LENGTH - 1 then
                do_write(i / BURST_LENGTH * BURST_LENGTH, burst_data);
            end if;
        end loop;

        report "Reading inc pattern:";
        for i in 0 to TEST_MEM_SIZE - 1 loop
            if (not USE_AUTOMATIC_REFRESH and (i mod 10 = 0)) then
                ocp_master.MFlag_CmdRefresh <= '1';
                wait until rising_edge(clk) and ocp_slave.SFlag_RefreshAccept = '1';
                ocp_master.MFlag_CmdRefresh <= '0';
            end if;
            if (i mod BURST_LENGTH) = 0 then
                do_read(i, burst_data);
            end if;

            value := burst_data(i mod BURST_LENGTH);
            assert check(value = std_logic_vector(to_unsigned(i,value'length))) report "Word Read failed at " & natural'image(i) severity error;
        end loop;

        report "Test Finished with " & integer'image(errors) & " errors.";
        end_of_sim <= '1';
        report "Test Finished with " & integer'image(errors) & " errors." severity failure;
    end process control_test;

end architecture RTL;
