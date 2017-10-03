-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 28.6.2017 15:49:30 GMT

library ieee;
use ieee.std_logic_1164.all;
use IEEE.NUMERIC_STD.ALL;

use work.log2;
--use work.common_pkg.all;

entity tb_dcache is
end tb_dcache;



architecture tb of tb_dcache is

--constant  MASTER_DATA_WIDTH : natural := 128;
constant  MASTER_DATA_WIDTH : natural := 32;
constant  LINE_SIZE : natural := 4;
--constant  LINE_SIZE : natural := 16;
constant  LINE_SIZE_BYTES : natural := MASTER_DATA_WIDTH/8 * LINE_SIZE;
constant  LINE_SIZE_WORDS : natural := LINE_SIZE_BYTES / 4;
constant  CACHE_SIZE : natural :=128;
constant  CACHE_SIZE_BYTES : natural := CACHE_SIZE*MASTER_DATA_WIDTH/8;



    signal clk_i     : std_logic;
    signal rst_i     : std_logic;
    signal wbs_cyc_i : std_logic;
    signal wbs_stb_i : std_logic;
    signal wbs_we_i  : std_logic;
    signal wbs_sel_i : std_logic_vector (3 downto 0);
    signal wbs_ack_o : std_logic;
    signal wbs_adr_i : std_logic_vector (31 downto 2);
    signal wbs_dat_o : std_logic_vector (31 downto 0);
    signal wbs_dat_i : std_logic_vector (31 downto 0);


    signal wbm_cyc_o : std_logic;
    signal wbm_stb_o : std_logic;
    signal wbm_we_o  : std_logic;
    signal wbm_cti_o : std_logic_vector (2 downto 0);
    signal wbm_bte_o : std_logic_vector (1 downto 0);
    signal wbm_sel_o : std_logic_vector (MASTER_DATA_WIDTH/8-1 downto 0);
    signal wbm_ack_i : std_logic :='0';
    signal wbm_adr_o : std_logic_vector (31 downto log2.log2(MASTER_DATA_WIDTH/8));
    signal wbm_dat_i : std_logic_vector (MASTER_DATA_WIDTH-1 downto 0) := (others => 'X');
    signal wbm_dat_o : std_logic_vector(MASTER_DATA_WIDTH-1 downto 0);



    constant TbPeriod : time := 10 ns; -- EDIT Put right period here
    signal TbClock : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

    type t_mstate is (m_idle,m_readburst,m_retire);
    signal mstate : t_mstate:=m_idle;

    subtype t_wbm_dat is std_logic_vector (wbm_dat_i'range);

     -- our simulated RAM is twiche the cache size, this is enough for write testing...
     type t_simul_ram is array (0 to CACHE_SIZE*2-1) of std_logic_vector(MASTER_DATA_WIDTH-1 downto 0);

     signal ram : t_simul_ram := (others=>( others=>'U'));

    -- Testbench can run in pattern mode, for address read tests
    -- or in ram mode for write/read tests.
    type t_sim_mode is (sim_pattern, sim_ram);
    signal sim_mode : t_sim_mode := sim_pattern;

    impure function get_pattern(adr : std_logic_vector(wbm_adr_o'range)) return t_wbm_dat is
    variable bitpos : natural;
    variable word_adr : unsigned(adr'low-1 downto 0);
    variable d : t_wbm_dat;
    begin
      bitpos:=0;
      word_adr:=to_unsigned(0,word_adr'length);
      for i in 0 to MASTER_DATA_WIDTH/32-1 loop
        d(bitpos+31 downto bitpos) := adr & std_logic_vector(word_adr);
        bitpos:=bitpos+32;
        word_adr:=word_adr+4;
      end loop;
      return d;
    end;


    function hex_string(x: std_logic_vector) return string is
        variable xx: std_logic_vector(x'length-1 downto 0);
        variable i: integer:=0;
        variable ii: integer;
        variable c: integer;
        variable s: string(x'length downto 1);
    begin
        xx:=x;
        loop
            ii:=i*4;
            exit when ii>xx'high;
            if ii+3<=xx'high then
                c:=to_integer(unsigned(xx(ii+3 downto ii)));
            else
                c:=to_integer(unsigned(xx(xx'high downto ii)));
            end if;

            case c is
            when 0 => s(i+1):='0';
            when 1 => s(i+1):='1';
            when 2 => s(i+1):='2';
            when 3 => s(i+1):='3';
            when 4 => s(i+1):='4';
            when 5 => s(i+1):='5';
            when 6 => s(i+1):='6';
            when 7 => s(i+1):='7';
            when 8 => s(i+1):='8';
            when 9 => s(i+1):='9';
            when 10 => s(i+1):='A';
            when 11 => s(i+1):='B';
            when 12 => s(i+1):='C';
            when 13 => s(i+1):='D';
            when 14 => s(i+1):='E';
            when 15 => s(i+1):='F';
            when others => s(i+1):='X';
            end case;

            i:=i+1;
        end loop;
        return s(i downto 1);
    end function;

begin

    dut : entity work.bonfire_dcache
    generic  map (
      MASTER_DATA_WIDTH => MASTER_DATA_WIDTH,
      LINE_SIZE => LINE_SIZE,
      CACHE_SIZE => CACHE_SIZE
    )
    port map (clk_i     => clk_i,
              rst_i     => rst_i,
              wbs_cyc_i => wbs_cyc_i,
              wbs_stb_i => wbs_stb_i,
              wbs_we_i  => wbs_we_i,
              wbs_sel_i => wbs_sel_i,
              wbs_ack_o => wbs_ack_o,
              wbs_adr_i => wbs_adr_i,
              wbs_dat_o => wbs_dat_o,
              wbs_dat_i => wbs_dat_i,
              wbm_cyc_o => wbm_cyc_o,
              wbm_stb_o => wbm_stb_o,
              wbm_we_o  => wbm_we_o,
              wbm_cti_o => wbm_cti_o,
              wbm_bte_o => wbm_bte_o,
              wbm_sel_o => wbm_sel_o,
              wbm_ack_i => wbm_ack_i,
              wbm_adr_o => wbm_adr_o,
              wbm_dat_i => wbm_dat_i,
              wbm_dat_o => wbm_dat_o);

    -- Clock generation
    TbClock <= not TbClock after TbPeriod/2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that clk_i is really your main clock signal
    clk_i <= TbClock;

    -- Main Memory simulation read, depends on sim_mode

    process(wbm_adr_o,wbm_stb_o,wbm_we_o)
    begin
      if wbm_stb_o='1' and wbm_we_o='0' then
         if sim_mode=sim_pattern then
           wbm_dat_i <= get_pattern(wbm_adr_o);
         else
           wbm_dat_i <= ram(to_integer(unsigned(wbm_adr_o)));
         end if;
       else
         wbm_dat_i <=   (others=>'X');
       end if;

    end process;


    mem_simul: process(clk_i) -- Simulates the Main Memory slave


    begin

      if rising_edge(clk_i) then
          case mstate is
            when m_idle =>
              if wbm_cyc_o='1' and wbm_stb_o='1' then
                if wbm_we_o='0' then
                  mstate <= m_readburst;
                else
                   for i in wbm_sel_o'range loop
                     if wbm_sel_o(i)='1' then
                       ram(to_integer(unsigned(wbm_adr_o)))((i+1)*8-1 downto i*8) <= wbm_dat_o((i+1)*8-1 downto i*8);
                     end if;
                   end loop;
                   mstate <= m_retire;
                end if;
                wbm_ack_i <= '1'; -- Set ack signal
              end if;

            when m_readburst =>
                if wbm_cti_o="000" or wbm_cti_o="111" then
                  wbm_ack_i<= '0';
                  mstate<=m_idle;
                end if;
            when m_retire =>
               wbm_ack_i<= '0';
               mstate<=m_idle;
          end case;
      end if;
    end process;


    stimuli : process
     variable d : std_logic_vector(wbs_dat_i'range);
     variable s : boolean;
     variable temp : std_logic_vector(31 downto 0);

       procedure wb_read(address : in std_logic_vector(31 downto 0);
                         data: out std_logic_vector(wbs_dat_i'range) )  is
            begin
                wait until rising_edge(clk_i);
                wbs_adr_i <= address(wbs_adr_i'range);

                wbs_we_i <= '0';
                wbs_cyc_i <= '1';
                wbs_stb_i <= '1';
                wbs_sel_i <= "1111";
                wait until wbs_ack_o = '1' and rising_edge (clk_i);

                data:= wbs_dat_o;
                wbs_stb_i <= '0';
                wbs_cyc_i <= '0';

       end procedure;

       procedure  read_loop(from: in std_logic_vector(31 downto 0);
                            length : in natural;
                            success : out boolean ) is

       variable s : boolean;
       variable adr : std_logic_vector (from'range);
       variable d: std_logic_vector (31 downto 0);

       begin

         success:=true;
         for i in 0 to length-1 loop
           adr:= std_logic_vector(unsigned(from)+i*4);
           wb_read(std_logic_vector(adr),d);
           s:= d = adr;
           if s then
             report "Sucessfull read from address " & hex_string(adr);
           else
             report "Error reading from address " & hex_string(adr) & " Data:" & hex_string(d)
             severity error;
             success:=false;
           end if;
         end loop;

       end procedure;


        procedure wb_write(address : in std_logic_vector(31 downto 0);
                           data: in std_logic_vector(wbs_dat_i'range) )  is
        begin
          wait until rising_edge(clk_i);
          wbs_adr_i <= address(wbs_adr_i'range);
          wbs_dat_i <= data;
          wbs_we_i <= '1';
          wbs_cyc_i <= '1';
          wbs_stb_i <= '1';
          wbs_sel_i <= "1111";

          wait until wbs_ack_o = '1' and rising_edge (clk_i);
          wbs_stb_i <= '0';
          wbs_cyc_i <= '0';

        end procedure;


        procedure wb_write_byte(address: std_logic_vector(31 downto 0);
                                dbyte : in std_logic_vector (7 downto 0) ) is
        begin
         wait until rising_edge(clk_i);
         wbs_adr_i <= address(wbs_adr_i'range);
         wbs_dat_i <= (others => 'X' );
         case address(1 downto 0) is
           when "00" =>
             wbs_sel_i<="0001";
             wbs_dat_i(7 downto 0)<=dbyte;
           when "01" =>
             wbs_sel_i<="0010";
             wbs_dat_i(15 downto 8)<=dbyte;
           when "10" =>
             wbs_sel_i<="0100";
             wbs_dat_i(23 downto 16)<=dbyte;
           when "11" =>
             wbs_sel_i<="1000";
             wbs_dat_i(31 downto 24)<=dbyte;
           when others=>
             report "Was soll das hier?"
             severity error;
         end case;
         wbs_we_i <= '1';
         wbs_cyc_i <= '1';
         wbs_stb_i <= '1';
         wait until wbs_ack_o = '1' and rising_edge (clk_i);
         wbs_stb_i <= '0';
         wbs_cyc_i <= '0';

        end procedure;

        procedure write_string(s:string;address:std_logic_vector(31 downto 0)) is
        variable adr: unsigned(address'range);
        begin
          adr:=unsigned(address);
          for i in 1 to s'length loop
            wb_write_byte(std_logic_vector(adr),
                          std_logic_vector(to_unsigned(character'pos(s(i)),8)));
            adr:=adr+1;
          end loop;
        end;


        -- Initalize the complete simul ram over the cache
        -- tests as side effect the write back logic
        procedure write_all is
        variable adr: std_logic_vector(31 downto 0);
        begin
          for i in 0 to ram'length*(MASTER_DATA_WIDTH/32) -1 loop
             adr:=std_logic_vector(to_unsigned(i*4,32));
             wb_write(adr,adr);
          end loop;
        end procedure;



    begin
        -- EDIT Adapt initialization as needed
        rst_i <= '0';
        wbs_cyc_i <= '0';
        wbs_stb_i <= '0';
        wbs_we_i <= '0';
        wbs_sel_i <= (others => '0');
        wbs_adr_i <= (others => '0');
        wbs_dat_i <= (others => '0');


        -- EDIT Add stimuli here
        wait for 5 * TbPeriod;

--         sim_mode<=sim_ram;
--         write_all;

--         -- Read the whole RAM areas
--         read_loop(X"00000000",32,s); ---ram'length*(MASTER_DATA_WIDTH/32),s);

        -- Do different read test in "pattern" mode, this allows to test in the whole
        -- address range
        sim_mode<=sim_pattern;
      
        read_loop(X"00000000",LINE_SIZE_WORDS*2,s); -- read two cache lines
        assert s report "Test failed" severity failure;
        read_loop(X"00000000",LINE_SIZE_WORDS*2,s); -- read  the same two cache lines
        assert s report "Test failed" severity failure;
        read_loop(std_logic_vector(to_unsigned(CACHE_SIZE_BYTES-LINE_SIZE_BYTES,32)),LINE_SIZE_WORDS,s); -- read  from last line
        assert s report "Test failed" severity failure;
        read_loop(std_logic_vector(to_unsigned(CACHE_SIZE_BYTES,32)),LINE_SIZE_WORDS,s); -- wrap around, should invalidate line 0
        assert s report "Test failed" severity failure;
        temp:=X"FFFFFFFF" and not std_logic_vector(to_unsigned(LINE_SIZE_BYTES-1,32));
        read_loop(std_logic_vector(temp(31 downto 0)),LINE_SIZE_WORDS,s); -- read from end of address range
        assert s report "Test failed" severity failure;

        report "Read Test finished";
        sim_mode<=sim_ram;

        -- Basic write test
        wb_write(X"00000000",X"AABBCCDD");
        wb_read(X"00000000",d);
        if d=X"AABBCCDD" then
          report "Write successfull";
        else
          report "Write error" severity failure;
        end if;

        -- Byte Write Test
        sim_mode<=sim_ram;
        write_string("The quick brown fox jumps over the lazy dog",X"00000000");


         --Initalize all (RAM and Cache)

        write_all;

        -- Read the whole RAM areas
        read_loop(X"00000000",ram'length*(MASTER_DATA_WIDTH/32),s);
        assert s report "Test failed" severity error;
        -- Stop the clock and hence terminate the simulation
        TbSimEnded <= '1';
        wait;
    end process;

end tb;

-- Configuration block below is required by some simulators. Usually no need to edit.
