----------------------------------------------------------------------------------

-- Create Date: 04/03/2019
-- Design Name:
-- Module Name: bonfire_dcache_Multi_sets - Behavioral

-- The Bonfire Processor Project, (c) 2016-2019 Thomas Hornschuh

--

-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.log2;
use work.bonfire_dcache_help.all;

entity bonfire_dcache_multi_sets is

generic(

  LOG2_SETS : natural :=0; -- 0= 1 set, 1= 2 sets, etc ...
  CL_BITS : natural; -- Bits for adressing a word in a cache line
  CACHE_ADR_BITS : natural; -- total adress bits for cache
  TAG_RAM_BITS : natural; -- number of address bits stored in tag ram

  ADDRESS_BITS : natural := 30;  -- Number of bits of chacheable address range
  MASTER_WIDTH_BYTES : natural := 4;
  DEVICE_FAMILY : string :=""
);
Port (
  clk_i: in std_logic;
  rst_i: in std_logic;

  adr_i : in std_logic_vector(ADDRESS_BITS+1 downto 2);
  en_i : in std_logic;

  we_i : in std_logic; -- Tag RAM write enable (update...)
  dirty_i : in std_logic;
  valid_i : in std_logic;

  tag_index_o : out unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);
  hit_o : out std_logic;
  miss_o : out std_logic;
  dirty_miss_o : out std_logic;
  tag_value_o : out unsigned(TAG_RAM_BITS-1 downto 0);
  buffer_index_o : out unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);

  selected_set_o: out std_logic_vector(max(LOG2_SETS-1,0) downto 0) -- Out: selected cache set to use

);

end entity;

architecture Behavioral of bonfire_dcache_multi_sets is

constant NUM_SETS : natural := 2** LOG2_SETS;

type t_set_interface is record

--  we_i :  std_logic; -- Tag RAM write enable (update...)
--  dirty_i :  std_logic;
--  valid_i :  std_logic;

  tag_index_o :  unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);
  hit_o :  std_logic;
  miss_o :  std_logic;
  dirty_miss_o :  std_logic;
  tag_value_o :  unsigned(TAG_RAM_BITS-1 downto 0);
  tag_valid_o : std_logic;
  buffer_index_o :  unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);

end record;

type  t_sets is array( 0 to NUM_SETS-1 ) of t_set_interface;

subtype t_setindex is natural range 0 to NUM_SETS-1;

signal sets : t_sets;
signal selected_set_index : t_setindex;
signal round_robin : unsigned(max(LOG2_SETS-1,0) downto 0) := ( others=> '0');
signal we : std_logic_vector( 0 to NUM_SETS-1 );
signal dirty_miss : std_logic;
signal purge : std_logic; -- Valid cache line has been purged


--signal hit : std_logic;

begin

  buffer_index_o <= sets(0).buffer_index_o; -- all buffer indexes are identical...
  tag_index_o <= sets(0).tag_index_o;
  tag_value_o <= sets(selected_set_index).tag_value_o;
  dirty_miss_o <= dirty_miss;


  gensets: for i in sets'range generate
    inst_bonfire_dcache_set : entity work.bonfire_dcache_set
    generic map (
      CL_BITS            => CL_BITS,
      CACHE_ADR_BITS     => CACHE_ADR_BITS,
      TAG_RAM_BITS       => TAG_RAM_BITS,
      ADDRESS_BITS       => ADDRESS_BITS,
      MASTER_WIDTH_BYTES => MASTER_WIDTH_BYTES,
      DEVICE_FAMILY      => DEVICE_FAMILY
    )
    port map (
      clk_i   => clk_i,
      rst_i   => rst_i,
      adr_i   => adr_i,
      en_i    => en_i,
      we_i    => we(i),
      dirty_i => dirty_i,
      valid_i => valid_i,
      tag_index_o => sets(i).tag_index_o,
      hit_o   => sets(i).hit_o,
      miss_o  => sets(i).miss_o,
      dirty_miss_o => sets(i).dirty_miss_o,
      tag_value_o => sets(i).tag_value_o,
      tag_valid_o => sets(i).tag_valid_o,
      buffer_index_o =>sets(i).buffer_index_o
    );


  end generate;

  -- write enable multiplexer
  we_en_mux: process(selected_set_index,we_i)
  variable e : std_logic;
  begin

    for i in sets'range loop
      if  i = selected_set_index then
        e := we_i;
      else
        e := '0';
      end if;
     we(i) <= e;
    end loop;

  end process;




  find_set : process (en_i,sets,round_robin)
  variable hit : std_logic;
  variable selected_set : natural;
  variable found : boolean;

  begin
    hit := '0';
    miss_o <= '0';
    dirty_miss  <= '0';
    purge <= '0';
    selected_set := 0;
    if en_i='1' then
      for i in sets'range loop
        if sets(i).hit_o = '1' then
          hit := '1';
          selected_set := i;
          exit;
        end if;
      end loop;
      if hit = '0' and  sets(0).miss_o = '1' then
        found := false;
        for i in sets'range loop
          if sets(i).tag_valid_o = '0' then
            -- found "free" cache line
            selected_set := i;
            miss_o <= '1';
            found := true;
            exit;
          end if;
        end loop;
        if not found then -- no free cache line
           if NUM_SETS>1 then
             selected_set := to_integer(round_robin);
             purge <= '1';
           end if;

           miss_o <= '1';
           dirty_miss <= sets(selected_set).dirty_miss_o;
        end if;
      end if;
    end if;
    selected_set_index <= selected_set;
    hit_o <= hit;
  end process;


multi_sets: if NUM_SETS>=1 generate  begin

  selected_set_o <= std_logic_vector(to_unsigned(selected_set_index,selected_set_o'length));

  process (clk_i)
  begin

    if rising_edge(clk_i) then
      if we_i = '1' and purge = '1'  then
        round_robin <= round_robin + 1;
      end if;
    end if;

  end process;

end generate;

end Behavioral;
