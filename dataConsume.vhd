--ARTIX-7 35T cpg236-1
-- FPGA
-- Group 10 - Signed

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
USE IEEE.NUMERIC_STD.ALL;
use work.common_pack.all;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use UNISIM.VPKG.ALL;

entity dataConsume is
port (
    	  clk:		in std_logic;
		reset:		in std_logic; -- synchronous reset
		start: in std_logic; -- goes high to signal data transfer
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
		ctrlIn: in std_logic;
		ctrlOut: out std_logic;
		data: in std_logic_vector(7 downto 0);
		dataReady: out std_logic;
		byte: out std_logic_vector(7 downto 0);
		seqDone: out std_logic;
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0);
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) -- index 3 holds the peak
  	);
end dataConsume;

architecture Behavioral of dataConsume is

	type state_type is (INIT, s1,dready,compare, final);--five states
	signal curState, nextState: state_type;
	signal counter, num, peak_index: integer:= 0; ---counter:count current times of data received
	----num:number of data need to be send
	--peak_index: current PEAK INDEX
	signal max_num: std_logic_vector(7 downto 0) := "00000000";
	signal window,cur_window,max_window:  CHAR_ARRAY_TYPE(6 downto 0):=(others => (others => '0')); ---shift window start from index 3 to left for one bit
	 ---cur_window:current window shift from 0 index to left for one bit
	--max_window: current max window
    	signal ctrl_detect, ctrlIn_reg, ctrlOut_reg, start_done, final_enable, rest_enable: std_logic:= '0'; --detect whether the ctrlIn changed
	 ---ctrlin_reg:register ctrlIn
	   ----register ctrlOut
	function bcd_to_int(bcd_array: std_logic_vector(3 downto 0)) return integer is ----convert bcd to integer
        variable result: integer := 0;
        begin
        case bcd_array is
                when "0000" => result := 0;  -- '0'
                when "0001" => result := 1;  -- '1'
                when "0010" => result := 2;  -- '2'
                when "0011" => result := 3;  -- '3'
                when "0100" => result := 4;  -- '4'
                when "0101" => result := 5;  -- '5'
                when "0110" => result := 6;  -- '6'
                when "0111" => result := 7;  -- '7'
                when "1000" => result := 8;  -- '8'
                when "1001" => result := 9;  -- '9'
                when others => result := 0;  -- space character
        end case;
       return result;
    end function;
    

begin
	start_detect: PROCESS(start, clk) -----------used to detect start signal
	BEGIN
	IF start = '1' THEN
		start_done <= '1';
	ELSE start_done <= '0';
	END IF;
	END PROCESS;
-------------------------------------------------------------------------------------
	alter_ctrlOut: PROCESS(clk, reset, rest_enable)----used to change the ouput of ctrlOut
	BEGIN
		IF reset='1'OR rest_enable='1' THEN
			ctrlOut_reg <= '0';
			num <= 0,0,0;
		ELSIF rising_edge(clk) THEN
			IF start = '1' THEN
			     num <= 100*bcd_to_int(numWords_bcd(2))+10*bcd_to_int(numWords_bcd(1)) + bcd_to_int(numWords_bcd(0));
			     ctrlOut_reg <= not ctrlOut_reg;
		    END IF;
	     END IF;
	END PROCESS;
	ctrlOut <= ctrlOut_reg;
	
-------------------------------------------------------------------------------------

	REG_ctrlIn: PROCESS(ctrlIn_reg, clk, reset) --------used to register ctrlIn, in order to compare it later
	BEGIN
    	IF reset = '1' OR curState = INIT THEN
	       	ctrlIn_reg <= '0';
	    ELSIF rising_edge(clk) THEN
		     ctrlIn_reg <= ctrlIn;
	    END IF;
	END PROCESS;
	ctrl_detect <= ctrlIn_reg xor ctrlIn;---comparing whether the ctrlIn is changed
-------------------------------------------------------------------------------------

	detect_ctrlIn: PROCESS(clk, reset, rest_enable) --------used to detect whether the ctrlIn changed or not, if changed, shift the window
	BEGIN
	IF rising_edge(clk) THEN
		IF reset = '1' OR rest_enable = '1' THEN
		      counter <= 0;
		      window <= (others => (others => '0'));
     		  cur_window <= (others => (others => '0'));
		ELSIF ctrl_detect = '1' THEN
		      counter <= counter + 1;
        	  window(6 downto 3) <= window(5 downto 3) & data; ---start from index 3 shift to left for one bit
        	  cur_window <= cur_window(5 downto 0) & data;  --shift one bit every time   
		END IF;
	END IF;
	END PROCESS;
-------------------------------------------------------------------------------------
	shift_window: process(clk, reset, curState, rest_enable) -----one new data comes in, if the number > current max number, shift window to new data
      BEGIN
        IF rising_edge(clk) THEN
            IF reset ='1' OR rest_enable = '1' THEN
                max_num <= "00000000";
                peak_index <= 0;
	            max_window <= (others => (others => '0'));
	        ELSif curState = dready then
	           IF signed(max_num) < signed(data) THEN ----if current data bigger than current max value, replace max_num by data
	               max_num <= data;
	               peak_index <= counter -1;
	               max_window <= window;
	           ELSIF cur_window(3) = max_window(3) THEN ---if these two value are same means new data come in but less than current max, then append cur_window(2 downto 0) to max_window(2 downto 0)
	               max_window(2 downto 0) <= cur_window(2 downto 0);
	           END IF;
	        END IF;
	     END IF;      
      END PROCESS;
-------------------------------------------------------------------------------------
	assign: process(clk, reset, final_enable, rest_enable) ------after all numbers come out, assign value to dataresults and maxindex
      BEGIN
        IF rising_edge(clk) THEN
            IF reset = '1' OR rest_enable = '1' THEN
                dataResults <= (others => (others => '0'));
	            maxIndex <= (others => (others => '0')); 
            ELSIF final_enable = '1' THEN
                for i in 6 downto 0 loop
	               dataResults(i) <= max_window(i);
	           END loop;
	           maxIndex(2) <= std_logic_vector(unsigned(to_unsigned(peak_index / 100, 4)));---used to convert integer to BCD 
               maxIndex(1) <=std_logic_vector(unsigned(to_unsigned((peak_index mod 100) / 10, 4)));
               maxIndex(0) <= std_logic_vector(unsigned(to_unsigned(peak_index mod 10, 4)));
	       END IF;
	    END IF;
      END PROCESS;
      
-------------------------------------------------------------------------------------
	fsm:process(curState, data, start, ctrl_detect) -----state convert
	BEGIN
	seqDone <= '0';
	dataReady <= '0';
	   CASE curState IS
	       WHEN INIT => -----initial state, to initialise all enable signal
	           rest_enable <= '0';
	           final_enable <= '0';
	           IF start_done = '1' THEN
	               nextState <= s1;
	           ELSE nextState <= INIT;
	           END IF;

	       WHEN s1 => ------wait for ctrlin change
	           IF ctrl_detect = '1' THEN
	               nextState <= dready;
	           ELSE nextState <= s1;
	           END IF;
	           
          WHEN dready => ----keep one clock dataready
	           dataReady <= '1';
	           nextState <= compare;
	
	      WHEN compare=>----compare the new data and current max data
	           IF counter < num THEN
	               nextState <=  s1;
	           ELSE nextState <= final;
	               final_enable <= '1';
	           END IF;
	              
	     WHEN final=>----output seqDone and assigne value
	           seqDone <= '1';
	           rest_enable <= '1';
	           nextState <= INIT;

	   END CASE;
	END PROCESS;

-------------------------------------------------------------------------------------
	seq_state: PROCESS (clk, reset)----sequential logic
	BEGIN
    	IF reset = '1' THEN
      	curState <= INIT;
    	ELSIF clk'EVENT AND clk='1' THEN
      	curState <= nextState;
    	END IF;
  	END PROCESS; -- seq
	--------------------------------------------------
	byte <= data;

END;