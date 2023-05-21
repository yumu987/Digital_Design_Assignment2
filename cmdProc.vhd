-----------------------------------------------------
-- cmdProc.vhd
-- ARTIX-7 35T cpg236-1
-- FPGA
-- Group 10 - Signed
-----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- use ieee.std_logic_unsigned.all;
use ieee.std_logic_signed.all;
use work.common_pack.all;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
use UNISIM.VPKG.ALL;

entity cmdProc is
    port (
        clk:		  in std_logic;
		reset:		  in std_logic;
		rxnow:		  in std_logic;
		rxData:		  in std_logic_vector(7 downto 0);
		txData:		  out std_logic_vector(7 downto 0);
		rxdone:		  out std_logic;
		ovErr:		  in std_logic;
		framErr:	  in std_logic;
		txnow:		  out std_logic;
		txdone:		  in std_logic;
		start:        out std_logic;
		numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
		dataReady:    in std_logic;
		byte:         in std_logic_vector(7 downto 0);
		maxIndex:     in BCD_ARRAY_TYPE(2 downto 0);
		dataResults:  in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
		seqDone:      in std_logic
    );
end cmdProc;

architecture behav of cmdProc is

    type state_type is (
		INIT, FIRST, SECOND, THIRD, FOURTH, L, P, L_LF, L_CR, P_LF, P_CR, LF, CR,
		INIT_ECHO, FIRST_ECHO, SECOND_ECHO, THIRD_ECHO, A_LF, A_CR, ZERO, ANNN,
		WAIT_BYTE, PRINT_BYTE_UPPER, PRINT_BYTE_LOWER, PRINT_BYTE_SPACE, FINAL_WAIT,
		PEAK_UPPER, PEAK_LOWER, PEAK_SPACE, FIRST_INDEX, SECOND_INDEX, THIRD_INDEX,
		L_FIRST_UPPER, L_FIRST_LOWER, L_FIRST_SPACE, L_SECOND_UPPER, L_SECOND_LOWER, L_SECOND_SPACE,
		L_THIRD_UPPER, L_THIRD_LOWER, L_THIRD_SPACE, L_FOURTH_UPPER, L_FOURTH_LOWER, L_FOURTH_SPACE,
		L_FIFTH_UPPER, L_FIFTH_LOWER, L_FIFTH_SPACE, L_SIXTH_UPPER, L_SIXTH_LOWER, L_SIXTH_SPACE,
		L_SEVENTH_UPPER, L_SEVENTH_LOWER, CHECK, SAFE,
        EQUAL, EQUAL_HELP, EQUAL_LF, EQUAL_CR,
        ANNN_EQUAL, ANNN_EQUAL_HELP, ANNN_EQUAL_LF, ANNN_EQUAL_CR,
        P_EQUAL, P_EQUAL_HELP, P_EQUAL_LF, P_EQUAL_CR,
        L_EQUAL, L_EQUAL_HELP, L_EQUAL_LF, L_EQUAL_CR
    );

    signal curState, nextState: state_type;
    signal resultL : CHAR_ARRAY_TYPE(0 to 6); -- register in L
    signal peakP : std_logic_vector(7 downto 0); -- register for peak in P
    signal indexP : std_logic_vector(11 downto 0); -- register for index in P
    signal ascii_result : CHAR_ARRAY_TYPE(0 to 13); -- register for saving ASCII results in L
    signal reg_maxIndex : BCD_ARRAY_TYPE(2 downto 0); -- register for maxIndex
    signal reg_dataResults : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- register for dataResults
	signal reg_numWords : BCD_ARRAY_TYPE(2 downto 0); -- register for numWords_bcd
	signal reg_seqDone : std_logic; -- register for seqDone
	signal tmp : std_logic; -- coordinate with seqDone and reg_seqDone in different processes
	signal ini_maxIndex : BCD_ARRAY_TYPE(2 downto 0); -- initialization signal for maxIndex
	signal ini_dataResults : CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- initialization signal for dataResults
    signal counter : integer range 0 to 5 := 0; -- counter for symbol "="
    signal re_counter : std_logic; -- reset signal for counter if it is '1' (counter <= 0)
    signal en_counter : std_logic; -- enable signal for counter if it is '1' (counter <= counter + 1)

	----------------------------------------
	-- Function for txData output
	----------------------------------------
    function hexa_to_ascii(input : std_logic_vector) return std_logic_vector is
        variable output : std_logic_vector(7 downto 0);
        begin          
            case input is
                when "0000" => output := "00110000";  -- '0'
                when "0001" => output := "00110001";  -- '1'
                when "0010" => output := "00110010";  -- '2'
                when "0011" => output := "00110011";  -- '3'
                when "0100" => output := "00110100";  -- '4'
                when "0101" => output := "00110101";  -- '5'
                when "0110" => output := "00110110";  -- '6'
                when "0111" => output := "00110111";  -- '7'
                when "1000" => output := "00111000";  -- '8'
                when "1001" => output := "00111001";  -- '9'
                when "1010" => output := "01000001";  -- 'A'
                when "1011" => output := "01000010";  -- 'B'
                when "1100" => output := "01000011";  -- 'C'
                when "1101" => output := "01000100";  -- 'D'
                when "1110" => output := "01000101";  -- 'E'
                when "1111" => output := "01000110";  -- 'F'
                when others => output := "00100000";  -- space character
            end case;
        return output;
    end function;
	----------------------------------------

	----------------------------------------
	-- Function for numWords_bcd output
	----------------------------------------
    function ascii_to_hexa(input : std_logic_vector) return std_logic_vector is
        variable output : std_logic_vector(3 downto 0);
        begin
            case input is
				when "00110000" => output := "0000"; -- '0'
				when "00110001" => output := "0001"; -- '1'
				when "00110010" => output := "0010"; -- '2'
				when "00110011" => output := "0011"; -- '3'
				when "00110100" => output := "0100"; -- '4'
				when "00110101" => output := "0101"; -- '5'
				when "00110110" => output := "0110"; -- '6'
				when "00110111" => output := "0111"; -- '7'
				when "00111000" => output := "1000"; -- '8'
				when "00111001" => output := "1001"; -- '9'
				when others =>     output := "1111"; -- 'F'
            end case;
        return output;
    end function;
	----------------------------------------
    
    begin
        -----------------------------------------------------
        nextStateLogic: process(curState, rxnow, rxData, txdone, byte, dataReady, reg_seqDone, reg_numWords, reg_dataResults, reg_maxIndex, peakP, indexP, ascii_result, counter) -- main logic
        begin
		
			----------------------------------------
			-- Signal Initialization
			----------------------------------------
            rxdone <= '0';
            txnow <= '0';
            start <= '0';
			tmp <= '0'; -- reset signal itself
            re_counter <= '0'; -- reset signal itself
            en_counter <= '0'; -- reset signal itself
			ini_maxIndex(2) <= "0000"; -- '0'
			ini_maxIndex(1) <= "0000"; -- '0'
			ini_maxIndex(0) <= "0000"; -- '0'
			ini_dataResults(6) <= "11111111"; -- 'F'
			ini_dataResults(5) <= "11111111"; -- 'F'
			ini_dataResults(4) <= "11111111"; -- 'F'
			ini_dataResults(3) <= "11111111"; -- 'F'
			ini_dataResults(2) <= "11111111"; -- 'F'
			ini_dataResults(1) <= "11111111"; -- 'F'
			ini_dataResults(0) <= "11111111"; -- 'F'
			----------------------------------------
			
            case curState is
				
				----------------------------------------
				-- ECHO and Rx Pattern Recognizer Below
				----------------------------------------
				when INIT_ECHO => -- ECHO
				tmp <= '1'; -- assign reg_seqDone to '0' (reset)
				if rxnow = '1' and txdone = '1' then
					txnow <= '1';
					txData <= rxData;
					rxdone <= '1';
					nextState <= INIT;
				else -- rxnow = '0' or txdone = '0'
					nextState <= INIT_ECHO;
				end if;

                when INIT => -- P, L and ANNN commands recognizer
                if rxData = "01001100" or rxData = "01101100" then -- L or l
			        nextState <= L_LF;
		        elsif rxData = "01010000" or rxData = "01110000" then -- P or p
			        nextState <= P_LF;
		        elsif rxData = "01000001" or rxData = "01100001" then -- A or a
			        nextState <= FIRST_ECHO;
		        else -- invalid commands
			        nextState <= INIT_ECHO;
                end if;
				
				when FIRST_ECHO => -- ECHO
				if rxnow = '1' and txdone = '1' then
					txnow <= '1';
					txData <= rxData;
					rxdone <= '1';
					nextState <= FIRST;
				else -- rxnow = '0' or txdone = '0'
					nextState <= FIRST_ECHO;
				end if;

                when FIRST => -- First N
                if rxData = "00110000" or rxData = "00110001" or rxData = "00110010" or rxData = "00110011" or rxData = "00110100" or rxData = "00110101" or rxData = "00110110" or rxData = "00110111" or rxData = "00111000" or rxData = "00111001" then			
                    reg_numWords(2) <= ascii_to_hexa(rxData);
			        nextState <= SECOND_ECHO;
		        else
			        nextState <= INIT;
		        end if;
				
				when SECOND_ECHO => -- ECHO
				if rxnow = '1' and txdone = '1' then
					txnow <= '1';
					txData <= rxData;
					rxdone <= '1';
					nextState <= SECOND;
				else -- rxnow = '0' or txdone = '0'
					nextState <= SECOND_ECHO;
				end if;

	            when SECOND => -- Second N
                if rxData = "00110000" or rxData = "00110001" or rxData = "00110010" or rxData = "00110011" or rxData = "00110100" or rxData = "00110101" or rxData = "00110110" or rxData = "00110111" or rxData = "00111000" or rxData = "00111001" then			
                    reg_numWords(1) <= ascii_to_hexa(rxData);
			        nextState <= THIRD_ECHO;
		        else
			        nextState <= INIT;
		        end if;
				
				when THIRD_ECHO => -- ECHO
				if rxnow = '1' and txdone = '1' then
					txnow <= '1';
					txData <= rxData;
					rxdone <= '1';
					nextState <= THIRD;
				else -- rxnow = '0' or txdone = '0'
					nextState <= THIRD_ECHO;
				end if;

                when THIRD => -- Third N
                if rxData = "00110000" or rxData = "00110001" or rxData = "00110010" or rxData = "00110011" or rxData = "00110100" or rxData = "00110101" or rxData = "00110110" or rxData = "00110111" or rxData = "00111000" or rxData = "00111001" then			
                    reg_numWords(0) <= ascii_to_hexa(rxData);
			        nextState <= A_LF;
		        else
			        nextState <= INIT;
		        end if;
				----------------------------------------
				
				----------------------------------------
				-- aNNN / ANNN Format Helper and numWords_bcd Logic Helper Below
				----------------------------------------
				when A_LF =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= A_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= A_LF;
                end if;
				
				when A_CR =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= ANNN_EQUAL; -- printing "=====" separation line
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= A_CR;
                end if;
				
				when ZERO => -- a000 / A000 will be isolated and does not interact with dataConsume
				if reg_numWords(2) = "0000" and reg_numWords(1) = "0000" and reg_numWords(0) = "0000" then -- A000 / a000
					nextState <= INIT_ECHO;
				else -- not A000 / a000
					nextState <= ANNN;
				end if;
				
				when ANNN => -- assert numWords_bcd, special case a000 / A000 has been removed
				numWords_bcd(2) <= reg_numWords(2);
				numWords_bcd(1) <= reg_numWords(1);
				numWords_bcd(0) <= reg_numWords(0);
				nextState <= CHECK;
				----------------------------------------

                ----------------------------------------
                -- "=" Helper Below
                -- For aNNN / ANNN
                ----------------------------------------
                when ANNN_EQUAL =>
                if counter < 5 then -- counter = 0, 1, 2, 3, 4
                    nextState <= ANNN_EQUAL_HELP;
                else -- counter = 5 or counter > 5, "=" has been printed out and finished
                    nextState <= ANNN_EQUAL_LF;
                end if;

                when ANNN_EQUAL_HELP =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00111101"; -- "="
                    en_counter <= '1'; -- counter <= counter + 1
					nextState <= ANNN_EQUAL; -- back to ANNN_EQUAL for next "=" printing
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= ANNN_EQUAL_HELP;
                end if;

                when ANNN_EQUAL_LF =>
                re_counter <= '1'; -- reset counter in 'next state' instead of 'current state'
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= ANNN_EQUAL_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= ANNN_EQUAL_LF;
                end if;

                when ANNN_EQUAL_CR =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= ZERO; -- check special case a000 / A000
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= ANNN_EQUAL_CR;
                end if;
                ----------------------------------------
				
				----------------------------------------
				-- P and L Signal Format Helper and Input Helper Below
				----------------------------------------
				when L_LF =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= L_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_LF;
                end if;
				
				when L_CR =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= L_EQUAL; -- printing "=====" separation line
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_CR;
                end if;

				when L => -- load L's results
				resultL(0) <= reg_dataResults(6); -- dataResults(55 downto 48)
                resultL(1) <= reg_dataResults(5); -- dataResults(47 downto 40)
		        resultL(2) <= reg_dataResults(4); -- dataResults(39 downto 32)
		        resultL(3) <= reg_dataResults(3); -- dataResults(31 downto 24)
		        resultL(4) <= reg_dataResults(2); -- dataResults(23 downto 16)
		        resultL(5) <= reg_dataResults(1); -- dataResults(15 downto 8)
		        resultL(6) <= reg_dataResults(0); -- dataResults(7 downto 0)
                nextState <= L_FIRST_UPPER;
				
				when P_LF =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= P_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= P_LF;
                end if;
				
				when P_CR =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= P_EQUAL; -- printing "=====" separation line
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= P_CR;
                end if;
				
				when P => -- load P's results
				peakP <= reg_dataResults(3); -- dataResults(31 downto 24)
                indexP <= reg_maxIndex(2) & reg_maxIndex(1) & reg_maxIndex(0); -- hexadecimal
                nextState <= PEAK_UPPER;
				----------------------------------------

                ----------------------------------------
                -- "=" Helper Below
                -- For P and L
                ----------------------------------------
                when P_EQUAL =>
                if counter < 5 then -- counter = 0, 1, 2, 3, 4
                    nextState <= P_EQUAL_HELP;
                else -- counter = 5 or counter > 5, "=" has been printed out and finished
                    nextState <= P_EQUAL_LF;
                end if;

                when P_EQUAL_HELP =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00111101"; -- "="
                    en_counter <= '1'; -- counter <= counter + 1
					nextState <= P_EQUAL; -- back to P_EQUAL for next "=" printing
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= P_EQUAL_HELP;
                end if;

                when P_EQUAL_LF =>
                re_counter <= '1'; -- reset counter in 'next state' instead of 'current state'
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= P_EQUAL_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= P_EQUAL_LF;
                end if;

                when P_EQUAL_CR =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= P; -- P
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= P_EQUAL_CR;
                end if;

                when L_EQUAL =>
                if counter < 5 then -- counter = 0, 1, 2, 3, 4
                    nextState <= L_EQUAL_HELP;
                else -- counter = 5 or counter > 5, "=" has been printed out and finished
                    nextState <= L_EQUAL_LF;
                end if;

                when L_EQUAL_HELP =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00111101"; -- "="
                    en_counter <= '1'; -- counter <= counter + 1
					nextState <= L_EQUAL; -- back to L_EQUAL for next "=" printing
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_EQUAL_HELP;
                end if;

                when L_EQUAL_LF =>
                re_counter <= '1'; -- reset counter in 'next state' instead of 'current state'
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= L_EQUAL_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_EQUAL_LF;
                end if;

                when L_EQUAL_CR =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= L; -- L
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_EQUAL_CR;
                end if;
                ----------------------------------------
				
				----------------------------------------
				-- State Checking Helper Below
				-- For Byte Printing
				----------------------------------------
				when CHECK =>
				if reg_seqDone = '1' then -- after aNNN / ANNN, initialize reg_seqDone signal if it is '1'
					nextState <= SAFE;
				elsif reg_seqDone = '0' then -- if reg_seqDone is '0', it can directly interact with dataConsume with start signal
					nextState <= FOURTH;
				else -- other circumstances (exception)
					nextState <= CHECK;
				end if;
				
				when SAFE =>
				tmp <= '1'; -- reset reg_seqDone to '0'
				nextState <= FOURTH;
				
				when FOURTH => -- start
                start <= '1';
                nextState <= WAIT_BYTE;
                               
                when WAIT_BYTE => -- dataReady
                if dataReady = '1' then
                    nextState <= PRINT_BYTE_UPPER;
                else -- dataReady = '0'
                    nextState <= WAIT_BYTE;
                end if;

                when FINAL_WAIT =>
                if reg_seqDone = '1' then -- transfer of all bytes have been finished
                    nextState <= LF;
                else -- reg_seqDone = '0'
                    nextState <= FOURTH;
                end if;
				----------------------------------------
				
				----------------------------------------
				-- Line Feed and Carriage Return Format Helper Below
				-- For L, P and Byte Printing
				----------------------------------------
				when LF =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= LF;
                end if;
				
				when CR =>
				if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= EQUAL; -- printing "=====" separation line
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= CR;
                end if;
				----------------------------------------

                ----------------------------------------
                -- "=" Helper Below
                -- For L, P and Byte Printing
                ----------------------------------------
                when EQUAL =>
                if counter < 5 then -- counter = 0, 1, 2, 3, 4
                    nextState <= EQUAL_HELP;
                else -- counter = 5 or counter > 5, "=" has been printed out and finished
                    nextState <= EQUAL_LF;
                end if;

                when EQUAL_HELP =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00111101"; -- "="
                    en_counter <= '1'; -- counter <= counter + 1
					nextState <= EQUAL; -- back to EQUAL for next "=" printing
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= EQUAL_HELP;
                end if;

                when EQUAL_LF =>
                re_counter <= '1'; -- reset counter in 'next state' instead of 'current state'
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001010"; -- line feed
					nextState <= EQUAL_CR;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= EQUAL_LF;
                end if;

                when EQUAL_CR =>
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00001101"; -- carriage return
					nextState <= INIT_ECHO; -- back to all initial state
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= EQUAL_CR;
                end if;
                ----------------------------------------

				----------------------------------------
				-- Byte Printing Below
				----------------------------------------
                when PRINT_BYTE_UPPER => -- byte_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(byte(7 downto 4)); -- upper
					nextState <= PRINT_BYTE_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= PRINT_BYTE_UPPER;
                end if;

                when PRINT_BYTE_LOWER => -- byte_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(byte(3 downto 0)); -- lower
					nextState <= PRINT_BYTE_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= PRINT_BYTE_LOWER;
                end if;

                when PRINT_BYTE_SPACE => -- byte_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= FINAL_WAIT;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= PRINT_BYTE_SPACE;
                end if;
				----------------------------------------

				----------------------------------------
				-- P Printing Below
				----------------------------------------
                when PEAK_UPPER => -- peak_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(peakP(7 downto 4)); -- upper
					nextState <= PEAK_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= PEAK_UPPER;
                end if;
                
                when PEAK_LOWER => -- peak_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(peakP(3 downto 0)); -- lower
					nextState <= PEAK_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= PEAK_LOWER;
                end if;
                
                when PEAK_SPACE => -- peak_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= FIRST_INDEX;
                else -- txdone = '0';
                    txnow <= '0';
                    nextState <= PEAK_SPACE;
                end if;

                when FIRST_INDEX => -- 1st index
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(indexP(11 downto 8));
					nextState <= SECOND_INDEX;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= FIRST_INDEX;
                end if;

                when SECOND_INDEX => -- 2nd index
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(indexP(7 downto 4));
					nextState <= THIRD_INDEX;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= SECOND_INDEX;
                end if;

                when THIRD_INDEX => -- 3rd index
                if txdone = '1' then
                    txnow <= '1';
					txData <= hexa_to_ascii(indexP(3 downto 0));
					nextState <= LF;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= THIRD_INDEX;
                end if;
				----------------------------------------

				----------------------------------------
				-- L Printing Below
				----------------------------------------
                when L_FIRST_UPPER => -- 1_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(0);
					nextState <= L_FIRST_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FIRST_UPPER;
                end if;
                
                when L_FIRST_LOWER => -- 1_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(1);
					nextState <= L_FIRST_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FIRST_LOWER;
                end if;
                
                when L_FIRST_SPACE => -- 1_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= L_SECOND_UPPER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FIRST_SPACE;
                end if;

                when L_SECOND_UPPER => -- 2_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(2);
					nextState <= L_SECOND_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SECOND_UPPER;
                end if;
                
                when L_SECOND_LOWER => -- 2_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(3);
					nextState <= L_SECOND_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SECOND_LOWER;
                end if;
                
                when L_SECOND_SPACE => -- 2_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= L_THIRD_UPPER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SECOND_SPACE;
                end if;
                
                when L_THIRD_UPPER => -- 3_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(4);
					nextState <= L_THIRD_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_THIRD_UPPER;
                end if;
                
                when L_THIRD_LOWER => -- 3_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(5);
					nextState <= L_THIRD_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_THIRD_LOWER;
                end if;
                
                when L_THIRD_SPACE => -- 3_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= L_FOURTH_UPPER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_THIRD_SPACE;
                end if;

                when L_FOURTH_UPPER => -- 4_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(6);
					nextState <= L_FOURTH_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FOURTH_UPPER;
                end if;
                
                when L_FOURTH_LOWER => -- 4_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(7);
					nextState <= L_FOURTH_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FOURTH_LOWER;
                end if;
                
                when L_FOURTH_SPACE => -- 4_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= L_FIFTH_UPPER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FOURTH_SPACE;
                end if;
                
                when L_FIFTH_UPPER => -- 5_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(8);
					nextState <= L_FIFTH_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FIFTH_UPPER;
                end if;
                
                when L_FIFTH_LOWER => -- 5_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(9);
					nextState <= L_FIFTH_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FIFTH_LOWER;
                end if;
                
                when L_FIFTH_SPACE => -- 5_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= L_SIXTH_UPPER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_FIFTH_SPACE;
                end if;
                
                when L_SIXTH_UPPER => -- 6_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(10);
					nextState <= L_SIXTH_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SIXTH_UPPER;
                end if;
                
                when L_SIXTH_LOWER => -- 6_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(11);
					nextState <= L_SIXTH_SPACE;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SIXTH_LOWER;
                end if;
                
                when L_SIXTH_SPACE => -- 6_space
                if txdone = '1' then
                    txnow <= '1';
					txData <= "00100000"; -- space character
					nextState <= L_SEVENTH_UPPER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SIXTH_SPACE;
                end if;
                
                when L_SEVENTH_UPPER => -- 7_upper
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(12);
					nextState <= L_SEVENTH_LOWER;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SEVENTH_UPPER;
                end if;
                
                when L_SEVENTH_LOWER => -- 7_lower
                if txdone = '1' then
                    txnow <= '1';
					txData <= ascii_result(13);
					nextState <= LF;
                else -- txdone = '0'
                    txnow <= '0';
                    nextState <= L_SEVENTH_LOWER;
                end if;
				----------------------------------------
                
            end case;
			
        end process;
        -----------------------------------------------------
        seqLogic: process(clk, reset) -- sequential logic
        begin
            if reset = '1' then -- high reset
                curState <= INIT_ECHO;
            elsif rising_edge(clk) then -- clk'event and clk = '1'
                curState <= nextState;
            end if;
        end process;
        -----------------------------------------------------
        counterLogic: process(clk, reset, re_counter, en_counter) -- counter logic for reset counter and enable increment of counter
        begin
            if reset = '1' or re_counter = '1' then
                counter <= 0;
            elsif rising_edge(clk) and en_counter = '1' then -- flip-flop counter, latch counter will cause double increment
                counter <= counter + 1;
            end if;
        end process;
        -----------------------------------------------------
        regLogic: process(clk, reset, seqDone, tmp, maxIndex, dataResults, ini_maxIndex, ini_dataResults) -- dataResults are only available when seqDone = '1'
        begin
			if reset = '1' then -- reset maxIndex and dataResults
				reg_maxIndex <= ini_maxIndex; -- '0'
				reg_dataResults <= ini_dataResults; -- 'F'
			elsif rising_edge(clk) then -- clk'event and clk = '1'
				if seqDone = '1' then -- register seqDone when seqDone = '1'
					reg_maxIndex <= maxIndex;
					reg_dataResults <= dataResults;
					reg_seqDone <= '1';
				elsif tmp = '1' then -- reset reg_seqDone
					reg_seqDone <= '0';
				end if;
			end if;
        end process;
        -----------------------------------------------------
        lLogic: process(clk, resultL) -- L signals process
        begin
			if rising_edge(clk) then -- clk'event and clk = '1'
				for i in 0 to 6 loop -- 7 waves output             
					ascii_result(2*i) <= hexa_to_ascii(resultL(i)(7 downto 4)); -- upper
					ascii_result(2*i+1) <= hexa_to_ascii(resultL(i)(3 downto 0)); -- lower
				end loop;
			end if;
        end process;
        -----------------------------------------------------
    end behav; -- cmdProc