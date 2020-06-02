----------------------------------------------------------------------------------
--DATA PROCESSOR: 
--This data processor communicates with the command processor in order to generate bytes whenever asked
-- & detect and save the peak of each sequence of bytes generated
-- & also saves the 3 bytes before and 3 bytes after the peak as well as the index number of the peak in BCD form
--Also it communicates to the command processor the results,and 2 signals dataReady and seqDone which signal
--that a new byte is generated and that all the bytes of a sequence has been generated respectively.
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;
use work.common_pack.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;


entity dataConsume is
  	port (
	    clk: in std_logic;
		reset: in std_logic; -- synchronous reset
		start: in std_logic;		
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
		ctrlIn: in std_logic;
		ctrlOut: out std_logic ; --:= '0' (default value)
		data: in std_logic_vector(7 downto 0);
		dataReady: out std_logic;
		byte: out std_logic_vector(7 downto 0);
		seqDone: out std_logic;
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0);		
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) 
  	);
end dataConsume;



architecture Behavioral of dataConsume is

TYPE state_type IS (IDLE,NEW_BYTE);  -- List your states here 	
SIGNAL curState, nextState: state_type;
SIGNAL prev_ctrlIn: std_logic; --to detect transition in ctrlIn which signals new byte is generated
SIGNAL send_ctrlOut: std_logic; --to transition the ctrlOut & signal that we need new byte generated
SIGNAL numWords_counter : integer; --counter of bytes generated
SIGNAL genByte_counter : integer; --for peak detection
SIGNAL numWords_dec : integer;--numWords in decimal form (BCD to decimal conversion)
SIGNAL last_bytes : CHAR_ARRAY_TYPE(0 to 2); -- register for saving the last 4 bytes generated at every moment
SIGNAL peak : std_logic_vector(7 downto 0);
--enable/control signals & coutners
SIGNAL dataReady_reg, seqDone_reg: std_logic;
SIGNAL reset_dR, reset_counter : std_logic;
SIGNAL dataResultsBefore_en, dataResultsAfter1_en, dataResultsAfter2_en, dataResultsAfter3_en: boolean;
SIGNAL maxIndexUpdate_en, counter_en, not_ctrlOut_en, last_bytes_en, genByte_count_en : boolean;
SIGNAL newSeq_rst, newPeak_rst : std_logic;

--MAIN CODE BELOW:
begin


	--================ ##SEQUENTIAL LOGIC FOR REGISTERS## =======================
	
	-- REGISTERS FOR DATA GENERATOR COMMUNICATION (prev_ctrlIn, send_ctrlOut, ctrlOut)
	----saves the current ctrlIn value as prev_ctrlIn in order to be possible to compare ctrlIn with its previous value
	ctrlIn_ff : process(clk,reset) 
        begin
			if reset = '1' then
			  prev_ctrlIn <= '-';

            elsif rising_edge(clk) then
               prev_ctrlIn <= ctrlIn;  --to later on detect transition of ctrlIn
            end if;
        end process;

    -- save the value that we want ctrlOut to have 
    -- when the enable signal is true then the ctrlOut signal has to be transitioned
	send_ctrlOut_ff : process(clk,reset)
        begin
			if reset = '1' then
			  send_ctrlOut <= '0';

            elsif rising_edge(clk) then
				if not_ctrlOut_en = true then
					send_ctrlOut<= not(send_ctrlOut); 
				end if;
			end if;
        end process;
		
	-- the ctrlOut signal is updated by the send_ctrlOut value 	
	ctrlOut_ff : process(clk,reset)
        begin
			if reset = '1' then
			  ctrlOut <= '0';

            elsif rising_edge(clk) then
               ctrlOut <= send_ctrlOut; 
            end if;
        end process;


    -- REGISTERS FOR COMMAND PROCESSOR COMMUNICATION (dataReady, byte, seqDone, numWords_counter, numWords_dec, last_bytes)
	-- updates the dataReady signal by the dataReady_reg signal combi_out process
	dataReady_ff : process(clk,reset)
        begin
			if reset = '1' then
			  dataReady <= '0';

            elsif rising_edge(clk) then
               dataReady <= dataReady_reg;  --to later on detect transition of ctrlIn
            end if;
        end process;
		
	-- updates the byte signal with the input data signal	
	byte_ff : process(clk,reset)
        begin
			if reset = '1' then
                byte <= "--------";

            elsif rising_edge(clk) then
                byte <= data;  --to later on detect transition of ctrlIn
            end if;
        end process;
		
		
	-- updates the seqDone signal by the seqDone_reg signal combi_out process
	seqDone_ff : process(clk,reset)
        begin
			if reset = '1' then
			  seqDone <= '0';

            elsif rising_edge(clk) then
               seqDone <= seqDone_reg;  --to later on detect transition of ctrlIn
            end if;
        end process;
		
		
	--counter which counts the number of bytes generated and resets when there is a new sequence
	--these two events are indicated by counter_en and reset_counter signals
	numWords_counter_ff : process(clk,reset)
		variable count: integer;
		begin	
            if reset = '1' then
                count := -1;

            elsif rising_edge(clk) then
				if reset_counter='1'then 
					count := 0;
				elsif  counter_en=true then
					count := count + 1;
				end if;
			end if;
			
			numWords_counter <= count;
        end process;
		
	
	--bcd to decimal conversion for numWords_bcd input
	numWords_dec_ff : process(clk,reset) 
	begin
		if reset = '1' then
			numWords_dec <= 0;

		elsif rising_edge(clk) then
			numWords_dec <= (TO_INTEGER(UNSIGNED(numWords_bcd(0)))) + (TO_INTEGER(UNSIGNED(numWords_bcd(1)))*10) + (TO_INTEGER(UNSIGNED(numWords_bcd(2)))*100);
		end if;
	end process;


    --update last bytes, which save the 3 last generated bytes at every moment--
	last_bytes_ff : process(clk,reset) 
	begin
		if reset = '1' then
		   last_bytes(0) <= "--------";
           last_bytes(1) <= "--------";
           last_bytes(2) <= "--------";

		elsif rising_edge(clk) then
			if last_bytes_en=true then
				last_bytes(0) <= last_bytes(1);
				last_bytes(1) <= last_bytes(2);
				last_bytes(2) <=  data;
			end if;
		end if;
	end process;
	
	
	seq_state: PROCESS (clk, reset) 
	BEGIN
        --asynchronous active high reset
        IF reset = '1' THEN
          curState <= IDLE;
    
        ELSIF rising_edge(clk) THEN
          curState <= nextState;
    
        END IF;
	END PROCESS; -- seq

	
	-- REGISTERS FOR PEAK DETECTION (dataResults, peak, maxIndex, genByte_counter)
	dataRes_ff : process(clk,reset)
    begin
		if reset = '1' then
		   dataResults <= (others => x"00");
            
		elsif rising_edge(clk) then
               if reset_dR = '1' then
                    dataResults <= (others => x"00");               
                    
               elsif (dataResultsBefore_en = true) then --saving byte as peak & the 3 before peak bytes
                    dataResults(3) <= data; --peak is new byte
                    dataResults(4) <= last_bytes(2);
                    dataResults(5) <= last_bytes(1);
                    dataResults(6) <= last_bytes(0);
                    peak <= data;
                                  
                elsif (dataResultsAfter1_en = true) then --saving byte as 1st after peak byte
                    dataResults(2)<= data;
                elsif (dataResultsAfter2_en = true) then --saving byte as 2nd after peak byte
                    dataResults(1)<= data;
                elsif (dataResultsAfter3_en = true) then --saving byte as 3rd after peak byte
                    dataResults(0)<= data;
                end if;
		end if;
     end process;
	 
    --decimal to bcd conversion for maxIndex output
	maxIndex_ff : process(clk,reset) 
	
		variable maxIndex_var2 : integer range 0 to 9;
		variable maxIndex_var1 : integer range 0 to 9;
		variable maxIndex_var0 : integer range 0 to 9;

        begin
			if reset = '1' then
			  maxIndex(2) <= "0000";
			  maxIndex(1) <= "0000";
			  maxIndex(0) <= "0000";

            elsif rising_edge(clk) then
				if (maxIndexUpdate_en = true) then
					maxIndex_var2 := (numWords_counter+1)/ 100;
					maxIndex_var1 := ((numWords_counter+1) - (maxIndex_var2 * 100))/10;
					maxIndex_var0 := (((numWords_counter+1) - (maxIndex_var2 * 100)) - (maxIndex_var1 *10));
					
					maxIndex(2) <= std_logic_vector(to_unsigned(maxIndex_var2, 4));
					maxIndex(1) <= std_logic_vector(to_unsigned(maxIndex_var1, 4));
					maxIndex(0) <= std_logic_vector(to_unsigned(maxIndex_var0, 4));
				end if;
            end if;
        end process;


    --counter which saves the number of bytes after detecting a peak in order to know when to save the after peak bytes        
     genByte_count_ff : process(clk,reset) 
     variable count: integer;
        begin
			if reset = '1' then
			  count := 0;
			  
            elsif rising_edge(clk) then
                if genByte_count_en = true then
                    count:= count+1;
                elsif newSeq_rst = '1' then
                    count:= 0;
                elsif newPeak_rst = '1' then
                    count:= 1;
                end if;
           			  
            end if;
            genByte_counter <= count;
        end process;
--	--================ ##END_SEQUENTIAL LOGIC FOR REGISTERS## =======================



	 --~~~~~~~~~~~~~~~~~~ ##BYTE GENERATION (NEXT STATE LOGIC)## ~~~~~~~~~~~~~~~~~~~~~
    byteGenerator_nextState: process(curState,start, ctrlIn, prev_ctrlIn)
	--signals changing in this process: nextState
	BEGIN
        CASE curState IS
			WHEN IDLE => 
				IF (start = '1') THEN 
					nextState <= NEW_BYTE;   
				ELSE
					nextState <= IDLE;
				END IF;	  
		  
			WHEN NEW_BYTE =>
			   IF (start = '1') THEN --if start is high
				  IF ((prev_ctrlIn XOR ctrlIn) = '1') THEN --checks for transition of ctrlIn which indicates that byte is ready
					nextState <= IDLE;
				  ELSE 
					nextState <= NEW_BYTE; --waiting until transition of ctrlIn
			      END IF;
			   ELSE
			   nextState <= IDLE; --waiting until transition of ctrlIn
			   END IF;
					
  			WHEN OTHERS => null;
				
        end case;
	END PROCESS;
	 --~~~~~~~~~~~~~~~~~~ ##END_ BYTE GENERATION (NEXT STATE LOGIC)## ~~~~~~~~~~~~~~~~~~~~~


	 -- ======================================= ##OUTPUT LOGIC## ===========================================
    combi_out: process(curState,start, ctrlIn, prev_ctrlIn, numWords_counter, numWords_dec)
	--signals changing in this process due to the control signals update:
	--send_ctrlOut,dataReady, seqDone, numWords_counter, last_bytes

	BEGIN
		--default signal values
		not_ctrlOut_en <= false;
		dataReady_reg <= '0';
		seqDone_reg <= '0';
		counter_en <= false;
		reset_counter <= '0';
		last_bytes_en <= false;
        ----------------------
		
        CASE curState IS
			WHEN IDLE => 
				IF (start = '1') THEN 
					not_ctrlOut_en <= true; --transition ctrlOut to trigger byte generator
				END IF;	  
				          				
			WHEN NEW_BYTE =>
			   IF (start = '1') THEN --if start is high
				  IF ((prev_ctrlIn XOR ctrlIn) = '1') THEN --checks for transition of ctrlIn which indicates that byte is ready
				  
                    counter_en <= true;     --increase numWords_counter that indicates number of bytes generated
                    last_bytes_en <= true;  --update the last_bytes signal with 3 last bytes generated
                    dataReady_reg <= '1';   --update dataReady to '1'
					
					IF (numWords_counter+1 = numWords_dec) THEN --if all bytes of sequence generated
						seqDone_reg <= '1';  --update seqDone to '1'
						reset_counter <= '1';--reset numWords_counter to 0	
					END IF;
			     END IF;
			   END IF;
					                    
			WHEN OTHERS => null;
				
        end case;

	END PROCESS;		
	--      ==================================== ##END_OUTPUT LOGIC## =================================

	

	 -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##PEAK DETECTION LOGIC## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    peak_detection: PROCESS(curState,start, ctrlIn)
	--signals changing in this process: genByte_counter, dataResults, maxIndex
	BEGIN
		--default signal values
        genByte_count_en <= false;
        newPeak_rst <= '0';
        newSeq_rst <= '0';
        dataResultsBefore_en <= false;
        dataResultsAfter1_en <= false;
        dataResultsAfter2_en <= false;
        dataResultsAfter3_en <= false;
        reset_dR <= '0';
        maxIndexUpdate_en <= false;
        ---------------------------

        CASE curState IS
			WHEN IDLE => 	
				IF (numWords_counter = 0) THEN --if all bytes of sequence generated
                    newSeq_rst <= '1'; --reset genByte_counter
                    reset_dR <= '1';   --reset dataResults 
                END IF;			
		  
			WHEN NEW_BYTE =>
				IF (start = '1') THEN --if start still high
				  IF ((prev_ctrlIn XOR ctrlIn) = '1') THEN --checks for transition of ctrlIn which indicates that byte is ready
                    
                    --if genByte_counter=0 then saves new byte as peak directly (without checking)
                    --or otherwise if the new byte is the new peak
                    IF (genByte_counter=0) OR (data > peak) THEN 
                        dataResultsBefore_en <= true; --save peak & 3 before peak bytes in dataResults and new byte in peak signal
                        maxIndexUpdate_en <= true;    --update maxIndex 
                        newPeak_rst <= '1';	          --reset genByte_counter to 1
                        
                    ELSE --sequence not done 
                        genByte_count_en <= true; --increase genByte_counter 
                        IF     genByte_counter=1 THEN dataResultsAfter1_en <= true;  --save new byte as 1st after peak
                        ELSIF  genByte_counter=2 THEN dataResultsAfter2_en <= true;  --save new byte as 2nd after peak
                        ELSIF  genByte_counter=3 THEN dataResultsAfter3_en <= true;  --save new byte as 3rd after peak
                        END IF;  
                    END IF;
				  END IF;
				END IF;

			WHEN OTHERS => null;
				
        end case;
	END PROCESS;
	 -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##END_PEAK DETECTION LOGIC## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     

end Behavioral;
