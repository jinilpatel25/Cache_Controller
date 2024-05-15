In this project, we will equip our 68k processor with a Cache controller based on a unified instruction and data design.  The storage for the cache will be made up of SRAM present within the FPGA. Our design will based around a single-level cache as there is no benefit to having level 2 or 3 caches when the memory speed for all of them is the same.
Ideally, we would like separate data and instruction caches, since the two types of cache can be optimized slightly differently, but the softcore 68k processor does not provide the necessary signals required to determine if the current memory access being made is for an instruction op-code fetch or for data/variables. 
