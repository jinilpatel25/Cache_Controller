There is a need for microcomputer firmware to be stored in ROM so that the CPU can boot directly from it when power is applied. Having large amounts of ROM that the CPU can execute code directly from is becoming less “fashionable” because the required chips to support it are both physically large (48-pin packages are common) and not particularly dense (in an age of multi-megabyte or even gigabyte storage) due to their reliance on NOR flash technology with larger storage cells. 

Over the last decade, there has been a big shift towards using smaller, denser NAND Flash devices with serial load/store interfaces. This makes for a physically smaller device (8-pin surface mount packages are unbelievably tiny). However, the serial nature of these devices requires a dedicated hardware controller to sit between the CPU and the Flash memory. 

The idea is that at power on, a small amount of ROM that the CPU can execute code directly from (e.g. our ROM from Lab 1 that holds our debug monitor) is used to “load” the actual application firmware from Flash memory, via the SPi controller, into Dram from where it can then be executed. That is our goal for this lab.

Being serial-based means that we have to introduce a dedicated controller to handle the interface and protocol between the CPU and the flash memory chip itself and write the appropriate driver software to talk to the memory chip.
