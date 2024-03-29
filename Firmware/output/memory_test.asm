; C:\USERS\ADMIN\DESKTOP\ASSIGNMENT1\SRAM_CONTROLLER\FIRMWARE\MEMORY_TEST.C - Compiled by CC68K  Version 5.00 (c) 1991-2005  Peter J. Fondse
; #include <stdio.h>
; #include <string.h>
; #include <ctype.h>
; //IMPORTANT
; //
; // Uncomment one of the two #defines below
; // Define StartOfExceptionVectorTable as 08030000 if running programs from sram or
; // 0B000000 for running programs from dram
; //
; // In your labs, you will initially start by designing a system with SRam and later move to
; // Dram, so these constants will need to be changed based on the version of the system you have
; // building
; //
; // The working 68k system SOF file posted on canvas that you can use for your pre-lab
; // is based around Dram so #define accordingly before building
; #define StartOfExceptionVectorTable 0x08030000
; // #define StartOfExceptionVectorTable 0x0B000000
; /**********************************************************************************************
; **	Parallel port addresses
; **********************************************************************************************/
; #define PortA   *(volatile unsigned char *)(0x00400000)
; #define PortB   *(volatile unsigned char *)(0x00400002)
; #define PortC   *(volatile unsigned char *)(0x00400004)
; #define PortD   *(volatile unsigned char *)(0x00400006)
; #define PortE   *(volatile unsigned char *)(0x00400008)
; /*********************************************************************************************
; **	Hex 7 seg displays port addresses
; *********************************************************************************************/
; #define HEX_A        *(volatile unsigned char *)(0x00400010)
; #define HEX_B        *(volatile unsigned char *)(0x00400012)
; #define HEX_C        *(volatile unsigned char *)(0x00400014)    // de2 only
; #define HEX_D        *(volatile unsigned char *)(0x00400016)    // de2 only
; /**********************************************************************************************
; **	LCD display port addresses
; **********************************************************************************************/
; #define LCDcommand   *(volatile unsigned char *)(0x00400020)
; #define LCDdata      *(volatile unsigned char *)(0x00400022)
; /********************************************************************************************
; **	Timer Port addresses
; *********************************************************************************************/
; #define Timer1Data      *(volatile unsigned char *)(0x00400030)
; #define Timer1Control   *(volatile unsigned char *)(0x00400032)
; #define Timer1Status    *(volatile unsigned char *)(0x00400032)
; #define Timer2Data      *(volatile unsigned char *)(0x00400034)
; #define Timer2Control   *(volatile unsigned char *)(0x00400036)
; #define Timer2Status    *(volatile unsigned char *)(0x00400036)
; #define Timer3Data      *(volatile unsigned char *)(0x00400038)
; #define Timer3Control   *(volatile unsigned char *)(0x0040003A)
; #define Timer3Status    *(volatile unsigned char *)(0x0040003A)
; #define Timer4Data      *(volatile unsigned char *)(0x0040003C)
; #define Timer4Control   *(volatile unsigned char *)(0x0040003E)
; #define Timer4Status    *(volatile unsigned char *)(0x0040003E)
; /*********************************************************************************************
; **	RS232 port addresses
; *********************************************************************************************/
; #define RS232_Control     *(volatile unsigned char *)(0x00400040)
; #define RS232_Status      *(volatile unsigned char *)(0x00400040)
; #define RS232_TxData      *(volatile unsigned char *)(0x00400042)
; #define RS232_RxData      *(volatile unsigned char *)(0x00400042)
; #define RS232_Baud        *(volatile unsigned char *)(0x00400044)
; /*********************************************************************************************
; **	PIA 1 and 2 port addresses
; *********************************************************************************************/
; #define PIA1_PortA_Data     *(volatile unsigned char *)(0x00400050)         // combined data and data direction register share same address
; #define PIA1_PortA_Control *(volatile unsigned char *)(0x00400052)
; #define PIA1_PortB_Data     *(volatile unsigned char *)(0x00400054)         // combined data and data direction register share same address
; #define PIA1_PortB_Control *(volatile unsigned char *)(0x00400056)
; #define PIA2_PortA_Data     *(volatile unsigned char *)(0x00400060)         // combined data and data direction register share same address
; #define PIA2_PortA_Control *(volatile unsigned char *)(0x00400062)
; #define PIA2_PortB_data     *(volatile unsigned char *)(0x00400064)         // combined data and data direction register share same address
; #define PIA2_PortB_Control *(volatile unsigned char *)(0x00400066)
; /*********************************************************************************************************************************
; (( DO NOT initialise global variables here, do it main even if you want 0
; (( it's a limitation of the compiler
; (( YOU HAVE BEEN WARNED
; *********************************************************************************************************************************/
; unsigned int i, x, y, z, PortA_Count;
; unsigned char Timer1Count, Timer2Count, Timer3Count, Timer4Count ;
; /*******************************************************************************************
; ** Function Prototypes
; *******************************************************************************************/
; void Wait1ms(void);
; void Wait3ms(void);
; void Init_LCD(void) ;
; void LCDOutchar(int c);
; void LCDOutMess(char *theMessage);
; void LCDClearln(void);
; void LCDline1Message(char *theMessage);
; void LCDline2Message(char *theMessage);
; int sprintf(char *out, const char *format, ...) ;
; /*****************************************************************************************
; **	Interrupt service routine for Timers
; **
; **  Timers 1 - 4 share a common IRQ on the CPU  so this function uses polling to figure
; **  out which timer is producing the interrupt
; **
; *****************************************************************************************/
; void Timer_ISR()
; {
       section   code
       xdef      _Timer_ISR
_Timer_ISR:
; if(Timer1Status == 1) {         // Did Timer 1 produce the Interrupt?
       move.b    4194354,D0
       cmp.b     #1,D0
       bne.s     Timer_ISR_1
; Timer1Control = 3;      	// reset the timer to clear the interrupt, enable interrupts and allow counter to run
       move.b    #3,4194354
; PortA = Timer1Count++ ;     // increment an LED count on PortA with each tick of Timer 1
       move.b    _Timer1Count.L,D0
       addq.b    #1,_Timer1Count.L
       move.b    D0,4194304
Timer_ISR_1:
; }
; if(Timer2Status == 1) {         // Did Timer 2 produce the Interrupt?
       move.b    4194358,D0
       cmp.b     #1,D0
       bne.s     Timer_ISR_3
; Timer2Control = 3;      	// reset the timer to clear the interrupt, enable interrupts and allow counter to run
       move.b    #3,4194358
; PortC = Timer2Count++ ;     // increment an LED count on PortC with each tick of Timer 2
       move.b    _Timer2Count.L,D0
       addq.b    #1,_Timer2Count.L
       move.b    D0,4194308
Timer_ISR_3:
; }
; if(Timer3Status == 1) {         // Did Timer 3 produce the Interrupt?
       move.b    4194362,D0
       cmp.b     #1,D0
       bne.s     Timer_ISR_5
; Timer3Control = 3;      	// reset the timer to clear the interrupt, enable interrupts and allow counter to run
       move.b    #3,4194362
; HEX_A = Timer3Count++ ;     // increment a HEX count on Port HEX_A with each tick of Timer 3
       move.b    _Timer3Count.L,D0
       addq.b    #1,_Timer3Count.L
       move.b    D0,4194320
Timer_ISR_5:
; }
; if(Timer4Status == 1) {         // Did Timer 4 produce the Interrupt?
       move.b    4194366,D0
       cmp.b     #1,D0
       bne.s     Timer_ISR_7
; Timer4Control = 3;      	// reset the timer to clear the interrupt, enable interrupts and allow counter to run
       move.b    #3,4194366
; HEX_B = Timer4Count++ ;     // increment a HEX count on HEX_B with each tick of Timer 4
       move.b    _Timer4Count.L,D0
       addq.b    #1,_Timer4Count.L
       move.b    D0,4194322
Timer_ISR_7:
       rts
; }
; }
; /*****************************************************************************************
; **	Interrupt service routine for ACIA. This device has it's own dedicate IRQ level
; **  Add your code here to poll Status register and clear interrupt
; *****************************************************************************************/
; void ACIA_ISR()
; {}
       xdef      _ACIA_ISR
_ACIA_ISR:
       rts
; /***************************************************************************************
; **	Interrupt service routine for PIAs 1 and 2. These devices share an IRQ level
; **  Add your code here to poll Status register and clear interrupt
; *****************************************************************************************/
; void PIA_ISR()
; {}
       xdef      _PIA_ISR
_PIA_ISR:
       rts
; /***********************************************************************************
; **	Interrupt service routine for Key 2 on DE1 board. Add your own response here
; ************************************************************************************/
; void Key2PressISR()
; {}
       xdef      _Key2PressISR
_Key2PressISR:
       rts
; /***********************************************************************************
; **	Interrupt service routine for Key 1 on DE1 board. Add your own response here
; ************************************************************************************/
; void Key1PressISR()
; {}
       xdef      _Key1PressISR
_Key1PressISR:
       rts
; /************************************************************************************
; **   Delay Subroutine to give the 68000 something useless to do to waste 1 mSec
; ************************************************************************************/
; void Wait1ms(void)
; {
       xdef      _Wait1ms
_Wait1ms:
       move.l    D2,-(A7)
; int  i ;
; for(i = 0; i < 1000; i ++)
       clr.l     D2
Wait1ms_1:
       cmp.l     #1000,D2
       bge.s     Wait1ms_3
       addq.l    #1,D2
       bra       Wait1ms_1
Wait1ms_3:
       move.l    (A7)+,D2
       rts
; ;
; }
; /************************************************************************************
; **  Subroutine to give the 68000 something useless to do to waste 3 mSec
; **************************************************************************************/
; void Wait3ms(void)
; {
       xdef      _Wait3ms
_Wait3ms:
       move.l    D2,-(A7)
; int i ;
; for(i = 0; i < 3; i++)
       clr.l     D2
Wait3ms_1:
       cmp.l     #3,D2
       bge.s     Wait3ms_3
; Wait1ms() ;
       jsr       _Wait1ms
       addq.l    #1,D2
       bra       Wait3ms_1
Wait3ms_3:
       move.l    (A7)+,D2
       rts
; }
; /*********************************************************************************************
; **  Subroutine to initialise the LCD display by writing some commands to the LCD internal registers
; **  Sets it for parallel port and 2 line display mode (if I recall correctly)
; *********************************************************************************************/
; void Init_LCD(void)
; {
       xdef      _Init_LCD
_Init_LCD:
; LCDcommand = 0x0c ;
       move.b    #12,4194336
; Wait3ms() ;
       jsr       _Wait3ms
; LCDcommand = 0x38 ;
       move.b    #56,4194336
; Wait3ms() ;
       jsr       _Wait3ms
       rts
; }
; /*********************************************************************************************
; **  Subroutine to initialise the RS232 Port by writing some commands to the internal registers
; *********************************************************************************************/
; void Init_RS232(void)
; {
       xdef      _Init_RS232
_Init_RS232:
; RS232_Control = 0x15 ; //  %00010101 set up 6850 uses divide by 16 clock, set RTS low, 8 bits no parity, 1 stop bit, transmitter interrupt disabled
       move.b    #21,4194368
; RS232_Baud = 0x1 ;      // program baud rate generator 001 = 115k, 010 = 57.6k, 011 = 38.4k, 100 = 19.2, all others = 9600
       move.b    #1,4194372
       rts
; }
; /*********************************************************************************************************
; **  Subroutine to provide a low level output function to 6850 ACIA
; **  This routine provides the basic functionality to output a single character to the serial Port
; **  to allow the board to communicate with HyperTerminal Program
; **
; **  NOTE you do not call this function directly, instead you call the normal putchar() function
; **  which in turn calls _putch() below). Other functions like puts(), printf() call putchar() so will
; **  call _putch() also
; *********************************************************************************************************/
; int _putch( int c)
; {
       xdef      __putch
__putch:
       link      A6,#0
; while((RS232_Status & (char)(0x02)) != (char)(0x02))    // wait for Tx bit in status register or 6850 serial comms chip to be '1'
_putch_1:
       move.b    4194368,D0
       and.b     #2,D0
       cmp.b     #2,D0
       beq.s     _putch_3
       bra       _putch_1
_putch_3:
; ;
; RS232_TxData = (c & (char)(0x7f));                      // write to the data register to output the character (mask off bit 8 to keep it 7 bit ASCII)
       move.l    8(A6),D0
       and.l     #127,D0
       move.b    D0,4194370
; return c ;                                              // putchar() expects the character to be returned
       move.l    8(A6),D0
       unlk      A6
       rts
; }
; /*********************************************************************************************************
; **  Subroutine to provide a low level input function to 6850 ACIA
; **  This routine provides the basic functionality to input a single character from the serial Port
; **  to allow the board to communicate with HyperTerminal Program Keyboard (your PC)
; **
; **  NOTE you do not call this function directly, instead you call the normal getchar() function
; **  which in turn calls _getch() below). Other functions like gets(), scanf() call getchar() so will
; **  call _getch() also
; *********************************************************************************************************/
; int _getch( void )
; {
       xdef      __getch
__getch:
       link      A6,#-4
; char c ;
; while((RS232_Status & (char)(0x01)) != (char)(0x01))    // wait for Rx bit in 6850 serial comms chip status register to be '1'
_getch_1:
       move.b    4194368,D0
       and.b     #1,D0
       cmp.b     #1,D0
       beq.s     _getch_3
       bra       _getch_1
_getch_3:
; ;
; return (RS232_RxData & (char)(0x7f));                   // read received character, mask off top bit and return as 7 bit ASCII character
       move.b    4194370,D0
       and.l     #255,D0
       and.l     #127,D0
       unlk      A6
       rts
; }
; /******************************************************************************
; **  Subroutine to output a single character to the 2 row LCD display
; **  It is assumed the character is an ASCII code and it will be displayed at the
; **  current cursor position
; *******************************************************************************/
; void LCDOutchar(int c)
; {
       xdef      _LCDOutchar
_LCDOutchar:
       link      A6,#0
; LCDdata = (char)(c);
       move.l    8(A6),D0
       move.b    D0,4194338
; Wait1ms() ;
       jsr       _Wait1ms
       unlk      A6
       rts
; }
; /**********************************************************************************
; *subroutine to output a message at the current cursor position of the LCD display
; ************************************************************************************/
; void LCDOutMessage(char *theMessage)
; {
       xdef      _LCDOutMessage
_LCDOutMessage:
       link      A6,#-4
; char c ;
; while((c = *theMessage++) != 0)     // output characters from the string until NULL
LCDOutMessage_1:
       move.l    8(A6),A0
       addq.l    #1,8(A6)
       move.b    (A0),-1(A6)
       move.b    (A0),D0
       beq.s     LCDOutMessage_3
; LCDOutchar(c) ;
       move.b    -1(A6),D1
       ext.w     D1
       ext.l     D1
       move.l    D1,-(A7)
       jsr       _LCDOutchar
       addq.w    #4,A7
       bra       LCDOutMessage_1
LCDOutMessage_3:
       unlk      A6
       rts
; }
; /******************************************************************************
; *subroutine to clear the line by issuing 24 space characters
; *******************************************************************************/
; void LCDClearln(void)
; {
       xdef      _LCDClearln
_LCDClearln:
       move.l    D2,-(A7)
; int i ;
; for(i = 0; i < 24; i ++)
       clr.l     D2
LCDClearln_1:
       cmp.l     #24,D2
       bge.s     LCDClearln_3
; LCDOutchar(' ') ;       // write a space char to the LCD display
       pea       32
       jsr       _LCDOutchar
       addq.w    #4,A7
       addq.l    #1,D2
       bra       LCDClearln_1
LCDClearln_3:
       move.l    (A7)+,D2
       rts
; }
; /******************************************************************************
; **  Subroutine to move the LCD cursor to the start of line 1 and clear that line
; *******************************************************************************/
; void LCDLine1Message(char *theMessage)
; {
       xdef      _LCDLine1Message
_LCDLine1Message:
       link      A6,#0
; LCDcommand = 0x80 ;
       move.b    #128,4194336
; Wait3ms();
       jsr       _Wait3ms
; LCDClearln() ;
       jsr       _LCDClearln
; LCDcommand = 0x80 ;
       move.b    #128,4194336
; Wait3ms() ;
       jsr       _Wait3ms
; LCDOutMessage(theMessage) ;
       move.l    8(A6),-(A7)
       jsr       _LCDOutMessage
       addq.w    #4,A7
       unlk      A6
       rts
; }
; /******************************************************************************
; **  Subroutine to move the LCD cursor to the start of line 2 and clear that line
; *******************************************************************************/
; void LCDLine2Message(char *theMessage)
; {
       xdef      _LCDLine2Message
_LCDLine2Message:
       link      A6,#0
; LCDcommand = 0xC0 ;
       move.b    #192,4194336
; Wait3ms();
       jsr       _Wait3ms
; LCDClearln() ;
       jsr       _LCDClearln
; LCDcommand = 0xC0 ;
       move.b    #192,4194336
; Wait3ms() ;
       jsr       _Wait3ms
; LCDOutMessage(theMessage) ;
       move.l    8(A6),-(A7)
       jsr       _LCDOutMessage
       addq.w    #4,A7
       unlk      A6
       rts
; }
; /*********************************************************************************************************************************
; **  IMPORTANT FUNCTION
; **  This function install an exception handler so you can capture and deal with any 68000 exception in your program
; **  You pass it the name of a function in your code that will get called in response to the exception (as the 1st parameter)
; **  and in the 2nd parameter, you pass it the exception number that you want to take over (see 68000 exceptions for details)
; **  Calling this function allows you to deal with Interrupts for example
; ***********************************************************************************************************************************/
; void InstallExceptionHandler( void (*function_ptr)(), int level)
; {
       xdef      _InstallExceptionHandler
_InstallExceptionHandler:
       link      A6,#-4
; volatile long int *RamVectorAddress = (volatile long int *)(StartOfExceptionVectorTable) ;   // pointer to the Ram based interrupt vector table created in Cstart in debug monitor
       move.l    #134414336,-4(A6)
; RamVectorAddress[level] = (long int *)(function_ptr);                       // install the address of our function into the exception table
       move.l    -4(A6),A0
       move.l    12(A6),D0
       lsl.l     #2,D0
       move.l    8(A6),0(A0,D0.L)
       unlk      A6
       rts
; }
; /******************************************************************************************************************************
; * Start of user program
; ******************************************************************************************************************************/
; unsigned char * RamWriter;
; unsigned char * start_address;
; unsigned char * end_address;
; unsigned int test_type;
; unsigned int user_data;
; unsigned char * current_address;
; unsigned char *  intermediate_address;
; int address_increment;
; int address_length_flag;
; unsigned int read_write_test;
; void main()
; {
       xdef      _main
_main:
       link      A6,#-172
       movem.l   D2/A2/A3/A4/A5,-(A7)
       lea       _printf.L,A2
       lea       _current_address.L,A3
       lea       _scanflush.L,A4
       lea       _scanf.L,A5
; unsigned int row, i=0, count=0, counter1=1;
       clr.l     -168(A6)
       clr.l     -164(A6)
       move.l    #1,-160(A6)
; int mem_error_flag;
; char c, text[150] ;
; int PassFailFlag = 1 ;
       move.l    #1,-4(A6)
; i = x = y = z = PortA_Count =0;
       clr.l     _PortA_Count.L
       clr.l     _z.L
       clr.l     _y.L
       clr.l     _x.L
       clr.l     -168(A6)
; Timer1Count = Timer2Count = Timer3Count = Timer4Count = 0;
       clr.b     _Timer4Count.L
       clr.b     _Timer3Count.L
       clr.b     _Timer2Count.L
       clr.b     _Timer1Count.L
; InstallExceptionHandler(PIA_ISR, 25) ;          // install interrupt handler for PIAs 1 and 2 on level 1 IRQ
       pea       25
       pea       _PIA_ISR.L
       jsr       _InstallExceptionHandler
       addq.w    #8,A7
; InstallExceptionHandler(ACIA_ISR, 26) ;		    // install interrupt handler for ACIA on level 2 IRQ
       pea       26
       pea       _ACIA_ISR.L
       jsr       _InstallExceptionHandler
       addq.w    #8,A7
; InstallExceptionHandler(Timer_ISR, 27) ;		// install interrupt handler for Timers 1-4 on level 3 IRQ
       pea       27
       pea       _Timer_ISR.L
       jsr       _InstallExceptionHandler
       addq.w    #8,A7
; InstallExceptionHandler(Key2PressISR, 28) ;	    // install interrupt handler for Key Press 2 on DE1 board for level 4 IRQ
       pea       28
       pea       _Key2PressISR.L
       jsr       _InstallExceptionHandler
       addq.w    #8,A7
; InstallExceptionHandler(Key1PressISR, 29) ;	    // install interrupt handler for Key Press 1 on DE1 board for level 5 IRQ
       pea       29
       pea       _Key1PressISR.L
       jsr       _InstallExceptionHandler
       addq.w    #8,A7
; Timer1Data = 0x10;		// program time delay into timers 1-4
       move.b    #16,4194352
; Timer2Data = 0x20;
       move.b    #32,4194356
; Timer3Data = 0x15;
       move.b    #21,4194360
; Timer4Data = 0x25;
       move.b    #37,4194364
; Timer1Control = 3;		// write 3 to control register to Bit0 = 1 (enable interrupt from timers) 1 - 4 and allow them to count Bit 1 = 1
       move.b    #3,4194354
; Timer2Control = 3;
       move.b    #3,4194358
; Timer3Control = 3;
       move.b    #3,4194362
; Timer4Control = 3;
       move.b    #3,4194366
; Init_LCD();             // initialise the LCD display to use a parallel data interface and 2 lines of display
       jsr       _Init_LCD
; Init_RS232() ;          // initialise the RS232 port for use with hyper terminal
       jsr       _Init_RS232
; //-----------------------Common Section-----------------------
; printf("\r\nWhich test do you want to perform? Enter '0' for Read or '1' for Write: "); //prompt user for read or write test
       pea       @memory~1_1.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%u", &read_write_test);
       pea       _read_write_test.L
       pea       @memory~1_2.L
       jsr       (A5)
       addq.w    #8,A7
; while(read_write_test > 1){ //check for valid input
main_1:
       move.l    _read_write_test.L,D0
       cmp.l     #1,D0
       bls.s     main_3
; printf("\r\nInvalid Input!");
       pea       @memory~1_3.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nWhich test do you want to perform? Enter '0' for Read or '1' for Write: ");
       pea       @memory~1_4.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%u", &read_write_test);
       pea       _read_write_test.L
       pea       @memory~1_5.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_1
main_3:
; }
; printf("\r\nSpecify the memory test type. Input '0' for Bytes, '1' for Word, and '2' for Long Word: "); //prompt user for test type
       pea       @memory~1_6.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%u", &test_type);
       pea       _test_type.L
       pea       @memory~1_7.L
       jsr       (A5)
       addq.w    #8,A7
; while (test_type > 2) // check for valid input
main_4:
       move.l    _test_type.L,D0
       cmp.l     #2,D0
       bls.s     main_6
; {
; printf("\r\nInvalid Input!");
       pea       @memory~1_8.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nSpecify the memory test type. Input '0' for Bytes, '1' for Word, and '2' for Long Word: ");
       pea       @memory~1_9.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%u", &test_type);
       pea       _test_type.L
       pea       @memory~1_10.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_4
main_6:
; }
; //***************Get Addresses***************
; scanflush();
       jsr       (A4)
; printf("\r\nProvide a start address for your data: ");
       pea       @memory~1_11.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &start_address);
       pea       _start_address.L
       pea       @memory~1_12.L
       jsr       (A5)
       addq.w    #8,A7
; if (test_type != 0) // check that we're aligned properly for start address for word and long word
       move.l    _test_type.L,D0
       beq       main_11
; {
; while ((unsigned int)start_address % 2) // odd number address
main_9:
       move.l    _start_address.L,-(A7)
       pea       2
       jsr       ULDIV
       move.l    4(A7),D0
       addq.w    #8,A7
       tst.l     D0
       beq.s     main_11
; {
; printf("\r\n Odd address is not allowed for word or long word!");
       pea       @memory~1_13.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; printf("\r\nProvide a start address for your data: ");
       pea       @memory~1_14.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &start_address);
       pea       _start_address.L
       pea       @memory~1_15.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_9
main_11:
; }
; }
; while (start_address < 0x08020000 || start_address > 0x08030000) //protect data leak into other memories
main_12:
       move.l    _start_address.L,D0
       cmp.l     #134348800,D0
       blo.s     main_15
       move.l    _start_address.L,D0
       cmp.l     #134414336,D0
       bls.s     main_14
main_15:
; {
; scanflush();
       jsr       (A4)
; printf("\r\nError: Invalid address! Start address cannot be less than 08020000 or greater than 08030000");
       pea       @memory~1_16.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nProvide a start address for your data: ");
       pea       @memory~1_17.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &start_address);
       pea       _start_address.L
       pea       @memory~1_18.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_12
main_14:
; }
; printf("\r\nYou have entered %x for the start address", start_address);
       move.l    _start_address.L,-(A7)
       pea       @memory~1_19.L
       jsr       (A2)
       addq.w    #8,A7
; scanflush();
       jsr       (A4)
; printf("\r\nProvide an end address for your data: "); //protect data leak into other memories
       pea       @memory~1_20.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &end_address); //protect data leak into other memories
       pea       _end_address.L
       pea       @memory~1_21.L
       jsr       (A5)
       addq.w    #8,A7
; address_length_flag = 1;
       move.l    #1,_address_length_flag.L
; while(address_length_flag == 1){
main_16:
       move.l    _address_length_flag.L,D0
       cmp.l     #1,D0
       bne       main_18
; if(end_address <= start_address || end_address > 0x08030000)
       move.l    _end_address.L,D0
       cmp.l     _start_address.L,D0
       bls.s     main_21
       move.l    _end_address.L,D0
       cmp.l     #134414336,D0
       bls.s     main_19
main_21:
; {
; scanflush();
       jsr       (A4)
; printf("\r\nError: Invalid address! End address should not be less than or equal to start address or greater than 08030000");
       pea       @memory~1_22.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nProvide an end address for your data: "); //protect data leak into other memories
       pea       @memory~1_23.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%x", &end_address); //protect data leak into other memories
       pea       _end_address.L
       pea       @memory~1_24.L
       jsr       (A5)
       addq.w    #8,A7
; continue;
       bra       main_33
main_19:
; }
; if(test_type == 0){
       move.l    _test_type.L,D0
       bne.s     main_22
; address_length_flag = 0;
       clr.l     _address_length_flag.L
; printf("\r\n++++++++++++");
       pea       @memory~1_25.L
       jsr       (A2)
       addq.w    #4,A7
       bra       main_33
main_22:
; }
; else{
; if((unsigned int)end_address % 2) // odd number address
       move.l    _end_address.L,-(A7)
       pea       2
       jsr       ULDIV
       move.l    4(A7),D0
       addq.w    #8,A7
       tst.l     D0
       beq.s     main_24
; {
; printf("\r\n Odd address is not allowed for word or long word!");
       pea       @memory~1_26.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; printf("\r\nProvide an end address for your data: ");
       pea       @memory~1_27.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &end_address);
       pea       _end_address.L
       pea       @memory~1_28.L
       jsr       (A5)
       addq.w    #8,A7
; continue;
       bra       main_33
main_24:
; }
; if(!((end_address - start_address) < 2) & test_type == 1){
       move.l    _end_address.L,D0
       sub.l     _start_address.L,D0
       cmp.l     #2,D0
       blt.s     main_28
       moveq     #1,D0
       bra.s     main_29
main_28:
       clr.l     D0
main_29:
       move.l    _test_type.L,D1
       cmp.l     #1,D1
       bne.s     main_30
       moveq     #1,D1
       bra.s     main_31
main_30:
       clr.l     D1
main_31:
       and.l     D1,D0
       beq.s     main_26
; address_length_flag = 0;
       clr.l     _address_length_flag.L
; printf("\r\n*****************");
       pea       @memory~1_29.L
       jsr       (A2)
       addq.w    #4,A7
       bra       main_33
main_26:
; }
; else if(!((end_address - start_address) < 4) & test_type == 2){
       move.l    _end_address.L,D0
       sub.l     _start_address.L,D0
       cmp.l     #4,D0
       blt.s     main_34
       moveq     #1,D0
       bra.s     main_35
main_34:
       clr.l     D0
main_35:
       move.l    _test_type.L,D1
       cmp.l     #2,D1
       bne.s     main_36
       moveq     #1,D1
       bra.s     main_37
main_36:
       clr.l     D1
main_37:
       and.l     D1,D0
       beq.s     main_32
; address_length_flag = 0;
       clr.l     _address_length_flag.L
; printf("\r\n-----------------------");
       pea       @memory~1_30.L
       jsr       (A2)
       addq.w    #4,A7
       bra.s     main_33
main_32:
; }
; else{
; scanflush();
       jsr       (A4)
; printf("\r\nError: Data cannot be fitted in given address range");
       pea       @memory~1_31.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nProvide an end address for your data: "); //protect data leak into other memories
       pea       @memory~1_32.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%x", &end_address);
       pea       _end_address.L
       pea       @memory~1_33.L
       jsr       (A5)
       addq.w    #8,A7
; continue;
       bra       main_33
main_33:
       bra       main_16
main_18:
; }
; }
; }
; // input long word
; // start_address 0802_0002
; // end_address 0802_000a
; switch (test_type) { // check if byte, word, or long word
       move.l    _test_type.L,D0
       cmp.l     #1,D0
       beq.s     main_41
       bhi.s     main_44
       tst.l     D0
       beq.s     main_40
       bra.s     main_38
main_44:
       cmp.l     #2,D0
       beq.s     main_42
       bra.s     main_38
main_40:
; case 0:
; address_increment = 1; // byte
       move.l    #1,_address_increment.L
; break;
       bra.s     main_39
main_41:
; case 1:
; address_increment = 2; // word
       move.l    #2,_address_increment.L
; user_data = 0x0000 + user_data;
       clr.b     D0
       ext.w     D0
       ext.l     D0
       add.l     _user_data.L,D0
       move.l    D0,_user_data.L
; break;
       bra.s     main_39
main_42:
; case 2:
; address_increment = 4; // long word
       move.l    #4,_address_increment.L
; break;
       bra.s     main_39
main_38:
; default:
; address_increment = 1; // byte
       move.l    #1,_address_increment.L
main_39:
; }
; //-----------------------Read Section-----------------------
; if(!read_write_test){
       tst.l     _read_write_test.L
       bne       main_45
; for(current_address = start_address; current_address < end_address; current_address += address_increment){
       move.l    _start_address.L,(A3)
main_47:
       move.l    (A3),D0
       cmp.l     _end_address.L,D0
       bhs       main_49
; if(test_type == 0){ //read a byte
       move.l    _test_type.L,D0
       bne.s     main_50
; printf("\r\nData at location %x: %x", current_address, *current_address);
       move.l    (A3),A0
       move.b    (A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),-(A7)
       pea       @memory~1_34.L
       jsr       (A2)
       add.w     #12,A7
       bra       main_53
main_50:
; }
; else if(test_type == 1){//read a word
       move.l    _test_type.L,D0
       cmp.l     #1,D0
       bne.s     main_52
; printf("\r\nWord at location %x: %x%x", current_address, *current_address, *(current_address+1));
       move.l    (A3),A0
       move.b    1(A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),A0
       move.b    (A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),-(A7)
       pea       @memory~1_35.L
       jsr       (A2)
       add.w     #16,A7
       bra       main_53
main_52:
; }
; else{
; if( (end_address - current_address) < 3){//read a long word
       move.l    _end_address.L,D0
       sub.l     (A3),D0
       cmp.l     #3,D0
       bge.s     main_54
; break;
       bra       main_49
main_54:
; }
; printf("\r\nLong word at location %x: %x%x%x%x", current_address, *current_address, *(current_address+1), *(current_address+2), *(current_address+3));
       move.l    (A3),A0
       move.b    3(A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),A0
       move.b    2(A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),A0
       move.b    1(A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),A0
       move.b    (A0),D1
       and.l     #255,D1
       move.l    D1,-(A7)
       move.l    (A3),-(A7)
       pea       @memory~1_36.L
       jsr       (A2)
       add.w     #24,A7
main_53:
       move.l    _address_increment.L,D0
       add.l     D0,(A3)
       bra       main_47
main_49:
       bra       main_101
main_45:
; }
; }
; }
; //-----------------------Write Section-----------------------
; else{
; // 1 Byte
; if (test_type == 0)
       move.l    _test_type.L,D0
       bne       main_56
; {
; printf("\r\nEnter the data in format XX: ");
       pea       @memory~1_37.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%x", &user_data);
       pea       _user_data.L
       pea       @memory~1_38.L
       jsr       (A5)
       addq.w    #8,A7
; while (user_data < 0 || user_data > 255) // out of range/bounds
main_58:
       move.l    _user_data.L,D0
       cmp.l     #0,D0
       blo.s     main_61
       move.l    _user_data.L,D0
       cmp.l     #255,D0
       bls.s     main_60
main_61:
; {
; scanflush();
       jsr       (A4)
; printf("\rData larger than byte!\n");
       pea       @memory~1_39.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\rEnter the data in format XX: ");
       pea       @memory~1_40.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &user_data);
       pea       _user_data.L
       pea       @memory~1_41.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_58
main_60:
       bra       main_72
main_56:
; }
; }
; //*************** Get One Word ***************
; else if (test_type == 1)
       move.l    _test_type.L,D0
       cmp.l     #1,D0
       bne       main_62
; {
; scanflush();
       jsr       (A4)
; printf("\r\nEnter the data in format XXXX: ");
       pea       @memory~1_42.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &user_data);
       pea       _user_data.L
       pea       @memory~1_43.L
       jsr       (A5)
       addq.w    #8,A7
; while (user_data < 0 || user_data > 65535) // out of range/bounds
main_64:
       move.l    _user_data.L,D0
       cmp.l     #0,D0
       blo.s     main_67
       move.l    _user_data.L,D0
       cmp.l     #65535,D0
       bls.s     main_66
main_67:
; {
; printf("\r\nData larger than a word!");
       pea       @memory~1_44.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nEnter the data in format XXXX: ");
       pea       @memory~1_45.L
       jsr       (A2)
       addq.w    #4,A7
; scanflush();
       jsr       (A4)
; scanf("%x", &user_data);
       pea       _user_data.L
       pea       @memory~1_46.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_64
main_66:
       bra       main_72
main_62:
; }
; }
; //*************** Get Long Word ***************
; else if (test_type == 2)
       move.l    _test_type.L,D0
       cmp.l     #2,D0
       bne       main_72
; {
; scanflush();
       jsr       (A4)
; printf("\rEnter the data in format XXXXXXXX: ");
       pea       @memory~1_47.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &user_data);
       pea       _user_data.L
       pea       @memory~1_48.L
       jsr       (A5)
       addq.w    #8,A7
; while (user_data < 0 || user_data > 4294967295) // out of range/bounds
main_70:
       move.l    _user_data.L,D0
       cmp.l     #0,D0
       blo.s     main_73
       move.l    _user_data.L,D0
       cmp.l     #-1,D0
       bls.s     main_72
main_73:
; {
; scanflush();
       jsr       (A4)
; printf("\r\nData larger than a long word!");
       pea       @memory~1_49.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nEnter the data in format XXXXXXXX: ");
       pea       @memory~1_50.L
       jsr       (A2)
       addq.w    #4,A7
; scanf("%x", &user_data);
       pea       _user_data.L
       pea       @memory~1_51.L
       jsr       (A5)
       addq.w    #8,A7
       bra       main_70
main_72:
; }
; }
; // word XX_XX --> upper 8 bits ((0xFFFF & input) >> 8), shift address by 1, then lower 8 bits (0x00FF & input)
; // long word --> upper 8, shift by 1, next 8, shift by 1, next 8, shift by 1, write least significant 8, then done.
; mem_error_flag = 0;
       clr.l     D2
; for(current_address = start_address; current_address < end_address; current_address += address_increment){
       move.l    _start_address.L,(A3)
main_74:
       move.l    (A3),D0
       cmp.l     _end_address.L,D0
       bhs       main_76
; if((current_address - start_address)%10000 == 0){
       move.l    (A3),D0
       sub.l     _start_address.L,D0
       move.l    D0,-(A7)
       pea       10000
       jsr       LDIV
       move.l    4(A7),D0
       addq.w    #8,A7
       tst.l     D0
       bne.s     main_77
; printf("\r\nWriting %x at location %x", user_data, current_address); // Reports the progress every 10k (0x2800) locations
       move.l    (A3),-(A7)
       move.l    _user_data.L,-(A7)
       pea       @memory~1_52.L
       jsr       (A2)
       add.w     #12,A7
main_77:
; }
; if(test_type == 0){
       move.l    _test_type.L,D0
       bne       main_79
; *(current_address) = user_data;
       move.l    _user_data.L,D0
       move.l    (A3),A0
       move.b    D0,(A0)
; if (*(current_address) != user_data) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    (A0),D0
       and.l     #255,D0
       cmp.l     _user_data.L,D0
       beq.s     main_81
; {
; printf("\r\nError writing %x to address %x", user_data, current_address);
       move.l    (A3),-(A7)
       move.l    _user_data.L,-(A7)
       pea       @memory~1_53.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra       main_76
main_81:
       bra       main_99
main_79:
; }
; }
; else if(test_type == 1){
       move.l    _test_type.L,D0
       cmp.l     #1,D0
       bne       main_83
; *(current_address) = (0xFF00 & user_data) >> 8;
       move.w    #65280,D0
       and.l     #65535,D0
       and.l     _user_data.L,D0
       lsr.l     #8,D0
       move.l    (A3),A0
       move.b    D0,(A0)
; if (*(current_address) != ((0xFF00 & user_data) >> 8)) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    (A0),D0
       and.l     #255,D0
       move.w    #65280,D1
       and.l     #65535,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       cmp.l     D1,D0
       beq.s     main_85
; {
; printf("\r\nError writing %x to address %x", (0xFF00 & user_data) >> 8, current_address);
       move.l    (A3),-(A7)
       move.w    #65280,D1
       and.l     #65535,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       move.l    D1,-(A7)
       pea       @memory~1_54.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra       main_76
main_85:
; }
; *(current_address + 1) = (0x00FF & user_data);
       move.w    #255,D0
       ext.l     D0
       and.l     _user_data.L,D0
       move.l    (A3),A0
       move.b    D0,1(A0)
; if (*(current_address + 1) != (0x00FF & user_data)) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    1(A0),D0
       and.l     #255,D0
       move.w    #255,D1
       ext.l     D1
       and.l     _user_data.L,D1
       cmp.l     D1,D0
       beq.s     main_87
; {
; printf("\r\nError writing %x to address %x", (0x00FF & user_data), (current_address+1));
       move.l    (A3),D1
       addq.l    #1,D1
       move.l    D1,-(A7)
       move.w    #255,D1
       ext.l     D1
       and.l     _user_data.L,D1
       move.l    D1,-(A7)
       pea       @memory~1_55.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra       main_76
main_87:
       bra       main_99
main_83:
; }
; }
; else if(test_type == 2)
       move.l    _test_type.L,D0
       cmp.l     #2,D0
       bne       main_99
; {
; if( (end_address - current_address) < 3){ //  to ensure we do not go past end address
       move.l    _end_address.L,D0
       sub.l     (A3),D0
       cmp.l     #3,D0
       bge       main_91
; *(current_address) = (0xFF000000 & user_data) >> 24;
       move.l    #-16777216,D0
       and.l     _user_data.L,D0
       lsr.l     #8,D0
       lsr.l     #8,D0
       lsr.l     #8,D0
       move.l    (A3),A0
       move.b    D0,(A0)
; *(current_address + 1) = (0x00FF0000 & user_data) >> 16;
       move.l    #16711680,D0
       and.l     _user_data.L,D0
       lsr.l     #8,D0
       lsr.l     #8,D0
       move.l    (A3),A0
       move.b    D0,1(A0)
; printf("\r\nError: End address limit reached");
       pea       @memory~1_56.L
       jsr       (A2)
       addq.w    #4,A7
; printf("\r\nCannot write %x and %x",(0x0000FF00 & user_data) >> 8, (0x000000FF) & user_data);
       move.w    #255,D1
       ext.l     D1
       and.l     _user_data.L,D1
       move.l    D1,-(A7)
       move.w    #65280,D1
       and.l     #65535,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       move.l    D1,-(A7)
       pea       @memory~1_57.L
       jsr       (A2)
       add.w     #12,A7
; break;
       bra       main_76
main_91:
; }
; *(current_address) = (0xFF000000 & user_data) >> 24;
       move.l    #-16777216,D0
       and.l     _user_data.L,D0
       lsr.l     #8,D0
       lsr.l     #8,D0
       lsr.l     #8,D0
       move.l    (A3),A0
       move.b    D0,(A0)
; if (*(current_address) != ((0xFF000000 & user_data) >> 24)) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    (A0),D0
       and.l     #255,D0
       move.l    #-16777216,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       lsr.l     #8,D1
       lsr.l     #8,D1
       cmp.l     D1,D0
       beq.s     main_93
; {
; printf("\r\nError writing %x to address %x", (0xFF000000 & user_data) >> 24, current_address);
       move.l    (A3),-(A7)
       move.l    #-16777216,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       lsr.l     #8,D1
       lsr.l     #8,D1
       move.l    D1,-(A7)
       pea       @memory~1_58.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra       main_76
main_93:
; }
; *(current_address + 1) = (0x00FF0000 & user_data) >> 16;
       move.l    #16711680,D0
       and.l     _user_data.L,D0
       lsr.l     #8,D0
       lsr.l     #8,D0
       move.l    (A3),A0
       move.b    D0,1(A0)
; if (*(current_address + 1) != ((0x00FF0000 & user_data) >> 16)) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    1(A0),D0
       and.l     #255,D0
       move.l    #16711680,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       lsr.l     #8,D1
       cmp.l     D1,D0
       beq.s     main_95
; {
; printf("\r\nError writing %x to address %x", (0x00FF0000 & user_data) >> 16, (current_address+1));
       move.l    (A3),D1
       addq.l    #1,D1
       move.l    D1,-(A7)
       move.l    #16711680,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       lsr.l     #8,D1
       move.l    D1,-(A7)
       pea       @memory~1_59.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra       main_76
main_95:
; }
; *(current_address + 2) = (0x0000FF00 & user_data) >> 8;
       move.w    #65280,D0
       and.l     #65535,D0
       and.l     _user_data.L,D0
       lsr.l     #8,D0
       move.l    (A3),A0
       move.b    D0,2(A0)
; if (*(current_address + 2) != ((0x0000FF00 & user_data) >> 8)) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    2(A0),D0
       and.l     #255,D0
       move.w    #65280,D1
       and.l     #65535,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       cmp.l     D1,D0
       beq.s     main_97
; {
; printf("\r\nError writing %x to address %x", (0x0000FF00 & user_data) >> 8, (current_address+2));
       move.l    (A3),D1
       addq.l    #2,D1
       move.l    D1,-(A7)
       move.w    #65280,D1
       and.l     #65535,D1
       and.l     _user_data.L,D1
       lsr.l     #8,D1
       move.l    D1,-(A7)
       pea       @memory~1_60.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra       main_76
main_97:
; }
; *(current_address + 3) = (0x000000FF) & user_data;
       move.w    #255,D0
       ext.l     D0
       and.l     _user_data.L,D0
       move.l    (A3),A0
       move.b    D0,3(A0)
; if (*(current_address + 3) != ((0x000000FF) & user_data)) //read the data and confirm if it is written correctly
       move.l    (A3),A0
       move.b    3(A0),D0
       and.l     #255,D0
       move.w    #255,D1
       ext.l     D1
       and.l     _user_data.L,D1
       cmp.l     D1,D0
       beq.s     main_99
; {
; printf("\r\nError writing %x to address %x", (0x000000FF) & user_data, (current_address+3));
       move.l    (A3),D1
       addq.l    #3,D1
       move.l    D1,-(A7)
       move.w    #255,D1
       ext.l     D1
       and.l     _user_data.L,D1
       move.l    D1,-(A7)
       pea       @memory~1_61.L
       jsr       (A2)
       add.w     #12,A7
; mem_error_flag = 1;
       moveq     #1,D2
; break;
       bra.s     main_76
main_99:
       move.l    _address_increment.L,D0
       add.l     D0,(A3)
       bra       main_74
main_76:
; }
; }
; }
; if(mem_error_flag == 0){
       tst.l     D2
       bne.s     main_101
; printf("\r\nWriting finished at %08x", end_address);
       move.l    _end_address.L,-(A7)
       pea       @memory~1_62.L
       jsr       (A2)
       addq.w    #8,A7
main_101:
; }
; }
; // progress function --> counter every 256 so print out every 16
; // 0x08020000 -> 0x08021000
; // Now we need to prompt the user for start and end addresses
; // 0802_0000 start
; // 0802_0020 end
; // type: byte
; // 4 bytes (pattern goes 4 times)
; // Pseudocode:
; // take difference between end and start addresses
; // divide the difference by test type (size)
; // repeat in a for loop
; /*************************************************************************************************
; **  Test of scanf function
; *************************************************************************************************/
; // scanflush() ;                       // flush any text that may have been typed ahead
; // printf("\r\nEnter Integer: ") ;
; // scanf("%d", &i) ;
; // printf("You entered %d", i) ;
; // sprintf(text, "Hello CPEN 412 Student") ;
; // LCDLine1Message(text) ;
; // printf("\r\nHello CPEN 412 Student\r\nYour LEDs should be Flashing") ;
; // printf("\r\nYour LCD should be displaying") ;
; while(1)
main_103:
       bra       main_103
; ;
; // programs should NOT exit as there is nothing to Exit TO !!!!!!
; // There is no OS - just press the reset button to end program and call debug
; }
       section   const
@memory~1_1:
       dc.b      13,10,87,104,105,99,104,32,116,101,115,116,32
       dc.b      100,111,32,121,111,117,32,119,97,110,116,32
       dc.b      116,111,32,112,101,114,102,111,114,109,63,32
       dc.b      69,110,116,101,114,32,39,48,39,32,102,111,114
       dc.b      32,82,101,97,100,32,111,114,32,39,49,39,32,102
       dc.b      111,114,32,87,114,105,116,101,58,32,0
@memory~1_2:
       dc.b      37,117,0
@memory~1_3:
       dc.b      13,10,73,110,118,97,108,105,100,32,73,110,112
       dc.b      117,116,33,0
@memory~1_4:
       dc.b      13,10,87,104,105,99,104,32,116,101,115,116,32
       dc.b      100,111,32,121,111,117,32,119,97,110,116,32
       dc.b      116,111,32,112,101,114,102,111,114,109,63,32
       dc.b      69,110,116,101,114,32,39,48,39,32,102,111,114
       dc.b      32,82,101,97,100,32,111,114,32,39,49,39,32,102
       dc.b      111,114,32,87,114,105,116,101,58,32,0
@memory~1_5:
       dc.b      37,117,0
@memory~1_6:
       dc.b      13,10,83,112,101,99,105,102,121,32,116,104,101
       dc.b      32,109,101,109,111,114,121,32,116,101,115,116
       dc.b      32,116,121,112,101,46,32,73,110,112,117,116
       dc.b      32,39,48,39,32,102,111,114,32,66,121,116,101
       dc.b      115,44,32,39,49,39,32,102,111,114,32,87,111
       dc.b      114,100,44,32,97,110,100,32,39,50,39,32,102
       dc.b      111,114,32,76,111,110,103,32,87,111,114,100
       dc.b      58,32,0
@memory~1_7:
       dc.b      37,117,0
@memory~1_8:
       dc.b      13,10,73,110,118,97,108,105,100,32,73,110,112
       dc.b      117,116,33,0
@memory~1_9:
       dc.b      13,10,83,112,101,99,105,102,121,32,116,104,101
       dc.b      32,109,101,109,111,114,121,32,116,101,115,116
       dc.b      32,116,121,112,101,46,32,73,110,112,117,116
       dc.b      32,39,48,39,32,102,111,114,32,66,121,116,101
       dc.b      115,44,32,39,49,39,32,102,111,114,32,87,111
       dc.b      114,100,44,32,97,110,100,32,39,50,39,32,102
       dc.b      111,114,32,76,111,110,103,32,87,111,114,100
       dc.b      58,32,0
@memory~1_10:
       dc.b      37,117,0
@memory~1_11:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,32,115
       dc.b      116,97,114,116,32,97,100,100,114,101,115,115
       dc.b      32,102,111,114,32,121,111,117,114,32,100,97
       dc.b      116,97,58,32,0
@memory~1_12:
       dc.b      37,120,0
@memory~1_13:
       dc.b      13,10,32,79,100,100,32,97,100,100,114,101,115
       dc.b      115,32,105,115,32,110,111,116,32,97,108,108
       dc.b      111,119,101,100,32,102,111,114,32,119,111,114
       dc.b      100,32,111,114,32,108,111,110,103,32,119,111
       dc.b      114,100,33,0
@memory~1_14:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,32,115
       dc.b      116,97,114,116,32,97,100,100,114,101,115,115
       dc.b      32,102,111,114,32,121,111,117,114,32,100,97
       dc.b      116,97,58,32,0
@memory~1_15:
       dc.b      37,120,0
@memory~1_16:
       dc.b      13,10,69,114,114,111,114,58,32,73,110,118,97
       dc.b      108,105,100,32,97,100,100,114,101,115,115,33
       dc.b      32,83,116,97,114,116,32,97,100,100,114,101,115
       dc.b      115,32,99,97,110,110,111,116,32,98,101,32,108
       dc.b      101,115,115,32,116,104,97,110,32,48,56,48,50
       dc.b      48,48,48,48,32,111,114,32,103,114,101,97,116
       dc.b      101,114,32,116,104,97,110,32,48,56,48,51,48
       dc.b      48,48,48,0
@memory~1_17:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,32,115
       dc.b      116,97,114,116,32,97,100,100,114,101,115,115
       dc.b      32,102,111,114,32,121,111,117,114,32,100,97
       dc.b      116,97,58,32,0
@memory~1_18:
       dc.b      37,120,0
@memory~1_19:
       dc.b      13,10,89,111,117,32,104,97,118,101,32,101,110
       dc.b      116,101,114,101,100,32,37,120,32,102,111,114
       dc.b      32,116,104,101,32,115,116,97,114,116,32,97,100
       dc.b      100,114,101,115,115,0
@memory~1_20:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,110,32
       dc.b      101,110,100,32,97,100,100,114,101,115,115,32
       dc.b      102,111,114,32,121,111,117,114,32,100,97,116
       dc.b      97,58,32,0
@memory~1_21:
       dc.b      37,120,0
@memory~1_22:
       dc.b      13,10,69,114,114,111,114,58,32,73,110,118,97
       dc.b      108,105,100,32,97,100,100,114,101,115,115,33
       dc.b      32,69,110,100,32,97,100,100,114,101,115,115
       dc.b      32,115,104,111,117,108,100,32,110,111,116,32
       dc.b      98,101,32,108,101,115,115,32,116,104,97,110
       dc.b      32,111,114,32,101,113,117,97,108,32,116,111
       dc.b      32,115,116,97,114,116,32,97,100,100,114,101
       dc.b      115,115,32,111,114,32,103,114,101,97,116,101
       dc.b      114,32,116,104,97,110,32,48,56,48,51,48,48,48
       dc.b      48,0
@memory~1_23:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,110,32
       dc.b      101,110,100,32,97,100,100,114,101,115,115,32
       dc.b      102,111,114,32,121,111,117,114,32,100,97,116
       dc.b      97,58,32,0
@memory~1_24:
       dc.b      37,120,0
@memory~1_25:
       dc.b      13,10,43,43,43,43,43,43,43,43,43,43,43,43,0
@memory~1_26:
       dc.b      13,10,32,79,100,100,32,97,100,100,114,101,115
       dc.b      115,32,105,115,32,110,111,116,32,97,108,108
       dc.b      111,119,101,100,32,102,111,114,32,119,111,114
       dc.b      100,32,111,114,32,108,111,110,103,32,119,111
       dc.b      114,100,33,0
@memory~1_27:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,110,32
       dc.b      101,110,100,32,97,100,100,114,101,115,115,32
       dc.b      102,111,114,32,121,111,117,114,32,100,97,116
       dc.b      97,58,32,0
@memory~1_28:
       dc.b      37,120,0
@memory~1_29:
       dc.b      13,10,42,42,42,42,42,42,42,42,42,42,42,42,42
       dc.b      42,42,42,42,0
@memory~1_30:
       dc.b      13,10,45,45,45,45,45,45,45,45,45,45,45,45,45
       dc.b      45,45,45,45,45,45,45,45,45,45,0
@memory~1_31:
       dc.b      13,10,69,114,114,111,114,58,32,68,97,116,97
       dc.b      32,99,97,110,110,111,116,32,98,101,32,102,105
       dc.b      116,116,101,100,32,105,110,32,103,105,118,101
       dc.b      110,32,97,100,100,114,101,115,115,32,114,97
       dc.b      110,103,101,0
@memory~1_32:
       dc.b      13,10,80,114,111,118,105,100,101,32,97,110,32
       dc.b      101,110,100,32,97,100,100,114,101,115,115,32
       dc.b      102,111,114,32,121,111,117,114,32,100,97,116
       dc.b      97,58,32,0
@memory~1_33:
       dc.b      37,120,0
@memory~1_34:
       dc.b      13,10,68,97,116,97,32,97,116,32,108,111,99,97
       dc.b      116,105,111,110,32,37,120,58,32,37,120,0
@memory~1_35:
       dc.b      13,10,87,111,114,100,32,97,116,32,108,111,99
       dc.b      97,116,105,111,110,32,37,120,58,32,37,120,37
       dc.b      120,0
@memory~1_36:
       dc.b      13,10,76,111,110,103,32,119,111,114,100,32,97
       dc.b      116,32,108,111,99,97,116,105,111,110,32,37,120
       dc.b      58,32,37,120,37,120,37,120,37,120,0
@memory~1_37:
       dc.b      13,10,69,110,116,101,114,32,116,104,101,32,100
       dc.b      97,116,97,32,105,110,32,102,111,114,109,97,116
       dc.b      32,88,88,58,32,0
@memory~1_38:
       dc.b      37,120,0
@memory~1_39:
       dc.b      13,68,97,116,97,32,108,97,114,103,101,114,32
       dc.b      116,104,97,110,32,98,121,116,101,33,10,0
@memory~1_40:
       dc.b      13,69,110,116,101,114,32,116,104,101,32,100
       dc.b      97,116,97,32,105,110,32,102,111,114,109,97,116
       dc.b      32,88,88,58,32,0
@memory~1_41:
       dc.b      37,120,0
@memory~1_42:
       dc.b      13,10,69,110,116,101,114,32,116,104,101,32,100
       dc.b      97,116,97,32,105,110,32,102,111,114,109,97,116
       dc.b      32,88,88,88,88,58,32,0
@memory~1_43:
       dc.b      37,120,0
@memory~1_44:
       dc.b      13,10,68,97,116,97,32,108,97,114,103,101,114
       dc.b      32,116,104,97,110,32,97,32,119,111,114,100,33
       dc.b      0
@memory~1_45:
       dc.b      13,10,69,110,116,101,114,32,116,104,101,32,100
       dc.b      97,116,97,32,105,110,32,102,111,114,109,97,116
       dc.b      32,88,88,88,88,58,32,0
@memory~1_46:
       dc.b      37,120,0
@memory~1_47:
       dc.b      13,69,110,116,101,114,32,116,104,101,32,100
       dc.b      97,116,97,32,105,110,32,102,111,114,109,97,116
       dc.b      32,88,88,88,88,88,88,88,88,58,32,0
@memory~1_48:
       dc.b      37,120,0
@memory~1_49:
       dc.b      13,10,68,97,116,97,32,108,97,114,103,101,114
       dc.b      32,116,104,97,110,32,97,32,108,111,110,103,32
       dc.b      119,111,114,100,33,0
@memory~1_50:
       dc.b      13,10,69,110,116,101,114,32,116,104,101,32,100
       dc.b      97,116,97,32,105,110,32,102,111,114,109,97,116
       dc.b      32,88,88,88,88,88,88,88,88,58,32,0
@memory~1_51:
       dc.b      37,120,0
@memory~1_52:
       dc.b      13,10,87,114,105,116,105,110,103,32,37,120,32
       dc.b      97,116,32,108,111,99,97,116,105,111,110,32,37
       dc.b      120,0
@memory~1_53:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_54:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_55:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_56:
       dc.b      13,10,69,114,114,111,114,58,32,69,110,100,32
       dc.b      97,100,100,114,101,115,115,32,108,105,109,105
       dc.b      116,32,114,101,97,99,104,101,100,0
@memory~1_57:
       dc.b      13,10,67,97,110,110,111,116,32,119,114,105,116
       dc.b      101,32,37,120,32,97,110,100,32,37,120,0
@memory~1_58:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_59:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_60:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_61:
       dc.b      13,10,69,114,114,111,114,32,119,114,105,116
       dc.b      105,110,103,32,37,120,32,116,111,32,97,100,100
       dc.b      114,101,115,115,32,37,120,0
@memory~1_62:
       dc.b      13,10,87,114,105,116,105,110,103,32,102,105
       dc.b      110,105,115,104,101,100,32,97,116,32,37,48,56
       dc.b      120,0
       section   bss
       xdef      _i
_i:
       ds.b      4
       xdef      _x
_x:
       ds.b      4
       xdef      _y
_y:
       ds.b      4
       xdef      _z
_z:
       ds.b      4
       xdef      _PortA_Count
_PortA_Count:
       ds.b      4
       xdef      _Timer1Count
_Timer1Count:
       ds.b      1
       xdef      _Timer2Count
_Timer2Count:
       ds.b      1
       xdef      _Timer3Count
_Timer3Count:
       ds.b      1
       xdef      _Timer4Count
_Timer4Count:
       ds.b      1
       xdef      _RamWriter
_RamWriter:
       ds.b      4
       xdef      _start_address
_start_address:
       ds.b      4
       xdef      _end_address
_end_address:
       ds.b      4
       xdef      _test_type
_test_type:
       ds.b      4
       xdef      _user_data
_user_data:
       ds.b      4
       xdef      _current_address
_current_address:
       ds.b      4
       xdef      _intermediate_address
_intermediate_address:
       ds.b      4
       xdef      _address_increment
_address_increment:
       ds.b      4
       xdef      _address_length_flag
_address_length_flag:
       ds.b      4
       xdef      _read_write_test
_read_write_test:
       ds.b      4
       xref      LDIV
       xref      _scanf
       xref      ULDIV
       xref      _scanflush
       xref      _printf
