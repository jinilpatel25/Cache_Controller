; C:\USERS\ADMIN\DESKTOP\ASSIGNMENT4\CACHE_CONTROLLER\FIRMWARE\SPEEDTEST.C - Compiled by CC68K  Version 5.00 (c) 1991-2005  Peter J. Fondse
; #include <stdio.h>
; /*********************************************************************************************
; **	RS232 port addresses defined as pointers
; *********************************************************************************************/
; #define RS232_Control     (*(volatile unsigned char *)(0x00400040))
; #define RS232_Status      (*(volatile unsigned char *)(0x00400040))
; #define RS232_TxData      (*(volatile unsigned char *)(0x00400042))
; #define RS232_RxData      (*(volatile unsigned char *)(0x00400042))
; #define RS232_Baud        (*(volatile unsigned char *)(0x00400044))
; /*********************************************************************************************
; **  Subroutine to initialise the RS232 Port by writing some commands to the internal registers
; **	Call this function at the start of the program before you attempt to read or write to hyperterminal
; *********************************************************************************************/
; void Init_RS232(void)
; {
       section   code
       xdef      _Init_RS232
_Init_RS232:
; RS232_Control = 0x15 ; //  %00010101 set up serial port to use divide by 16 clock, set RTS low, 8 bits no parity, 1 stop bit, transmitter interrupt disabled
       move.b    #21,4194368
; RS232_Baud = 0x1 ;      // program serial port speed: 000 = 230 kbaud, 001 = 115k, 010 = 57.6k, 011 = 38.4k, 100 = 19.2, all others = 9600
       move.b    #1,4194372
       rts
; }
; /*********************************************************************************************************
; **  Subroutine to provide a low level output function to 6850 ACIA
; **  This routine provides the basic functionality to output a single character to the serial Port
; **  to allow the board to communicate with HyperTerminal Program
; **
; **  NOTE you do NOT call this function directly, instead  call the normal putchar() function
; **  which in turn calls _putch() below.
; **
; **	Other functions like puts(), printf() call putchar() so will
; **  call _putch() below so it's fully integrates into the C standard library routines
; *********************************************************************************************************/
; int _putch(int c)
; {
       xdef      __putch
__putch:
       link      A6,#0
; // write the character to the RS232 port first - comment out if not wanted
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
; **  which in turn calls _getch() below).
; **	Other functions like gets(), scanf() call getchar() so will
; **  call _getch() below so it's fully integrates into the C standard library routines
; *********************************************************************************************************/
; int _getch( void )
; {
       xdef      __getch
__getch:
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
       rts
; }
; int a[100][100], b[100][100], c[100][100];
; int i, j, k, sum;
; int main(void)
; {
       xdef      _main
_main:
       movem.l   A2/A3/A4/A5,-(A7)
       lea       _i.L,A2
       lea       _k.L,A3
       lea       _j.L,A4
       lea       _sum.L,A5
; Init_RS232();
       jsr       _Init_RS232
; printf("\n\nStart.....");
       pea       @speedt~1_1.L
       jsr       _printf
       addq.w    #4,A7
; for(i=0; i <50; i ++)  {
       clr.l     (A2)
main_1:
       move.l    (A2),D0
       cmp.l     #50,D0
       bge       main_3
; printf("%d ", i);
       move.l    (A2),-(A7)
       pea       @speedt~1_2.L
       jsr       _printf
       addq.w    #8,A7
; for(j=0; j < 50; j++)  {
       clr.l     (A4)
main_4:
       move.l    (A4),D0
       cmp.l     #50,D0
       bge       main_6
; sum = 0 ;
       clr.l     (A5)
; for(k=0; k <50; k++)   {
       clr.l     (A3)
main_7:
       move.l    (A3),D0
       cmp.l     #50,D0
       bge       main_9
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
; sum = sum + b[i][k] * b[k][j] + a[i][k] * c[i][j];
       move.l    (A5),D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _b.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A3),D0
       muls      #400,D0
       lea       _b.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    (A2),D1
       muls      #400,D1
       lea       _a.L,A0
       add.l     D1,A0
       move.l    (A3),D1
       lsl.l     #2,D1
       move.l    D0,-(A7)
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A1
       add.l     D0,A1
       move.l    (A7)+,D0
       move.l    D0,-(A7)
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    0(A0,D1.L),-(A7)
       move.l    0(A1,D0.L),-(A7)
       jsr       LMUL
       move.l    (A7),D1
       addq.w    #8,A7
       move.l    (A7)+,D0
       add.l     D1,D0
       move.l    D0,(A5)
       addq.l    #1,(A3)
       bra       main_7
main_9:
; }
; c[i][j] = sum ;
       move.l    (A2),D0
       muls      #400,D0
       lea       _c.L,A0
       add.l     D0,A0
       move.l    (A4),D0
       lsl.l     #2,D0
       move.l    (A5),0(A0,D0.L)
       addq.l    #1,(A4)
       bra       main_4
main_6:
       addq.l    #1,(A2)
       bra       main_1
main_3:
; }
; }
; printf("\n\nDone.....");
       pea       @speedt~1_3.L
       jsr       _printf
       addq.w    #4,A7
; return 0 ;
       clr.l     D0
       movem.l   (A7)+,A2/A3/A4/A5
       rts
; }
       section   const
@speedt~1_1:
       dc.b      10,10,83,116,97,114,116,46,46,46,46,46,0
@speedt~1_2:
       dc.b      37,100,32,0
@speedt~1_3:
       dc.b      10,10,68,111,110,101,46,46,46,46,46,0
       section   bss
       xdef      _a
_a:
       ds.b      40000
       xdef      _b
_b:
       ds.b      40000
       xdef      _c
_c:
       ds.b      40000
       xdef      _i
_i:
       ds.b      4
       xdef      _j
_j:
       ds.b      4
       xdef      _k
_k:
       ds.b      4
       xdef      _sum
_sum:
       ds.b      4
       xref      LMUL
       xref      _printf
