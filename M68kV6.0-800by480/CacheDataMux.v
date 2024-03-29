module CacheDataMux(
    input ValidHit0_H,
    input ValidHit1_H,
    input ValidHit2_H,
    input ValidHit3_H,
	 input ValidHit4_H,
	 input ValidHit5_H,
	 input ValidHit6_H,
	 input ValidHit7_H,
    input [15:0] Block0_In,
    input [15:0] Block1_In,
    input [15:0] Block2_In,
    input [15:0] Block3_In,
	 input [15:0] Block4_In,
	 input [15:0] Block5_In,
	 input [15:0] Block6_In,
	 input [15:0] Block7_In,

    output reg [15:0] DataOut
);

always @(*) begin
    case({ValidHit7_H,ValidHit6_H,ValidHit5_H,ValidHit4_H,ValidHit3_H,ValidHit2_H,ValidHit1_H,ValidHit0_H})
    8'b0000_0001: DataOut <= Block0_In;
    8'b0000_0010: DataOut <= Block1_In;
    8'b0000_0100: DataOut <= Block2_In;
    8'b0000_1000: DataOut <= Block3_In;
	 8'b0001_0000: DataOut <= Block4_In;
	 8'b0010_0000: DataOut <= Block5_In;
	 8'b0100_0000: DataOut <= Block6_In;
	 8'b1000_0000: DataOut <= Block7_In;
    default: DataOut <= 16'bx;
    endcase
end

endmodule