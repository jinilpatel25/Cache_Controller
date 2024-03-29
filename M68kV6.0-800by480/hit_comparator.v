module hit_comparator(
    input [20:0] AddressBus,
    input [20:0] TagData,
    output reg Hit_H
);

always @(AddressBus, TagData) begin
    if(AddressBus == TagData) Hit_H <= 1'b1;
    else Hit_H <= 1'b0;
end

endmodule