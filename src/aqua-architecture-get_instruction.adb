separate (Aqua.Architecture)
function Get_Instruction
  (Instruction : Octet)
  return Aqua_Instruction
is
begin
   case Instruction is
      when 0 =>
         return A_Halt;
      when 1 =>
         return A_Nop;
      when 2 =>
         return A_Rts;
      when 3 =>
         return A_Clr;
      when 4 =>
         return A_Dec;
      when 5 =>
         return A_Inc;
      when 6 =>
         return A_Neg;
      when 7 =>
         return A_Not;
      when 8 =>
         return A_Tst;
      when 9 =>
         return A_Mov;
      when 10 =>
         return A_Cmp;
      when 11 =>
         return A_Add;
      when 12 =>
         return A_And;
      when 13 =>
         return A_Div;
      when 14 =>
         return A_Mul;
      when 15 =>
         return A_Or;
      when 16 =>
         return A_Trap;
      when 17 =>
         return A_Trap;
      when 18 =>
         return A_Trap;
      when 19 =>
         return A_Trap;
      when 20 =>
         return A_Trap;
      when 21 =>
         return A_Trap;
      when 22 =>
         return A_Trap;
      when 23 =>
         return A_Trap;
      when 24 =>
         return A_Trap;
      when 25 =>
         return A_Trap;
      when 26 =>
         return A_Trap;
      when 27 =>
         return A_Trap;
      when 28 =>
         return A_Trap;
      when 29 =>
         return A_Trap;
      when 30 =>
         return A_Trap;
      when 31 =>
         return A_Trap;
      when 32 =>
         return A_Get_Property;
      when 33 =>
         return A_Get_Property;
      when 34 =>
         return A_Get_Property;
      when 35 =>
         return A_Get_Property;
      when 36 =>
         return A_Get_Property;
      when 37 =>
         return A_Get_Property;
      when 38 =>
         return A_Get_Property;
      when 39 =>
         return A_Get_Property;
      when 40 =>
         return A_Get_Property;
      when 41 =>
         return A_Get_Property;
      when 42 =>
         return A_Get_Property;
      when 43 =>
         return A_Get_Property;
      when 44 =>
         return A_Get_Property;
      when 45 =>
         return A_Get_Property;
      when 46 =>
         return A_Get_Property;
      when 47 =>
         return A_Get_Property;
      when 48 =>
         return A_Set_Property;
      when 49 =>
         return A_Iterator_Start;
      when 50 =>
         raise Bad_Instruction with "32";
      when 51 =>
         raise Bad_Instruction with "33";
      when 52 =>
         return A_Jmp;
      when 53 =>
         return A_Jsr;
      when 54 =>
         raise Bad_Instruction with "36";
      when 55 =>
         raise Bad_Instruction with "37";
      when 56 =>
         raise Bad_Instruction with "38";
      when 57 =>
         raise Bad_Instruction with "39";
      when 58 =>
         raise Bad_Instruction with "3A";
      when 59 =>
         raise Bad_Instruction with "3B";
      when 60 =>
         raise Bad_Instruction with "3C";
      when 61 =>
         raise Bad_Instruction with "3D";
      when 62 =>
         raise Bad_Instruction with "3E";
      when 63 =>
         raise Bad_Instruction with "3F";
      when 64 =>
         raise Bad_Instruction with "40";
      when 65 =>
         return A_Br;
      when 66 =>
         return A_Bne;
      when 67 =>
         return A_Beq;
      when 68 =>
         return A_Bge;
      when 69 =>
         return A_Blt;
      when 70 =>
         return A_Bgt;
      when 71 =>
         return A_Ble;
      when 72 =>
         return A_Bpl;
      when 73 =>
         return A_Bmi;
      when 74 =>
         return A_Bhi;
      when 75 =>
         return A_Blos;
      when 76 =>
         return A_Bvc;
      when 77 =>
         return A_Bvs;
      when 78 =>
         return A_Bcc;
      when 79 =>
         return A_Bcs;
      when 80 =>
         return A_Clr;
      when 81 =>
         return A_Dec;
      when 82 =>
         return A_Inc;
      when 83 =>
         return A_Neg;
      when 84 =>
         return A_Not;
      when 85 =>
         return A_Tst;
      when 86 =>
         raise Bad_Instruction with "56";
      when 87 =>
         raise Bad_Instruction with "57";
      when 88 =>
         raise Bad_Instruction with "58";
      when 89 =>
         raise Bad_Instruction with "59";
      when 90 =>
         raise Bad_Instruction with "5A";
      when 91 =>
         raise Bad_Instruction with "5B";
      when 92 =>
         raise Bad_Instruction with "5C";
      when 93 =>
         raise Bad_Instruction with "5D";
      when 94 =>
         raise Bad_Instruction with "5E";
      when 95 =>
         raise Bad_Instruction with "5F";
      when 96 =>
         return A_Mov;
      when 97 =>
         return A_Cmp;
      when 98 =>
         return A_Add;
      when 99 =>
         return A_And;
      when 100 =>
         return A_Div;
      when 101 =>
         return A_Mul;
      when 102 =>
         return A_Or;
      when 103 =>
         return A_Sub;
      when 104 =>
         return A_Xor;
      when 105 =>
         return A_Ash;
      when 106 =>
         return A_Lsh;
      when 107 =>
         raise Bad_Instruction with "6B";
      when 108 =>
         raise Bad_Instruction with "6C";
      when 109 =>
         raise Bad_Instruction with "6D";
      when 110 =>
         raise Bad_Instruction with "6E";
      when 111 =>
         raise Bad_Instruction with "6F";
      when 112 =>
         return A_Add_3;
      when 113 =>
         return A_And_3;
      when 114 =>
         return A_Div_3;
      when 115 =>
         return A_Mul_3;
      when 116 =>
         return A_Or_3;
      when 117 =>
         return A_Sub_3;
      when 118 =>
         return A_Xor_3;
      when 119 =>
         raise Bad_Instruction with "77";
      when 120 =>
         raise Bad_Instruction with "78";
      when 121 =>
         raise Bad_Instruction with "79";
      when 122 =>
         raise Bad_Instruction with "7A";
      when 123 =>
         raise Bad_Instruction with "7B";
      when 124 =>
         raise Bad_Instruction with "7C";
      when 125 =>
         raise Bad_Instruction with "7D";
      when 126 =>
         raise Bad_Instruction with "7E";
      when 127 =>
         raise Bad_Instruction with "7F";
      when 128 =>
         return A_Iterator_Next;
      when 129 =>
         return A_Iterator_Next;
      when 130 =>
         return A_Iterator_Next;
      when 131 =>
         return A_Iterator_Next;
      when 132 =>
         return A_Iterator_Next;
      when 133 =>
         return A_Iterator_Next;
      when 134 =>
         return A_Iterator_Next;
      when 135 =>
         return A_Iterator_Next;
      when 136 =>
         return A_Iterator_Next;
      when 137 =>
         return A_Iterator_Next;
      when 138 =>
         return A_Iterator_Next;
      when 139 =>
         return A_Iterator_Next;
      when 140 =>
         return A_Iterator_Next;
      when 141 =>
         return A_Iterator_Next;
      when 142 =>
         return A_Iterator_Next;
      when 143 =>
         return A_Iterator_Next;
      when 144 =>
         return A_Clr;
      when 145 =>
         return A_Dec;
      when 146 =>
         return A_Inc;
      when 147 =>
         return A_Neg;
      when 148 =>
         return A_Not;
      when 149 =>
         return A_Tst;
      when 150 =>
         raise Bad_Instruction with "96";
      when 151 =>
         raise Bad_Instruction with "97";
      when 152 =>
         raise Bad_Instruction with "98";
      when 153 =>
         raise Bad_Instruction with "99";
      when 154 =>
         raise Bad_Instruction with "9A";
      when 155 =>
         raise Bad_Instruction with "9B";
      when 156 =>
         raise Bad_Instruction with "9C";
      when 157 =>
         raise Bad_Instruction with "9D";
      when 158 =>
         raise Bad_Instruction with "9E";
      when 159 =>
         raise Bad_Instruction with "9F";
      when 160 =>
         return A_Mov;
      when 161 =>
         return A_Cmp;
      when 162 =>
         return A_Add;
      when 163 =>
         return A_And;
      when 164 =>
         return A_Div;
      when 165 =>
         return A_Mul;
      when 166 =>
         return A_Or;
      when 167 =>
         return A_Sub;
      when 168 =>
         return A_Xor;
      when 169 =>
         return A_Ash;
      when 170 =>
         return A_Lsh;
      when 171 =>
         raise Bad_Instruction with "AB";
      when 172 =>
         raise Bad_Instruction with "AC";
      when 173 =>
         raise Bad_Instruction with "AD";
      when 174 =>
         raise Bad_Instruction with "AE";
      when 175 =>
         raise Bad_Instruction with "AF";
      when 176 =>
         return A_Add_3;
      when 177 =>
         return A_And_3;
      when 178 =>
         return A_Div_3;
      when 179 =>
         return A_Mul_3;
      when 180 =>
         return A_Or_3;
      when 181 =>
         return A_Sub_3;
      when 182 =>
         return A_Xor_3;
      when 183 =>
         raise Bad_Instruction with "B7";
      when 184 =>
         raise Bad_Instruction with "B8";
      when 185 =>
         raise Bad_Instruction with "B9";
      when 186 =>
         raise Bad_Instruction with "BA";
      when 187 =>
         raise Bad_Instruction with "BB";
      when 188 =>
         raise Bad_Instruction with "BC";
      when 189 =>
         raise Bad_Instruction with "BD";
      when 190 =>
         raise Bad_Instruction with "BE";
      when 191 =>
         raise Bad_Instruction with "BF";
      when 192 =>
         return A_Fadd;
      when 193 =>
         return A_Fadd;
      when 194 =>
         return A_Fsub;
      when 195 =>
         return A_Fsub;
      when 196 =>
         return A_Fmul;
      when 197 =>
         return A_Fmul;
      when 198 =>
         return A_Fdiv;
      when 199 =>
         return A_Fdiv;
      when 200 =>
         return A_Fsqrt;
      when 201 =>
         return A_Fsqrt;
      when 202 =>
         return A_Fexp;
      when 203 =>
         return A_Fexp;
      when 204 =>
         return A_Fln;
      when 205 =>
         return A_Fln;
      when 206 =>
         raise Bad_Instruction with "CE";
      when 207 =>
         raise Bad_Instruction with "CF";
      when 208 =>
         return A_Clr;
      when 209 =>
         return A_Dec;
      when 210 =>
         return A_Inc;
      when 211 =>
         return A_Neg;
      when 212 =>
         return A_Not;
      when 213 =>
         return A_Tst;
      when 214 =>
         raise Bad_Instruction with "D6";
      when 215 =>
         raise Bad_Instruction with "D7";
      when 216 =>
         raise Bad_Instruction with "D8";
      when 217 =>
         raise Bad_Instruction with "D9";
      when 218 =>
         raise Bad_Instruction with "DA";
      when 219 =>
         raise Bad_Instruction with "DB";
      when 220 =>
         raise Bad_Instruction with "DC";
      when 221 =>
         raise Bad_Instruction with "DD";
      when 222 =>
         raise Bad_Instruction with "DE";
      when 223 =>
         raise Bad_Instruction with "DF";
      when 224 =>
         return A_Mov;
      when 225 =>
         return A_Cmp;
      when 226 =>
         return A_Add;
      when 227 =>
         return A_And;
      when 228 =>
         return A_Div;
      when 229 =>
         return A_Mul;
      when 230 =>
         return A_Or;
      when 231 =>
         return A_Sub;
      when 232 =>
         return A_Xor;
      when 233 =>
         return A_Ash;
      when 234 =>
         return A_Lsh;
      when 235 =>
         raise Bad_Instruction with "EB";
      when 236 =>
         raise Bad_Instruction with "EC";
      when 237 =>
         raise Bad_Instruction with "ED";
      when 238 =>
         raise Bad_Instruction with "EE";
      when 239 =>
         raise Bad_Instruction with "EF";
      when 240 =>
         return A_Add_3;
      when 241 =>
         return A_And_3;
      when 242 =>
         return A_Div_3;
      when 243 =>
         return A_Mul_3;
      when 244 =>
         return A_Or_3;
      when 245 =>
         return A_Sub_3;
      when 246 =>
         return A_Xor_3;
      when 247 =>
         raise Bad_Instruction with "F7";
      when 248 =>
         raise Bad_Instruction with "F8";
      when 249 =>
         raise Bad_Instruction with "F9";
      when 250 =>
         raise Bad_Instruction with "FA";
      when 251 =>
         raise Bad_Instruction with "FB";
      when 252 =>
         raise Bad_Instruction with "FC";
      when 253 =>
         raise Bad_Instruction with "FD";
      when 254 =>
         raise Bad_Instruction with "FE";
      when 255 =>
         raise Bad_Instruction with "FF";
   end case;
end Get_Instruction;
