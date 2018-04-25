with Aqua.Memory;

package Aqua.Architecture is

   Bad_Instruction : exception;

   Register_Count : constant := 32;
   type Register_Index is range 0 .. Register_Count - 1;

   R_PC  : constant Register_Index := 31;
   R_SP  : constant Register_Index := 30;
   R_FP  : constant Register_Index := 29;

   type Registers is array (Register_Index) of Word;

   function Register_Name (R : Register_Index) return String;

   type Addressing_Mode is
     (Small_Immediate,
      Register,
      Indexed_8,
      Postincrement, Predecrement,
      Indexed_32);

   subtype Deferrable_Mode is Addressing_Mode range Register .. Indexed_8;

   type Condition_Code is
     (Always, EQ, LT, LE, MI, LOS, VS, CS);

   type Aqua_Instruction is
     (A_Halt, A_Nop, A_Rts, A_Return,
      A_Clr, A_Dec, A_Inc, A_Neg, A_Not, A_Tst, A_Ldj, A_Stj,
      A_Mov, A_Cmp, A_Add, A_And, A_Div, A_Mul, A_Or, A_Sub, A_Xor,
      A_Ash, A_Lsh,
      A_Add_3, A_And_3, A_Div_3, A_Mul_3, A_Or_3, A_Sub_3, A_Xor_3,
      A_Seq_3, A_Sne_3, A_Sgt_3, A_Slt_3, A_Sge_3, A_Sle_3,
      A_Fadd, A_Fsub, A_Fmul, A_Fdiv,
      A_Fsqrt, A_Fexp, A_Fln,
      A_Br, A_Bne, A_Beq, A_Bge, A_Blt, A_Bgt, A_Ble, A_Bpl, A_Bmi,
      A_Bhi, A_Blos, A_Bvc, A_Bvs, A_Bcc, A_Bcs,
      A_Jmp, A_Jsr, A_Goto, A_Call,
      A_Trap);

   subtype No_Operand_Instruction is
     Aqua_Instruction range A_Halt .. A_Return;
   subtype Single_Operand_Instruction is
     Aqua_Instruction range A_Clr .. A_Stj;
   subtype Double_Operand_Instruction is
     Aqua_Instruction range A_Mov .. A_Lsh;
   subtype Triple_Operand_Instruction is
     Aqua_Instruction range A_Add_3 .. A_Xor_3;
   subtype Triple_Set_Instruction is
     Aqua_Instruction range A_Seq_3 .. A_Sle_3;
   subtype Single_Operand_Float_Instruction is
     Aqua_Instruction range A_Fsqrt .. A_Fln;
   subtype Double_Operand_Float_Instruction is
     Aqua_Instruction range A_Fadd .. A_Fdiv;
   subtype Float_Instruction is Aqua_Instruction range A_Fadd .. A_Fln;
   subtype Branch_Instruction is
     Aqua_Instruction range A_Br .. A_Bcs;
   subtype Jump_Instruction is
     Aqua_Instruction range A_Jmp .. A_Jsr;
   subtype Sized_Instruction is
     Aqua_Instruction range A_Clr .. A_Xor_3;

   type Operand_Type is
      record
         Mode     : Addressing_Mode;
         Deferred : Boolean;
         Register : Register_Index;
         Lit      : Octet;
      end record;

   function Get_Instruction
     (Instruction : Octet)
      return Aqua_Instruction;

   function Calculate_Instruction
     (Instruction : Octet)
      return Aqua_Instruction;

   function Get_Size
     (Instruction : Octet)
      return Data_Size;

   function Get_Mode_Size (Mode : Addressing_Mode) return Data_Size
   is (case Mode is
          when Indexed_32 => Word_32_Size,
          when Indexed_8  => Word_8_Size,
          when others     => raise Constraint_Error with
            "cannot calculate size for mode " & Mode'Img);

   function Get_Operand
     (Op : Octet)
      return Operand_Type;

   function Encode
     (Instruction : Aqua_Instruction;
      Size        : Data_Size := Word_32_Size;
      Immediate   : Octet := 0)
      return Octet;

   function Encode
     (Operand : Operand_Type)
      return Octet;

   type Architecture_Interface is limited interface;

   function Get_R
     (Arch : in out Architecture_Interface;
      Index : Register_Index)
      return Word
      is abstract;

   procedure Set_R
     (Arch : in out Architecture_Interface;
      Index : Register_Index;
      Value : Word)
   is abstract;

   function Next_Value
     (Arch : in out Architecture_Interface;
      Size : Data_Size)
      return Word
      is abstract;

   procedure Set_R
     (Arch : in out Architecture_Interface'Class;
      R    : Register_Index;
      Size : Data_Size;
      From : Word);

   function Get_Address
     (Arch    : in out Architecture_Interface'Class;
      Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      Memory  : in out Aqua.Memory.Memory_Type'Class)
      return Address;

   procedure Read
     (Arch    : in out Architecture_Interface'Class;
      Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      Memory  : in out Aqua.Memory.Memory_Type'Class;
      Value   :    out Word);

   procedure Write
     (Arch    : in out Architecture_Interface'Class;
      Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      Memory  : in out Aqua.Memory.Memory_Type'Class;
      Value   : Word);

end Aqua.Architecture;
