procedure Operator is

   Var1 : constant Boolean := True;
   Var2 : constant Boolean := True;

   Res1 : constant Boolean := Var1 and then
     Var2;

   Res2 : Boolean := Var1 or else
     Var2;

   Res3 : Boolean := Var1 or
     Var2;

   Res4 : Boolean := Var1 and
     Var2;

   Res5 : Boolean := Var1
     and then
     Var2;

   Res6 : Boolean := Var1
     and then Var2;

   Res6 : Boolean := Var1
     or else Var2;

   --  year and then

begin
   null;
end Operator;
