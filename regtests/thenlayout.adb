procedure ThenLayout is
   Toto : Natural := 9;
begin
   if Toto = 8 then
      null;
   end if;

   if Toto = 8
     or else Toto = 12
   then
      null;
   end if;

   if Toto = 2
     and then Toto = 12 then
      null;
   end if;

   if Gh = 12 then
      null;
   elsif Gh = 9 then
      null;
   end if;

   if Gh = 12
     and then Gh = "then"
   then
      null;
   end if;

   if Gh = 12
     and then Gh = "then" then
      null;
   end if;
end ThenLayout;
