with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure main is
   -- Variables globales
   SalaCant    : Integer := 0;
   MangasCant  : Integer := 0;
   Camion1     : Integer := 0;
   Camion2     : Integer := 0;
   PasilloLibre: Boolean := True;

   -- Generadores para tiempos aleatorios
   subtype Tiempo_Ordeñe is Integer range 1 .. 3;
   subtype Tiempo_Vacuna is Integer range 1 .. 2;

   package Rand_Ordeñe is new Ada.Numerics.Discrete_Random(Tiempo_Ordeñe);
   package Rand_Vacuna is new Ada.Numerics.Discrete_Random(Tiempo_Vacuna);

   Gen_Ordeñe : Rand_Ordeñe.Generator;
   Gen_Vacuna : Rand_Vacuna.Generator;

   -- Procedure para ordeñar
   procedure Ordeñar(ID : Integer) is
      Segundos : Tiempo_Ordeñe;
   begin
      -- Esperar lugar en sala
      while SalaCant >= 15 loop
         delay 0.1;
      end loop;

      SalaCant := SalaCant + 1;
      Put_Line("La vaca" & Integer'Image(ID) & " está entrando al área de ordeñe");

      Segundos := Rand_Ordeñe.Random(Gen_Ordeñe);
      delay Duration(Segundos);

      Put_Line("La vaca" & Integer'Image(ID) & " está saliendo del área de ordeñe");
      SalaCant := SalaCant - 1;
   end Ordeñar;

   -- Procedure para vacunar
   procedure Vacunar(ID : Integer) is
      Segundos : Tiempo_Vacuna;
   begin
      -- Esperar pasillo libre
      while not PasilloLibre loop
         delay 0.1;
      end loop;
      PasilloLibre := False;
      Put_Line("La vaca" & Integer'Image(ID) & " está entrando al área de vacunación");

      -- Esperar manga libre
      while MangasCant >= 5 loop
         delay 0.1;
      end loop;
      MangasCant := MangasCant + 1;
      PasilloLibre := True;

      Segundos := Rand_Vacuna.Random(Gen_Vacuna);
      delay Duration(Segundos);

      -- Salida por pasillo
      while not PasilloLibre loop
         delay 0.1;
      end loop;
      PasilloLibre := False;
      Put_Line("La vaca" & Integer'Image(ID) & " está saliendo del área de vacunación");
      MangasCant := MangasCant - 1;
      PasilloLibre := True;
   end Vacunar;

   -- Procedure para subir al camión
   procedure SubirCamion(ID : Integer) is
   begin
      if Camion1 < 50 then
         Camion1 := Camion1 + 1;
         Put_Line("La vaca" & Integer'Image(ID) & " está entrando al Camión 1");
      else
         Camion2 := Camion2 + 1;
         Put_Line("La vaca" & Integer'Image(ID) & " está entrando al Camión 2");
      end if;
   end SubirCamion;

   -- Task para cada vaca
   task type Vaca(ID : Integer);
   task body Vaca is
   begin
      Ordeñar(ID);
      Vacunar(ID);
      SubirCamion(ID);
   end Vaca;

begin
   Rand_Ordeñe.Reset(Gen_Ordeñe);
   Rand_Vacuna.Reset(Gen_Vacuna);
   Put_Line("Inicio del proceso del tambo...");

   -- Crear las 100 vacas
   for I in 1 .. 100 loop
      declare
         V : Vaca(I);
      begin
         null;
      end;
   end loop;

   -- Esperar hasta que los camiones estén llenos
   while Camion2 < 50 loop
      delay 0.5;
   end loop;

   Put_Line("¡Todos los camiones están llenos! Fin del proceso.");
end main;

--! Verificar que no corra de manera secuencial, no se como cambiarlo sin hacer todo el codigo de nuevo :(