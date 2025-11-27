
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   subtype Tiempo_Ordenar is Integer range 1 .. 3;
   subtype Tiempo_Vacuna  is Integer range 1 .. 2;
   subtype Accion_Range   is Integer range 1 .. 3;

   package Rand_Ordenar is new Ada.Numerics.Discrete_Random(Tiempo_Ordenar);
   package Rand_Vacuna  is new Ada.Numerics.Discrete_Random(Tiempo_Vacuna);
   package Rand_Accion  is new Ada.Numerics.Discrete_Random(Accion_Range);

   Gen_Ordenar : Rand_Ordenar.Generator;
   Gen_Vacuna  : Rand_Vacuna.Generator;
   Gen_Accion  : Rand_Accion.Generator;

   task Sala is
      entry Operar(ID : Integer; Accion : String; Exito : out Boolean);
   end Sala;

   task body Sala is
      Cant : Integer := 0;
   begin
      loop
         accept Operar(ID : Integer; Accion : String; Exito : out Boolean) do
            if Accion = "Entrar" then
               if Cant < 15 then
                  Cant := Cant + 1;
                  Exito := True;
                  Put("Vaca "); Put(ID); Put_Line(" entra a la sala");
               else
                  Exito := False;
               end if;
            else
               Cant := Cant - 1;
               Exito := True;
               Put("Vaca "); Put(ID); Put_Line(" sale de la sala");
            end if;
         end Operar;
      end loop;
   end Sala;

   task Mangas is
      entry Operar(ID : Integer; Accion : String; Exito : out Boolean);
   end Mangas;

   task body Mangas is
      Cant : Integer := 0;
   begin
      loop
         accept Operar(ID : Integer; Accion : String; Exito : out Boolean) do
            if Accion = "Entrar" then
               if Cant < 5 then
                  Cant := Cant + 1;
                  Exito := True;
                  Put("Vaca "); Put(ID); Put_Line(" entra a vacunación");
               else
                  Exito := False;
               end if;
            else
               Cant := Cant - 1;
               Exito := True;
               Put("Vaca "); Put(ID); Put_Line(" sale de vacunación");
            end if;
         end Operar;
      end loop;
   end Mangas;

   task Pasillo is
      entry Usar(ID : Integer);
   end Pasillo;

   task body Pasillo is
   begin
      loop
         accept Usar(ID : Integer) do
            Put("Vaca "); Put(ID); Put_Line(" usa el pasillo");
         end Usar;
      end loop;
   end Pasillo;

   task Camiones is
      entry Operar(ID : Integer; Accion : String; Exito : out Boolean);
   end Camiones;

   task body Camiones is
      C1 : Integer := 0;
      C2 : Integer := 0;
   begin
      loop
         accept Operar(ID : Integer; Accion : String; Exito : out Boolean) do
            if Accion = "Subir" then
               if C1 < 50 then
                  C1 := C1 + 1;
                  Exito := True;
                  Put("Vaca "); Put(ID); Put_Line(" sube al camión 1");
               elsif C2 < 50 then
                  C2 := C2 + 1;
                  Exito := True;
                  Put("Vaca "); Put(ID); Put_Line(" sube al camión 2");
               else
                  Exito := False;
               end if;
            else
               Exito := (C1 = 50 and C2 = 50);
            end if;
         end Operar;
      end loop;
   end Camiones;


   task type Vaca(ID : Integer);

   task body Vaca is
      Ordenada : Boolean := False;
      Vacunada : Boolean := False;
   begin
      loop
         exit when Ordenada and Vacunada;

         declare
            Acc : Integer := Rand_Accion.Random(Gen_Accion);
         begin
            if Acc = 1 and not Ordenada then
               
               loop
                  declare
                     Exito : Boolean;
                  begin
                     Sala.Operar(ID, "Entrar", Exito);
                     exit when Exito = True;
                  end;
                  delay 0.02;
               end loop;

               delay Duration(Rand_Ordenar.Random(Gen_Ordenar));

               declare
                  Exito : Boolean;
               begin
                  Sala.Operar(ID, "Salir", Exito);
               end;
               Ordenada := True;

            elsif Acc = 2 and not Vacunada then
               Pasillo.Usar(ID);

               loop
                  declare
                     Exito : Boolean;
                  begin
                     Mangas.Operar(ID, "Entrar", Exito);
                     exit when Exito = True;
                  end;
                  delay 0.02;
               end loop;

               delay Duration(Rand_Vacuna.Random(Gen_Vacuna));

               declare
                  Exito : Boolean;
               begin
                  Mangas.Operar(ID, "Salir", Exito);
               end;
               Pasillo.Usar(ID);

               Vacunada := True;
            end if;
         end;
      end loop;

   
      loop
         declare
            Exito : Boolean;
         begin
            Camiones.Operar(ID, "Subir", Exito);
            exit when Exito = True;
         end;
         delay 0.02;
      end loop;
   end Vaca;


   type VacaRef is access Vaca;
   Vacas : array (1 .. 100) of VacaRef;

   Lleno : Boolean;

begin
   Rand_Ordenar.Reset(Gen_Ordenar);
   Rand_Vacuna.Reset(Gen_Vacuna);
   Rand_Accion.Reset(Gen_Accion);

   for I in 1 .. 100 loop
      Vacas(I) := new Vaca(I);
   end loop;

   loop
      Camiones.Operar(0, "Lleno", Lleno);
      exit when Lleno = True;
      delay 0.5;
   end loop;

   Put_Line("Los dos camiones están llenos. Fin del programa.");
end Main;
