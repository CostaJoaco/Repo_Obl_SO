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


   protected Sala is
      entry Entrar(ID : Integer);
      procedure Salir(ID : Integer);
   private
      Cant : Integer := 0;
   end Sala;

   protected body Sala is
      entry Entrar(ID : Integer) when Cant < 15 is
      begin
         Cant := Cant + 1;
         Put_Line("Vaca" & Integer'Image(ID) & " entra a la sala");
      end Entrar;

      procedure Salir(ID : Integer) is
      begin
         Cant := Cant - 1;
         Put_Line("Vaca" & Integer'Image(ID) & " sale de la sala");
      end Salir;
   end Sala;


   protected Mangas is
      entry Entrar(ID : Integer);
      procedure Salir(ID : Integer);
   private
      Cant : Integer := 0;
   end Mangas;

   protected body Mangas is
      entry Entrar(ID : Integer) when Cant < 5 is
      begin
         Cant := Cant + 1;
         Put_Line("Vaca" & Integer'Image(ID) & " entra a vacunación");
      end Entrar;

      procedure Salir(ID : Integer) is
      begin
         Cant := Cant - 1;
         Put_Line("Vaca" & Integer'Image(ID) & " sale de vacunación");
      end Salir;
   end Mangas;

   
   protected Pasillo is
      entry Usar(ID : Integer);
   private
      Ocupado : Boolean := False;
   end Pasillo;

   protected body Pasillo is
      entry Usar(ID : Integer) when not Ocupado is
      begin
         Ocupado := True;
         Put_Line("Vaca" & Integer'Image(ID) & " usa el pasillo");
         Ocupado := False;
      end Usar;
   end Pasillo;

 
   protected Camiones is
      entry Subir(ID : Integer);
      function Lleno return Boolean;
   private
      C1 : Integer := 0;
      C2 : Integer := 0;
   end Camiones;

   protected body Camiones is
      entry Subir(ID : Integer) when C1 < 50 or C2 < 50 is
      begin
         if C1 < 50 then
            C1 := C1 + 1;
            Put_Line("Vaca" & Integer'Image(ID) & " sube al camión 1");
         else
            C2 := C2 + 1;
            Put_Line("Vaca" & Integer'Image(ID) & " sube al camión 2");
         end if;
      end Subir;

      function Lleno return Boolean is
      begin
         return C1 = 50 and C2 = 50;
      end Lleno;
   end Camiones;

   task type Vaca(ID : Integer);

   task body Vaca is
      Ordenada : Boolean := False;
      Vacunada : Boolean := False;
   begin
      loop
         exit when Ordenada and Vacunada;

         declare
            A : Integer := Rand_Accion.Random(Gen_Accion);
         begin
            if A = 1 and not Ordenada then
               Sala.Entrar(ID);
               delay Duration(Rand_Ordenar.Random(Gen_Ordenar));
               Sala.Salir(ID);
               Ordenada := True;

            elsif A = 2 and not Vacunada then
               Pasillo.Usar(ID);
               Mangas.Entrar(ID);
               delay Duration(Rand_Vacuna.Random(Gen_Vacuna));
               Mangas.Salir(ID);
               Pasillo.Usar(ID);
               Vacunada := True;
            end if;
         end;
      end loop;

      Camiones.Subir(ID);
   end Vaca;

   type VacaRef is access Vaca;
   Vacas : array (1 .. 100) of VacaRef;

begin
   Rand_Ordenar.Reset(Gen_Ordenar);
   Rand_Vacuna.Reset(Gen_Vacuna);
   Rand_Accion.Reset(Gen_Accion);

   
   for I in 1 .. 100 loop
      Vacas(I) := new Vaca(I);
   end loop;

   
   while not Camiones.Lleno loop
      delay 0.5;
   end loop;

   Put_Line("Los dos camiones están llenos. Fin del programa.");

end Main;
