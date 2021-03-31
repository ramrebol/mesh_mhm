
Mesh of the Multiscale Hybrid Finite Element method.

DESCRIPCION:
------------
  
  
   Mallador utilizado:
      - Triangle (2D): http://www.cs.cmu.edu/~quake/triangle.html
  
   INSTRUCCIONES:
   --------------
   1.- La malla inicial se genera con el siguiente comando:
  
         >  triangle -pqAa0.0625 cuadrado.poly
  
   2.- En el directorio ejemplo (ej: ./example/ej1/) correr
  
         >  ../../bin/mesh_mhm
  
       Esto generara el archivo cuadrado.area
  
   3.- Para crear la siguiente malla correr dentro del directorio
       ./example/ej1/mallas/ :
  
         >  triangle -rpqAa cuadrado.1.area
  
       Esto generara los archivos cuadrado.2.*
  
   --------------------------------------------------------------------------
  
   COMPILACION:
   -----------
  
   (A) Basica:
         > ifort decimal.f90 util.f90 tipos.f90 loading_data.f90 mesh_mhm.f90 -o mesh_mhm
   
   (B) Usando el makefile
         > make
   
  
   AUTOR:   RAMIRO REBOLLEDO
 
            Departamento de Ingenieria Matematica & CI2MA
            Universidad de Concepcion
            Casilla 160-C
            Concepcion, CHILE
            e-mail  :  ramrebol@gmail.com
  
  
   VERSION             : 0.1
   
   FECHA               : 27/JULIO/2016
   
   ULTIMA MODIFICACION :
