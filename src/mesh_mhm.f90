PROGRAM mesh_mhm
  !
  ! Revise ../README
  !
  USE decimal
  USE tipos
  USE util
  USE loading_data
  !
  IMPLICIT NONE
  !
  TYPE(mesh) :: malla
  !
  CALL load_input(malla)
  !
  CALL imprime_archivo_area(malla)
  ! 
END PROGRAM mesh_mhm
