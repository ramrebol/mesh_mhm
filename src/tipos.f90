MODULE tipos
  !
  ! Declaracion los nuevos tipos de variables
  !
  ! mesh : tipo de una malla de elementos finitos
  !
  USE decimal
  !
  IMPLICIT NONE
  !
  TYPE mesh
     !
     ! niter    : numero de iteracion de la malla
     ! name     : nombre completo de la malla (directorio+prefijo+niter)
     ! ndim     : dimension del problema (2 o 3)
     ! nnode    : numero total de nodos de la malla
     ! nelem    : numero total de elementos de la malla
     ! elem     : conectividad de la malla
     ! coord    : coordenadas de los nodos de la malla
     ! ref_node : referencia de cada coordenada
     ! ref_elem : referencia de cada elemento
     !
     CHARACTER(len=32)      :: name
     CHARACTER(len=4)       :: niter
     INTEGER                :: ndim,nnode,nelem
     INTEGER, POINTER       :: elem(:,:),ref_node(:),ref_elem(:)
     REAL(KIND=dp), POINTER :: coord(:,:)
     !
  END TYPE mesh
  !
END MODULE tipos
