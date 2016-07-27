MODULE util
  !
  ! Modulo que contiene diferentes programas que no pudieron ser 
  ! clasificados en otra parte
  !
  USE decimal
  USE tipos
  !
  IMPLICIT NONE
  !
CONTAINS
  !
  !
  SUBROUTINE imprime_archivo_area(malla)
    !
    ! Imprime archivos DirMalla/malla.niter.area para hacer refinamiento
    ! adaptativo en error a posteriori
    !
    ! !! Obligo a dividir por cuatro al elemento 1 de la malla !!
    !
    TYPE(mesh),INTENT(in)      :: malla
    INTEGER                    :: ounit,jj,kk,ndim,nelem
    INTEGER, ALLOCATABLE       :: mm(:)
    CHARACTER(LEN=32)          :: name_out
    REAL(KIND=dp), ALLOCATABLE :: coordN(:,:)
    !
    ndim = malla%ndim;  nelem = malla%nelem
    !
    ALLOCATE( coordN(3,2) , mm(3) )
    coordN = 0.0_dp;  mm = 0
    !
    name_out = TRIM(ADJUSTL(malla%name))//'.area'
    !
    CALL util_get_unit(ounit)
    OPEN(ounit,file=name_out, status='replace', action='write' )
    !
    WRITE(ounit,'(a)') '# generado por mesh_mhm.f90'
    !
    WRITE(ounit,*) nelem
    DO kk=1,nelem
       !
       mm  = malla%elem(kk,1:3) ! numero de los nodos del elemento kk
       !
       DO jj=1,3
          coordN(jj,:) = malla%coord(mm(jj),:)  ! coord nodos del elemento k
       END DO
       !
       IF (kk==1) THEN
          WRITE(ounit,*) kk,medidaK(coordN)/4.0_dp
       ELSE
!          WRITE(ounit,*) medidaK(coordN)
          WRITE(ounit,*) kk,-1
       END IF
       !
    END DO
    !
    CLOSE(ounit)
    !
  END SUBROUTINE imprime_archivo_area
  !
  !
  FUNCTION medidaK(coord)
    !
    ! calcula la medida (area) del triangulo con vertices coord:
    !
    !    coord : (x1,y1)
    !            (x2,y2)
    !            (x3,y3)
    !
    INTEGER                   :: ndim
    REAL(KIND=dp),INTENT(in)  :: coord(:,:)
    REAL(KIND=dp)             :: medidaK,x1,x2,x3,y1,y2,y3
    !
    ndim = 2;  medidaK = 0.0_dp
    !
    x1 = coord(1,1);  y1 = coord(1,2)
    x2 = coord(2,1);  y2 = coord(2,2)
    x3 = coord(3,1);  y3 = coord(3,2)
    !
    medidaK = ABS( (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1) ) / 2.0_dp
    !
  END FUNCTION medidaK
  !
  !
  SUBROUTINE util_get_unit (iunit)
    !
    !**************************************************************************
    !
    !! GET_UNIT returns a free FORTRAN unit number.
    !
    !
    !  Discussion:
    !
    !    A "free" FORTRAN unit number is an integer between 1 and 99 which
    !    is not currently associated with an I/O device.  A free FORTRAN unit
    !    number is needed in order to open a file with the OPEN command.
    !
    !  Modified:
    !
    !    02 March 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, integer IUNIT.
    !
    !    If IUNIT = 0, then no free FORTRAN unit could be found, although
    !    all 99 units were checked (except for units 5 and 6).
    !
    !    Otherwise, IUNIT is an integer between 1 and 99, representing a
    !    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
    !    are special, and will never return those values.
    !
    IMPLICIT NONE
    !
    INTEGER i, ios, iunit
    LOGICAL lopen
    !  
    iunit = 0
    !
    DO i = 1, 99    
       IF ( i /= 5 .AND. i /= 6 ) THEN        
          INQUIRE ( unit = i, opened = lopen, iostat = ios )        
          IF ( ios == 0 ) THEN
             IF ( .NOT. lopen ) THEN
                iunit = i
                RETURN
             END IF
          END IF
       END IF
    END DO
    !
  END SUBROUTINE util_get_unit
  !
END MODULE util
