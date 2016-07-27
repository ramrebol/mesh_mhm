MODULE loading_data
  !
  ! Modulo que contiene diferentes rutinas para cargar datos,
  ! en particular cargar malla
  !
  USE decimal
  USE tipos
  USE util
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC :: load_input
  !
CONTAINS
  !
  SUBROUTINE load_input(malla)
    !
    ! Lectura del archivo input y carga de datos de la malla.
    !
    CHARACTER(len=32) :: fout,name_sin_niter
    INTEGER           :: ii,iunit
    TYPE(mesh)        :: malla
    !
    ! Archivo desde donde se leeran los datos basicos...
    CALL util_get_unit(iunit)
    OPEN(unit=iunit,file='input',status='old',action='read')
    !
    ! Lectura de la malla
    PRINT*,'# Cargando datos de malla'
    CALL load_mesh(iunit,malla,name_sin_niter,fout)
    !
    CLOSE(iunit)
    !
  END SUBROUTINE load_input
  !
  !
  SUBROUTINE load_mesh(iunit,malla,name_sin_niter,fout)
    !
    ! Toma salida de triangle y genera la malla
    !
    TYPE(mesh)                :: malla
    INTEGER                   :: iunit,ii,jj,kk,ndim,nnode,nelem
    CHARACTER(len=32)         :: DIRmalla,prefijo,name_sin_niter,fout
    !
    READ(iunit,'(a32)') DIRmalla
    READ(iunit,'(a32)') prefijo
    READ(iunit,*) malla%niter
    PRINT*,malla%niter
    !
    name_sin_niter = TRIM(DIRmalla)//'/'//TRIM(prefijo)
    malla%name     = TRIM(name_sin_niter)//'.'//TRIM(malla%niter)
    !
    CALL load_coord(malla) ! coordenadas y sus referencias
    ndim  = malla%ndim
    nnode = malla%nnode
    !
    CALL load_elem(malla)  ! elementos y sus referencias
    !
  END SUBROUTINE load_mesh
  !
  !
  SUBROUTINE load_coord(malla)
    !
    ! Carga las coordenadas y sus referencias
    !   - malla%coord
    !   - malla%ref_node
    !
    TYPE(mesh)         :: malla
    CHARACTER(LEN=32)  :: name_node
    INTEGER            :: ii,jj,unit_node,nnode,ndim
    !
    name_node = TRIM(malla%name)//'.node'
    CALL util_get_unit(unit_node)
    OPEN(unit_node,file=name_node,status='old',action='read')
    READ(unit_node,*) nnode , ndim , ii , jj
    malla%nnode = nnode
    malla%ndim  = ndim
    !
    ALLOCATE(malla%coord(nnode,ndim) , malla%ref_node(nnode))
    malla%coord = 0.0_dp; malla%ref_node = 0
    !
    READ(unit_node,*) (jj,malla%coord(ii,:), malla%ref_node(ii), ii =1, nnode)
    !
    CLOSE(unit_node) ! archivo .node
    !
  END SUBROUTINE load_coord
  !
  !
  SUBROUTINE load_elem(malla)
    !
    ! Carga los elementos y sus referencias.
    !   - malla%elem
    !   - malla%ref_elem
    !
    TYPE(mesh)           :: malla
    CHARACTER(LEN=32)    :: name_elem
    INTEGER              :: ii,jj,kk,unit_elem,ndim,nelem
    !
    ndim = malla%ndim
    !
    name_elem = TRIM(malla%name)//'.ele'
    CALL util_get_unit(unit_elem)
    OPEN(unit_elem,file=name_elem,status='old',action='read')
    READ(unit_elem,*) nelem , ii , jj
    malla%nelem = nelem
    !
    ALLOCATE(malla%elem(nelem,ndim+1) , malla%ref_elem(nelem))
    malla%elem = 0; malla%ref_elem = 0
    !
    READ(unit_elem,*) (jj,malla%elem(ii,:), malla%ref_elem(ii), ii =1, nelem) 
    !
    CLOSE(unit_elem)
    !
  END SUBROUTINE load_elem
  !
END MODULE loading_data
