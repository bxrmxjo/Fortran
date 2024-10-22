program SUBMATRIZ_LU_MARIO_BERMEJO
implicit none
integer:: i, j, s, r, fil = 1, col= 1, ios
integer, parameter:: n=10, m=15, k=4
double precision:: resultado, mejor = -1.0e38 !inicializamos "mejor" muy negativo
double precision, dimension(:,:), allocatable :: X, A 
character(len=9):: formato

!---------------------------------------------------------
!                    Observaciones: 
!El programa ha sido escrito para que se pueda modificar 
!en cualquier momento el valor de los parametros m,n,k
!siempre y cuando el fichero de entrada respete dichas
!caracteristicas.
!---------------------------------------------------------


!Primera parte del programa (lectura y almacenamiento de datos) 


allocate(X(m,n), A(k,k))

open(unit = 10, file='datosLU_pablo.txt', iostat = ios) !Abrimos archivo de entrada
if (ios /= 0) then
  print*, "Error al abrir el archivo: iostat =", ios
  stop
end if

open(unit = 11, file='respuestasLU_pablo.txt', iostat = ios) !Abrimos/creamos el archivo de salida
if (ios /= 0) then
  print*, "Error al abrir/crear el archivo: iostat =", ios
  stop
end if

write(formato, '(A,I0,A)') '(', n, 'F8.4)'  ! Esto genera la cadena '(nF8.4)'

do i = 1, m !Leemos cada fila del archivo y almacenamos sus valores en la matriz A.
  read(10, formato) (X(i,j), j=1,n)
end do




! Segunda parte del programa (creacion de submatrices y f(A))




do s = 1, n-k+1    !Primero fijamos columna (más eficiente en términos de memoria)
  do r = 1, m-k+1
    A(1:k,1:k) = X(r:r+k-1,s:s+k-1) !Almacenamos cada submatriz en A  
    
    call f (A,k,resultado)
    if (resultado > mejor) then !Guardamos la información de la submatriz en caso de ser más óptima.
      mejor = resultado; fil = r ; col = s
    end if
  end do
end do

!Escribimos la solución en el fichero de salida
write(11, '(A35, //)') "Informacion de la solucion optima: "
write(11, *) " ======     ======            ==========="
write(11, '("r = |", I2, "|  s = |", I2, "|    f(A_rs) = |",F7.4, "|")') fil, col, mejor
write(11, *) " ======     ======            ==========="

!liberamos memoria y cerramos ficheros
deallocate(X); deallocate(A); close(unit=10); close(unit=11)
end program 






! Calculamos f(A) por separado (aunque no habría necesidad)
subroutine f(A,k,resultado)
implicit none
integer, intent(in) :: k  ! Tamaño de la submatriz
double precision, dimension(k,k), intent(in) :: A  ! Matriz de entrada
double precision sL, sU 
double precision, intent(out) :: resultado  ! Valor de f(A)
integer j

sL = 0.0
sU = 0.0

do j = 1,k
  sL = sL + sum(A(j:k,j)) !Almacenamos la suma de los elementos de L
  sU = sU + sum(A(1:j,j)) !Almacenamos la suma de los elementos de U
end do
resultado = sL - sU
end subroutine f




