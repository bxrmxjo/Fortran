module circunferencia_mod
  implicit none
  contains

  !Subrutina para calcular la ecuación de la circunferencia
  subroutine calcular_circunferencia(P, a, b, c, Ox, Oy, r, L, S, status)
    double precision, dimension(2, 3), intent(in) :: P  !Matriz de puntos (2 filas, 3 columnas)
    double precision, dimension(3,3) :: matriz
    double precision, dimension(3) :: w, x
    double precision, intent(out) :: a, b, c, Ox, Oy, r, L, S
    double precision :: det  !Determinante para el sistema
    double precision :: eps = 1.0e-10 !Declaramos una tolerancia para detectar (pseudo)dependencia lineal
    integer i, status

    !---------------------------------------------------
    ! Dados tres puntos del plano, las ecuaciones del sistema 
    ! a resolver tienen la siguiente forma:
    !          a*x + b*y + c = -x**2 -y**2
    !---------------------------------------------------

    do i = 1, 3
      matriz(i, 1) = P(1, i)
      matriz(i, 2) = P(2, i)
      matriz(i, 3) = 1.0
      w(i) = -P(1, i)**2 - P(2, i)**2
    end do
    
    
    !Si se verifica el "if", los puntos estarán alineados .
    !Se añade tolerancia eps para abordar el error que se acumula al trabajar con reales en ordenador.
    status = 0 
    if (abs(determinant(matriz)) < eps) then
        status = 1  !Código de error para identificar la situación
        a = 0.0
        b = 0.0
        c = 0.0
        Ox = 0.0
        Oy = 0.0
        r = 0.0
        L = 0.0
        S = 0.0
        return
		end if

    call resolver_sistema_cramer(matriz, w, x)

    !Extraemos los coeficientes
    a = x(1)
    b = x(2)
    c = x(3)

    !Calculamos el centro
    Ox = -a / 2.0
    Oy = -b / 2.0

    !Calculamos el radio
    r = sqrt(Ox**2 + Oy**2 - c)

    !Calculamos longitud y área
    L = 2.0 * 3.14159265358979 * r
    S = 3.14159265358979 * r**2
  end subroutine calcular_circunferencia



  !Funcion para calcular el determinante de una matriz 3x3
  double precision function determinant(A)
    double precision, dimension(3, 3), intent(in) :: A
    determinant = A(1,1) * (A(2,2) * A(3,3) - A(2,3) * A(3,2)) &
                - A(1,2) * (A(2,1) * A(3,3) - A(2,3) * A(3,1)) &
                + A(1,3) * (A(2,1) * A(3,2) - A(2,2) * A(3,1))
  end function determinant



  !Subrutina para resolver un sistema 3x3 usando la regla de Cramer
  subroutine resolver_sistema_cramer(A, w, x)
    double precision, dimension(3, 3), intent(in) :: A 
    double precision :: Ai(3, 3)
    double precision, dimension(3), intent(in) :: w      
    double precision, dimension(3), intent(out) :: x     
    double precision :: detA, detAi(3)                    
    integer :: i

    !Calculamos el determinante de A
    detA = determinant(A)	
		
    !Aplicar la regla de Cramer
    do i = 1, 3
      Ai = A  !Copiamos A en Ai
      Ai(:, i) = w  !Reemplazamos la i-ésima columna de Ai por w
      detAi(i) = determinant(Ai)
      x(i) = detAi(i) / detA
    end do
  end subroutine resolver_sistema_cramer

end module circunferencia_mod





!Programa principal
program CIRCUNFERENCIA_MARIO_BERMEJO
  use circunferencia_mod
  implicit none
  integer :: i, ios, status
  double precision :: a, b, c, Ox, Oy, r, L, S
  double precision, dimension(2, 3) :: P  !Matriz para los puntos (por columnas para mejorar el uso de memoria)

  !Abrimos el archivo de entrada
  open(unit=10, file='datosCIRCUNFERENCIA.txt', iostat=ios) 
  if (ios /= 0) then
    print*, "Error al abrir el archivo de entrada: iostat =", ios
    stop
  end if

  !Abrimos/creamos el archivo de salida para las respuestas
  open(unit=11, file='resultadosCIRCUNFERENCIA.txt', iostat=ios) 
  if (ios /= 0) then
    print*, "Error al abrir/crear el archivo de salida: iostat =", ios
    stop
  end if

  !El estilo del archivo de salida será casi igual al del archivo de entrada
  write(11, '(A)') "=============================================================================="
  write(11, '(A)') "| Puntos           | Ecuacion         | Centro            | r, L, S          |"

  !Leemos los puntos y calculamos la circunferencia para cada terna de puntos
  do i = 1, 5
    read(10, '(///,6X,F5.3,1X,F5.3)') P(1,1), P(2,1)
    read(10, '(6X,F5.3,1X,F5.3)') P(1,2), P(2,2)
    read(10, '(6X,F5.3,1X,F5.3)') P(1,3), P(2,3)
    
    !Calculamos la ecuación de la circunferencia
    call calcular_circunferencia(P, a, b, c, Ox, Oy, r, L, S, status)

    !Escribimos los resultados en el archivo de salida
    if (status /= 0) then
    	write(11, '(A)') "|----------------------------------------------------------------------------|"
    	write(11, '("| P1=(",F5.3,",",F5.3,") | Los puntos estan alineados o demasiado alineados        |")') P(1, 1), P(2, 1)
    	write(11, '("| P2=(",F5.3,",",F5.3,") | como para calcular la ecuacion de la circunferencia que |")') P(1, 2), P(2, 2)
    	write(11, '("| P3=(",F5.3,",",F5.3,") | pasa por ellos.                                         |")') P(1, 3), P(2, 3)
    	write(11, '(A,/)') "|----------------------------------------------------------------------------|"

    else
    	write(11, '(A)') "|----------------------------------------------------------------------------|"
    	write(11, '("| P1=(",F5.3,",",F5.3,") | a=",G15.8,"| Ox=",G15.8,"| r=",G15.8,"|")') P(1, 1), P(2, 1), a, Ox, r
    	write(11, '("| P2=(",F5.3,",",F5.3,") | b=",G15.8,"| Oy=",G15.8,"| L=",G15.8,"|")') P(1, 2), P(2, 2), b, Oy, L
    	write(11, '("| P3=(",F5.3,",",F5.3,") | c=",G15.8,"|                   | S=",G15.8,"|")') P(1, 3), P(2, 3), c, S
    	write(11, '(A,/)') "|----------------------------------------------------------------------------|"
  	end if
  end do

  !Cerramos los ficheros
  close(unit=10)
  close(unit=11)
end program CIRCUNFERENCIA_MARIO_BERMEJO
