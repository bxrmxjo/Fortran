!------------------------------------------------
! MODULO CON FUNCIONES Y SUBRUTINAS DEL PROGRAMA
!------------------------------------------------

module circunf_mod
	implicit none
	double precision, parameter :: eps = 1e-12
	contains 
	
	!Función que calcula la distancia entre dos puntos del plano.
	double precision function distancia(A, B)
	  double precision, dimension(2), intent(in) :: A, B
	  distancia = sqrt((B(1) - A(1))**2 + (B(2) - A(2))**2)
	end function distancia
	
	
	subroutine circulo_diam_extremos(A,ctot,O,r)
		integer i, j
		integer, intent(in):: ctot
		double precision, intent(out) ::  O(2), r
		double precision, intent(in) :: A(:,:) !Dimensión asumida
		integer :: npunt1, npunt2
		double precision ::  distmax, disttemp
		!--------------------------------------------------------------
		! Encuentra los dos puntos que mas distan y obtiene la 
		! circunferencia que los tiene por extremos de un diametro. 
		!--------------------------------------------------------------
		
	  distmax = 0.0
	  do i = 1, (ctot -1)
	    do j = i + 1, ctot
	      disttemp = distancia(A(:, i), A(:, j))
	      if (disttemp > distmax) then
	        distmax = disttemp
	        npunt1 = i    !Guardamos la posición del punto 1
	        npunt2 = j    !Guardamos la posición del punto 2
	      end if
	    end do
	  end do
	
	  O = (A(:, npunt1) + A(:, npunt2)) / 2.0    !El punto medio del diámetro es el centro
		r = distancia(A(:, npunt1), A(:, npunt2)) / 2.0

	end subroutine circulo_diam_extremos
	
	
	subroutine calcular_circunferencia(P, O, r)
    double precision, dimension(2, 3), intent(in) :: P  !Matriz de puntos (2 filas, 3 columnas)
    double precision:: matriz(3,3), w(3), x(3)
    double precision, intent(out) :: O(2), r
    integer i

    !---------------------------------------------------
    ! Dados tres puntos del plano, las ecuaciones del sistema 
    ! a resolver tienen la siguiente forma:
    !          a*x + b*y + c = -x**2 -y**2
    !---------------------------------------------------

    do i = 1, 3
      matriz(i, 1) = P(1, i); matriz(i, 2) = P(2, i);  matriz(i, 3) = 1.0
      w(i) = -P(1, i)**2 - P(2, i)**2
    end do
      
    call resolver_sistema_cramer(matriz, w, x)
    
    !Calculamos el centro
    O(1) = -x(1) / 2.0; O(2) = -x(2) / 2.0

    !Calculamos el radio
    r = sqrt(O(1)**2 + O(2)**2 - x(3))
  end subroutine calcular_circunferencia
  
  
  double precision function determinant(A)
    double precision, dimension(3, 3), intent(in) :: A
    determinant = A(1,1) * (A(2,2) * A(3,3) - A(2,3) * A(3,2)) &
                - A(1,2) * (A(2,1) * A(3,3) - A(2,3) * A(3,1)) &
                + A(1,3) * (A(2,1) * A(3,2) - A(2,2) * A(3,1))
  end function determinant



  subroutine resolver_sistema_cramer(A, w, x)
    double precision, intent(in) :: A (3,3), w(3)
    double precision :: Ai(3, 3), detA, detAi(3)  
    double precision, dimension(3), intent(out) :: x                   
    integer :: i

    !Calculamos el determinante de A (asumiendo que no es nulo al comprobarse antes de llamar a la subrutina)
    detA = determinant(A)	
		
    !Aplicamos la regla de Cramer
    do i = 1, 3
      Ai = A  !Copiamos A en Ai
      Ai(:, i) = w  !Reemplazamos la i-ésima columna de Ai por w
      detAi(i) = determinant(Ai)
      x(i) = detAi(i) / detA
    end do
  end subroutine resolver_sistema_cramer
  
  
  !Funcion para verificar si una circunferencia contiene a todos los puntos de una nube
	logical function contiene_a_todos(A, O, r, ctot)
    implicit none
    integer, intent(in) :: ctot
    double precision, intent(in):: A(2, ctot)  
    double precision, intent(in):: O(2)    
    double precision, intent(in):: r        
    integer :: i
    
		contiene_a_todos = .true.  ! Suponemos inicialmente que la circunferencia contiene a todos los puntos
		
		do i = 1, ctot
			if (distancia(A(:, i), O) - r > eps) then
	      contiene_a_todos = .false.
				return  !Salimos de la función si encontramos un punto fuera
			end if
		end do
	end function contiene_a_todos
end module circunf_mod



! ----------------------------------------------------------------------------
!							  	 	    BREVE EXPLICACION DEL PROGRAMA                        
! Se comprueba si se verifica la propiedad (1). Si no es así, se busca 
! exhaustivamente el circulo de menor radio que contenga a todos los puntos.
! ----------------------------------------------------------------------------

program CIRCULO_DE_DISPERSION_MARIO_BERMEJO
	use circunf_mod
  implicit none
  integer, parameter :: ctot = 40 , cmed = ctot/2 !ctot: cantidad puntos; cmed: cantidad filas de puntos
  integer :: puntos(3), i, j, k, ios, A(2,ctot)
  double precision :: r_min, r, P_extendido(3,3), P(2,3), A_double(2,ctot), O(2), D(ctot)   
  character(len=1) :: m(ctot)
  
  open(10, file = "datosCIRCULO_DE_DISPERSION.txt", iostat=ios)
  if (ios /= 0) then
    print*, "Error al abrir el archivo de entrada: iostat =", ios
    stop
  end if
  
  open(11, file = "respuestasCIRCULO_DE_DISPERSION.txt", iostat=ios)
  if (ios /= 0) then
    print*, "Error al abrir/crear el archivo de salida: iostat =", ios
    stop
  end if

  9000 format (5X, I2, 1X, I2, T45, I2, 1X, I2, /)

  read(10, '(//)') 
  
  !Leemos los puntos y los guardamos en una matriz de enteros A. 
  !Paralelamente, los guardamos en A_double, que guarda los puntos en double precision para los posteriores cálculos.
  do i = 1, cmed
    read(10, 9000) A(1, i), A(2, i), A(1, cmed + i), A(2, cmed + i)
    
    A_double(1,i) = dble(A(1,i)); A_double(2,i) = dble(A(2,i))
    A_double(1,cmed+i) = dble(A(1,cmed+i)); A_double(2,cmed+i) = dble(A(2,cmed+i))
  end do

	
	!Buscamos el par de puntos con mayor distancia entre sí y calculamos la circ. donde son extremos de un diametro.
  call circulo_diam_extremos(A_double,ctot,O,r)
  
  
	!Verificamos si contiene a todo punto de la nube. Sino, se buscará exhaustivamente el circulo de dispersion.
	if (contiene_a_todos(A_double,O,r,ctot)) go to 200
	
	
	r_min = 1000.d0 !Inicializamos el radio muy grande (no hace falta que sea enorme)	
	do i = 1, ctot-2
		do j = i+1, ctot-1
			do k = j+1, ctot
	
	
				P = reshape((/A_double(:,i), A_double(:,j), A_double(:,k)/),(/2,3/) )
				P_extendido = reshape((/ P(1,:), P(2,:), 1.d0, 1.d0, 1.d0 /), (/3, 3/))
				
				if (abs(determinant(P_extendido)) < eps) cycle !No se calcula la circunferencia porque están alineados
				
				call calcular_circunferencia(P,O,r) 
				if (contiene_a_todos(A_double,O,r,ctot) .and. r < r_min) then
					r_min = r
					puntos(1) = i; puntos(2) = j; puntos(3) = k
				end if
				
				
			end do
		end do
	end do
	
	!Calculamos (O,r) para los "mejores puntos".
	!Se hace una sola vez en vez de guardar en el bucle la circunferencia (en total se guardaría 9880 veces)
	P = reshape((/A_double(:,puntos(1)), A_double(:,puntos(2)), A_double(:,puntos(3))/),(/2,3/) )
	call calcular_circunferencia(P,O,r)


	!Obtenemos las nuevas distancias de cada punto al centro independientemente de si se cumplía (1) o (2)
	200 continue
	do i = 1,ctot
		D(i) = distancia(A_double(:,i), O)
		if (abs(D(i) - r) < eps) then 
			m(i) = "*"
		else 
			m(i) = " "
		end if
	end do
	
	
	!Cabecera resultados fichero de salida
	write(11, '("====================================   ====================================")')
	write(11, '("| P=(Px,Py) | Marca |  dist (O,P)  |   | P=(Px,Py) | Marca |  dist (O,P)  |")')
	write(11, '("|==================================|   |==================================|")')
	
	!Resultados de cada punto (menos el ultimo)
	do i = 1, cmed-1
		write(11, '("|",I2,":(",I2,",",I2,") |   ",A1,"   |",F14.10,"|   ")', advance= "no") i, A(1,i), A(2,i), m(i),D(i)
		write(11, '("|",I2,":(",I2,",",I2,") |   ",A1,"   |",F14.10,"|")') i+cmed, A(1,i+cmed), A(2,i+cmed), m(i+cmed), D(i+cmed)
		write(11, '("|----------------------------------|   |----------------------------------|")')
	end do
	
	!Resultados de la ultima pareja de puntos (separados de las demas para cerrar la "tabla" con "====..."
	write(11, '("|",I2,":(",I2,",",I2,") |   ",A1,"   |",F14.10,"|   ")', advance= "no") cmed, A(1,cmed), A(2,cmed), m(cmed),D(cmed)
	write(11, '("|",I2,":(",I2,",",I2,") |   ",A1,"   |",F14.10,"|")') ctot, A(1,ctot), A(2,ctot), m(ctot), D(ctot)
	write(11, '("====================================   ====================================")')
	
	!Coordenadas del centro y radio
	write(11, '(//,"         =========================================================")')
	write(11, '("Circulo: | Ox=",F14.10,"| Oy=",F14.10,"| r=",F14.10,"|")') O(1), O(2), r
	write(11, '("         =========================================================")')
	
	close(10); close(11)
end program CIRCULO_DE_DISPERSION_MARIO_BERMEJO

