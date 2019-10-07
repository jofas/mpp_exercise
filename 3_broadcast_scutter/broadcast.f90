program broadcast
  use mpi_f08

  implicit none

  integer, parameter :: N = 12
  integer, dimension(N) :: x
  integer :: rank, w_size
  integer :: i, scatter_s, lower_bound, upper_bound

  type(MPI_Status) :: stat

  call mpi_init()

  call mpi_comm_rank(mpi_comm_world, rank)

  call mpi_comm_size(mpi_comm_world, w_size)

  if (rank == 0) then
    x = [(i, i = 1, N)]

    do i = 1, w_size - 1
      call get_bounds(&
        i, w_size, N, lower_bound, upper_bound &
      )

      call mpi_ssend( &
        x(lower_bound:upper_bound), &
        upper_bound - lower_bound, &
        mpi_integer, i, 0, mpi_comm_world &
      )
    end do

  else
    x = -1

    call get_bounds(&
      rank, w_size, N, lower_bound, upper_bound &
    )

    call mpi_recv( &
      x(lower_bound:upper_bound), &
      upper_bound - lower_bound, mpi_integer, &
      0, 0, mpi_comm_world, stat &
    )
  end if

  print *, rank, ": ", x

  call mpi_finalize()

contains

  subroutine get_bounds(rank, w_size, n, lower, upper)
    integer, intent(in) :: rank, w_size, n
    integer, intent(out) :: lower, upper
    integer :: scatter_s

    scatter_s = (n - mod(n, w_size - 1)) / (w_size - 1)

    lower = (rank - 1) * scatter_s + 1
    upper = lower + scatter_s
    if (rank == w_size - 1) upper = n + 1
  end
end
