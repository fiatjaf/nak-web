import cats.effect.*
import cats.syntax.all.*
import cats.effect.syntax.all.*


/** race a list of IOs and return first to finish. This means even if the first
 *  to finish results in an error, it will be returned.
 * source: https://github.com/paul-snively/easyracer/commit/0642a62131e9127b28f32d781913b366539601e0#diff-9676066bc2a39fc2e916043cdb6c565c7ddce8fb8d85c17ea44dd7b2d193a6b6R94
 * // Fabio Labella's multiRace.
 * */
def multiRace[F[_]: Concurrent, A](fas: List[F[A]]): F[A] = {
    def spawn[B](fa: F[B]): Resource[F, Unit] =
      Resource.make(fa.start)(_.cancel).void

    def finish(fa: F[A], d: Deferred[F, Either[Throwable, A]]): F[Unit] =
      fa.attempt.flatMap(d.complete).void

    Deferred[F, Either[Throwable, A]]
    .flatMap { result =>
        fas
        .traverse(fa => spawn(finish(fa, result)))
        .use(_ => result.get.rethrow)
    }
}

def multiRaceAllFailOrFirstToSucceed[F[_]: Concurrent, A](tasks: List[F[A]]): F[A] =
  // Create a deferred to communicate the winning result or aggregated errors.
  for {
    // Deferred holds either a list of errors (if all fail) or a successful result.
    result   <- Deferred[F, Either[List[Throwable], A]]
    // Ref to accumulate all failures.
    errors   <- Ref.of[F, List[Throwable]](List.empty)
    // Ref to track remaining tasks.
    remaining <- Ref.of[F, Int](tasks.size)
    // Spawn each task concurrently.
    fibers <- tasks.traverse { task =>
      // Start each task concurrently.
      Concurrent[F].start {
        task.attempt.flatMap {
          case Right(a) =>
            // Try to complete with a successful result.
            result.complete(Right(a)).void
          case Left(e) =>
            // Record the failure.
            errors.update(e :: _) *>
            // Decrement the count of remaining tasks.
            remaining.modify(n => (n - 1, n - 1)).flatMap { rem =>
              // If this was the last task, complete the deferred with the aggregated errors.
              if (rem == 0)
                errors.get.flatMap(errs => result.complete(Left(errs)).void)
              else Concurrent[F].unit
            }
        }
      }
    }
    // Optionally, cancel the fibers after one finishes.
    // Here we wait for the result and then cancel all fibers.
    winner <- result.get
    _ <- fibers.traverse_(_.cancel)
    // If we got an error list, raise an exception, otherwise return the success.
    outcome <- winner match {
                 case Right(a)   => a.pure[F]
                 case Left(errs) => Concurrent[F].raiseError[A](new Exception(errs.map(_.getMessage).mkString("; ")))
               }
  } yield outcome
