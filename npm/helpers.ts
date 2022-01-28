import * as ChildProcess from "child_process";
import * as TE from "fp-ts/TaskEither";
import * as Util from "util";

import { apply, pipe } from "fp-ts/function";

export const getErrorOrElse =
  (defaultErrorMessage: string) => (error: unknown) =>
    error instanceof Error ? error : new Error(defaultErrorMessage);

export const execChildProcess = (command: string) =>
  pipe(
    TE.tryCatch(
      () => pipe(Util.promisify(ChildProcess.exec), apply(command)),
      getErrorOrElse(`Could not run ${command}`)
    ),
    TE.chain(({ stderr, stdout }) => {
      if (stderr.length > 0) {
        return TE.left(
          new Error(`Error running command: ${stderr.toString()}`)
        );
      } else {
        return TE.right(stdout);
      }
    })
  );
