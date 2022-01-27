import * as A from "fp-ts/Array";
import * as ChildProcess from "child_process";
import * as E from "fp-ts/Either";
import * as Https from "https";
import * as IOE from "fp-ts/IOEither";
import * as O from "fp-ts/Option";
import * as OS from "os";
import * as Path from "path";
import * as TE from "fp-ts/TaskEither";
import * as Util from "util";
import * as t from "io-ts";

import { apply, flow, pipe } from "fp-ts/function";

import { PathReporter } from "io-ts/PathReporter";
import download from "download";

const Asset = t.type(
  {
    id: t.number,
    name: t.string,
  },
  "Asset"
);

type Asset = t.TypeOf<typeof Asset>;

const Release = t.type(
  {
    assets: t.array(Asset),
  },
  "Release"
);

type Release = t.TypeOf<typeof Release>;
type OS = "macos" | "linux";

// __dirname will point to the dist/ folder since the code is transpiled prior to being ran with node.
const root = Path.resolve(__dirname, "..");
const dist = Path.resolve(root, "dist");
// Github api requires having a user agent header. This can be literally anything.
const userAgent = "unsplash-intlc";

const decode =
  <A>(codec: t.Decoder<unknown, A>) =>
  (input: string): E.Either<Error, A> => {
    return pipe(
      E.tryCatch(
        () => JSON.parse(input),
        (_error) => new Error(`Failed at decoding ${codec.name}: ${input}`)
      ),
      E.chain((input) => {
        const decoded = codec.decode(input);

        // very strange stuff here...
        if (E.isLeft(decoded)) {
          return E.left(
            pipe(
              PathReporter.report(decoded),
              (messages) => messages.join("\n"),
              (message) => new Error(message)
            )
          );
        } else {
          return decoded;
        }
      })
    );
  };

const getErrorOrElse = (defaultErrorMessage: string) => (error: unknown) =>
  error instanceof Error ? error : new Error(defaultErrorMessage);

const runChildProcess = (command: string) =>
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

const getOSFromPlatform = (platform: NodeJS.Platform): O.Option<OS> => {
  const isWindows = platform === "win32";
  const isMacOs = platform === "darwin";

  // We're kind of assuming here...
  // https://gist.github.com/cvan/ef28f73b88b991d4a9705314b2af2e78
  if (!isWindows && !isMacOs) {
    return O.some("linux");
  } else if (isMacOs) {
    return O.some("macos");
  } else {
    return O.none;
  }
};

const fetchReleaseAsset = (tag: string, os: OS) =>
  TE.tryCatch((): Promise<Asset> => {
    return new Promise((resolve, reject) => {
      let body: string = "";
      const request = Https.get(
        {
          host: "api.github.com",
          protocol: "https:",
          path: `/repos/unsplash/intlc/releases/tags/${tag}`,
          headers: {
            "user-agent": userAgent,
          },
        },
        (response) => {
          response.setEncoding("utf8");

          // Urgh...
          response.on("data", (chunk) => {
            body += chunk;
          });

          response.on("end", () =>
            pipe(
              body,
              decode(Release),
              E.chain(
                flow(
                  (release) => release.assets,
                  A.findFirst((asset) => asset.name === `intlc_${os}`),
                  E.fromOption(
                    () => new Error(`Could not find binary for OS: ${os}`)
                  )
                )
              ),
              E.fold(reject, resolve)
            )
          );
        }
      );

      request.on(
        "error",
        flow(getErrorOrElse("Requesting release failed"), reject)
      );
    });
  }, getErrorOrElse("Unknown error: could not fetch release"));

const downloadAsset = (asset: Asset) =>
  pipe(
    TE.tryCatch(
      () =>
        download(
          `https://api.github.com/repos/unsplash/intlc/releases/assets/${asset.id}`,
          dist,
          {
            filename: "intlc",
            headers: {
              "user-agent": userAgent,
              Accept: "application/octet-stream",
            },
          }
        ),
      getErrorOrElse("Could not download binary")
    ),
    TE.chainFirst((_) =>
      runChildProcess(`chmod +x ${Path.resolve(dist, "intlc")}`)
    )
  );

const getTag: IOE.IOEither<Error, string> = () =>
  pipe(
    process.argv,
    A.lookup(2),
    E.fromOption(() => new Error("Missing required git tag"))
  );

pipe(
  TE.fromIOEither(getTag),
  TE.chain((tag) =>
    pipe(
      getOSFromPlatform(OS.platform()),
      TE.fromOption(() => new Error("No binary can be found for your OS")),
      TE.chain((os) => fetchReleaseAsset(tag, os))
    )
  ),
  TE.chainFirst(downloadAsset),
  TE.map((asset) => `Downloaded asset ${asset.name} in ${dist}`)
)().then(console.log, console.error);
