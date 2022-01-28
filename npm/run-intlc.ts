#!/usr/bin/env node

import { execChildProcess } from "./helpers";

execChildProcess("./intlc")().then(console.log, console.error);
