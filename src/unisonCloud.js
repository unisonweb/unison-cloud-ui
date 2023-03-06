import "ui-core/css/ui.css";
import "ui-core/css/themes/unison-light.css";
import "ui-core/css/code.css";
import "ui-core/UI/CopyOnClick"; // Include web components
import "ui-core/Lib/OnClickOutside"; // Include web components
import detectOs from "ui-core/Lib/detectOs";
import preventDefaultGlobalKeyboardEvents from "ui-core/Lib/preventDefaultGlobalKeyboardEvents";
import * as Sentry from "@sentry/browser";
import { BrowserTracing } from "@sentry/tracing";

import { getCookie } from "./util";
import * as Metrics from "./metrics";

import "./css/unison-cloud.css";
import { Elm } from "./UnisonCloud.elm";

console.log(`
 _____     _
|  |  |___|_|___ ___ ___
|  |  |   | |_ -| . |   |
|_____|_|_|_|___|___|_|_|


`);

// ----------------------------------------------------------------------------

preventDefaultGlobalKeyboardEvents();

Metrics.init();

if (APP_ENV === "production") {
  Sentry.init({
    dsn: "https://8eb2ee6bb78d4131bdbb1b6a70f6b0c0@o4503934538547200.ingest.sentry.io/4504458036903936",
    integrations: [new BrowserTracing()],
    sampleRate: 0.25,
    environment: APP_ENV,
  });
}

// ----------------------------------------------------------------------------

const basePath = new URL(document.baseURI).pathname;

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiUrl: API_URL,
  xsrfToken: getCookie("XSRF-TOKEN"),
  appEnv: APP_ENV,
};

// The main entry point for the `UnisonCloud` target of the Codebase UI.
const app = Elm.UnisonCloud.init({ flags });

// Ports can be dead code eliminated, so we have to check if they exist
if (app.ports) {
  app.ports.trackEvent?.subscribe((eventName) => {
    Metrics.track(eventName);
  });
}
