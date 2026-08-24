import "ui-core/css/ui.css";
import "ui-core/css/themes/unison-light.css";
import "ui-core/css/code.css";
import "ui-core/UI/CopyOnClick"; // Web component
import "ui-core/UI/CopyrightYear"; // Web component
import "ui-core/Lib/OnClickOutside"; // Web component
import "./UnisonCloud/SupportChatWidget"; // web component
import detectOs from "ui-core/Lib/detectOs";
import preventDefaultGlobalKeyboardEvents from "ui-core/Lib/preventDefaultGlobalKeyboardEvents";

import { getCookie } from "./util";

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

// ----------------------------------------------------------------------------

const basePath = new URL(document.baseURI).pathname;

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiUrl: API_URL,
  websiteUrl: WEBSITE_URL,
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

  app.ports.copyText?.subscribe((text) => {
    navigator.clipboard.writeText(text);
  });

  app.ports.debugLog?.subscribe((text) => {
    console.debug(text);
  });
}
