import useLocalState, { LocalState } from "~/logic/state/local";
import useSettingsState from "~/logic/state/settings";
import GlobalApi from "../api-old/global";
import { BackgroundConfig, RemoteContentPolicy } from "~/types";
import { putEntry } from "@urbit/api/dist";
import { UrbitInterface } from "@urbit/http-api";
import { settings } from "@urbit/api";
import useApi from "../api";

const getBackgroundString = (bg: BackgroundConfig) => {
  if (bg?.type === "url") {
    return bg.url;
  } else if (bg?.type === "color") {
    return bg.color;
  } else {
    return "";
  }
};

export function useMigrateSettings() {
  const local = useLocalState();
  const { display, remoteContentPolicy, calm } = useSettingsState();
  const api = useApi();

  return async () => {
    let promises: Promise<any>[] = [];

    if (local.hideAvatars !== calm.hideAvatars) {
      promises.push(
        api.poke(settings.putEntry('calm', 'hideAvatars', local.hideAvatars))
      );
    }

    if (local.hideNicknames !== calm.hideNicknames) {
      promises.push(
        api.poke(settings.putEntry("calm", "hideNicknames", local.hideNicknames))
      );
    }

    if (
      local?.background?.type &&
      display.background !== getBackgroundString(local.background)
    ) {
      promises.push(
        api.poke(settings.putEntry(
          "display",
          "background",
          getBackgroundString(local.background)
        ))
      );
      promises.push(
        api.poke(settings.putEntry(
          "display",
          "backgroundType",
          local.background?.type
        ))
      );
    }

    Object.keys(local.remoteContentPolicy).forEach((_key) => {
      const key = _key as keyof RemoteContentPolicy;
      const localVal = local.remoteContentPolicy[key];
      if (localVal !== remoteContentPolicy[key]) {
        promises.push(
          api.poke(putEntry("remoteContentPolicy", key, localVal))
        );
      }
    });

    await Promise.all(promises);
    localStorage.removeItem("localReducer");
  };
}
