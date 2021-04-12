import useLocalState, { LocalState } from "~/logic/state/local";
import useSettingsState from "~/logic/state/settings";
import GlobalApi from "../api/global";
import { BackgroundConfig, RemoteContentPolicy } from "~/types";

const getBackgroundString = (bg: BackgroundConfig) => {
  if (bg?.type === "url") {
    return bg.url;
  } else if (bg?.type === "color") {
    return bg.color;
  } else {
    return "";
  }
};

export function useMigrateSettings(api: GlobalApi) {
  const local = useLocalState();
  const { display, remoteContentPolicy, calm } = useSettingsState();

  return async () => {
    let promises: Promise<any>[] = [];

    if (local.hideAvatars !== calm.hideAvatars) {
      promises.push(
        api.settings.putEntry("calm", "hideAvatars", local.hideAvatars)
      );
    }

    if (local.hideNicknames !== calm.hideNicknames) {
      promises.push(
        api.settings.putEntry("calm", "hideNicknames", local.hideNicknames)
      );
    }

    if (
      local?.background?.type &&
      display.background !== getBackgroundString(local.background)
    ) {
      promises.push(
        api.settings.putEntry(
          "display",
          "background",
          getBackgroundString(local.background)
        )
      );
      promises.push(
        api.settings.putEntry(
          "display",
          "backgroundType",
          local.background?.type
        )
      );
    }

    Object.keys(local.remoteContentPolicy).forEach((_key) => {
      const key = _key as keyof RemoteContentPolicy;
      const localVal = local.remoteContentPolicy[key];
      if (localVal !== remoteContentPolicy[key]) {
        promises.push(
          api.settings.putEntry("remoteContentPolicy", key, localVal)
        );
      }
    });

    await Promise.all(promises);
    localStorage.removeItem("localReducer");
  };
}
