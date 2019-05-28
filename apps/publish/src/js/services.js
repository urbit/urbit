import { isDMStation } from '/lib/util';
import { warehouse } from '/warehouse';
import { api } from '/api';

export function getStationDetails(station) {
  let host = station.split("/")[0].substr(1);
  let config = warehouse.store.configs[station];

  let ret = {
    type: "none",
    station: station,
    host: host,
    cir: station.split("/")[1],
  };

  let circleParts = ret.cir.split("-");

  if (ret.cir === "c") {
    ret.type = "aggregator";
  } else if (isDMStation(station)) {
    ret.type = "stream-dm";
  } else {
    ret.type = "stream-chat";
  }

  switch (ret.type) {
    case "stream-chat":
      ret.stationUrl = `/~chat/${station}`;
      ret.stationTitle = ret.cir;
      break;
    case "stream-dm":
      ret.stationTitle = ret.cir
        .split(".")
        .filter((mem) => mem !== api.authTokens.ship)
        .map((mem) => `~${mem}`)
        .join(", ");;
      ret.stationUrl = `/~landscape/stream?station=${station}`;
      break;
  }

  return ret;
}

