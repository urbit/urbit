import { getStationDetails } from '/services';
import _ from 'lodash';

export class NamesReducer {
  reduce(reports, store) {
    reports.forEach((rep) => {
      let ships = {};
      let details;

      switch (rep.type) {
        case "circle.cos.loc":
          ships[rep.from.ship] = [rep.from.path.split("/")[2]];
          rep.data.con.sis.forEach((mem) => ships[mem] = []);

          this.storeNames(ships, store.names);
          break;

        case "circle.cos.rem":
          Object.arrayify(rep.data).forEach(({key: station, value: config}) => {
            let details = getStationDetails(station);

            if (ships[details.host]) {
              ships[details.host] = _.uniq(ships[details.host].concat(details.cir));
            } else {
              ships[details.host] = [details.cir];
            }

            config.con.sis.forEach((mem) => {
              if (!ships[mem]) ships[mem] = [];
            });
          });
          this.storeNames(ships, store.names);
          break;
        case "circle.nes":
          this.storeMessagesNames(rep.data, store.names);
          break;
        case "circle.gram":
          this.storeMessagesNames([rep.data], store.names);
          break;
        // case "circle.pes.loc":
        //   stationName = `~${rep.from.ship}/${rep.from.path.split("/")[2]}`;
        //   this.updateConfig({pes: rep.data}, store.configs[stationName]);
        //   break;
        case "circle.config.dif.source":
          details = getStationDetails(rep.data.src);
          ships[details.host] = [details.cir];
          this.storeNames(ships, store.names);
          break;
        case "circle.config.dif.full":
          details = getStationDetails(rep.data.src[0]); // TODO:  API weirdness; we have to get name of new station from new station config's src property. Should maybe return a dict.
          ships[details.host] = [details.cir];
          this.storeNames(ships, store.names);
          break;
        case "circle.config.dif.permit":  // TODO:  This is very wonky, should be fixed with API discussion
          details = getStationDetails(rep.data.cir); // TODO:  API weirdness; we have to get name of new station from new station config's src property. Should maybe return a dict.
          ships[details.host] = [details.cir];
          this.storeNames(ships, store.names);
        // case "circle.config.dif.remove":
        //   delete store.names[rep.data.cir];
        //   break;
      }
    });
  }

  storeMessagesNames(messages, storeNames) {
    let ships = {};

    messages.forEach((message) => {
      let msg = message.gam;
      msg.aud.forEach((aud) => {
        let details = getStationDetails(aud); // TODO:  API weirdness; we have to get name of new station from new station config's src property. Should maybe return a dict.
        ships[details.host] = [details.cir];
      });

      ships[msg.aut] = ships[msg.aut] || [];
    });

    this.storeNames(ships, storeNames);
  }

  storeNames(ships, storeNames) {
    Object.arrayify(ships).forEach(({key: ship, value: stations}) => {
      let sttns = stations.filter(s => s !== "c");

      if (!storeNames[ship]) {
        storeNames[ship] = sttns;
      } else {
        storeNames[ship] = _.uniq(sttns.concat(storeNames[ship]))
      }
    });
  }
}
