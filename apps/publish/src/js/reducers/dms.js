//       let newSep = {
//         sep: {
//           inv: {
//             inv: true,
//             cir: "~zod/null"
//           }
//         },
//         wen: (new Date()).getTime()
//       };

// import { isDMStation, getMessageContent } from '/lib/util';
// import _ from 'lodash';
//
// export class DmsReducer {
//   reduce(reports, store) {
//     reports.forEach((rep) => {
//       switch (rep.type) {
//         case "circles":
//           if (_.isArray(rep.data)) {
//             let newStations = rep.data.filter(station => isDMStation(`${rep.from.ship}/${station}`));
//             store.dms.stations = _.uniq([...store.dms.stations, ...newStations]);
//             store.dms.stored = true;
//           } else if (rep.data.cir) {
//             if (rep.data.add) {
//               store.dms.stations = _.uniq([...store.dms.stations, rep.data.cir]);
//             } else {
//               store.dms.stations = _.filter(store.dms.stations, s => s !== rep.data.cir);
//             }
//           }
//           break;
//
//         case "circle.gram":
//           this.addStationsFromInvites([rep.data], store);
//           break;
//
//         case "circle.nes":
//           this.addStationsFromInvites(rep.data, store);
//           break;
//       }
//     });
//   }
//
//   addStationFromInvite(msgs, store) {
//     let inviteStations = [];
//     msgs.forEach(msg => {
//       let msgContent = getMessageContent(msg);
//       if (msgContent.type === "inv" && isDMStation(msgContent.content.sta)) {
//         inviteStations.push(msgContent.content.sta);
//       }
//     });
//
//     store.dms.stations = [...store.dms.stations, ...inviteStations];
//   }
// }
