import _ from 'lodash';
import { isDMStation, isRootCollection, getMessageContent } from '/lib/util';

const INBOX_MESSAGE_COUNT = 30;

export class MessagesReducer {
  reduce(reports, store) {
    reports.forEach((rep) => {
      let fromCircle = rep.from && rep.from.path.split("/")[2];
      let fromInbox = fromCircle === "inbox";

      switch (rep.type) {
        case "circle.nes":
          this.processMessages(rep.data, store);
          break;
        case "circle.gram":
          this.processMessages([rep.data], store);
          break;
        case "circle.config.dif.remove":
          delete store.messages.stations[rep.data.cir];
          break;
        case "circle.cos.loc":
          if (fromInbox) {
            store.messages.inbox.config = rep.data;
            store.messages.inbox.src = rep.data.src;
            this.storeInboxMessages(store);
          }
          break;
        case "circle.config.dif.source":
          if (fromInbox) {
            if (rep.data.add) {
              store.messages.inbox.src = [...store.messages.inbox.src, rep.data.src];
            } else {
              store.messages.inbox.src = store.messages.inbox.src.filter(src => src !== rep.data.src);
            }
            this.storeInboxMessages(store);
          }
          break;

        case "circle.config":
          fromInbox = rep.data.cir.includes("inbox");
          if (fromInbox && _.get(rep.data, 'dif.source', null)) {
            if (rep.data.dif.source.add) {
              store.messages.inbox.src = [...store.messages.inbox.src, rep.data.dif.source.src];
            } else {
              store.messages.inbox.src = store.messages.inbox.src.filter(src => src !== rep.data.dif.source.src);
            }
            this.storeInboxMessages(store);
          }

          break;

        case "landscape.prize":
          if (rep.data.inbox) {
            store.messages.inbox.src = [...store.messages.inbox.src, ...rep.data.inbox.config.src];
            store.messages.inbox.config = rep.data.inbox.config;
            this.processMessages(rep.data.inbox.messages, store);
            this.processMessages(rep.data.invites, store);
            this.storeInboxMessages(store);
          } else {
            console.log("WEIRD: no inbox property in landscape.prize?")
          }

          // if (fromInbox) {
          //   if (rep.data.add) {
          //     store.messages.inbox.src = [...store.messages.inbox.src, rep.data.src];
          //   } else {
          //     store.messages.inbox.src = store.messages.inbox.src.filter(src => src !== rep.data.src);
          //   }
          //   this.storeInboxMessages(store);
          // }
          break;

        case "dm.new": {
          store.messages.notifications = [...store.messages.notifications, ...rep.data];
          break;
        }

        case "dm.clear": {
          store.messages.notifications = store.messages.notifications.filter(n => !rep.data.includes(n.uid));
          break;
        }
      }
    });
  }

  processMessages(messages, store) {
    let msgs = messages.filter(m => {
      return !m.gam.aud.some(st => isRootCollection(st));
    });
    this.storeStationMessages(msgs, store);
    this.storeInboxMessages(store);
  }

  // TODO:  Make this more like storeInboxMessages
  storeStationMessages(messages, store) {
    messages.forEach((message) => {
      let msg = message.gam;
      msg.num = message.num;
      msg.aud.forEach((aud) => {
        let msgClone = { ...msg, aud: [aud] };
        let station = store.messages.stations[aud]

        if (!station) {
          store.messages.stations[aud] = [msgClone];
        } else if (station.findIndex(o => o.uid === msgClone.uid) === -1) {
          let newest = true;

          for (let i = 0; i < station.length; i++) {
            if (msgClone.wen < station[i].wen) {
              station.splice(i, 0, msgClone);
              newest = false;
              break;
            }
          }

          if (newest) station.push(msgClone);

          // Print messages by date, for debugging:
          // for (let msgClone of station.messages) {
          //   console.log(`msgClone ${msg.uid}: ${msg.wen}`);
          // }
        }
      })
    });
  }

  storeInboxMessages(store) {
    let messages = store.messages.inbox.src.reduce((msgs, src) => {
      let msgGroup = store.messages.stations[src];
      if (!msgGroup) return msgs;
      return msgs.concat(msgGroup.filter(this.filterInboxMessages));  // filter out app & accepted invite msgs
    }, []);

    let ret = _(messages)
      .sort((a, b) => b.wen - a.wen)    // sort by date
      // sort must come before uniqBy! if uniqBy detects a dupe, it takes
      // earlier element in the array. since we want later timestamps to
      // override, sort first
      .uniqBy('uid')                    // dedupe
      .slice(0, INBOX_MESSAGE_COUNT)    // grab the first 30 or so
      .value();                         // unwrap lodash chain
    // for (let msg of ret) {
    //   console.log(`msg ${msg.uid}: ${msg.wen}`);
    // }

    // store.messages.inbox.messages = [
    //   {
    //     aud: ["~zod/marzod.zod"],
    //     aut: "zod",
    //     sep: { lin: {
    //       msg: "Hey marzod!"
    //     }},
    //     uid: "0v4.85q7h.25nnt.5mhop.92c1u.3rhsa",
    //     wen: 1538084786999,
    //   }, {
    //     aud: ["~zod/marzod.zod"],
    //     aut: "marzod",
    //     sep: { lin: {
    //       msg: "oh hey zod"
    //     }},
    //     uid: "0v4.85q7h.25nnt.5mhop.92c1u.3rhfa",
    //     wen: 1538084787000,
    //   },
    //   ...ret
    // ];
    store.messages.inbox.messages = ret;
  }

  // Filter out of inbox:
  //   - app messages
  //   - accepted invites
  //   - all DM invites (should automatically accept)
  filterInboxMessages(msg) {
    let msgDetails = getMessageContent(msg);
    let typeApp = msgDetails.type === "app";
    let typeInv = msgDetails.type === "inv";
    // let isDmInvite = typeInv && isDMStation(msgDetails.content);
    let isInboxMsg = msg.aud[0].split("/")[1] === "inbox";
    let isEditUpdate = msgDetails.type === "edited item";
    // let hasResponded = typeInv && msgDetails.content === "~zod/null";

    if (typeApp) return false;
    if (typeInv) return false;
    // if (hasResponded) return false;
    if (isEditUpdate) return false;
    if (isInboxMsg) return false;

    return true;
  }
}
