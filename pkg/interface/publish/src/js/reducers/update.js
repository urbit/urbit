import _ from 'lodash';

export class UpdateReducer {
  reduce(json, state){
    if (json.invite) {
      this.reduceInvite(json.invite, state);
    } else if (json.unread) {
      this.reduceUnread(json.unread, state);
    }
  }

  reduceInvite(json, state) {
    let val = {
      title: json.title,
      coll:  json.coll,
      who:   json.who,
    };
    if (json.add) {
      state.invites.push(val);
    } else {
      let idx = _.findIndex(state.invites, val)
      _.pullAt(state.invites, [idx]);
    }
  }

  reduceUnread(json, state) {
    if (json.add) {
      state.unread = _.uniq(state.unread.concat(json.posts));
    } else {
      let idx = json.posts.map((val) => {
        return _.findIndex(state.unread, val);
      });
      _.pullAt(state.unread, idx);
    }
  }
}
