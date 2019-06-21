import _ from 'lodash';

export class UpdateReducer {
  reduce(json, state){
    if (json.invite) {
      this.reduceInvite(json, state);
    }
  }

  reduceInvite(json, state) {
    let val = {
      title: json.invite.title,
      coll:  json.invite.coll,
      who:   json.invite.who,
    };
    if (json.invite.add) {
      state.invites.push(val);
    } else {
      let idx = _.findIndex(state.invites, val)
      _.pullAt(state.invites, [idx]);
    }
    
  }
}
