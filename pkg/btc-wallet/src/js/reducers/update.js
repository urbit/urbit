import _ from 'lodash';


export class UpdateReducer {
  reduce(json, state) {
    console.log('update', json);
    if (json.providerStatus) {
      this.reduceProviderStatus(json.providerStatus, state);
    }
    if (json.connected) {
      this.reduceConnected(json.connected, state);
    }
    if (json.psbt) {
      this.reducePsbt(json.psbt, state);
    }
  }

  reduceProviderStatus(json, state) {
    state.providerPerms[json.provider] = json.permitted;
  }

  reduceConnected(json, state) {
    state.provider = true;
  }

  reducePsbt(json, state) {
    state.psbt = json;
  }
}
