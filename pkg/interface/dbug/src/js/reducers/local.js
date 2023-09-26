import _ from 'lodash';

export class LocalReducer {
  reduce(json, state) {
    const data = _.get(json, 'local', false);
    if (data) {
      this.status(data, state);
    //
      this.apps(data, state);
      this.app(data, state);
      this.appState(data, state);
      this.appFailed(data, state);
      this.verbResult(data, state);
      this.verbStatus(data, state);
    //
      this.threads(data, state);
    //
      this.amesPeers(data, state);
      this.amesPeer(data, state);
    //
      this.behnTimers(data, state);
    //
      this.clayCommits(data, state);
    //
      this.eyreBindings(data, state);
      this.eyreConnections(data, state);
      this.eyreAuthentication(data, state);
      this.eyreChannels(data, state);
    }
  }

  status(obj, state) {
    const data = _.get(obj, 'status', false);
    if (data) {
      state.status = data;
    }
  }

  // apps

  apps(obj, state) {
    const data = _.get(obj, 'apps', false);
    if (data) {
      Object.keys(data).map(app => {
        if (!state.apps[app]) {
          state.apps[app] = data[app];
        }
      });
    }
  }

  app(obj, state) {
    const data = _.get(obj, 'app', false);
    if (data) {
      if (state.apps[data.app]) data.state = state.apps[data.app].state;
      state.apps[data.app] = data;
    }
  }

  appState(obj, state) {
    const data = _.get(obj, 'appState', false);
    if (data) {
      state.apps[data.app].state = data.state;
    }
  }

  appFailed(obj, state) {
    const data = _.get(obj, 'appFailed', false);
    if (data) {
      console.log('loading app deets failed', data);
      state.apps[data] = { noDebug: true };
    }
  }

  verbResult(obj, state) {
    const data = _.get(obj, 'verbResult', false);
    if (data) {
      if (!state.apps[data.app]) state.apps[data.app] = {};
      if (!state.apps[data.app].events) state.apps[data.app].events = [];
      let msg = 'some event';
      if (data['on-init'])  msg = '+on-init';
      if (data['on-load'])  msg = '+on-load';
      if (data['on-poke'])  msg = '+on-poke with mark ' + data['on-poke'];
      if (data['on-watch']) msg = '+on-watch at path ' + data['on-watch'];
      if (data['on-leave']) msg = '+on-leave on path ' + data['on-leave'];
      if (data['on-agent']) msg = '+on-agent at wire ' + data['on-agent'].wire +
                                  ' with sign ' + data['on-agent'].sign;
      if (data['on-arvo'])  msg = '+on-arvo at wire ' + data['on-arvo'].wire +
                                  ' from vane ' + data['on-arvo'].vane +
                                  ' with sign ' + data['on-arvo'].sign;
      if (data['on-fail'])  msg = '+on-fail on ' + data['on-fail'];
      state.apps[data.app].events.push(msg);
    }
  }

  verbStatus(obj, state) {
    const data = _.get(obj, 'verbStatus', false);
    if (data) {
      if (!state.apps[data.app])      state.apps[data.app] = {};
      if (!state.apps[data.app].events) state.apps[data.app].events = [];
      state.apps[data.app].events.push(data.msg);
    }
  }

  // spider

  threads(obj, state) {
    const data = _.get(obj, 'threads', false);
    if (data) {
      state.threads = data;
    }
  }

  // ames

  amesPeers(obj, state) {
    const data = _.get(obj, 'amesPeers', false);
    if (data) {
      state.peers.known = data.known;
      state.peers.alien = data.alien;
    }
  }

  amesPeer(obj, state) {
    const data = _.get(obj, 'amesPeer', false);
    if (data) {
      state.peers.deets[data.who] = data;
    }
  }

  // behn

  behnTimers(obj, state) {
    const data = _.get(obj, 'behnTimers', false);
    if (data) {
      state.timers = data;
    }
  }

  // clay

  clayCommits(obj, state) {
    const data = _.get(obj, 'clayCommits', false);
    if (data) {
      console.log('clay comms', data);
      state.commits = data;
    }
  }

  // eyre

  eyreBindings(obj, state) {
    const data = _.get(obj, 'eyreBindings', false);
    if (data) {
      state.bindings = data;
    }
  }

  eyreConnections(obj, state) {
    const data = _.get(obj, 'eyreConnections', false);
    if (data) {
      state.connections = data;
    }
  }

  eyreAuthentication(obj, state) {
    const data = _.get(obj, 'eyreAuthentication', false);
    if (data) {
      state.authentication = data;
    }
  }

  eyreChannels(obj, state) {
    const data = _.get(obj, 'eyreChannels', false);
    if (data) {
      state.channels = data;
    }
  }
}