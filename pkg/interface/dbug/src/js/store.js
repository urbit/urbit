import { LocalReducer } from './reducers/local.js';
import _ from 'lodash';

class Store {
  constructor() {
    this.state = {
      status: null,
      apps: {},
      logs: {},
      logsRange: { oldest: null, newest: null },
      threads: {},
      peers: { known: [], alien: [], deets: {}},
      chums: { known: [], alien: [], deets: {}},
      timers: [],
      commits: [],
      bindings: [],
      cache: [],
      connections: [],
      authentication: {
        sessions: [],
        visitors: [],
        visiting: [],
      },
      channels: [],
      sidebarShown: true
    };

    this.localReducer = new LocalReducer();
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json;
    if (data.data) {
      json = data.data;
    } else {
      json = data;
    }

    console.log('event', json);
    this.localReducer.reduce(json, this.state);

    this.setState(this.state);
  }

  addLogs(gill, start, newLogs) {
    let agent = this.state.logs[gill];
    let logsRange = this.state.logsRange;

    if (!agent) {
      agent = {
        logs: newLogs,
        oldest: newLogs[0].now,
        newest: newLogs[newLogs.length - 1].now,
      }
      logsRange.oldest = Math.min(logsRange.oldest || agent.oldest, agent.oldest);
      logsRange.newest = Math.max(logsRange.newest || agent.newest, agent.newest);
    } else if (start < (agent.oldest || start + 1)) {
      agent.logs = [...newLogs, ...agent.logs];
      agent.oldest = newLogs[0].now;
      logsRange.oldest = Math.min(logsRange.oldest || agent.oldest, agent.oldest);
    } else if (start > agent.newest) {
      agent.logs = [...agent.logs, ...newLogs];
      agent.newest = newLogs[newLogs.length - 1].now;
      logsRange.newest = Math.max(logsRange.newest || agent.newest, agent.newest);
    } else {
      console.log(`assuming duplicate load for ${gill}, ignoring logs at ${start}`);
    }

    this.state.logs[gill] = agent;
    this.state.logsRange = logsRange;
    this.setState(this.state);
  }

  clearLogs() {
    this.state.logs = {};
    this.state.logsRange = { oldest: null, newest: null };
    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
