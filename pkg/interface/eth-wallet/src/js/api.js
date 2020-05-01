import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';

class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
    this.bindPaths = [];
  }

  bind(path, method, ship = this.authTokens.ship, appl = "eth-event-viewer", success, fail) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = window.urb.subscribe(ship, appl, path, 
      (err) => {
        fail(err);
      },
      (event) => {
        success({
          data: event,
          from: {
            ship,
            path
          }
        });
      },
      (err) => {
        fail(err);
      });
  }

  etheventviewer(data) {
    this.action("eth-event-viewer", "json", data);
  }

  action(appl, mark, data) {
    return new Promise((resolve, reject) => {
      window.urb.poke(ship, appl, mark, data,
        (json) => {
          resolve(json);
        }, 
        (err) => {
          reject(err);
        });
    });
  }

  getAbi(address) {
    api.action('eth-event-viewer', 'json', {
      'get-abi': {
        address
      }
    })
  }

  removeContract(address) {
    api.action('eth-event-viewer', 'json', {
      'remove-contract': {
        address
      }
    })
  }

  newContract(contract) {
    api.action('eth-event-viewer', 'json', {
      'add-contract': {
        address: contract.address,
        name: contract.name,
        'specific-events': contract.specificEvents,
        'abi-events': contract.abiEvents ? JSON.stringify(contract.abiEvents) : null,
        'event-logs': null,
      }
    })
  }

  setShowAllEvents(address, value) {
    const currFilter = store.state.eventFilters.find(filter => filter.address === address) || {address, showAllEvents:true, filters:[]};
    store.handleEvent({
      data: {
        local: {
          'eventFilters': [
            ...store.state.eventFilters.filter(filter => filter.address !== address),
            {
              ...currFilter,
              showAllEvents: value,
            }
          ]
        }
      }
    });
  }

  setEventFilters(address, value) {
    const currFilter = store.state.eventFilters.find(filter => filter.address === address) || {address, showAllEvents:true, filters:[]};
    store.handleEvent({
      data: {
        local: {
          'eventFilters': [
            ...store.state.eventFilters.filter(filter => filter.address !== address),
            {
              ...currFilter,
              filters: value,
            }
          ]
        }
      }
    });
  }
}
export let api = new UrbitApi();
window.api = api;
