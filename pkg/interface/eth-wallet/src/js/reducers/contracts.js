import _ from 'lodash';
import { getOrderedContracts, getOrderedLogs, getUniqueOrderedLogs, mapContract, splitContracts } from './utils';

export class ContractsReducer {
  reduce(json, state) {
    let data = json;
    if (data) {
      this.newContract(data, state);
      this.removeContract(data, state);
      this.contracts(data, state);
      this.abi(data, state);
      this.eventLog(data, state);
      this.history(data, state);
    }
  }

  removeContract(obj, state) {
    let data = _.get(obj, 'remove-contract', false);
    if (data) {
      state.contracts = getOrderedContracts(state.contracts.filter(contract => contract.address !== data));
    }
  }

  newContract(obj, state) {
    let data = _.get(obj, 'new-contract', false);
    if (data) {
      const newContract = {
        name: data.name,
        address: data.address,
        abiEvents: JSON.parse(data['abi-events']),
        specificEvents: data['specific-events'],
        eventLogs: getUniqueOrderedLogs(data['event-logs'])
      };
      state.contracts = getOrderedContracts([
        ...state.contracts,
        newContract
      ]);
    }
  }

  contracts(obj, state) {
    let data = _.get(obj, 'contracts', false);
    if (data) {
      state.contracts = getOrderedContracts(data.map(contract => mapContract(contract)));
    }
  }

  abi(obj, state) {
    let data = _.get(obj, 'abi-result', false);

    if (data) {
      state.abi = data && JSON.parse(data);
    }
  }

  eventLog(obj, state) {
    let data = _.get(obj, 'event-log', false);
    if (data) {
      const eventLog = data;
      const { existingContracts, currentContract } = splitContracts(state.contracts, eventLog.address);
      if (currentContract) {
        this.setContractsState(state, existingContracts, currentContract, eventLog)
      }
    }
  }

  setContractsState(state, existingContracts, currentContract, eventLog, unique) {
    const currentLogs = currentContract.eventLogs || []
    const logs = [...currentLogs, eventLog];

    const updatedContract = {
      ...currentContract,
      eventLogs: getOrderedLogs(logs)
    };
    state.contracts = getOrderedContracts([...existingContracts, updatedContract]);
  }

  history(obj, state) {
    let history = _.get(obj, 'history', false);
    if (history && history[0].address) {
      const address = history[0].address;

      const { existingContracts, currentContract } = splitContracts(state.contracts, address);
      const updatedContract = {
        ...currentContract,
        eventLogs: getUniqueOrderedLogs(history)
      };
      if (currentContract) {
        state.contracts = getOrderedContracts([...existingContracts, updatedContract]);
      }
    }
  }
}
