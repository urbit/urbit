import _ from 'lodash';

export function mapContract(contract) {
  return {
    name: contract.name,
    address: contract.address,
    abiEvents: JSON.parse(contract['abi-events']),
    specificEvents: contract['specific-events'],
    eventLogs: getUniqueOrderedLogs(contract['event-logs'])
  }
}

export function getOrderedLogs(logs) {
  if(!logs || logs.length === 0) {
    return [];
  }
  return _.orderBy(logs, 'mined.block-number', ['desc']);
}

export function getUniqueOrderedLogs(logs) {
  if(!logs || logs.length === 0) {
    return [];
  }
  return _.uniqWith(getOrderedLogs(logs), _.isEqual);
}

export function splitContracts(contracts, address) {
  const existingContracts = contracts.filter(contract => contract.address !== address);
  const currentContract = contracts.find(contract => contract.address === address);
  return { existingContracts, currentContract };
}
export function getOrderedContracts(contracts) {
  return _.orderBy(contracts, 'name', ['asc']);
}
