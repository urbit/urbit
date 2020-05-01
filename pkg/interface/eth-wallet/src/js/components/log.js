import React, { Component } from 'react';
import { getEventHashPairs } from '../lib/events';
import { Filter } from './lib/filter';
import _ from 'lodash';
import { api } from '/api';
import { getUniqueOrderedLogs } from '../reducers/utils';
import { Link } from 'react-router-dom';


export class EventLogs extends Component {
  constructor(props) {
    super(props);
    this.state = {
      contract: props.contract
    }
    this.updateContractWithUniqueEventsBound = this.updateContractWithUniqueEvents.bind(this);
    this.updateContractEventsDebounce = _.debounce(this.updateContractWithUniqueEventsBound, 300);
  }

  componentDidUpdate(prevProps) {
    const prevContract = prevProps.contract;
    const { contract } = this.props;
    if (prevContract && contract) {
      if (prevContract.address !== contract.address) {
        this.setState({ contract });
      }
      if (prevContract.eventLogs.length !== contract.eventLogs.length) {
        // render only after received a bulk of new events, not every new event
        // furthermore events are checked for uniqueness since it might be possible to receive them twice
        this.updateContractEventsDebounce(contract);
      }
    } else if (contract) {
      this.setState({ contract })
    }
  }

  updateContractWithUniqueEvents(contract) {
    const newContract = {
      ...contract,
      eventLog: {
        ...getUniqueOrderedLogs(contract)
      }
    }
    this.setState({ contract: newContract });
  }

  render() {
    const { filterOptions } = this.props;
    const { contract } = this.state;
    if (!contract) {
      return this.renderNoDataAvailable();
    }

    let { showAllEvents, filters } = filterOptions || { filters: [], showAllEvents: true };
    const hashPairs = getEventHashPairs(contract.abiEvents);

    let logs = contract.eventLogs || [];

    if (!showAllEvents && filters.length > 0) {
      logs = this.filterLogs(logs, hashPairs, filters);
    }

    // show max 200 entries
    logs = _.take(logs, 200);

    return (<div className="h-100-minus-2 relative">
        {this.renderFilterBar(contract.address, showAllEvents, hashPairs, filters)}
        {
          logs.length > 0 ? this.renderLog(logs, hashPairs, contract) : this.renderNoDataAvailable()
        }
      </div>
    )
  }

  renderFilterBar(address, showAllEvents, hashPairs, filters) {
    return <div className="flex flex-column flex-row ba bl-0 bt-0 br-0 b--solid b--gray4 b--gray1-d overflow-scroll"
                style={{ overflowY: 'hidden' }}>
      <div className="flex flex-column flex-row" style={{ paddingRight: '75px' }}>
        <Filter label="Show all Events" isActive={!showAllEvents} onClick={() => {
          api.setShowAllEvents(address, !showAllEvents);
        }}/>
        {
          showAllEvents || (hashPairs && this.renderFilters(hashPairs, filters))
        }
      </div>
      <div className="flex flex-column flex-row absolute bg-white right-0 top-0">
        <Link
          to={`/~eth-event-viewer/`}
        >
          <div className="f9 gray3"
               style={{ padding: '16px' }}
               onClick={() => api.removeContract(address)}>
            remove
          </div>
        </Link>
      </div>
    </div>
  }

  renderLog(logs, hashPairs, contract) {
    return <div className="h-100-minus-60 overflow-auto">
      <ul className="list pl0 ma0 dt w-100">
        {
          logs
            .map((eventLog, index) => {
              return (
                <a
                  href={`https://etherscan.io/tx/${eventLog.mined['transaction-hash']}`}
                  key={contract.address + '-' + index}
                  target={'_blank'}
                >
                  {this.renderListItem(eventLog, hashPairs, contract.abiEvents)}
                </a>
              );
            })
        }
      </ul>
    </div>;
  }

  renderFilters(hashPairs, filters) {
    const { contract } = this.state;
    const specificEvents = contract.specificEvents.map(event => {
      const name = event.split('(');
      return name ? name[0] : null;
    }).filter(name => name);

    return hashPairs
      .filter(p => specificEvents.length > 0 ? specificEvents.some(ev => ev === p.name) : true)
      .map(pair => {
        return (
          <Filter key={pair.name}
                  isActive={filters.some(filter => filter === pair.name)}
                  label={pair.name}
                  onClick={() => this.toggleFilter(pair.name, filters)}/>
        )
      })
  }

  renderListItem(eventLog, hashPairs) {
    const hashPair = hashPairs.find(hash => hash.hash === eventLog.topics[0]);

    return (
      <li
        className={'lh-copy pl3 pv3 ba bl-0 bt-0 br-0 b--solid b--gray4 b--gray1-d bg-animate pointer'}
      >
        <div className="flex flex-column flex-row nowrap">
          <div key="transaction-info mw-180-px">
            <p className="f9 truncate">{hashPair ? hashPair.name : '-'}</p>
            <p className="f9 gray3">Block No. {eventLog.mined['block-number']}</p>
          </div>
          {
            this.renderEventTopics(eventLog, hashPair)
          }
        </div>
      </li>
    );
  }

  renderEventTopics(eventLog, hashPair) {
    return eventLog.topics.map((topic, index) => {
      // first index is hash of topics
      if (index === 0) {
        return null;
      }
      const topicIndex = index - 1;
      return (<div className="ml2 mw-310-px" key={topic + topicIndex}>
        <p className="f9">{hashPair && hashPair.inputs[topicIndex] && hashPair.inputs[topicIndex].name}</p>
        <p className="f9 gray3">{topic}</p>
      </div>)
    })
  }

  renderNoDataAvailable() {
    return <div className="pl3 pr3 pt2 dt pb3 w-100 h-100-minus-56">
      <div className="f9 pt3 gray2 w-100 h-100 dtc v-mid tc">
        <p className="w-100 tc mb2">No contract data available.</p>
        <p className="w-100 tc">It might need some time, take a coffee and lean back.</p>
      </div>
    </div>;
  }

  filterLogs(logs, hashPairs, filters) {
    return logs.filter(log => {
      const filterHash = filter => hashPairs.find(pair => pair.name === filter) || { hash: null };
      return !filters.some(filter => filterHash(filter).hash === log.topics[0])
    });
  }

  toggleFilter(eventName, filters) {
    if (filters.some(filter => filter === eventName)) {
      this.removeFilter(eventName, filters);
    } else {
      this.addFilter(eventName, filters);
    }
  }

  addFilter(eventName, filters) {
    api.setEventFilters(this.state.contract.address, [...filters, eventName]);
  }

  removeFilter(eventName, filters) {
    api.setEventFilters(this.state.contract.address, filters.filter(filter => filter !== eventName));
  }
}
