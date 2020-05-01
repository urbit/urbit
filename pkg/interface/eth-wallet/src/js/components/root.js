import React, { Component } from 'react';
import { BrowserRouter, Switch, Route } from 'react-router-dom';
import { api } from '/api';
import { store } from '/store';
import { NewContract } from './new';
import { Skeleton } from './skeleton';
import { EventLogs } from './log';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    const { contracts, eventFilters } = this.state;
    return (
      <BrowserRouter>
        <Switch>
          <Route
            exact
            path="/~eth-event-viewer"
            render={() => {
              return (
                <Skeleton contracts={contracts}>
                  {this.renderBaseViewContent()}
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~eth-event-viewer/new"
            render={() => {
              return (
                <Skeleton contracts={this.state.contracts}>
                  <NewContract
                    abi={this.state.abi}
                    contracts={contracts}
                    onAcceptClicked={contract => api.newContract(contract)}
                  />
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~eth-event-viewer/:contract"
            render={props => {
              return (
                <Skeleton
                  selectedContract={props.match.params.contract}
                  contracts={contracts}
                >
                  <EventLogs
                    contract={contracts && contracts.find(contract => contract.address === props.match.params.contract)}
                    filterOptions={eventFilters.find(filter => filter.address === props.match.params.contract)}
                  />
                </Skeleton>
              );
            }}
          />
        </Switch>
      </BrowserRouter>
    );
  }

  renderBaseViewContent() {
    const { contracts } = this.state;
    let message = 'There are no contracts, feel free to add one.';
    if (contracts && contracts.length > 0) {
      message = 'Please select a contract.';
    }
    return <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
      <p className="f9 pt3 gray2 w-100 h-100 dtc v-mid tc">{message}</p>
    </div>
  }
}
