import React, { Component } from 'react';
import { EventsSelection } from './lib/events-selection';
import { Link } from 'react-router-dom';
import _ from 'lodash';
import web3Utils from 'web3-utils';
import { api } from '/api';

const initialState = {
  address: '',
  name: '',
  abiEvents: '',
  specificEvents: [],
  validAddress: false,
};

export class NewContract extends Component {
  constructor(props) {
    super(props);
    this.state = initialState;
    this.handleContractChangeBound = this.handleContractChange.bind(this);
    this.handleNameChangeBound = this.handleNameChange.bind(this);
  }

  componentDidUpdate(prevProps) {
    if(this.props.abi && this.props.abi !== prevProps.abi) {
      this.setState({abiEvents: this.props.abi.filter(topics => topics.type === 'event')})
    }
  }

  render() {
    const { address, abiEvents, name } = this.state;
    return (<div className="flex flex-column pa3">
      <div className="flex flex-row flex-wrap">
        <div className="w-100-s">
          <p className="f8 mt3 lh-copy db mb2">Contract Address</p>
          <textarea
            id="name"
            className="ba b--black-20 pa3 db w-70 b--gray4 f9 flex-basis-full-s focus-b--black focus-b--white-d
                      w-382-px w-auto-s"
            rows={1}
            placeholder="Beginning with 0x..."
            value={address}
            style={{ resize: 'none' }}
            onChange={this.handleContractChangeBound}
            aria-describedby="name-desc"
          />
          {this.renderInputStatus()}
          <p className="f8 mt3 lh-copy db mb2">Name<span className="gray3"> (Optional)</span></p>
          <textarea
            id="name"
            className="ba b--black-20 pa3 db w-70 b--gray4 f9 flex-basis-full-s focus-b--black focus-b--white-d
                      w-382-px w-auto-s"
            rows={1}
            placeholder="My Contract Name"
            value={name}
            style={{ resize: 'none' }}
            onChange={this.handleNameChangeBound}
            aria-describedby="name-desc"
          />
        </div>
        <div className="ml8 mt3 w-100-s ml0-s">
          <EventsSelection
            onEventsChanged={selectedEvents => {
              this.setState({specificEvents:selectedEvents})
            }}
            abi={abiEvents}/>
        </div>
      </div>
      <div className="flex mt3">
        <Link to="/~eth-event-viewer">
          <button className="db f9 green2 ba pa2 b--green2 bg-gray0-d pointer"
                  onClick={() => this.accept() }>
            Add Contract
          </button>
        </Link>
        <Link to="/~eth-event-viewer">
        <button className="f9 ml3 ba pa2 b--black pointer bg-transparent b--white-d white-d"
                onClick={() => this.setState({...initialState})}>
          Cancel
        </button>
        </Link>
      </div>
    </div>)
  }

  accept() {
    if(this.props.contracts.some(contract => contract.address === this.state.address)) {
      console.error('Contract already added.');
    } else if(this.state.address && this.state.validAddress && this.state.abiEvents) {
      this.props.onAcceptClicked(this.state);
      this.setState({...initialState});
    } else {
      console.error('No valid address or abi data...');
      this.setState({...initialState});
    }
  }

  renderInputStatus() {
    if(!this.state.validAddress && this.state.address) {
      return (<span className="f9 inter red2 db pt2">Must be a valid contract address.</span>);
    }
    return null;
  }

  isValidAddress(address) {
    return web3Utils.isAddress(address);
  };

  validateContractAddress(address) {
    if(!address){
      return
    }
    if(!this.isValidAddress(address)) {
      this.setState({validAddress:false});
    } else {
      api.getAbi(address);
      this.setState({validAddress:true});
    }
  }

  handleContractChange(event) {
    const address = event.target.value;
    _.debounce(() => this.validateContractAddress(address), 100)();
    this.setState({ address });
  }

  handleNameChange(event) {
    this.setState({ name: event.target.value });
  }
}
