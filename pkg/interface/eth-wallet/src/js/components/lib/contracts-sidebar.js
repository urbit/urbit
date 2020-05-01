import React, { Component } from 'react';
import { Link } from 'react-router-dom';

export class ContractsSidebar extends Component {
  render() {
    return (
      <div className="ba bl-0 bt-0 bb-0 b--solid b--gray4 b--gray1-d w-320-px w-100-s ba-0-s"
      >
        <div
          className="w-100 bg-transparent pa4 bb b--gray4 b--gray1-d"
          style={{ paddingBottom: '13px' }}
        >
          <Link to="/~eth-event-viewer/new">
            <p className="dib f9 pointer green2 gray4-d mr4">New Contract</p>
          </Link>
        </div>
        {this.renderContractsList()}
      </div>
    );
  }

  renderContractsList() {
    const { contracts } = this.props;

    if (!contracts) {
      return null;
    }
    return (
      <ul className="list pl0 ma0 mh-134-s">
        {contracts.map(contract => {
          return (
            <Link
              to={`/~eth-event-viewer/${contract.address}`}
              key={contract.address + contract.name}
            >
              {this.renderListItem(contract)}
            </Link>
          );
        })}
      </ul>
    );
  }

  renderListItem(contract) {
    const { selectedContract } = this.props;
    return (
      <li
        className={`lh-copy pl3 pv3 ba bl-0 bt-0 br-0 b--solid b--gray4 b--gray1-d bg-animate pointer ${
          selectedContract === contract.address ? 'bg-gray5' : 'bg-white'
        }`}
      >
        <div>
          {contract.name && <p className="f8">{contract.name}</p>}
          <p className="f9 gray3">{contract.address}</p>
        </div>
      </li>
    );
  }
}
