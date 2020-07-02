import React, { Component } from 'react';
import ConnectLedger from './lib/connectLedger';

export class AddWallet extends Component {
  render() {
    const { props } = this;
    return (
      <div>
        <p className="f8 mt3 lh-copy db white-d">Mnemonic seed</p>
        <textarea
          className={
            'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
            'focus-b--black focus-b--white-d'
          }
          rows={1}
          placeholder="benefit crew supreme gesture quantum web media hazard theory mercy wing kitten"
          style={{
            resize: 'none',
            height: 48,
            paddingTop: 14
          }}
          onChange={props.loadMnemonic}
        />
        <p className="f8 mt3 lh-copy db">Extended Public Key</p>
        <div className="mono wrap">
          <textarea
            className={
              'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
              'focus-b--black focus-b--white-d'
            }
            rows={1}
            placeholder={props.xpubkey}
            style={{
              resize: 'none',
              height: 48,
              paddingTop: 14
            }}
          />
        </div>
        <ConnectLedger
          loadXPubKey={props.loadFromLedger}
          network={props.network}
        />
      </div>
    );
  }
}

export default AddWallet;
